package App::BashCompletionF;

our $DATE = '2014-10-31'; # DATE
our $VERSION = '0.01'; # VERSION

use 5.010001;
use strict;
use warnings;

use File::Slurp::Tiny qw();
use List::Util qw(first);
use Perinci::Sub::Util qw(err);
use Text::Fragment qw();

our %SPEC;

sub _f_path {
    if ($>) {
        "$ENV{HOME}/.bash-completion-f";
    } else {
        "/etc/bash-completion-f";
    }
}

sub _read_parse_f {
    my $path = shift // _f_path();
    my $text = (-f $path) ? File::Slurp::Tiny::read_file($path) : "";
    my $listres = Text::Fragment::list_fragments(text=>$text);
    return $listres if $listres->[0] != 200;
    [200,"OK",{content=>$text, parsed=>$listres->[2]}];
}

sub _write_f {
    my $path = shift // _f_path();
    my $content = shift;
    File::Slurp::Tiny::write_file($path, $content);
    [200];
}

my %arg_file = (file => {
    summary => 'Use alternate location for the bash-completion-f file',
    schema => 'str*',
    description => <<'_',

By default, the `complete` scripts are put in a file either in
`/etc/bash-completion-f` (if running as root) or `~/.bash-completion-f` (if
running as normal user). This option sets another location.

_
    cmdline_aliases => {f=>{}},
});

my %arg_id = (id => {
    summary => 'Entry ID, for marker (usually command name)',
    schema  => ['str*', {match => $Text::Fragment::re_id}],
    req     => 1,
    pos     => 0,
});

my %arg_program = (program => {
    summary => 'Program name to add',
    schema => ['str*', {match=>$Text::Fragment::re_id}],
    req => 1,
    pos => 0,
});

$SPEC{add_entry} = {
    v => 1.1,
    summary => 'Add a completion entry',
    args => {
        %arg_id,
        content => {
            summary => 'Entry content (the actual "complete ..." bash command)',
            schema => 'str*',
            req => 1,
            pos => 1,
        },
        %arg_file,
    },
};
sub add_entry {
    my %args = @_;

    my $id = $args{id};
    my $content = $args{content};

    # XXX schema (coz when we're not using P::C there's no schema validation)
    $id =~ $Text::Fragment::re_id or
        return [400, "Invalid syntax for 'id', please use word only"];

    my $res = _read_parse_f($args{file});
    return err("Can't read entries", $res) if $res->[0] != 200;

    # avoid duplicate
    return [409, "Duplicate id '$id'"]
        if first {$_->{id} eq $id} @{$res->[2]{parsed}};

    # avoid clash with fragment marker
    $content =~ s/^(# (?:BEGIN|END) FRAGMENT)/ $1/gm;

    my $insres = Text::Fragment::insert_fragment(
        text=>$res->[2]{content}, id=>$id, payload=>$content);
    return err("Can't add", $insres) if $insres->[0] != 200;

    my $writeres = _write_f($args{file}, $insres->[2]{text});
    return err("Can't write", $writeres) if $writeres->[0] != 200;

    [200];
}

$SPEC{add_entry_pc} = {
    v => 1.1,
    summary => 'Add a completion entry (shortcut for Perinci::CmdLine-based CLI programs)',
    description => <<'_',

This is a shortcut for `add_entry`. Doing:

    % bash-completion-f add-pc foo

will be the same as:

    % bash-completion-f add --id foo 'complete -C foo foo'

_
    args => {
        %arg_program,
        %arg_file,
    },
};
sub add_entry_pc {
    my %args = @_;
    my $prog = $args{program};

    # XXX schema (coz when we're not using P::C there's no schema validation)
    $prog =~ $Text::Fragment::re_id or
        return [400, "Invalid syntax for 'program', ".
                    "please don't use strange characters"];

    add_entry(
        id => $prog,
        content => "complete -C '$prog' '$prog'",
    );
}

$SPEC{remove_entry} = {
    v => 1.1,
    summary => '',
    args => {
        %arg_id,
        %arg_file,
    },
};
sub remove_entry {
    my %args = @_;

    my $id = $args{id};

    # XXX schema (coz when we're not using P::C there's no schema validation)
    $id =~ $Text::Fragment::re_id or
        return [400, "Invalid syntax for 'id', please use word only"];

    my $res = _read_parse_f($args{file});
    return err("Can't read entries", $res) if $res->[0] != 200;

    my $delres = Text::Fragment::delete_fragment(
        text=>$res->[2]{content}, id=>$id);
    return err("Can't delete", $delres) if $delres->[0] !~ /200|304/;

    if ($delres->[0] == 200) {
        my $writeres = _write_f($args{file}, $delres->[2]{text});
        return err("Can't write", $writeres) if $writeres->[0] != 200;
    }

    [200];
}

$SPEC{list_entries} = {
    v => 1.1,
    summary => '',
    args => {
        %arg_file,
        detail => {
            schema => 'bool',
        },
    },
};
sub list_entries {
    my %args = @_;

    my $res = _read_parse_f($args{file} // _f_path());
    return $res if $res->[0] != 200;

    my @res;
    for (@{ $res->[2]{parsed} }) {
        if ($args{detail}) {
            push @res, {id=>$_->{id}, payload=>$_->{payload}};
        } else {
            push @res, $_->{id};
        }
    }

    [200, "OK", \@res];
}

sub _parse_entry {
    require Parse::CommandLine;

    my $payload = shift;
    $payload =~ /^(complete\s.+)/m # XXX support multiline 'complete' command
        or return [500, "Can't find 'complete' command"];

    my @argv = Parse::CommandLine::parse_command_line($1)
        or return [500, "Can't parse 'complete' command"];

    # strip options that take argument. XXX very rudimentary, should be more
    # proper (e.g. handle bundling).
    my $i = 0;
    while ($i < @argv) {
        if ($argv[$i] =~ /\A-[oAGWFCXPS]/) {
            splice(@argv, $i, 2);
            next;
        }
        $i++;
    }
    shift @argv; # strip 'complete' itself
    # XXX we just assume the names are at the end, should've stripped options
    # more properly
    my @names;
    for (reverse @argv) {
        last if /\A-/;
        push @names, $_;
    }

    [200, "OK", {names=>\@names}];
}

$SPEC{clean_entries} = {
    v => 1.1,
    summary => 'Delete entries for commands that are not in PATH',
    description => <<'_',

Sometimes when a program gets uninstalled, it still leaves completion entry.
This subcommand will search all entries for commands that are no longer found in
PATH and remove them.

_
    args => {
        %arg_file,
    },
};
sub clean_entries {
    require File::Which;

    my %args = @_;

    my $res = _read_parse_f($args{file});
    return err("Can't read entries", $res) if $res->[0] != 200;

    my $content = $res->[2]{content};
    my $deleted;
    for my $entry (@{ $res->[2]{parsed} }) {
        my $parseres = _parse_entry($entry->{payload});
        unless ($parseres->[0] == 200) {
            warn "Can't parse 'complete' command for entry '$entry->{id}': ".
                "$parseres->[1], skipped\n";
            next;
        }
        # remove if none of the names in complete command are in PATH
        my $found;
        for my $name (@{ $parseres->[2]{names} }) {
            if (File::Which::which($name)) {
                $found++; last;
            }
        }
        next if $found;
        say join(", ", @{$parseres->[2]{names}})." not found in PATH, ".
            "removing entry $entry->{id}";
        my $delres = Text::Fragment::delete_fragment(
            text=>$content, id=>$entry->{id});
        return err("Can't delete entry $entry->{id}", $delres)
            if $delres->[0] != 200;
        $deleted++;
        $content = $delres->[2]{text};
    }

    if ($deleted) {
        my $writeres = _write_f($args{file}, $content);
        return err("Can't write", $writeres) if $writeres->[0] != 200;
    }

    [200];
}

$SPEC{add_all_pc} = {
    v => 1.1,
    summary => 'Search PATH for scripts that use Perinci::CmdLine '.
        'and add completion for those',
    description => <<'_',
_
    args => {
        %arg_file,
    },
};
sub add_all_pc {
    my %args = @_;

    my $res = _read_parse_f($args{file});
    return err("Can't read entries", $res) if $res->[0] != 200;

    my $content = $res->[2]{content};

    # collect all the names mentioned
    my %names;
    for my $entry (@{ $res->[2]{parsed} }) {
        my $parseres = _parse_entry($entry->{payload});
        unless ($parseres->[0] == 200) {
            warn "Can't parse 'complete' command for entry '$entry->{id}': ".
                "$parseres->[1], skipped\n";
            next;
        }
        $names{$_}++ for @{ $parseres->[2]{names} };
    }

    my $added;
    for my $dir (split /:/, $ENV{PATH}) {
        opendir my($dh), $dir or next;
        #say "D:searching $dir";
        for my $prog (readdir $dh) {
            (-f "$dir/$prog") && (-x _) or next;
            $names{$prog} and next;

            # skip non-scripts
            open my($fh), "<", "$dir/$prog" or next;
            read $fh, my($buf), 2; $buf eq '#!' or next;
            # skip non perl
            my $shebang = <$fh>; $shebang =~ /perl/ or next;
            my $found;
            # skip unless we found something like 'use Perinci::CmdLine'
            while (<$fh>) {
                if (/^\s*(use|require)\s+Perinci::CmdLine(|::Any|::Lite)/) {
                    $found++; last;
                }
            }
            next unless $found;

            $prog =~ $Text::Fragment::re_id or next;

            #say "D:$dir/$prog is a Perinci::CmdLine program";

            my $insres = Text::Fragment::insert_fragment(
                text=>$content, id=>$prog,
                payload=>"complete -C '$prog' '$prog'");
            return err("Can't add entry $prog", $insres)
                if $insres->[0] != 200;
            $added++;
            $names{$prog}++;
            $content = $insres->[2]{text};
        }
    }

    if ($added) {
        my $writeres = _write_f($args{file}, $content);
        return err("Can't write", $writeres) if $writeres->[0] != 200;
    }

    [200];
}

1;
# ABSTRACT: Backend for bash-completion-f

__END__

=pod

=encoding UTF-8

=head1 NAME

App::BashCompletionF - Backend for bash-completion-f

=head1 VERSION

This document describes version 0.01 of App::BashCompletionF (from Perl distribution App-BashCompletionF), released on 2014-10-31.

=head1 FUNCTIONS


=head2 add_all_pc(%args) -> [status, msg, result, meta]

Search PATH for scripts that use Perinci::CmdLine and add completion for those.

Arguments ('*' denotes required arguments):

=over 4

=item * B<file> => I<str>

Use alternate location for the bash-completion-f file.

By default, the C<complete> scripts are put in a file either in
C</etc/bash-completion-f> (if running as root) or C<~/.bash-completion-f> (if
running as normal user). This option sets another location.

=back

Return value:

Returns an enveloped result (an array).

First element (status) is an integer containing HTTP status code
(200 means OK, 4xx caller error, 5xx function error). Second element
(msg) is a string containing error message, or 'OK' if status is
200. Third element (result) is optional, the actual result. Fourth
element (meta) is called result metadata and is optional, a hash
that contains extra information.

 (any)


=head2 add_entry(%args) -> [status, msg, result, meta]

Add a completion entry.

Arguments ('*' denotes required arguments):

=over 4

=item * B<content>* => I<str>

Entry content (the actual "complete ..." bash command).

=item * B<file> => I<str>

Use alternate location for the bash-completion-f file.

By default, the C<complete> scripts are put in a file either in
C</etc/bash-completion-f> (if running as root) or C<~/.bash-completion-f> (if
running as normal user). This option sets another location.

=item * B<id>* => I<str>

Entry ID, for marker (usually command name).

=back

Return value:

Returns an enveloped result (an array).

First element (status) is an integer containing HTTP status code
(200 means OK, 4xx caller error, 5xx function error). Second element
(msg) is a string containing error message, or 'OK' if status is
200. Third element (result) is optional, the actual result. Fourth
element (meta) is called result metadata and is optional, a hash
that contains extra information.

 (any)


=head2 add_entry_pc(%args) -> [status, msg, result, meta]

Add a completion entry (shortcut for Perinci::CmdLine-based CLI programs).

This is a shortcut for C<add_entry>. Doing:

 % bash-completion-f add-pc foo

will be the same as:

 % bash-completion-f add --id foo 'complete -C foo foo'

Arguments ('*' denotes required arguments):

=over 4

=item * B<file> => I<str>

Use alternate location for the bash-completion-f file.

By default, the C<complete> scripts are put in a file either in
C</etc/bash-completion-f> (if running as root) or C<~/.bash-completion-f> (if
running as normal user). This option sets another location.

=item * B<program>* => I<str>

Program name to add.

=back

Return value:

Returns an enveloped result (an array).

First element (status) is an integer containing HTTP status code
(200 means OK, 4xx caller error, 5xx function error). Second element
(msg) is a string containing error message, or 'OK' if status is
200. Third element (result) is optional, the actual result. Fourth
element (meta) is called result metadata and is optional, a hash
that contains extra information.

 (any)


=head2 clean_entries(%args) -> [status, msg, result, meta]

Delete entries for commands that are not in PATH.

Sometimes when a program gets uninstalled, it still leaves completion entry.
This subcommand will search all entries for commands that are no longer found in
PATH and remove them.

Arguments ('*' denotes required arguments):

=over 4

=item * B<file> => I<str>

Use alternate location for the bash-completion-f file.

By default, the C<complete> scripts are put in a file either in
C</etc/bash-completion-f> (if running as root) or C<~/.bash-completion-f> (if
running as normal user). This option sets another location.

=back

Return value:

Returns an enveloped result (an array).

First element (status) is an integer containing HTTP status code
(200 means OK, 4xx caller error, 5xx function error). Second element
(msg) is a string containing error message, or 'OK' if status is
200. Third element (result) is optional, the actual result. Fourth
element (meta) is called result metadata and is optional, a hash
that contains extra information.

 (any)


=head2 list_entries(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<detail> => I<bool>

=item * B<file> => I<str>

Use alternate location for the bash-completion-f file.

By default, the C<complete> scripts are put in a file either in
C</etc/bash-completion-f> (if running as root) or C<~/.bash-completion-f> (if
running as normal user). This option sets another location.

=back

Return value:

Returns an enveloped result (an array).

First element (status) is an integer containing HTTP status code
(200 means OK, 4xx caller error, 5xx function error). Second element
(msg) is a string containing error message, or 'OK' if status is
200. Third element (result) is optional, the actual result. Fourth
element (meta) is called result metadata and is optional, a hash
that contains extra information.

 (any)


=head2 remove_entry(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<file> => I<str>

Use alternate location for the bash-completion-f file.

By default, the C<complete> scripts are put in a file either in
C</etc/bash-completion-f> (if running as root) or C<~/.bash-completion-f> (if
running as normal user). This option sets another location.

=item * B<id>* => I<str>

Entry ID, for marker (usually command name).

=back

Return value:

Returns an enveloped result (an array).

First element (status) is an integer containing HTTP status code
(200 means OK, 4xx caller error, 5xx function error). Second element
(msg) is a string containing error message, or 'OK' if status is
200. Third element (result) is optional, the actual result. Fourth
element (meta) is called result metadata and is optional, a hash
that contains extra information.

 (any)

=head1 HOMEPAGE

Please visit the project's homepage at L<https://metacpan.org/release/App-BashCompletionF>.

=head1 SOURCE

Source repository is at L<https://github.com/perlancar/perl-App-BashCompletionF>.

=head1 BUGS

Please report any bugs or feature requests on the bugtracker website L<https://rt.cpan.org/Public/Dist/Display.html?Name=App-BashCompletionF>

When submitting a bug or request, please include a test-file or a
patch to an existing test-file that illustrates the bug or desired
feature.

=head1 AUTHOR

perlancar <perlancar@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2014 by perlancar@cpan.org.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
