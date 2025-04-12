# toobusy

This is an unfinished experiment for implementing a [notmuch]-like command-line tool for [iCalendars][rfc 5545].

## Status

This is presently an unfinished proof-of-concept prototype. Ideally, I would
like to rewrite this in a statically typed language like Haskell or Rust.
However, I would first have to write [Xapian] bindings for such a language and
presently I am not motivated enough to do so. Further, properly implementing
this in Guile requires improving the [guile-ics] library. For instance, this
library is presently lacking proper support for recurring events. Essentially,
making progress on this project requires working on either an [RFC 5545][rfc
5545] or [Xapian] library and neither seems particularly "fun" right now.

## Development Setup

Requires a working [Guix] installation:

	$ guix time-machine -C channels.scm -- shell
	[env] $ guile -L src -s ./bin/toobusy-index samples/*.ics
	[env] $ guile -L src -s ./bin/toobusy-list -s 01-01-1000 -e 01-01-3000
	[env] $ guile -L src -s ./bin/toobusy-search party

## Installation

Presently, this is supposed to be installed via [Guix]:

	$ guix time-machine -C channels.scm -- package -f package.scm

[guile-ics]: https://github.com/artyom-poptsov/guile-ics
[Guix]: https://guix.gnu.org
[notmuch]: https://notmuchmail.org/
[rfc 5545]: https://datatracker.ietf.org/doc/html/rfc5545
[Xapian]: http://www.xapian.org/
