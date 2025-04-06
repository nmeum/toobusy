# toobusy

This is an unfinished experiment for implementing a [notmuch]-like command-line tool for [iCalendars][rfc 5545].

## Status

This is presently an unfinished proof-of-concept. Ideally, I would like to
rewrite this in a statically typed language like Haskell or Rust. However, I
would first have to write Xapian bindings for such a language and presently I
am not motivated enough to do so. Further, properly implementing this in Guile
requires improving the [guile-ics] library. For instance, this library is
presently lacking proper support for reoccurring events.

## Development Setup

```
$ guix time-machine -C channels.scm -- shell
[env] $ guile -L src -s ./bin/toobusy-index samples/*.ics
[env] $ guile -L src -s ./bin/toobusy-list -s 01-01-1000 -e 01-01-3000
```

[guile-ics]: https://github.com/artyom-poptsov/guile-ics
[notmuch]: https://notmuchmail.org/
[rfc 5545]: https://datatracker.ietf.org/doc/html/rfc5545
