# toobusy

[notmuch] but for calendars.

## Status

PoC, nothing to see here yet.

## Usage

```
$ guix time-machine -C channels.scm -- guix shell -C
[env] $ guile -L src/
> (use-modules (toobusy main))
> (main '("example.ics"))
```

[notmuch]: https://notmuchmail.org/
