# toobusy

[notmuch] but for calendars.

## Status

PoC, nothing to see here yet.

## TODO

* [ ] `toobusy index`
* [ ] `toobusy list`
* [ ] `toobusy search`
* [ ] `toobusy new`
* [ ] Configuration file support
	* [ ] Support for multiple calendars
	* [ ] Setting a different stemmer
	* [ ] Configuration of `prefer-mdy?`

## Usage

```
$ guix time-machine -C channels.scm -- guix shell
[env] $ guile -L src/
> (use-modules (toobusy main))
> (main '("example.ics"))
```

[notmuch]: https://notmuchmail.org/
