.POSIX:

PREFIX  ?= /usr
BINDIR  ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share

GUILE ?= $(BINDIR)/guile
GUILE_MODULE_DIR ?= $(DATADIR)/guile/site/3.0
GUILE_COMPILED_MODULE_DIR ?= $(GUILE_MODULE_DIR)/site-ccache

########################################################################

BINFILES = bin/toobusy-list \
	   bin/toobusy-index
LIBFILES = src/toobusy/util.go \
	   src/toobusy/time.go \
	   src/toobusy/event.go \
	   src/toobusy/index.go \
	   src/toobusy/search.go \
	   src/toobusy/config.go \
	   src/toobusy/toobusy.go

GUILDFLAGS += -Warity-mismatch -Wbad-case-datum -Wduplicate-case-datum \
	      -Wformat -Wunbound-variable -Wunsupported-warning \
	      -Wunused-variable
GUILDFLAGS += -L src/

all: $(BINFILES) $(LIBFILES)
install:
	install -Dm644 $(LIBFILES) \
		-t "$(GUILE_COMPILED_MODULE_DIR)/toobusy"
	install -Dm644 $(SRCFILES:%.go=%.scm) \
		-t "$(GUILE_MODULE_DIR)/toobusy"
	install -Dm755 $(BINFILES) -t "$(DESTDIR)$(BINDIR)/"
clean:
	rm -f $(BINFILES) $(LIBFILES)

.in:
	sed "s|@GUILE@|$(GUILE)|" < $< > $@
	chmod +x $@
.scm.go:
	guild compile $(GUILDFLAGS) -o $@ $<

.PHONY: all install clean
.SUFFIXES: .scm .go .in
