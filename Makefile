.POSIX:

PREFIX  ?= /usr
BINDIR  ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share

GUILE ?= $(BINDIR)/guile
GUILE_MODULE_DIR ?= $(DATADIR)/guile/site/3.0
GUILE_COMPILED_MODULE_DIR ?= $(GUILE_MODULE_DIR)/site-ccache

########################################################################

BINFILES = bin/toobusy-list.scm
SRCFILES = src/toobusy/util.scm \
	   src/toobusy/index.scm \
	   src/toobusy/search.scm \
	   src/toobusy.scm
LIBFILES = $(SRCFILES:%.scm=%.go)

GUILDFLAGS += -Warity-mismatch -Wbad-case-datum -Wduplicate-case-datum \
	      -Wformat -Wunbound-variable -Wunsupported-warning \
	      -Wunused-toplevel -Wunused-variable
GUILDFLAGS += -L src/

all: $(BINFILES) $(LIBFILES)
install:
	install -Dm644 $(SRCFILES) -t "$(GUILE_MODULE_DIR)/toobusy"
	install -Dm644 $(LIBFILES) -t "$(GUILE_COMPILED_MODULE_DIR)/toobusy"
	mv "$(GUILE_MODULE_DIR)/toobusy/toobusy.scm" \
		"$(GUILE_MODULE_DIR)/toobusy.scm"
	mv "$(GUILE_COMPILED_MODULE_DIR)/toobusy/toobusy.go" \
		"$(GUILE_COMPILED_MODULE_DIR)/toobusy.go"
	install -Dm755 $(BINFILES) -t "$(DESTDIR)$(BINDIR)/"
clean:
	rm $(BINFILES) $(LIBFILES)

.scm.in.scm:
	sed "s|@GUILE@|$(GUILE)|" < $< > $@
	chmod +x $@
.scm.go:
	guild compile $(GUILDFLAGS) -o $@ $<

.PHONY: all install clean
.SUFFIXES: .scm .go .scm.in
