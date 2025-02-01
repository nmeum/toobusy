(use-modules (guix packages))

(specifications->manifest
  '("guile"
    "gcc-toolchain"
    "xapian"
    "guile-xapian"
    "guile-ics"

    "guile-readline"
    "coreutils-minimal"))
