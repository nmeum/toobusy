(use-modules (guix packages))

(specifications->manifest
  '("guile"
    "gcc-toolchain"
    "xapian"
    "guile-xapian"
    "guile-ics"

    "pdpmake"
    "guile-readline"
    "coreutils-minimal"))
