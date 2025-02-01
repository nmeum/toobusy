(list (channel
        (name 'nmeum)
        (url "https://github.com/nmeum/guix-channel.git")
        (branch "master")
        (commit
          "7f2e77c69f0b0620fd3422d46088c9f9dc4a59cc")
        (introduction
          (make-channel-introduction
            "808a00792c114c5c1662e8b1a51b90a2d23f313a"
            (openpgp-fingerprint
              "514E 833A 8861 1207 4F98  F68A E447 3B6A 9C05 755D"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "65348f3aa44f79aa5c97bfcf19e22f35d401dfd4")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
