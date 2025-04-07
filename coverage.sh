dune test --instrument-with bisect_ppx --force
bisect-ppx-report html
bisect-ppx-report summary
open _coverage/index.html