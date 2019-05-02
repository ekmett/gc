0.0.3 [2019.05.02]
------------------
* Fix the build on Windows.
* Remove the `doctests` test suite, as there were never any `doctest`s to
  begin with.

0.0.2
-----
* Add a library dependency on the `doctests` test suite

0.0.1
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0
-
* Initialized repository
