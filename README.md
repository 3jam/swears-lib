swears-lib - Heathkit H19(A) terminal handling library
======================================================
This module fascilitates controlling a Heathkit H19(A) terminal to
directly take advantage of some of its non-standard features. This
includes graphics characters (which are not VT-100 compatible) and the
normally inaccessible 25th line useful for status lines. This library
should be compatible with the Zenith Z19 as well, but this is
untested.

The [`braw` module](https://github.com/3jam/braw-lib) is required to
use this module.

![swears demo](https://github.com/3jam/swears-lib/blob/gh-pages/swears.gif)

Installation
------------
From the `swears-lib` directory, enter the following command to install
`swears`:

    raco pkg install

to uninstall, run:

    raco pkg remove swears-lib

Using swears-lib
-----------------
Documentation for the `swears` and `h19` modules are installed
with the package and can be found by running `raco docs`.
