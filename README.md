Plot-ho-matic
==

[![Hackage](https://img.shields.io/hackage/v/Plot-ho-matic.svg)](https://hackage.haskell.org/package/Plot-ho-matic) [![Build Status](https://travis-ci.org/ghorn/Plot-ho-matic.png?branch=master)](https://travis-ci.org/ghorn/Plot-ho-matic)

See the example in the examples folder for usage.

"user error: out of memory"
If you get this ^ error on OSX your cairo/pango/gtk may be linked to an XQuartz library.
 Add --extra-lib-dirs=/usr/local/lib (or wherever the correct libraries are) to your .cabal/config
