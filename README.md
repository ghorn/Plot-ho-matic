Plot-ho-matic
==

[![Hackage](https://img.shields.io/hackage/v/Plot-ho-matic.svg)](https://hackage.haskell.org/package/Plot-ho-matic) [![Build Status](https://travis-ci.org/ghorn/Plot-ho-matic.png?branch=master)](https://travis-ci.org/ghorn/Plot-ho-matic)

This is a GUI for high-performance live plotting with a convenient interface for
selecting which data from a structure to draw.

NEED IMAGE HERE

# usage
There is an `examples/` folder in the git repo.

# FAQ
"user error: out of memory"
If you get this ^ error on OSX your cairo/pango/gtk may be linked to an XQuartz library.
 Add --extra-lib-dirs=/usr/local/lib (or wherever the correct libraries are) to your .cabal/config

==
Special thanks to Chart and gtk2hs, which do all the heavy lifting.
