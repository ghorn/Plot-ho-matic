Plot-ho-matic (and Set-ho-matic)
==

[![Hackage](https://img.shields.io/hackage/v/Plot-ho-matic.svg)](https://hackage.haskell.org/package/Plot-ho-matic) [![Build Status](https://travis-ci.org/ghorn/Plot-ho-matic.png?branch=master)](https://travis-ci.org/ghorn/Plot-ho-matic)

Plot-ho-matic is a GUI for high-performance live plotting with a convenient TreeView interface for
selecting which elements to draw from a data structure. The focus is on ease of use, with optional advanced interfaces for more features.

![Plot-ho-matic in action](/../screenshots/screenshots/plotho_example.png?raw=true "Plot-ho-matic example")

The sister library Set-ho-matic is a GUI for editing haskell data and sending those changes to some running program. It can also query the program for it's latest data and has save and load features.

![Set-ho-matic in action](/../screenshots/screenshots/setho_example.png?raw=true "Set-ho-matic example")

Both Plot-ho-matic and Set-ho-matic rely heavily on [generic-accessors](http://hackage.haskell.org/package/generic-accessors) which uses GHC.Generics to create trees from haskell data.

# usage
There is an `examples/` folder in the git repo.

# FAQ
"user error: out of memory"
If you get this ^ error on OSX your cairo/pango/gtk may be linked to an XQuartz library.
 Add --extra-lib-dirs=/usr/local/lib (or wherever the correct libraries are) to your .cabal/config

==
Special thanks to Chart and gtk2hs, which do all the heavy lifting.
