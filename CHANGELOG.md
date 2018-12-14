HEAD
---
* RTS stats compatability
* upgrade stack LTS

0.12.2.0
---
* add a diff button to setHo

0.12.1.0
---
* "refresh" clears upstream version so you can see if you get a response

0.12.0.0
---
* optional configuration data (SetHo)
* configurable: allow disabling autocommit (SetHo)
* configurable: how to show Double/Float (default is now 'show') (SetHo)
* redraw plots upon resetting history (PlotHo)

0.11.{1,2}.0
---
* make settings aware of message counter

0.11.0.0
---
* newHistoryChannel' can now take meta without data

0.10.1.0
---
* set default xaxis types
* move reset axis range buttons higher

0.10.0.0
---
* Better error messages when the "impossible" happens :b
* Also combine partial common prefixes for plot titles/legends
* Replace the Plotter monad with a monomorphic (GADT) Channel type.
* Major reorganization to trigger off of GHC runtime instead of GTK events
* Require non-threaded RTS
* Plot all channels on each graph

0.9.0.10
---
* Fix a nasty space leak

0.9.0.9
---
* Add option to remember the min/max ranges past the view window

0.9.0.8
---
* Make title text smaller

0.9.0.7
---
* Put scrolledWindow inside expander

0.9.0.6
---
* Fix regression where signal selectors weren't growing
* rename {options -> opt, signals -> sig} to save space

0.9.0.5
---
* smaller minimum plot size
* use correct gtk column packing function

0.9.0.4
---
* code cleanup
* use Gtk draw signal instead of expose signal (fixes buggy behavior)
* more understandable and maintainable concurrency strategy
* change default max history length to 500

0.9.0.3
---
* initial settings selector box is big enough to see some fields
* combo box and text entry now share "staged" column

0.9.0.2
---
* Add autocommit toggle
* Add "take upstream" button
* Implemented saving/loading files

0.9.0.1
---
* workaround issue where toggling expander hides the plot

0.9.0.0
---
* generic-accessors 0.6.0.0 compatibility
* rewrite settings app to use dynamic data
* upgrade to GTK3

0.8.0.0
---
* Switch from Tree ([String], String, Maybe (a -> [[(Double, Double)]]))
         to   Tree ([String], Either String (a -> [[(Double, Double)]]))

0.7.0.1
---
* Make the list of channels a scrollable thing.

0.7.0.0
---
* Better legend and title.

0.5.0.5
---
* Compatability with generic-accessors 0.2

0.5.0.4
---
* Performance improvement

0.5.0.1
---
* Hierarchical visibility clicking and inconsistent state
* Merge new signal tree with old

0.5.0.0
---
* Use Accessors from generic-accessors package
* Major performance improvements (only draw new data)
* Both "history" and custom plot interfaces
* Unify this package with the dynobud plotter

0.4.0.4
---
remove Generic requirement for some Lookup instances

0.4
---
* Performance improvements
* Safer monadic API
* More general plottable types

0.3 (Unreleased development version)
---
* switch from Template Haskell to GHC.Generics

0.2
---
* Cleaner API

0.1
---
* Initial release (moved from rawesome repo)
