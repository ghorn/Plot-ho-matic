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
