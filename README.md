Courseviz R-package
===================
An R-package for visualizing course information. Made for use with
[dybber/absalon-scraper](/dybber/absalon-scraper), see example input
format in that repository.

Authors
-------
Martin Dybdal (dybber@dybber.dk)

TODO
----
 * Make it an actually installable R-package
 * Plots for approved/unapproved assignments
 * Generate multipage-report with information for each exercise class
 * Plot dropouts based on assignments not submitted
 * Visualizations across multiple courses

Installation
------------
Depends on R-packages `ggplot2`, `scales`, `plyr` and `gridExtra`.

To install, run this command in the R REPL:
```R
install.packages(c("ggplot2", "scales", "plyr", "gridExtra"))
```
