# randomForestExplainer 0.10.2

* Remove dependency on reshape2 in favour of tidyr (@olivroy, #33)

* Silence deprecation warnings from ggplot2 and dplyr (@olivroy, #29)

* Use testthat 3rd edition. (@olivroy, #33)

# randomForestExplainer 0.10.1

* Small tweaks to `explain_forest()`.

# randomForestExplainer 0.10.0

## New features
* Added support for ranger forests.
* Added support for unsupervised randomForest.
* Added tests for most functions.

## Bug fixes
* Fixed bug for `explain_forest()` not finding templates.
* Added more intuitive error message for `explain_forest()` when local `importance` is absent.
