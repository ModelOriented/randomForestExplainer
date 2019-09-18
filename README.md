# randomForestExplainer

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/randomForestExplainer)](https://cran.r-project.org/package=randomForestExplainer)
[![Build Status](https://travis-ci.org/ModelOriented/randomForestExplainer.svg?branch=master)](https://travis-ci.org/ModelOriented/randomForestExplainer)
[![codecov](https://codecov.io/gh/ModelOriented/randomForestExplainer/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/randomForestExplainer)
[![DOI](https://zenodo.org/badge/97007621.svg)](https://zenodo.org/badge/latestdoi/97007621)

A set of tools to understand what is happening inside a Random Forest. A detailed discussion of the package and importance measures it implements can be found here: [Master thesis on randomForestExplainer](https://cdn.staticaly.com/gh/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer_Master_thesis.pdf).

## Installation

randomForestExplainer can be installed from [CRAN](https://cran.r-project.org/package=randomForestExplainer) as follows:

```{r}
install.packages("randomForestExplainer")
```

To install and load the latest version of `randomForestExplainer` from **Github** run:

```{r}
if (!require(devtools)) install.packages("devtools")
devtools::install_github("ModelOriented/randomForestExplainer")

library(randomForestExplainer)
```

## Vignette

* [Understanding random forests with randomForestExplainer](https://modeloriented.github.io/randomForestExplainer/articles/randomForestExplainer.html)

## Cheatsheets

* [A one-page summary](https://github.com/ModelOriented/randomForestExplainer/blob/master/materials/cheatsheet.pdf)

## Examples

* [Initial vignette for glioblastoma data](https://cdn.staticaly.com/gh/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer/inst/doc/randomForestExplainer.html)
