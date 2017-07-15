# randomForestExplainer

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/randomForestExplainer)](https://cran.r-project.org/package=factorMerger)
[![Pending Pull-Requests](http://githubbadges.herokuapp.com/MI2DataLab/randomForestExplainer/pulls.svg)](https://github.com/MI2DataLab/randomForestExplainer/pulls)
[![Github Issues](http://githubbadges.herokuapp.com/MI2DataLab/randomForestExplainer/issues.svg)](https://github.com/MI2DataLab/randomForestExplainer/issues)

A set of tools to understand what is happening inside a Random Forest. A detailed discussion of the package and importance measures it implements can be found here: [Master thesis on randomForestExplainer](https://rawgit.com/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer_Master_thesis.pdf).

## Instalation

randomForestExplainer can be installed from [CRAN](https://cran.r-project.org/package=randomForestExplainer) as follows:

```{r}
install.packages("randomForestExplainer")
```

To install and load the latest version of `randomForestExplainer` from **Github** run:

```{r}
if (!require(devtools)) install.packages("devtools")
devtools::install_github("MI2DataLab/randomForestExplainer")

library(randomForestExplainer)
```

## Vignette

* [Understanding random forests with randomForestExplainer](https://rawgit.com/MI2DataLab/randomForestExplainer/master/inst/doc/randomForestExplainer.html)

## Examples

* [Initial vignette for glioblastoma data](https://rawgit.com/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer/inst/doc/randomForestExplainer.html)
