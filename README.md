
## Introduction

This is an accompanying repository for the paper:

Peter Phillips and Zhentao Shi (2018): "Boosting the Hodrick-Prescott Filter" (to provide the arxiv link)

We offer two R script files.

* `BoostedHP.R` contains the R function to implement the automated boosted HP filter.
The inputs and outputs are detailed in the beginning of the function.
* `testing_example.R` is a script to demonstrate the usage of the filtering procedure.

We welcome comments about the paper and the code.


#### Future Plan

A very preliminary R package can be installed by running in `R`
```
library(devtools)
install_github("zhentaoshi/Boosted_HP_filter/BoostedHP")
library(BoostedHP)
```
The package is in progress with assistance from [Yang Chen](https://github.com/chenyang45).
