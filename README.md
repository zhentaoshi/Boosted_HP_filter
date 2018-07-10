
## Introduction

This is an accompanying repository for the paper:

Peter Phillips and Zhentao Shi (2018): "Boosting the Hodrick-Prescott Filter" (to provide the arxiv link)

We offer two R script files.

* `BoostedHP.R` contains the R function to implement the automated boosted HP filter.
The inputs and outputs are detailed in the beginning of the function.
* `testing_example.R` is a script to demonstrate the usage of the filtering procedure.

We welcome comments about the paper and the code.


#### Install

A very preliminary R package can be installed by running in `R`
```
install.packages("devtools")
devtools::install_github("chenyang45/BoostedHP/BoostedHP")
library(BoostedHP)
```
The package is in progress with assistance from [Yang Chen](https://github.com/chenyang45/BoostedHP).

#### Example
```
load("Ireland_GDP.RData") # Ireland Annual GDP example in the paper
source("BoostedHP.R")

lam = 100 # tuning parameter for the annaul data

# raw HP filter
bx_HP = BoostedHP(IRE, lambda = lam, iter= FALSE)

# by BIC
bx_BIC = BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "BIC")

# by ADF
bx_ADF = BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "adf", sig_p = 0.050)

# summarize the outcome
outcome = cbind(IRE, bx_HP$trend, bx_BIC$trend, bx_ADF$trend) 
matplot(  outcome, type = "l", ylab = "", lwd = rep(2,4)  )
```
This is the simple pictrue after the matplot command above:![image](https://github.com/chenyang45/BoostedHP/blob/master/work_memo/readme_example_plot.png)
