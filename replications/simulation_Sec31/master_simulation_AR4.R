#========================================================================
#  simulation : MSE, BIAS
#
# R version 3.4.4 (2018-03-15) -- "Someone to Lean On"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# 
# Author : Shi Zhentao, zhentao.shi@link.cuhk.edu.hk
# Date : 2018-06-25
# Version 1.0

# use the DGP with trend

#========================================================================


rm(list = ls() )

library(mFilter)
library(tseries)
library(magrittr)
library(plyr)
library(dynlm)

source("do_simulation_AR4.R")



#######################################################


# parameter setting
n <- 100 # length of the time series
m <- 50 # number of time series in each data set
h <- 5 # number of data set


######## graph 1
typeD = "3rd"
set.seed(365)
data0 = DGP_y( n, h, m, typeD, 0.5)
do_simulation( )
# rm(data0)

####### graph 2
typeD = "4th"
set.seed(123)
data0 = DGP_y( n, h, m, typeD, 0.5)
do_simulation( )
rm(data0)




###### graph 3
typeD = "sin"
set.seed(356)
data0 = DGP_y( n, h, m, typeD, 0.5)
do_simulation( )
rm(data0)


###### graph 4
typeD = "cos"
set.seed(357)
data0 = DGP_y( n, h, m, typeD, 0.5)
do_simulation( )
rm(data0)
