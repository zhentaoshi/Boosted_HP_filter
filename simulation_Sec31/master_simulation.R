
rm(list = ls() )

library(mFilter)
library(tseries)
library(magrittr)

library(plyr)


source("BoostedHP1.R") # BoostedHP.R need to be contained in the working document.
source("do_simulation.R")



#######################################################


# parameter setting
n <- 100 # length of the time series
m <- 50 # number of time series in each data set
h <- 1000 # number of data sets
R <- 40 # maximum times of iterations
lambda <- 1600 # smoothing parameter


# n <- 100 # length of the time series
# m <- 10 # number of time series in each data set
# h <- 100 # number of data set
# R <- 10 # maximum times of iterations

######## graph 1
typeD = "3rd"
set.seed(365)
data0 = DGP_y( n, h, m, typeD, 0.5)
do_simulation( )
rm(data0)

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


###### graph 4 (for R1)
typeD = "cos"
set.seed(357)
data0 = DGP_y( n, h, m, typeD, 0.5)
do_simulation( )
rm(data0)
