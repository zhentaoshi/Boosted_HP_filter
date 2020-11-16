

rm(list = ls())  

 
library(mFilter)
library(tseries)
library(ggplot2)
library(reshape)
library(scales)
library(magrittr)
library(plyr)


library(xts)
library(dynlm)
library(reshape2)


source("BoostedHP.R")

#----- get the matrix needed to get the iterated cycle of HP-filter--------
myhp <- function(n, lambda){
  
  imat <- diag(n)
  Ln <- rbind(matrix(0, 1, n), diag(1, n - 1, n))
  Ln <- (imat - Ln) %*% (imat - Ln)
  Q <- t(Ln[3:n, ])
  SIGMA.R <- t(Q) %*% Q
  SIGMA.n <- diag(n - 2)
  QQ <- lambda * Q %*% solve( SIGMA.n + lambda * SIGMA.R ) %*% t(Q)
  return( QQ )
}

####################


DGP_sto = function( n, alpha0 ){
  # generate the stochastic trend 
  f0 = cumsum( rnorm( n ) )
  
  discard = 50
  nd = n+discard
  
  u =  rnorm( nd ) # the error term
  e = rep(0,nd )
  for (nn in 2:nd){
    e[nn] = alpha0 * e[nn-1] + u[nn] + u[nn-1] # an AR process
  }
  e = e[ (discard + 1): nd]
  
  y = f0 + e

  
  return( list( obs = y, sto_trend = y - e) )
}


# parameter setting

n <- 100 # data size
m <- 2 
R <- 150 # times of iterations
lambda <- 1600 # smoothing parameter

pick_iter <- c(1,2,4,8,16,32, 64, 128) # the selected iterate HP-filter trend to be plot


set.seed(20190707)

xx = DGP_sto(n, 0.5)

x0 = xx$obs
x1 = xx$sto_trend

QQ <- myhp(n, lambda) # the matrix

######### generate three types of data

type <- c("random","trend","structurebreak")

for(i in 1:length(type)){
  type_i <- type[i]
  if (type_i == "random") {
    f_rw = rep(0,n)
    x <- x0
  }
  if (type_i == "structurebreak"){
    x <- x0
    f_br = c( rep( 0, n/2), rep(20, n/2) )
    x = f_br + x0;
  }
  if (type_i == "trend"){
   f_tr =  10 * (rev(1:n)/n ) ^ 2 + 30 * ((1:n)/n ) ^ 4  # 0.5*(1:n) - 10
   #  f_tr = 10  * cos( 2* pi * (1:n)/40 )
    x <-  x0 + f_tr; 
  }
  
  x_trend <- matrix(0,  nrow = n, ncol = R)
  x.hp.1600 <- hpfilter(x, type = "lambda", freq = 1600)
  x.trend.1600 <- x.hp.1600$trend
  x.c <- x.hp.1600$cycle
  
  for (i in 1:R){
    
    x.c <- QQ %*% matrix(x.c)
    x_trend[, i] <- x - x.c
    
  }
  
  assign(paste0("raw_",type_i),x)
  trend <- x_trend[ ,pick_iter]
  assign(paste0("trend_",type_i),trend)
}


#-----------------------------------------------combined graph produced by ggplot--------------------------------------------------------


#------------------------------------------------- manage the data for ggplot------------------------------------------------------------

iteration <- paste0("m = ",pick_iter)

#structure break

colnames(trend_structurebreak) <- iteration
structurebreak <- melt(trend_structurebreak) %>% as.data.frame()
structurebreak$type <- rep("Stoc. Trend+Mean Shift", length = length(structurebreak$value))
structurebreak$rawdata <- rep(raw_structurebreak, length( iteration ) )
structurebreak$deter <- rep(f_br, length( iteration ) )
structurebreak$trend <- rep(f_br+x1, length( iteration ) )

# random

colnames(trend_random) <- iteration
random <- melt(trend_random) %>% as.data.frame()
random$type <- rep("Stoc. Trend", length = length(random$value))
random$rawdata <- rep(raw_random, length( iteration ) )
random$deter <- rep(f_rw, length( iteration ) )
random$trend <- rep(f_rw + x1, length( iteration ) )
# trend

colnames(trend_trend) <- iteration
trend <- melt(trend_trend) %>% as.data.frame()
trend$type <- rep("Stoc.+Deter. Trends", length = length(trend$value))
trend$rawdata <- rep(raw_trend, length( iteration ) )
trend$deter <- rep(f_tr, length( iteration ) )
trend$trend <- rep(f_tr+x1, length( iteration ) )

# manage data.frame for ggplot
data_ggplot <- rbind(structurebreak,random,trend)
names(data_ggplot) <- c("ID","iteration","value","type","rawdata", "deter", "trend")
data_ggplot$iteration <- factor(data_ggplot$iteration,levels = paste0("m = ",pick_iter))
# data_ggplot$type <- factor(data_ggplot$type,levels = c("random","trend","structure break"))
data_ggplot$type <- factor(data_ggplot$type,levels = c("Stoc. Trend",
                                                       "Stoc.+Deter. Trends",
                                                       "Stoc. Trend+Mean Shift"))
 

types =  levels( data_ggplot$type ) 
itera = levels(data_ggplot$iteration )


MSE_all = matrix(0, 3, 4)

for ( jj in 1:3){
    print(jj ) 
  
    ii = types[jj]
    
    data_temp = data_ggplot[ data_ggplot$iteration == itera[1], ]
    data_temp = data_temp[ (data_temp$type == ii),  ]
    p1 <- ggplot(data_temp) 
   
    
    # cover the break with white color
    
    br <- data.frame(x1 = c(0,0,50), x2 = c(0,0,51), y1 = c(0,0,0), y2 = c(0,0,25) )
    br$type = as.factor( levels( data_ggplot$type ) )
 
    
    
    x = ts(data_temp$rawdata)
    regAR4 = dynlm( x  ~ L(x, c(1:4) ) )
    AR4_series = c( rep(NA,4), predict(regAR4) )
    



    bHP_BIC = BoostedHP(x, lambda = 1600, iter= TRUE, test_type = "BIC", sig_p = 0.010, Max_Iter = 100) 
    
    bHP_adf = BoostedHP(x, lambda = 1600, iter= TRUE, test_type = "adf", sig_p = 0.010, Max_Iter = 100) 
      
    HP = BoostedHP(x, lambda = 1600, iter= FALSE, test_type = "none", sig_p = 0.010, Max_Iter = 100) 


    est_error = data.frame( 
              HP_raw  = HP$trend - data_temp$trend, 
              bHP_adf = bHP_adf$trend - data_temp$trend, 
              bHP_BIC = bHP_BIC$trend - data_temp$trend, 
              AR4 = AR4_series - data_temp$trend )
    est_trun = est_error[ 5:(n-4), ]
    
    var_all = apply(est_trun, 2, var)
    # print(var_all)
    MSE_all[jj, ] = as.vector( var_all )

}

colnames(MSE_all) = c("HP", "ADF", "BIC", "AR4")
rownames(MSE_all) = c("Stoc. Trend",
                      "Stoc.+Deter. Trends",
                      "Stoc. Trend+Mean Shift")

print(MSE_all)
