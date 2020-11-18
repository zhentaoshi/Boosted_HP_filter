
rm(list = ls())

library(tseries)
library(magrittr)
library(xts)
library(knitr)
library(neverhpfilter)
library(dynlm)
source("BoostedHP_pub.R")

## self defined functions ##############
compare <- function(x, trend){
  
  x = as.ts(x)
  n = length(x)
  
  d0 = BoostedHP( x, lambda=1600, iter = FALSE)$trend

  bhp.adf = BoostedHP( x, lambda=1600, iter = TRUE, test_type =  "adf", sig_p = 0.05, Max_Iter = 200) 
  d1 = bhp.adf$trend
  stop.adf005 = bhp.adf$iter_num
  
  
  bhp.adf = BoostedHP( x, lambda=1600, iter = TRUE, test_type =  "adf", sig_p = 0.01, Max_Iter = 200) 
  d2 = bhp.adf$trend
  stop.adf001 = bhp.adf$iter_num
  
  bhp.bic = BoostedHP( x, lambda=1600, iter = TRUE, test_type =  "BIC", Max_Iter = 200)
  d3 = bhp.bic$trend
  stop.bic = bhp.bic$iter_num
  
  reg = dynlm( x  ~ L(x, c(1:4) ) )
  d4 = c( rep(NA,4), predict(reg) )
  
  dd = cbind( x, d0, d1, d2, d3, d4 ) 
  
  trim_dd = dd[ 5:(n-4),  ]
  return( list(MSE = colMeans( (trim_dd[,2:6] - trend[ 5:(n-4) ] )^2, na.rm = TRUE ), 
            stop = c(stop.adf005, stop.adf001, stop.bic) )   )

}
##############################################
 
set.seed(200)

Rep = 5000
DD = matrix(0, Rep, 20)
STOP = matrix(0, Rep, 12)

n = 100

pts1 <- Sys.time()

for (r in 1:Rep){
  print(r)
  noise = rnorm(n)
  
  x1 = cumsum(noise)
  trend1 = x1

  out = compare(x1, trend1)
  DD[r, 1:5] =  out$MSE
  STOP[r, 1:3] = out$stop
  
  dett = c( rep(0, n/2), 1:(n/2) )
  x2 = c( noise[1:(n*0.5)], cumsum( noise[(n *0.5 + 1):n]  ) ) + dett
  trend2 = c( rep(0, 0.5*n ), cumsum( noise[(n *0.5 + 1):n] ) ) + dett


  out = compare(x2, trend2)
  DD[r, 6:10] = out$MSE
  STOP[r, 4:6] = out$stop
  
  dett =  5 * (1:n)^( 1/5 ) * cos( 0.05*pi*(1:n)^(.9) )
  
  x3 = x1 + dett
  trend3 = trend1+dett
  
  out = compare(x3, trend3)
  DD[r, 11:15] = out$MSE
  STOP[r, 7:9] = out$stop
  
  x4 = x2 + dett
  trend4 = trend2+dett
  
  out = compare(x4, trend4)
  DD[r, 16:20] = out$MSE
  STOP[r, 10:12] = out$stop
  
}

D_mean = colMeans(DD)
D_mean = matrix(D_mean, 4, 5, byrow = TRUE)
D_mean = D_mean[c(1,3,2,4), ]


S_mean = colMeans(STOP)
S_mean = matrix(S_mean, 4, 3, byrow = TRUE)
S_mean = S_mean[c(1,3,2,4), ]

############# 
cat("\n MSE \n")
print(D_mean)

cat("\n stopping time \n")
print(S_mean)

save(DD, STOP, D_mean, S_mean, file = "Sec32.Rdata")

pts2 = Sys.time() - pts1
print(pts2)

write.csv( cbind(D_mean, S_mean),  file = paste0("Sec32_", n, "_Rep_", Rep, ".csv")  )
