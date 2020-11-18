
rm(list = ls())

library(tseries)
library(magrittr)
library(xts)
library(knitr)
library(neverhpfilter)
library(dynlm)
library(magrittr)
library(reshape2)
library(ggplot2)
library(plyr)
source("BoostedHP_pub.R")

## self defined functions ##############
compare <- function(x){
  
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
  colnames(dd) = c("data", "HP", "ADF005", "ADF001", "bHP-BIC", "AR(4)")
  return(dd)
}
##############################################
 

set.seed(888)


n = 100

par(mfrow = c(3,1))

  ###########################
noise = rnorm(n)  
x1 = cumsum(noise)
trend1 = x1

out = compare(x1)[,c(1,2,5,6)]
data.out = data.frame(out[, c(2:4, 1)])
data.out$time = 1:n
data.out$DGP = "DGP3"
data.out$type1 = "Trend"
data.out$trend = trend1


out_diff = ( out[ ,c(2:4)] - trend1 )

data_temp = data.frame( out_diff )
data_temp$data = NA
data_temp$time = 1:n
data_temp$DGP = "DGP3"
data_temp$type1 = "Residual"
data_temp$trend = trend1
data.out = rbind( data.out, data_temp) 

###############################  
  
  dett = c( rep(0, n/2), 1:(n/2) )
  x2 = c( noise[1:(n*0.5)], cumsum( noise[(n *0.5 + 1):n]  ) ) + dett
  trend2 = c( rep(0, 0.5*n ), cumsum( noise[(n *0.5 + 1):n] ) ) + dett

  out = compare(x2)[,c(1,2,5,6)]
  
  data_temp = data.frame(out[, c(2:4, 1)])
  data_temp$time = 1:n
  data_temp$DGP = "DGP5"
  data_temp$type1 = "Trend"
  data_temp$trend = trend2
  data.out = rbind( data.out, data_temp) 

  
  out_diff = ( out[ ,c(2:4)] - trend2 )
  data_temp = data.frame( out_diff )
  data_temp$data = NA
  data_temp$time = 1:n
  data_temp$DGP = "DGP5"
  data_temp$type1 = "Residual"
  data_temp$trend = trend2
  data.out = rbind( data.out, data_temp) 
  

  

  dett_cyc =  5 * (1:n)^( 1/5 ) * cos( 0.05*pi*(1:n)^(.9) )
  
  x3 = x1 + dett_cyc
  trend3 = trend1+dett_cyc
  
  
  
  
  out = compare(x3)[,c(1,2,5,6)]
  
  data_temp = data.frame(out[, c(2:4,1)])
  data_temp$time = 1:n
  data_temp$DGP = "DGP4"
  data_temp$type1 = "Trend"
  data_temp$trend = trend3
  
  data.out = rbind(data.out, data_temp)
  
  out_diff = ( out[ ,c(2:4)] - trend3 )
  data_temp = data.frame( out_diff )
  data_temp$data = NA
  data_temp$time = 1:n
  data_temp$DGP = "DGP4"
  data_temp$type1 = "Residual"
  data_temp$trend = trend3
  data.out = rbind( data.out, data_temp) 
  

  
  ##################################


  #   
  x4 = x2 + dett_cyc
  trend4 = trend2+dett_cyc
  out = compare(x4)[,c(1,2,5,6)]
  
  
  data_temp = data.frame(out[, c(2:4,1)])
  data_temp$time = 1:n
  data_temp$DGP = "DGP6"
  data_temp$type1 = "Trend"
  data_temp$trend = trend4
  
  data.out = rbind(data.out, data_temp)
  

  
  out_diff =  out[ ,c(2:4)] - trend4 
  data_temp = data.frame( out_diff )
  data_temp$data = NA
  data_temp$time = 1:n
  data_temp$DGP = "DGP6"
  data_temp$type1 = "Residual"
  data_temp$trend = trend4
  data.out = rbind( data.out, data_temp) 

##################################  
  
data.out = data.out[ data.out$type1 == "Trend", ] # remove "type1"  
data.plot = melt( data.out, id.vars = c("time",  "DGP", "data", "trend"), measure.vars = c("HP", "bHP.BIC", "AR.4.") )
  
  
data.plot$variable = revalue( data.plot$variable, c("AR.4." = "AR(4)") )
data.plot$variable = revalue( data.plot$variable, c("bHP.BIC" = "bHP-BIC") )
data.plot$variable = ordered( data.plot$variable, levels = c("data", "HP", "bHP-BIC", "AR(4)") )

#############################



graph_self = function(data.plot ){

    p1 = ggplot(data.plot)
    
    
    p1 = p1 + geom_line(aes( x = time, y = value, col = variable  ), alpha = .7, lwd = 1 ) # +  scale_size_manual(values=c(0,1,1,1) ) 

    
    p1 <- p1 + geom_line(aes(x = time, y = trend),  colour = "gray50", lwd = 1, alpha = 0.5) 
    p1 <- p1 + geom_point(aes(x = time, y = trend),  colour = "gray50", size = 1 ) 
    
    p1 = p1 + scale_colour_manual(values = c( "red",  "#619CFF",    "violet") )
    
    
    p1 = p1 + facet_grid(    DGP ~.  , scales = "free_y" )
    
    theme1 = theme_bw() +   theme(legend.title.align = 0.5, 
            plot.title = element_text(hjust = 0.5),
            legend.position="bottom", 
            legend.title = element_blank(), 
            text = element_text(size = 16),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=16),
            axis.title = element_text(size=16),
            legend.text=element_text(size=16)
      )
    p1 = p1 + theme1 + labs(x= "", y = "")
    return(p1)
}



p1 = graph_self( data.plot[ (data.plot$DGP == "DGP4")|data.plot$DGP == "DGP6", ]     )


cairo_pdf("FigureB3.pdf", width = 12, height = 12, 
          family = "Times", fallback_resolution = 1000)
  plot(p1)
dev.off()
