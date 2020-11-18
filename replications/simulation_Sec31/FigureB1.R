

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

####################################

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
data_ggplot$type <- factor(data_ggplot$type,levels = c("Stoc. Trend",
                                                       "Stoc.+Deter. Trends",
                                                       "Stoc. Trend+Mean Shift"))
#-------------------------------------------------------------------------------------------
theme1 = theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position="none", 
    text = element_text(size = 12),
    legend.title=element_text(size=12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
    legend.text=element_text(size=12)
  )


types =  levels( data_ggplot$type ) 
itera = levels(data_ggplot$iteration )



ii = types[2] # decide what group of graphs to show





data_temp = data_ggplot[ data_ggplot$iteration == itera[1], ]
data_temp = data_temp[ (data_temp$type == ii),  ]
p1 <- ggplot(data_temp) 


p1 <- p1 + geom_line(aes(ID, trend),  colour = "gray50") 
p1 <- p1 + geom_point(aes(ID, trend),  colour = "gray50", size = 0.2 ) # the trend 


p1 <- p1 +  theme1
p1 <- p1 + ylab(" ") + xlab("") 



br <- data.frame(x1 = c(0,0,50), x2 = c(0,0,51), y1 = c(0,0,0), y2 = c(0,0,25) )
br$type = as.factor( levels( data_ggplot$type ) )

# the key curves

p1 <- p1 + geom_line( aes(ID, value , colour = iteration )) # +  


cairo_pdf( "FigureB1a.pdf", width = 8, height = 4,
           family="Times", fallback_resolution = 1000   )  
print(p1)
dev.off()






x = ts(data_temp$rawdata)
regAR4 = dynlm( x  ~ L(x, c(1:4) ) )
AR4_series = c( rep(NA,4), predict(regAR4) )


data_temp = data_ggplot[ data_ggplot$iteration == itera[1], ]

data_temp = data_temp[ (data_temp$type == ii),  ]


p1 <- ggplot(data_temp) 
p1 <- p1 + geom_line(aes(ID, trend),  colour = "gray50") 
p1 <- p1 + geom_point(aes(ID, trend),  colour = "gray50", size = 0.2 ) # the trend 
p1 = p1 + geom_line( aes(ID, AR4_series), colour = "violet")

p1 <- p1 +  theme1
p1 <- p1 + ylab(" ") + xlab("") 

if (ii == types[1]) { p1 = p1 + ylim(-5,10) }

cairo_pdf( "FigureB1c.pdf", width = 8, height = 4,
           family="Times", fallback_resolution = 1000   )  
print(p1)
dev.off()



data_temp = data_ggplot[ (data_ggplot$type == ii),  ]
data_temp$type = droplevels(data_temp$type)

data_temp = data_temp[ data_temp$iteration == itera[8], ]
data_temp$iteration = droplevels(data_temp$iteration)




# use bHP-BIC


p1 <- ggplot(data_temp) 
p1 <- p1 + geom_line(aes(ID, trend),  colour = "gray50") 
p1 <- p1 + geom_point(aes(ID, trend),  colour = "gray50", size = 0.2 ) # the trend 
p1 <- p1 +  theme1
p1 <- p1 + ylab(" ") + xlab("") 

if (ii == types[1]) { p1 = p1 + ylim(-5,10) }

br <- data.frame(x1 = c(0,0,50), x2 = c(0,0,51), y1 = c(0,0,0), y2 = c(0,0,25) )
br$type = as.factor( levels( data_ggplot$type ) )

# the key curves
source("BoostedHP_pub.R")
bHP_BIC = BoostedHP(x, lambda = 1600, iter= TRUE, test_type = "BIC", sig_p = 0.1,  Max_Iter = 100)
bHP_BIC_trend = bHP_BIC$trend


p1 <- p1 + geom_line( aes(ID, value ), color = "#107FDF" )
p1 <- p1 + geom_line( aes(ID, bHP_BIC_trend ), color = "orange" ) 

cairo_pdf( "FigureB1b.pdf", width = 8, height = 4,
           family="Times", fallback_resolution = 1000   )  
print(p1)
dev.off()

