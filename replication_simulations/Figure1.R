rm(list = ls())  

library(mFilter)
library(tseries)
library(ggplot2)
library(reshape)
library(scales)
library(magrittr)
library(plyr)


#----- get the matrix needed to get the iterated cycle of HP-filter--------
myhp <- function(n, lambda){
  # this matrix formulation is based on the 
  # R function "hpfilter" from package "mFilter"
  
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



# get the simulated data : structure break, random, trend



set.seed(20190707) # the seed for the table 


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
                                                       "Stoc. Trend+Mean Shift") )
#-------------------------------------------------------------------------------------------
theme1 = theme_bw( # base_family = "Times New Roman"
  ) +  theme(legend.title.align = 0.5, 
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom", 
        text = element_text(size = 22),
        legend.title=element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=22),
        axis.title = element_text(size=22),
        legend.text=element_text(size=22)
        )




# data_ggplot = data_ggplot[ data_ggplot$type != "Stoc. Trend", ]
data_ggplot$type = droplevels(data_ggplot$type)



p1 <- ggplot(data_ggplot) 


p1 <- p1 + geom_line( aes(ID, deter), colour = "gray70", linetype = 2)

p1 <- p1 + geom_line(aes(ID, trend),  colour = "gray50") 
p1 <- p1 + geom_point(aes(ID, trend),  colour = "gray50", size = 0.2 ) # the trend 


# cover the break with white color

br <- data.frame(x1 = c(0,0,50), x2 = c(0,0,51), y1 = c(0,0,0), y2 = c(0,0,25) )
# br <- data.frame(x1 = c(0,50), x2 = c(0,51), y1 = c(0,0), y2 = c(0,25) )
br$type = as.factor( levels( data_ggplot$type ) )
p1 = p1 + geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), color = "white", data = br, size = 1 )

# the key curves

p1 <- p1 + geom_line( aes(ID, value , colour = iteration )) + #, alpha = 0.5 ) +
  scale_color_brewer(name = "Iterations m=", palette = "RdBu" , type = "div", 
                     labels = as.character(pick_iter) ) +
  guides(colour = guide_legend(nrow = 1))
p1 <- p1 + geom_point( aes(ID, rawdata),  size = 1 ) 

p1 <- p1 + facet_grid( type ~.,  scales = "free_y")
p1 <- p1 +  theme1
p1 <- p1 + ylab(" ") + xlab("Time") 






########################3 rediausl
data_ggplot$residual = data_ggplot$rawdata -   data_ggplot$value 
data_ggplot$residual_true = data_ggplot$rawdata -   data_ggplot$trend
data_ggplot$dev_trend = data_ggplot$value - data_ggplot$trend   


p2 <- ggplot(data_ggplot) 
p2 <- p2 + geom_line( aes(ID, 0), colour = "gray70", linetype = 2)
p2 <- p2 + geom_line( aes(ID, dev_trend , colour = iteration ), alpha = 0.5 ) +
  scale_color_brewer(name = "Iterations m = ", palette = "RdBu" , type = "div", 
                     labels = as.character(pick_iter) ) +
  guides(colour = guide_legend(nrow = 1))

p2 <- p2 + facet_grid( type ~.  , scales = "free_y")
p2 <- p2 +  theme1 + theme(legend.position="none" )
p2 <- p2 + ylab(" ")  + xlab("") 

#####################3



cairo_pdf( "Figure1.pdf", width = 15, height = 16, family = "Times", fallback_resolution = 1000)
  print(p1)
dev.off()
# ggsave("Figure1.pdf", plot = p1, width = 15, height = 16, units = "cm", dpi = 1000 )

summ = ddply(.data = data_ggplot, .(type, iteration), summarize,  sq_dev = sum( dev_trend^2 )   )
print(summ)

