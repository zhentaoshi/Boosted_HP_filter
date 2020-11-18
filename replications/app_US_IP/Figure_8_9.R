#Import the data
rm(list = ls() )


load(file = "residuals.Rdata")
load(file = "residuals_y.Rdata")
dcycle$IP = df$y

#Two sets of graph starting from 1919-01-01 and 2000-01-01
dcycle_1919 = dcycle
dcycle_2000 = dcycle[as.Date(dcycle$DATE)>="2000-01-01",]

#Load the required packages for the graph
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

library(lattice)
library(gridExtra)


graph_fun <- function(series_df, recess_df)  {
  ggp = ggplot(series_df, aes(x = DATE, y = IP )) + geom_point( )
  
  if (colnames(series_df[2]) == "HP" ){
    ggp = ggp + geom_line(aes(x = DATE, y = IP - HP ), color = "red", size = 1, alpha=0.9 )
  } else if ( (colnames(series_df[2]) == "BIC" ) ){
    ggp = ggp + geom_line(aes(x = DATE, y = IP - BIC ), color = "blue", size = 1, alpha=0.9 )
  } else if ((colnames(series_df[2]) == "AR4" )){
    ggp = ggp + geom_line(aes(x = DATE, y = IP - AR4 ), color = "violet", size = 1, alpha=0.9 )
  }

  ggp = ggp + xlab("")+ ylab("") 
  ggp = ggp + annotate("rect",xmin = as.Date(recess_df$start, "%Y-%m-%d"),
                                 xmax= as.Date(recess_df$end, "%Y-%m-%d"),
                                 ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")
  
  # browser()
  if ( recess_df[1,1] < "1990-01-01" ) {
      ggp = ggp + scale_x_date(date_breaks = "5 year", date_minor_breaks = "1 year", date_labels= "%Y")
  } else {
    ggp = ggp + scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels= "%Y" )
  }
  ggp = ggp + theme_bw()+ theme(legend.position = "none")
  
  return(ggp)
}


## recession dates
load("date_recession.Rdata")
recession_1919.df = recession.df[recession.df$start>=min(dcycle_1919$DATE),]
recession_2000.df = recession.df[recession.df$start>=min(dcycle_2000$DATE),]


ggp_1919 = graph_fun(dcycle_1919[, c("DATE","HP","IP")], recession_1919.df )
cairo_pdf("Figure8a.pdf", width = 12, height = 6, family = "Times",
          fallback_resolution = 1000)
print(ggp_1919)
dev.off()

ggp_1919 = graph_fun(dcycle_1919[, c("DATE","BIC","IP")], recession_1919.df )
cairo_pdf("Figure8b.pdf", width = 12, height = 6, family = "Times",
          fallback_resolution = 1000)
print(ggp_1919)
dev.off()


ggp_1919 = graph_fun(dcycle_1919[, c("DATE","AR4","IP")], recession_1919.df )
cairo_pdf("Figure8c.pdf", width = 12, height = 6, family = "Times",
          fallback_resolution = 1000)
print(ggp_1919)
dev.off()




####################################

ggp_2000 = graph_fun(dcycle_2000[, c("DATE","HP","IP")], recession_2000.df )
cairo_pdf("Figure9a.pdf", width = 12, height = 6, family = "Times",
          fallback_resolution = 1000)
  print(ggp_2000)
dev.off()

ggp_2000 = graph_fun(dcycle_2000[, c("DATE","BIC","IP")], recession_2000.df )
cairo_pdf("Figure9b.pdf", width = 12, height = 6, family = "Times",
          fallback_resolution = 1000)
print(ggp_2000)
dev.off()


ggp_2000 = graph_fun(dcycle_2000[, c("DATE","AR4","IP")], recession_2000.df )
cairo_pdf("Figure9c.pdf", width = 12, height = 6, family = "Times",
          fallback_resolution = 1000)
print(ggp_2000)
dev.off()


