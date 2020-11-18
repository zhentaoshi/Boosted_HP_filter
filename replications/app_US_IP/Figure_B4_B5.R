#Import the data
rm(list = ls() )


#Getting the recession data


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
  ggp = ggplot(series_df, aes(x = DATE, y = value, color = variable )) + geom_point( )
  
  if ( all(series_df$variable == "IP" )  ){
    ggp = ggp + scale_colour_manual(values =  "black", labels = c( "IP"))
  } else {
    ggp = ggp + geom_line() + scale_colour_manual(values = c( "red", "blue", "violet"), labels = c( "HP", "bHP-BIC", "AR4"))
    ggp = ggp + geom_hline(yintercept=0 )
  }
  ggp = ggp + xlab("")+ ylab("") + facet_grid( variable ~. ) 
  
  
  ggp = ggp + annotate("rect",xmin = as.Date(recess_df$start, "%Y-%m-%d"),
                                 xmax= as.Date(recess_df$end, "%Y-%m-%d"),
                                 ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")
  
  
  if ( recess_df[1,1] < "1921-01-01" ) {
      ggp = ggp + scale_x_date(date_breaks = "5 year", date_minor_breaks = "1 year", date_labels= "%Y")
  } else {
    ggp = ggp + scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels= "%Y" )
  }
  
  if ( !any(series_df$variable == "IP" )  ){ 
    ggp = ggp + theme(axis.text.x = element_blank(),axis.ticks = element_blank() )
  }
  
  ggp = ggp + theme_bw()+ theme(legend.position = "none")
  
  return(ggp)
}


load("date_recession.Rdata")


recession_1919.df = recession.df[recession.df$start>=min(dcycle_1919$DATE),]
recession_2000.df = recession.df[recession.df$start>=min(dcycle_2000$DATE),]

#Prepare the data
data_1919 <- dcycle_1919 %>%
  select(DATE, `HP`, `BIC`, `AR4`) %>%
  gather(key = "variable", value = "value", -DATE)
data_1919$variable[ data_1919$variable == "BIC"] = "bHP-BIC"
data_1919$variable = factor(data_1919$variable, levels = c("HP", "bHP-BIC", "AR4") )

data_2000 <- dcycle_2000 %>%
  select(DATE, `HP`, `BIC`, `AR4`) %>%
  gather(key = "variable", value = "value", -DATE)
data_2000$variable[ data_2000$variable == "BIC"] = "bHP-BIC"
data_2000$variable = factor(data_2000$variable, levels = c("HP", "bHP-BIC", "AR4") )

#####################################
data_1919_IP <- dcycle_1919 %>%
  select(DATE, `IP`) %>%
  gather(key = "variable", value = "value", -DATE)


ggp_1919 = graph_fun(data_1919, recession_1919.df )
ggp_1919_IP = graph_fun(data_1919_IP, recession_1919.df )
ggp_1 = grid.arrange(ggp_1919_IP, ggp_1919, nrow=2)



cairo_pdf( "FigureB4.pdf", 
           width = 10,  height = 8,
           fallback_resolution = 1000, family = "Times")
grid.arrange(ggp_1919_IP, ggp_1919, nrow=2)
dev.off()

##################################################
data_2000_IP <- dcycle_2000 %>%
  select(DATE, `IP`) %>%
  gather(key = "variable", value = "value", -DATE)


ggp_2000 = graph_fun(data_2000, recession_2000.df )
ggp_2000_IP = graph_fun(data_2000_IP, recession_2000.df )




cairo_pdf( "FigureB5.pdf", 
           width = 10,  height = 8,
           fallback_resolution = 1000, family = "Times")
grid.arrange(ggp_2000_IP, ggp_2000, nrow=2)
dev.off()
