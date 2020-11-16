
rm(list = ls())

library(rlist)
library(plyr)

options(scipen = 2000) # scientific notation

load("hp_result_adf.RData") # regression the data of ith case 

regression_hp_annual <- vector(mode = "list", length = 0L)
regression_hpraw_annual <- vector(mode = "list", length = 0L)

for(s in 9) # 9 for Ireland
{
  message("regression on ",s,"th country's annual data",">>> ", Sys.time() )
  une <- as.numeric(na.omit(eval(parse(text = data_name[s+22])))) # annual raw une of US
  une_hp_cyc <- as.numeric(na.omit(eval(parse(text = paste0('cyc_of_',s+22))))) # cycle of annual iteration HP-filter
  une_hpraw_tr <- as.matrix(na.omit(eval(parse(text = paste0('data_tr_matr_',s+22)))))[,1] # trend of raw annual HP-filter
  
  
  gdp <- as.numeric(na.omit(eval(parse(text = data_name[s+2])))) # annual raw gdp of US
  gdp_hp_cyc <- na.omit(eval(parse(text = paste0('cyc_of_',s+2)))) # cycle of annual iteration HP-filter
  gdp_hpraw_tr <- as.matrix(na.omit(eval(parse(text = paste0('data_tr_matr_',s+2)))))[,1] # trend of raw annual HP-filter
  
  une_cyc_raw <- une - une_hpraw_tr
  gdp_cyc_raw <- gdp - gdp_hpraw_tr
  
  # no intercept is allowed
  lm_hp <- lm( une_hp_cyc/100 ~ gdp_hp_cyc -1 ) # omitting intercept
  lm_hpraw <- lm(une_cyc_raw/100 ~ gdp_cyc_raw -1) # omitting intercept
  
  regression_hp <- summary(lm_hp)
  regression_hpraw <- summary(lm_hpraw)
  
  coe_hp <- data.frame(regression_hp$coefficients)
  coe_hpraw <- data.frame(regression_hpraw$coefficients)
}


gdp_raw = gdp_cyc_raw
gdp_adf = gdp_hp_cyc
une_raw = une_cyc_raw
une_adf = une_hp_cyc


load("hp_result_BIC.RData") # regression the data of ith case 

regression_hp_annual <- vector(mode = "list", length = 0L)
regression_hpraw_annual <- vector(mode = "list", length = 0L)

for(s in 9) # 9 for Ireland
{
  message("regression on ",s,"th country's annual data",">>> ", Sys.time() )
  une <- as.numeric(na.omit(eval(parse(text = data_name[s+22])))) # annual raw une of US
  une_bic <- as.numeric(na.omit(eval(parse(text = paste0('cyc_of_',s+22))))) # cycle of annual iteration HP-filter
  une_hpraw_tr <- as.matrix(na.omit(eval(parse(text = paste0('data_tr_matr_',s+22)))))[,1] # trend of raw annual HP-filter
  
  
  gdp <- as.numeric(na.omit(eval(parse(text = data_name[s+2])))) # annual raw gdp of US
  gdp_bic <- na.omit(eval(parse(text = paste0('cyc_of_',s+2)))) # cycle of annual iteration HP-filter
  gdp_hpraw_tr <- as.matrix(na.omit(eval(parse(text = paste0('data_tr_matr_',s+2)))))[,1] # trend of raw annual HP-filter
  

  
  une_cyc_raw <- une - une_hpraw_tr
  gdp_cyc_raw <- gdp - gdp_hpraw_tr
  
  # no intercept is allowed
  lm_hp <- lm( une_hp_cyc/100 ~ gdp_hp_cyc -1 ) # omitting intercept
  lm_hpraw <- lm(une_cyc_raw/100 ~ gdp_cyc_raw -1) # omitting intercept
  
  regression_hp <- summary(lm_hp)
  regression_hpraw <- summary(lm_hpraw)
  
  coe_hp <- data.frame(regression_hp$coefficients)
  coe_hpraw <- data.frame(regression_hpraw$coefficients)
}

##########################33 start ggplot 2 #####################3

library(ggplot2)
library(reshape2)
library(gridExtra)
library(lattice)

# graph 1

data.simple = data.frame(year = 1981:2016, minus_unemployment = -une_cyc_raw/100, GDP = gdp_cyc_raw)
data.simple = melt(data.simple, measure.vars = c( "GDP", "minus_unemployment") )
names(data.simple)[2] = "series"

theme1 = theme_bw() +  theme(axis.title.x = element_blank(),
                             strip.text = element_text( size = 12),
                             axis.text = element_text( size = 12),
                             legend.title = element_blank(),
                             legend.text = element_text( size = 12))

p1 = ggplot(data.simple) + geom_line(aes( x = year, y = value, color = series, lty = series), lwd = 2) + theme1
p1 = p1 +  labs( x = "", y = "") +
  ggtitle( "Simple HP filter. GDP (solid red) vs. (negative) employment (dash blue) " )
p1 = p1 + scale_color_manual(values=c("red", "blue"))


# graph 2

data.iter = data.frame(year = 1981:2016, minus_unemployment = -une_hp_cyc/100, GDP = gdp_hp_cyc)
data.iter = melt(data.iter, measure.vars = c( "GDP", "minus_unemployment") )
names(data.iter)[2] = "series"

p2 = ggplot(data.iter) + geom_line(aes( x = year, y = value, color = series, lty = series), lwd = 2) + theme1
p2 = p2 +  labs( x = "", y = "") + 
  ggtitle( "Iterated HP filter stopped by ADF. GDP (19 iter., solid red) vs. (negative) employment (2 iter., dash blue)" )
p2 = p2 + scale_color_manual(values=c("red", "blue"))

# graph 3
data.gdp = data.frame(year = 1981:2016, HP = gdp_cyc_raw, ADF = gdp_hp_cyc, BIC = gdp_bic)
data.gdp = melt(data.gdp, measure.vars = c( "HP", "ADF", "BIC") )


data.gdp$variable = revalue( data.gdp$variable, c("BIC" = "bHP-BIC") )
data.gdp$variable = revalue( data.gdp$variable, c("ADF" = "bHP-ADF") )

names(data.gdp)[2] = "series"

p3 = ggplot(data.gdp) + geom_line(aes( x = year, y = value, color = series), lwd = 2) + theme1
p3 = p3 +  labs( x = "", y = "") +   theme( legend.position = "none" ) +ggtitle( "GDP")


##########################3
# graph 4
data.une = data.frame(year = 1981:2016, HP = -une_cyc_raw/100, ADF = -une_hp_cyc/100, BIC = -une_bic/100 )
data.une = melt(data.une, measure.vars = c( "HP", "ADF", "BIC") )


data.une$variable = revalue( data.une$variable, c("BIC" = "bHP-BIC") )
data.une$variable = revalue( data.une$variable, c("ADF" = "bHP-ADF") )

names(data.une)[2] = "series"

p4 = ggplot(data.une) + geom_line(aes( x = year, y = value, color = series ), lwd = 2) + theme1
p4 = p4 +  labs( x = "", y = "") + theme( legend.position = "bottom" ) + ggtitle( "(negative) unemployment rate" )


cairo_pdf("Figure6.pdf", width = 6, height = 6,
          family = "Times", fallback_resolution = 1000)
  grid.arrange( p3, p4, nrow=2)
dev.off()

