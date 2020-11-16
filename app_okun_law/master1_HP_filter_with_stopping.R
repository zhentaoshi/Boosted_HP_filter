# H-P filter

# ===============================================================
# data description: 
# find further detail from data_cleaning  
#
# Note : there are some NAs of unemployment rate data. 
#
#=====================================================================
rm(list = ls())

source("BoostedHP.R")

# load the data

load("datawb_name.RData")
load("gdp_all.RData")
load("une_all.RData")
load("data_wb_new.RData")

start_time<-Sys.time()

# pair the NA of unemployment rate with GDP
# take log of GDP

gdp_20_annual[is.na(unewb_20_annual)] <- NA
gdp_20_quarterly[is.na(une_20_quarterly)] <- NA
gdp_us_annual <- gdp_us_annual[,(length(gdp_us_annual)-length(une_us_annual)+1):length(gdp_us_annual)]
gdp_us_quarterly <- gdp_us_quarterly[,(length(gdp_us_quarterly)-length(une_us_quarterly)+1):length(gdp_us_quarterly)]


gdp_20_annual <- log(gdp_20_annual)
gdp_20_quarterly <- log(gdp_20_quarterly)
gdp_us_annual <- log(gdp_us_annual)
gdp_us_quarterly <- log(gdp_us_quarterly)

#------------------------------------

# start looping for the 84 sets of data
# set the parameter firstly

Max_Iter <- 100L # the maximum number of the iteration time
sig_p <- 0.01 # it is set slightly than 1%, because the reported p-value of adf is 1% if smaller than that
stopping_time <- rep(0,length(data_name))

############################################################
# looping for annual data

N = length(data_name)

for (test_type in c("BIC", "adf")){
  for(s in 1:N){
    
    if (s %in% 1:(N/2) ) {
      lambda = 100 # for annual data
    } else {
      lambda = 1600 # for quarterly data
    }
    
    data <- eval(parse(text = data_name[s]))
    data <- na.omit(as.numeric(data))
    
    message("smoothing for the data : ",parse(text = data_name[s]))
    
    results <- BoostedHP(data, lambda, iter=TRUE, test_type = test_type, sig_p = sig_p, Max_Iter = Max_Iter)
    assign(paste0('data_tr_matr_',s),results$trend_hist)
    assign(paste0('cyc_of_',s),results$cycle)
    stopping_time[s] <- results$iter_num
    message("stop at ",results$iter_num,"th iteration time")
  }
  save.image ( paste0("hp_result_", test_type, ".RData") )
  
} # end the loop of stopping criterion


