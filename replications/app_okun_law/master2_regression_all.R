

rm(list = ls())

## set your work document path to "HP filter with AIC criterion"

library(rlist)

options(scipen = 2000) # scientific notation

# use case_names_load as the index of the data to be regress
# use case_names_save as the index of the regression results' name to be saved
# the loaded data are the results of running "HP_filter with AIC.R" in different cases

case_names_load <- c("hp_result_BIC.Rdata",
                     "hp_result_adf.RData" 
                     )
case_names_save <- c("regression_result_BIC",
                     "regression_result_adf"
                     )


for(i in 1:length(case_names_load)){ # you can omit this line and choose i to be any number of the
                                     # cases in thich you may want to see individually 
  
   load(case_names_load[i]) # regression the data of ith case 
  

#=========================================================
#------------------------------------
# for 20 countries
#------------------------------------
# set the empty list firstly

regression_hp_annual <- vector(mode = "list", length = 0L)
regression_hpraw_annual <- vector(mode = "list", length = 0L)
regression_hp_quarterly <- vector(mode = "list", length = 0L)
regression_hpraw_quarterly <- vector(mode = "list", length = 0L)

# for annual data of iteration HP-filter and raw HP-filter
coe_hp_annual <- rep(0,20)
coe_hpraw_annual <- rep(0,20)

adjr_hp_annual <- rep(0,20)
adjr_hpraw_annual <- rep(0,20)

r_hp_annual <- rep(0,20)
r_hpraw_annual <- rep(0,20)

p_hp_annual <- rep(0,20)
p_hpraw_annual <- rep(0,20)

#m <- length(row.names(gdp_20_annual)) # length of the period of data


for(s in 1 : 20)
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
  #sd <- coe$Std..Error
  
  p_hp_annual[s] <- coe_hp$Pr...t..
  p_hpraw_annual[s] <- coe_hpraw$Pr...t..
  
  coe_hpraw_annual[s] <- coe_hpraw$Estimate
  coe_hp_annual[s] <- coe_hp$Estimate
  
  adjr_hp_annual[s] <- regression_hp$adj.r.squared
  adjr_hpraw_annual[s] <- regression_hpraw$adj.r.squared
  
  r_hp_annual[s] <- regression_hp$r.squared
  r_hpraw_annual[s] <- regression_hpraw$r.squared
  
  # save the sth regression in lm_hp_s and lm_hpraw_s in the seperate list for checking
  regression_hp_annual <- list.append(regression_hp_annual, lm_hp)
  regression_hpraw_annual <- list.append(regression_hpraw_annual,lm_hpraw)
  
}


#---------------------------------------------------------------
# for quarterly data of iteration HP-filter and raw HP-filter

sumcoe_hp_quarterly <- rep(0,20)
sumcoe_hpraw_quarterly <- rep(0,20)

adjr_hp_quarterly <- rep(0,20)
adjr_hpraw_quarterly <- rep(0,20)

r_hp_quarterly <- rep(0,20)
r_hpraw_quarterly <- rep(0,20)

p_hp_quarterly <- vector(mode = "list", length = 0L)
p_hpraw_quarterly <- vector(mode = "list", length = 0L)

#m <- length(row.names(gdp_20_quarterly)) # length of the period of data

for(s in 1 : 20)
{
  message("regression on ",s,"th country's quarterly data",">>> ", Sys.time() )
  
  une <- as.numeric(na.omit(eval(parse(text = data_name[s+64])))) # quarterly raw une of US
  une_hp_cyc <- as.numeric(na.omit(eval(parse(text = paste0('cyc_of_',s+64))))) # cycle of quarterly iteration HP-filter
  une_hpraw_tr <- as.matrix(as.numeric(na.omit(eval(parse(text = paste0('data_tr_matr_',s+64))))))[,1] # trend of raw quarterly HP-filter
  
  
  gdp <- as.numeric(na.omit(eval(parse(text = data_name[s+44])))) # quarterly raw gdp of US
  gdp_hp_cyc <- na.omit(eval(parse(text = paste0('cyc_of_',s+44)))) # cycle of quarterly iteration HP-filter
  gdp_hpraw_tr <- as.matrix(as.numeric(na.omit(eval(parse(text = paste0('data_tr_matr_',s+44))))))[,1] # trend of raw quarterly HP-filter
  
  n <- length(une)
  
  une_cyc <- une_hp_cyc[3:n]
  gdp_cyc_1 <- gdp_hp_cyc[3:n] 
  gdp_cyc_2 <- gdp_hp_cyc[2:(n-1)] 
  gdp_cyc_3 <- gdp_hp_cyc[1:(n-2)] 
  
  une_cyc_raw <- une[3:n] - une_hpraw_tr[3:n]
  gdp_cyc_1_raw <- gdp[3:n] - gdp_hpraw_tr[3:n]
  gdp_cyc_2_raw <- gdp[2:(n-1)] - gdp_hpraw_tr[2:(n-1)]
  gdp_cyc_3_raw <- gdp[1:(n-2)] - gdp_hpraw_tr[1:(n-2)]
  
  lm_hp <- lm(une_cyc/100 ~ gdp_cyc_1 + gdp_cyc_2
              + gdp_cyc_3 - 1) # omitting intercept
  lm_hpraw <- lm(une_cyc_raw/100 ~ gdp_cyc_1_raw + gdp_cyc_2_raw
                 + gdp_cyc_3_raw - 1) # omitting intercept
  
  regression_hp <- summary(lm_hp)
  regression_hpraw <- summary(lm_hpraw)
  
  coe_hp <- data.frame(regression_hp$coefficients)
  coe_hpraw <- data.frame(regression_hpraw$coefficients)
  p_hp_quarterly <- list.append(p_hp_quarterly, coe_hp$Pr...t..)
  p_hpraw_quarterly <- list.append(p_hpraw_quarterly,coe_hpraw$Pr...t..)
  
  sumcoe_hpraw_quarterly[s] <- sum(lm_hpraw$coefficients)
  sumcoe_hp_quarterly[s] <- sum(lm_hp$coefficients)
  
  adjr_hp_quarterly[s] <- regression_hp$adj.r.squared
  adjr_hpraw_quarterly[s] <- regression_hpraw$adj.r.squared
  
  r_hp_annual[s] <- regression_hp$r.squared
  r_hpraw_annual[s] <- regression_hpraw$r.squared
  
  # save the sth regression in lm_hp_s and lm_hpraw_s in the seperate list for checking
  regression_hp_quarterly <- list.append(regression_hp_quarterly, lm_hp)
  regression_hpraw_quarterly <- list.append(regression_hpraw_quarterly,lm_hpraw)
  
}

# save all the regression results about annual data
annual_results <- data.frame(stop_gdp = stopping_time[3:22],
                        stop_une = stopping_time[23:42],
                        coe = coe_hp_annual,
                        R2 = adjr_hp_annual,
                        coe_raw = coe_hpraw_annual, 
                        R2_raw = adjr_hpraw_annual)

# save all the regression results about quarterly data
quarterly_results  <- data.frame( stop_gdp = stopping_time[45:64],
                            stop_une = stopping_time[65:84],
                            sum_coe = sumcoe_hp_quarterly, 
                            R2 = adjr_hp_quarterly, 
                            sum_coe_raw = sumcoe_hpraw_quarterly, 
                            R2_raw = adjr_hpraw_quarterly )

# assignment the saved regression results to the specified name 

assign(paste0(case_names_save[i],"_annual_results"),annual_results)  
assign(paste0(case_names_save[i],"_quarterly_results"),quarterly_results)


# remove everything except the results of regression and index of data name and i.

list_all <- ls()

rm(list=list_all[which(list_all != paste0(case_names_save[i],"_annual_results")
                       & list_all != paste0(case_names_save[i],"_quarterly_results")
                       & list_all != 'case_names_load'
                       & list_all != 'case_names_save'
                       & list_all != 'i'  )])

# save the remaining data into your work document with specified name

save.image(paste0(case_names_save[i],".RData"))

}


