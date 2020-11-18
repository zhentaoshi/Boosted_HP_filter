## import the data " deseasoned " data as the time series for 
## all countries

rm(list=ls())
source("BoostedHP.R")

start_time<-Sys.time()

options(scipen = 2000) # scientific notation 

#--------------- set your work document where there are seperated_countries_data------------ 
# setwd("..")
setwd("data/seperated_countries_data")
#-------------------------------------------------------------------------------------------

library(rlist)
library(stringr)
#library(mFilter)
#----------------------1---- you can use this command to get the countryfile_name----------------------------------------------
# countryfile_name <- dir(getwd()) # to get the name list of all the imported original data ordered by country name
#----------------------2---- or you can just run the below command which I typied all the country_file name for you------------

countryfile_name <- c("argentina.csv", "australia.csv", "austria.csv", "belgium.csv", 
  "brazil.csv", "canada.csv", "denmark.csv", "ecuador.csv", "finland.csv", 
  "israel.csv", "korea.csv", "malaysia.csv", "mexico.csv", "netherlands.csv", 
  "new zealand.csv", "norway.csv", "peru.csv", "philippines.csv", 
  "portugal.csv", "slovak rep.csv", "south africa.csv", "spain.csv", 
  "sweden.csv", "switzerland.csv", "thailand.csv", "turkey.csv"
)
#------------------------------------------------------------------------------------------------------------------------------

# need the function "function_hp_all.R" to be in the path which Rstudio could search

#---------------------------------------------
# set the parameter we need before looping

lambda <- 1600
sig_p <- 0.010
# Max_Iter <- 50
#---------------------------------------------
# set the empty vector first

statistic_raw <- rep(0,9)
statistic_AIC <- rep(0,9)
statistic_BIC <- rep(0,9)
statistic_adf <- rep(0,9)
statistic_pp <- rep(0,9)

# save the cycle of each country's HP-cycle component
data_cyc_raw <- vector(mode = "list",length = length(countryfile_name))
data_cyc_AIC <- vector(mode = "list",length = length(countryfile_name))
data_cyc_BIC <- vector(mode = "list",length = length(countryfile_name))
data_cyc_adf <- vector(mode = "list",length = length(countryfile_name))
data_cyc_pp <- vector(mode = "list",length = length(countryfile_name))

# mark the length of each time series
length <- matrix(0,nrow = length(countryfile_name),ncol = 3)
# mark the date of each country's data set
date <- vector(mode = "list",length = length(countryfile_name))

#---------------------------------------------
# construct a new function for convenience

moments <- function(data,filter_type) # filter_type are: raw, AIC,BIC,adf,pp
{ 
  
  
  data <-data[2:length(data[,1]),c(2,3,7)]  
  data <- apply(data,2,as.numeric) %>% log() %>% na.omit()
  if(filter_type == "raw"){
    data_cyc <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= FALSE)$cycle)
    num <- c(1,1,1) 
    }          
              
  
  if(filter_type == "AIC"){
    #hp_results <- apply(data,2,function(x) HP_filter(x, lambda,  iter= T, test_type = "AIC"))
     
    data_cyc <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, sig_p = sig_p, test_type = "AIC")$cycle)
    num <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, test_type = "AIC")$iter_num )
    }
  
  if(filter_type == "BIC"){
    data_cyc <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, sig_p = sig_p, test_type = "BIC")$cycle)
    num <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, test_type = "BIC")$iter_num )
    }
  if(filter_type == "adf"){
    
    data_cyc <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, sig_p = sig_p, test_type = "adf")$cycle)
    num <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, test_type = "adf")$iter_num )
    }
  if(filter_type == "pp"){
    
    data_cyc <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, sig_p = sig_p, test_type = "pp")$cycle)
    num <- apply(data,2,function(x) BoostedHP(x, lambda,  iter= TRUE, test_type = "pp")$iter_num )
    }
  
  data_cyc <- data.frame(data_cyc)
  names(data_cyc) <- c("C","I","GDP")
  sd <- apply(data_cyc,2 , function(x) sd(x))
  sd_y <- sd[3];rho_y <- cor(data_cyc$GDP[-1],data_cyc$GDP[-length(data_cyc$GDP)])
  #rho <- apply(data_cyc,2 , function(x) cor(x) )
  sdc_over_sdy <- sd[1]/sd[3]
  sdi_over_sdy <- sd[2]/sd[3]
  #sd_tb_over_y <- sd(data_cyc$NX/data_cyc$GDP)
  #rho_tb_over_y_y <- cor(data_cyc$NX/data_cyc$GDP,data_cyc$GDP)
  rho_c_y <- cor(data_cyc$C,data_cyc$GDP)
  rho_i_y <- cor(data_cyc$I,data_cyc$GDP)
  iter_C <- num[1]
  iter_I <- num[2]
  iter_GDP <- num[3]
  result <- list(data_cyc,data.frame(sd_y,rho_y,sdc_over_sdy,sdi_over_sdy,rho_c_y,rho_i_y,iter_C,iter_I,iter_GDP ))
}

#---------------------------------------------
# start looping for 26 countries

for (i in 1: length(countryfile_name)){
  
  # import the ith country
  
  message("dealing with the ",i,"th country at the time of ",Sys.time())
  data <- read.csv(countryfile_name[i]) # import the data from the file :seperated_countries_data
  
  # import the length of each time series of country i
  
  length[i,1] <- length(data[,2])-1
  length[i,2] <- length(data[,3])-1
  length[i,3] <- length(data[,7])-1
  
  # import the date of dataset of country i
  date[[i]] <- as.character(data[,1])[-1]
  
  # calculate the key moments of sevarel type of filtered data of country i
  
  result_raw <- moments(data,filter_type="raw")
  data_cyc_raw[[i]] <- result_raw[[1]]# collect the cycle component of each country of C,I,GDP
  result_raw <- result_raw[[2]]
  
  result_AIC <- moments(data,filter_type="AIC")
  data_cyc_AIC[[i]] <- result_AIC[[1]]
  result_AIC <- result_AIC[[2]]
  
  result_BIC <- moments(data,filter_type="BIC")
  data_cyc_BIC[[i]] <- result_BIC[[1]]
  result_BIC <- result_BIC[[2]]
  
  result_adf <- moments(data,filter_type="adf")
  data_cyc_adf[[i]] <- result_adf[[1]]
  result_adf <- result_adf[[2]]
  
  result_pp <- moments(data,filter_type="pp")
  data_cyc_pp[[i]] <- result_pp[[1]]
  result_pp <- result_pp[[2]]
  
  # collect the statistical data of sevarel type of filtered data of country i
  
  statistic_raw <- rbind(statistic_raw,result_raw)
  statistic_AIC <- rbind(statistic_AIC,result_AIC)
  statistic_BIC <- rbind(statistic_BIC,result_BIC)
  statistic_adf <- rbind(statistic_adf,result_adf)
  statistic_pp <- rbind(statistic_pp,result_pp)
  
  
  
}


statistic_raw <- statistic_raw[-1,]
statistic_AIC <- statistic_AIC[-1,]
statistic_BIC <- statistic_BIC[-1,]
statistic_adf <- statistic_adf[-1,]
statistic_pp <- statistic_pp[-1,]

#---------------just to mark the country name for check-------------------------------------

row.names(statistic_raw) <- strsplit(countryfile_name,".csv", fixed = TRUE)
row.names(statistic_AIC) <- strsplit(countryfile_name,".csv", fixed = TRUE)
row.names(statistic_BIC) <- strsplit(countryfile_name,".csv", fixed = TRUE)
row.names(statistic_adf) <- strsplit(countryfile_name,".csv", fixed = TRUE)
row.names(statistic_pp) <- strsplit(countryfile_name,".csv", fixed = TRUE)
row.names(length) <- strsplit(countryfile_name,".csv", fixed = TRUE)
length <- as.data.frame(length)
names(length) <- c("C","Y","GDP")

#-------------------------------------------------------------------------------------


#--------------------mean of emerging and developed countries----------------------------
# get the mean of the key statistics of the emerging market and developed market separately


all_country <- c("argentina", "australia", "austria", "belgium", "brazil", 
                 "canada", "denmark", "ecuador", "finland", "israel", "korea", 
                 "malaysia", "mexico", "netherlands", "new zealand", "norway", 
                 "peru", "philippines", "portugal", "slovak rep", "south africa", 
                 "spain", "sweden", "switzerland", "thailand", "turkey")
emerging_country <- c("argentina",  "brazil", 
                       "ecuador",  "israel", "korea", 
                      "malaysia", "mexico",  
                      "peru", "philippines",  "slovak rep", "south africa", 
                       "thailand", "turkey")

developed_country <- c( "australia", "austria", "belgium", 
                      "canada", "denmark",  "finland", 
                      "netherlands", "new zealand", "norway", 
                       "portugal", 
                      "spain", "sweden", "switzerland")


#------------------------------------------------------------------------------------------------------- 
#-----------get the index of the location of countries of two kinds in the data.frame-----

index_emerging <- match(emerging_country,all_country) 
index_developed <- match(developed_country,all_country)

#-----------emerging country------------------------------------

emerging_country_raw <- apply( statistic_raw[index_emerging,],2,function(x) mean(x))
emerging_country_AIC <- apply( statistic_AIC[index_emerging,],2,function(x) mean(x))
emerging_country_BIC <- apply( statistic_BIC[index_emerging,],2,function(x) mean(x))
emerging_country_adf <- apply( statistic_adf[index_emerging,],2,function(x) mean(x))
emerging_country_pp <- apply( statistic_pp[index_emerging,],2,function(x) mean(x))
emerging_country_length <- apply(length[index_emerging,],2, function(x) mean(x))

#-----------developed country-----------------------------------
developed_country_raw <- apply( statistic_raw[index_developed,],2,function(x) mean(x))
developed_country_AIC <- apply( statistic_AIC[index_developed,],2,function(x) mean(x))
developed_country_BIC <- apply( statistic_BIC[index_developed,],2,function(x) mean(x))
developed_country_adf <- apply( statistic_adf[index_developed,],2,function(x) mean(x))
developed_country_pp <- apply( statistic_pp[index_developed,],2,function(x) mean(x))
developed_country_length <- apply(length[index_developed,],2, function(x) mean(x))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# this part is just to get the subset of the key moments of emerging countries
# and developed countries separately
#++++++++++++++++ you can skip this part++++++++++++++++++++++++++++++++++++++++++++++++

#-----------emerging country------------------------------------

emerging_country_raw_mom <- statistic_raw[index_emerging,]
emerging_country_AIC_mom <- statistic_AIC[index_emerging,]
emerging_country_BIC_mom <- statistic_BIC[index_emerging,]
emerging_country_adf_mom <- statistic_adf[index_emerging,]
emerging_country_pp_mom <- statistic_pp[index_emerging,]
emerging_country_length_detail <- length[index_emerging,]

#-----------developed country-----------------------------------
developed_country_raw_mom <- statistic_raw[index_developed,]
developed_country_AIC_mom <- statistic_AIC[index_developed,]
developed_country_BIC_mom <- statistic_BIC[index_developed,]
developed_country_adf_mom <- statistic_adf[index_developed,]
developed_country_pp_mom <- statistic_pp[index_developed,]
developed_country_length_detail <- length[index_developed,]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

result_all <- data.frame(emerging_country_raw,developed_country_raw,
                         emerging_country_BIC,developed_country_BIC,
                         emerging_country_adf,developed_country_adf
                         
                         )

rm(data,i,result_adf,result_AIC,result_BIC,result_pp,result_raw  )

end_time <- Sys.time()

setwd("..")
setwd("..")
save.image("results_moments.RData")

