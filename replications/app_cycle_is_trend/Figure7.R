rm(list = ls())

library(ggplot2)
library(reshape2)
library(scales)

#========================================================================================================================
#------------------------------------------------------
# part 1 : manage data for ggplot structure
#------------------------------------------------------


load("results_moments.RData")

# remove everything except the useful time series
index <- !(ls() %in% c("data_cyc_adf","data_cyc_AIC","data_cyc_BIC",
                      "data_cyc_pp","data_cyc_raw","date","emerging_country",
                      "developed_country","all_country")) 
rm(list = ls()[index])


date_seq <- seq(from = 1979.75, to = 2003.25, by = 0.25)

#-----------------------------------------------------------

index_hp_type <- c("data_cyc_raw","data_cyc_AIC","data_cyc_BIC",
                   "data_cyc_adf","data_cyc_pp")
hp_type <- c("type 1 : raw","AIC","type 2 : BIC","type 3 : adf","pp")

for(j in 1 : length(index_hp_type)){
  
  message("dealding with #",hp_type[j],"# type of iteration HP-filter at ",Sys.time())
  
  data_cyc_j <- eval(parse(text = index_hp_type[j]))
  message("start loop for 26 countries")

  data_cyc <- data.frame(data_cyc_j[[1]])
  data_cyc$country <- all_country[1]
  
  # use reshape2 package to get the melt data frame of C,Y,GDP
  data_cyc <- melt(data_cyc, ID=c("country"))
  data_cyc$date <- rep(date_seq[(96-(length(data_cyc[,1])/3)):95],3)
  
  
  for( i in 2: length(all_country)){
    
    data_cyc_i <- data.frame(data_cyc_j[[i]])
    data_cyc_i$country <- all_country[i]
    
    # use reshape2 package to get the melt data frame of C,Y,GDP
    data_cyc_i <- melt(data_cyc_i, ID=c("country"))
    data_cyc_i$date <- rep(date_seq[(96-(length(data_cyc_i[,1])/3)):95],3)
    
    data_cyc <- rbind(data_cyc,data_cyc_i)
    
  }
  
  
  
  data_cyc$type <- hp_type[j]
  
  assign( paste0("data_all_",hp_type[j]), data_cyc  )
}

data_ggplot_all <- rbind(`data_all_type 1 : raw`,#data_all_AIC,
                         `data_all_type 2 : BIC`,`data_all_type 3 : adf`)
                         #data_all_pp)

#----------- mark the ID for emerging or developed-------------------
index_emerging <-  data_ggplot_all$country %in% emerging_country 
index_developed <- data_ggplot_all$country %in% developed_country 

data_ggplot_all$ID[index_emerging] <- "emerging"
data_ggplot_all$ID[index_developed] <- "developed"



#------------clean other variables---------------------------------------------------------------------------------------

rm(i,j,data_cyc_i,data_cyc_j,statistic_raw,data_cyc,
   `data_all_type 3 : adf`,data_all_AIC,`data_all_type 2 : BIC`,
   data_all_pp,`data_all_type 1 : raw`)


#========================================================================================================================
#------------------------------------------------------
# part 2 : start plot with data
#------------------------------------------------------

# all in one graph

# rename the three times
data_ggplot_all[ data_ggplot_all == "type 1 : raw"  ] = "HP"
data_ggplot_all[ data_ggplot_all == "type 2 : BIC"  ] = "bHP-BIC"
data_ggplot_all[ data_ggplot_all == "type 3 : adf"  ] = "bHP-ADF"

levels(data_ggplot_all$variable)[1:2] =  c("Consumption", "Investment")


# put them into the correct order
data_ggplot_all$type = as.factor( data_ggplot_all$type )
data_ggplot_all$type = factor(data_ggplot_all$type,levels(data_ggplot_all$type)[c(3, 1, 2)])

# start the plot
sp_1_1 <- ggplot() +
  geom_line(data = data_ggplot_all, aes(x = date,y = value,colour = country), 
            alpha = 1/3, lwd= 1)

sp_1_2 <-  sp_1_1 + facet_grid(variable ~ type, scales = "free_y")


sp_1_3 <- sp_1_2 + scale_colour_manual( # black is for emerging country
  values = c("argentina" = "black","brazil" = "black","ecuador" = "black",
             "israel"= "black", "korea"= "black", "malaysia"= "black", 
             "mexico"= "black", "peru"= "black", "philippines"= "black",
             "slovak rep"= "black", "south africa"= "black", 
             "thailand"= "black", "turkey"= "black",
             
             # green for developed country
             "australia"="green", "austria"="green", "belgium"="green", 
             "canada"="green", "denmark"="green", "finland"="green", 
             "netherlands"="green", "new zealand"="green", "norway"="green",
             "portugal"="green", "spain"="green", "sweden"="green", "switzerland"="green"))

sp_1_4 = sp_1_3 + theme_bw() + labs( x = "", y = "")  +  theme(
                # plot.title = element_text(hjust = 0.5, size = 16),
                text = element_text( size = 18),
                legend.position="none" ) 


cairo_pdf(  "Figure7.pdf", width = 12, height = 12,
            family = "Times", fallback_resolution = 1000)
  print(sp_1_4)
dev.off()



