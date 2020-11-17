# rec0 <- read.csv("USRECD.csv", header = TRUE)
# 
# xts::xtsible(rec0)
# 
# recession <- xts::as.xts(rec0 )
# 
# recession <- xts::xts(rec0)

recession = quantmod::getSymbols('USRECD',src='FRED', auto.assign=FALSE); 

drecession <- diff(recession) # the difference of recession
recession.start <- time(drecession[drecession==1]) # if drecession==1, recession.start
recession.end <- time(drecession[drecession==-1])-30 # because it is datelen We go back to the beginning of month
# recession.df <- data.frame(recession.start, recession.end[2:length(recession.end)])
recession.df <- data.frame(recession.start[1:(length(recession.start)-1)], 
                           recession.end[2:length(recession.end)])
colnames(recession.df) <- c("start","end")
save(recession.df, file = "date_recession.Rdata")
