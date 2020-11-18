rm(list = ls() )

load( "regression_result_adf.Rdata" )
# load( "regression_result_AIC.Rdata" )
load( "regression_result_BIC.Rdata" )


library(ggplot2)
library(reshape2)
library(gridExtra)

countries = c(
  "Australia",           "Austria",            
  "Belgium",             "Canada",           
  "Denmark",             "Finland",          
  "France",              "Germany",        
  "Ireland",             "Italy",       
  "Japan" ,              "Netherlands" ,      
  "New Zealand",       "Norway",   
  "Portugal",            "Spain",    
  "Sweden",              "Switzerland",   
  "United Kingdom",    "United States"  
)


coe_q = data.frame(country = countries,
                   HP = regression_result_BIC_annual_results$coe_raw,
                   BIC = regression_result_BIC_annual_results$coe,
                   ADF = regression_result_adf_annual_results$coe)
coe_r = data.frame(country = countries,
                   HP = regression_result_BIC_annual_results$R2_raw,
                   BIC = regression_result_BIC_annual_results$R2,
                   ADF = regression_result_adf_annual_results$R2)




mybar_plot = function(dat, title_name){
  dat2 = melt(dat, id.vars = "country", measure.vars = c("HP", "ADF", "BIC") )
  names(dat2)[2] = c("estimator")
  
  
  p1 = ggplot(dat2)
  p1 = p1 + geom_col(aes(y = value, x = estimator, fill = estimator))
  p1 = p1 + facet_wrap(  facets = ~country) 
  
  
  p1 = p1 + theme_bw() + labs( title = title_name, x = "", y = "")  
  p1 = p1 + theme(strip.text = element_text( size = 12),
                  axis.text = element_text( size = 12),
                  plot.title = element_text( size = 14),
                  legend.position="none")
}

p1 = mybar_plot(coe_q, "coefficient")

p2 = mybar_plot(coe_r, "R-squared")


cairo_pdf('Figure5.pdf', width = 8, height = 12,
           family = "Times", fallback_resolution = 1000)
  grid.arrange(p1, p2, nrow=2)
dev.off()
