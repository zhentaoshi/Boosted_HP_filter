# this is for the bias-variance trade-off graphs

# the whole script consists of the function "generate_plot" and calls of this function


generate_plot = function(datafilename){
  
  load(datafilename)
  
  library(ggplot2)
  library(reshape2)
  
  theme1 = theme_bw() + theme(axis.title.x = element_blank(),
                              text = element_text( size = 22),
                              legend.position = "bottom",
                              legend.title = element_blank() )
  
  bv = data.frame( R = 1:R0, bias_sq = f_bias_R, variance = f_var_R )
  bv_plt = melt(bv, measure.vars = c("bias_sq", "variance"))
  bv_plt$MSE = MSE
  
  
  p1 = ggplot(bv_plt)
  p1 = p1 + geom_col( aes(R, value, fill = variable ), position = "dodge" )
  p1 = p1 + scale_fill_manual( name = "", values = c( "purple", "orange") )

  p1 = p1 + guides ( color = guide_legend(title = "") )
  p1 = p1 + xlab("Iterations") + ylab(" ") + theme1
  
  p1 = p1 + geom_point( aes(R, MSE, col = "black "),  size = 2 )
  p1 = p1 + scale_color_manual(values = "black", labels = "MSE", guide = TRUE)
  
  p1 = p1 + geom_line( aes(R, MSE))
  
  
  print(p1)
  
  
  ##########################################
  bv_hist = data.frame( ADF = iter_sum_ADF, BIC = iter_sum_BIC )
  bv_hist_plt = melt( bv_hist, measure.var =  c("ADF", "BIC") )
  names(bv_hist_plt)[1] = "Criterion"
  
  p2 = ggplot(bv_hist_plt)
  p2 = p2 + geom_histogram( 
    breaks = seq(0.5, 9, by = 0.25) - 0.01,  position = "identity", alpha = 0.5,
    aes(x = value, fill = Criterion, color = Criterion ) )
  p2 = p2 + scale_color_manual(values = c(3,4) )
  p2 = p2 + scale_fill_manual(values = c(3,4) )
  p2 = p2 + xlab("Iterations") + ylab(" ") + theme1
  p2 = p2 + scale_x_continuous(breaks=1:10   )
  
  print(p2)
  
  ############### combine two graphs #################
  library(lattice)
  library(gridExtra)

  
  cairo_pdf(  paste0(datafilename, ".pdf"), width = 16, height = 5, 
              family = "Times", fallback_resolution = 1000)
  grid.arrange(p1, p2, ncol=2)
  dev.off()
}

# call the self-defined function "generate_plot"

generate_plot("simulation_data3rd.Rdata") # Figure 3 (a)
generate_plot("simulation_data4th.Rdata") # Figure 3 (b)


generate_plot("simulation_data3rdlambda6400.Rdata") # Figure B2 (a)
generate_plot("simulation_data4thlambda6400.Rdata") # Figure B2 (b)

generate_plot("simulation_data3rdlambda400.Rdata") # Figure B2 (c)
generate_plot("simulation_data4thlambda400.Rdata") # Figure B2 (d)
