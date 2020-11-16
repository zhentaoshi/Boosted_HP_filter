

# generate the data
DGP_y = function( n, h, m, typeD, alpha0 ){
  f0 = matrix( rnorm( n * h ), nrow = n )  # n * h
  
  

  
  if (typeD == "3rd"){
    deter =  500 * ( (1:n ) / n )^3 
    f0 = apply( f0, 2, cumsum )
    f0 = f0 + deter # random walk + trend
  }
  
  if (typeD == "4th"){
    deter =  500 * ( (1:n)  / n )^4  
    f0 = apply( f0, 2, cumsum )
    f0 = f0 + deter # random walk + trend
  }
  
  if (typeD == "sin"){
    deter = c( rep(0, n/2), 1:(n/2) ) 
    
    f0 = apply( f0, 2, cumsum )
    f0 = f0 + deter # random walk + trend
    f0[ 1:(n/2), ] = 0 # for structural change 
    f0 = f0 + 5 * (1:n)^( 1/5 ) * cos( 0.05*pi*(1:n)^(.9) ) # add cycle
    
    # for this DGP, the average ADF is 1.6 and average BIC is 5.8, perhaps too far away
    
    
    #    c( rev( 500 * ( (1:(0.5*n) ) / (0.5*n) )^3 ), 500 * ( (1:(0.5*n))  / ( n*0.5 ) )^4 )
      # 5 * (1:n)^( 1/5 ) * cos( 0.05*pi*(1:n)^(.9) ) #  10  * cos( 2* pi * (1:n)/40 ) # 10 year business cycle
  }
  
  if (typeD == "cos"){ # added for R1's comments
    deter = cos(0.5 * pi * (1:n) ) # c( rep(0, n/2), 1:(n/2) ) 
    
    f0 = apply( f0, 2, cumsum )
    f0 = f0 + deter # random walk + trend
    f0[ 1:(n/2), ] = 0 # for structural change 
    f0 = f0  # + 5 * (1:n)^( 1/5 ) * cos( 0.05*pi*(1:n)^(.9) ) # add cycle
  }
  
  

  
  
  
  F0 = rep(f0, each = m)
  F0 = array(F0, c(m, n, h ) )
  F0 = aperm(F0, c(2,1,3 ) ) # the trend, or mean
  
  discard = 50
  nd = n+discard
  
  u = array( rnorm( nd * m * h), c( nd, m, h) ) # the error term
  e = u
  for (nn in 2:nd){
    e[nn, , ] = alpha0 * e[nn-1, , ] + u[nn, , ] + u[nn-1, , ] # an AR process
  }
  e = e[ (discard + 1): nd, , ]
  
  Y0 = F0 + e 
  return( list(Y0 = Y0, F0 = F0) ) 
}



############## implement the simulation




do_simulation = function(  ){
  
  
  pts0 <- Sys.time()
  
  Y0 = data0$Y0
  F0 = data0$F0
  
  
  matplot( Y0[, 1, 1:4 ], type = "l")
  
  Trend0 = array(0, c(n, m, h, R ))
  
  for (hh in 1:h){
    print( hh )
    for (mm in 1:m){
      y = Y0[, mm, hh]
      this_BHP = BoostedHP( y, lambda = lambda,  Max_Iter = R+1 )
      Trend0[, mm, hh, ] = this_BHP$trend_hist
    }
  }
  
  # the variance
  R0 = R-1
  
  
  # pts0 = Sys.time()
  f_var = array(0, c(n, h, R0))
  
  
  # doing explicit loop is much faster than the seemingly smart "aaply"
  # f_var = aaply(Trend0, c(1,3,4), var, .progress = "text"  ) # dim(f_var): n, h, R
  
  for (nn in 1:n){
    print(nn)
    for (hh in 1:h){
      for (RR in 1:R0){
        f_var[nn, hh, RR] = var( Trend0[nn, , hh, RR]  )
      }
    }
  }
  # pts1 = Sys.time() - pts0
  # print(pts1)
  
  
  
  f_var_R = aaply( f_var, 3, mean, .progress = "text" )
  print("variance is done")
  
  
  
  
  
  # the bias
  f_bias = array(0, c(n, m, h, R0) )
  for (RR in 1:R0){
    f_bias[, , , RR] = Trend0[, , , RR] - F0
  }
  # f_bias = aaply(Trend0, 4, .fun = function(x) {x - F0}, .progress = "text")
  
  # f_bias = aperm(f_bias, c( 2 ,3, 4, 1 ) )
  
  f_bias2 = array(0, c(n, h, R0))
  
  for (nn in 1:n){
    print(nn)
    for (hh in 1:h){
      for (RR in 1:R0){
        f_bias2[nn, hh, RR] = mean( f_bias[nn, , hh, RR]  )
      }
    }
  }
  

  
  f_bias2 = f_bias2^2
  
  f_bias_R = aaply( f_bias2, 3, mean, .progress = "text" )
  print("bias is done")
  
  
  pts1 = Sys.time() - pts0 # check time elapse
  print(pts1)
  
  MSE = f_bias_R + f_var_R
  matplot( cbind(f_bias_R, f_var_R,  MSE  ), type = "l")
  
  
  
  ############# BIC ####################
  
  iter_crit_BIC = matrix(0, nrow = m, ncol = h )
  for (hh in 1:h){
    print( hh )
    for (mm in 1:m){
      y = Y0[, mm, hh]
      this_BHP = BoostedHP( y, lambda = lambda, test_type = "BIC"  )
      iter_crit_BIC[mm, hh] = this_BHP$iter_num
    }
  }
  
  print(iter_crit_BIC)
  iter_sum_BIC = apply(iter_crit_BIC, 2, mean)
  hist(iter_sum_BIC)
  
  ############ ADF ##########################
  
  iter_crit_ADF = matrix(0, nrow = m, ncol = h )
  for (hh in 1:h){
    print( hh )
    for (mm in 1:m){
      y = Y0[, mm, hh]
      this_BHP = BoostedHP( y, lambda = lambda, test_type = "adf", sig_p = 0.050 )
      iter_crit_ADF[mm, hh] = this_BHP$iter_num
    }
  }
  
  print(iter_crit_ADF)
  iter_sum_ADF = apply(iter_crit_ADF, 2, mean)
  hist(iter_sum_ADF)
  
  
  ##################################
  pts1 = Sys.time() - pts0 # check time elapse
  print(pts1)
  
  ############ save ###################
  save( typeD, iter_sum_BIC, iter_sum_ADF, iter_crit_ADF, iter_crit_BIC, f_bias_R, f_var_R, MSE, n, m, h, R0,   
        file = paste0("simulation_data", typeD, "lambda", lambda, ".Rdata") )
}
