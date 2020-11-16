
# this script complements "do_simulation.R"
# because AR(4) is added later

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
  # dim(F0) = n by m by h
  
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
  
  
  Trend0 = array(0, c(n-4, m, h ))
  
  for (hh in 1:h){
    print( hh )
    for (mm in 1:m){
      y = Y0[, mm, hh]
      y = as.ts(y)
      reg = dynlm( y  ~ L(y, c(1:4) ) )
      Trend0[, mm, hh]  = predict(reg) 
    }
  }


  f_var = array(0, c(n-4, h))
  
  for (nn in 1:(n-4) ){
    for (hh in 1:h){
        f_var[nn, hh] = var( Trend0[nn, , hh]  )
    }
  }
  
  
  
  f_var_R = f_var
  
  ###############################
  F0_trim = array(F0, c(n, m, h) )
  F0_trim = F0_trim[ 5:n, , ]
  F0_trim = as.vector( F0_trim )
  
  f_bias = Trend0 - F0_trim

  
  f_bias2 = array(0, c(n-4, h))
  
  for (nn in 1:(n-4) ){
    print(nn)
    for (hh in 1:h){
        f_bias2[nn, hh] = mean( f_bias[nn, , hh], na.rm = TRUE  )
    }
  }
  

  
  f_bias2 = f_bias2^2
  
  f_bias_R = f_bias2
  
  
  pts1 = Sys.time() - pts0 # check time elapse
  print(pts1)
  
  browser()
  MSE = f_bias_R + f_var_R
  # matplot( cbind(f_bias_R, f_var_R,  MSE  ), type = "l")
  

  ##################################
  pts1 = Sys.time() - pts0 # check time elapse
  print(pts1)
  
}
