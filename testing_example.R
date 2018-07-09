rm(list = ls() )

source("BoostedHP.R")

DGP_sto = function( n, alpha0 ){
  # generate the stochastic component as in the paper
  f0 = cumsum( rnorm( n ) )

  discard = 50
  nd = n+discard

  u =  rnorm( nd ) # the error term
  e = rep(0,nd )
  for (nn in 2:nd){
    e[nn] = alpha0 * e[nn-1] + u[nn] + u[nn-1] # an AR process
  }
  e = e[ (discard + 1): nd]

  y = f0 + e
  return( y )
}


# parameter setting

n <- 100 # data size
x = DGP_sto(n, 0.5)


########## usage of the repeated HP filter ##########33

# raw HP filter
bx_HP = BoostedHP(x, lambda = 1600, iter= FALSE)$trend

# by BIC
bx_BIC = BoostedHP(x, lambda = 1600, iter= TRUE, test_type = "BIC")$trend

# by ADF
bx_ADF = BoostedHP(x, lambda = 1600, iter= TRUE, test_type = "adf", sig_p = 0.010)$trend


matplot( cbind(x, bx_HP, bx_BIC, bx_ADF), type = "l", ylab = "", lwd = rep(2,4)  )
