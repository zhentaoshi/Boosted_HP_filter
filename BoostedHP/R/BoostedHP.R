# Boosting the Hodrick-Prescott Filter
# by Peter Phillips and Zhentao Shi (2018)
#
#================================================================
# R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
# Copyright (C) 2017 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 2018-04-22
# by Zhentao Shi, zhentao.shi@cuhk.edu.hk
#    Chen Yang,   chen_yang@link.cuhk.edu.hk
#
# ===============================================================
#
# Version 1.0
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(tseries)
library(expm)

#' one in all function of iterated HP-filter
#'
#' @param x
#' @param lambda
#' @param iter
#' @param test_type
#' @param sig_p
#' @param Max_Iter
#'
#' @return cycle,trend_hist,iter_num,IC_hist,adf_p_hist.
#' @export
#'
#' @examples bx_HP = BoostedHP(x, lambda = 1600, iter= FALSE)$trend
BoostedHP <- function(x, lambda = 1600, iter= TRUE, test_type = "none", sig_p = 0.050, Max_Iter = 100) {


  # Require Package: tseries, expm

  # Inputs
  #   x: a univariate time series
  #   lambda: the tuning parameter in the HP filter (base learner). Default is 1600.
  #   iter: logical.
  #       If iter = FALSE, the function returns the simple HP filter (fit only once).
  #       If iter = TRUE, the boosted HP filter.
  #   test_type (stopping criterion):
  #       If ="adf" or "BIC", the two stopping criteria in the paper.
  #       If = "none", iterated until Max_Iter
  #   sig_p: the significance level of the ADF test as the stopping criterion.
  #           It is useful only when test_type == "adf".
  #   Max_Iter: the maximum number of iterations.

  # Outputs
  #   $cycle: the cyclical components in the final round
  #   $trend_hist: the estimated trend in each iteration
  #   $iter_num: the total number of iterations
  #   $IC_hist: the path of the information criterion along the iterations
  #   $adf_p_hist: the path of the ADF test p-value along the iterations





  if (!is.numeric(x) || anyNA(x) ) {
    stop("argument is not numeric or containing NAs: returning NA")
    return(NA_real_)
  }

  ## generating trend operator matrix "S"

  n <- length(x) # data size


  I_n <-  diag(n)
  D_temp <- rbind(matrix(0, 1, n), diag(1, n - 1, n))
  D_temp <- (I_n - D_temp) %*% (I_n - D_temp)
  D <- t( D_temp[3:n, ] )

  # Equation 4 in PJ
  S <- solve( I_n + lambda * D %*% t(D) )
  mS = diag(n) - S



  ## the simple HP-filter

  if(iter==FALSE){

    message("conducted the simple HP filter")

    # get the trend and cycle
    x_f <- S %*% x
    x_c <- x - x_f
    result <-list(cycle = x_c, trend_hist = x_f, trend = x - x_c)

  }

  ####################################################

  ## the boosted HP filter

  if(iter==TRUE) {


    ### ADF test as the stopping criterion
    if(test_type =="adf"  ) {
      message("iterated HP filter with ",test_type," test criterion")

      r <- 1
      stationary <- FALSE
      x_c <- x

      x_f <- matrix(0, n, Max_Iter)
      adf_p <- rep(0, Max_Iter)

      while( (r <= Max_Iter) & (stationary == FALSE)){

        x_c <- ( diag(n) - S ) %*% x_c # update
        x_f[, r] <- x - x_c

        adf_p_r <- adf.test(x_c, alternative = "stationary")$p.value
        # x_c is the residual after the mean and linear trend being removed by HP filter
        # we use the critical value for the ADF distribution with
        # the intercept and linear trend specification

        # adf_p_r =  ur.df( x_c, type = "none",  lags = 1); #  trunc((length(x_c)-1)^(1/3) ) )
        # adf_p_r = 0.06 * ( adf_p_r@teststat > - 2.6 )
        # adf_p_r = unitrootTest(x_c, type = "nc", lags = trunc((length(x_c)-1)^(1/3) )  )@test$p.value
        adf_p[r] <- adf_p_r

        sig_p = sig_p # + 0.001 # due to the way that R reports the p-value
        if(test_type == "adf")   stationary <- (adf_p_r <= sig_p)


        # Truncate the storage matrix and vectors
        if(stationary == TRUE){
          R <- r
          x_f <- x_f[, 1:R]
          adf_p <- adf_p[1:R]
          break
        }

        r <- r + 1
      } # end the while loop

      if( r > Max_Iter ){
        R <- Max_Iter
        warning("The number of iterations exceeds the limit.
                The residual cycle remains non-stationary.")
      }

      result <- list( cycle = x_c, trend_hist = x_f,  test_type = test_type,
                      adf_p_hist= adf_p, iter_num = R,
                      trend  = x - x_c)
      } else {



        ### compute BIC
        message( paste0("iterated HP filter with ", test_type, " criterion") )

        # assignment
        r <- 0
        x_c_r <- x
        x_f <- matrix(0, n, Max_Iter)
        IC <- rep(0, Max_Iter)
        IC_decrease = TRUE


        I_S_0 = diag(n) - S
        c_HP = I_S_0 %*% x
        I_S_r = I_S_0


        while( r < Max_Iter ) {
          # while r CAN not be <= Max_Iter because the following command r <- r+1, it will produce out of bound of script error

          r <- r + 1

          x_c_r = I_S_r %*% x  # this is the cyclical component after m iterations
          x_f[, r] = x - x_c_r
          B_r <- diag(n) -  I_S_r
          IC[r] =   var (x_c_r ) / var( c_HP ) +  log( n )/ (n - sum(diag (S) ) ) * sum( diag( B_r ) )

          I_S_r = I_S_0 %*% I_S_r # update for the next round

          # the BIC stopping criterion
          if(  test_type == "BIC"){
            if (r >= 2) {  IC_decrease = ( IC[r] <= IC[r-1] )  }
            if (IC_decrease == FALSE)       {break}
          }
          # else test_type = "none", keep iteration until Max_Iter

        } # end of the while loop


        # final assignment
        R = r - 1;
        x_f <- as.matrix(x_f[, 1:R])
        x_c <- x - x_f[,R]

        #if ( r == Max_Iter)


        result <- list( cycle = x_c, trend_hist = x_f,  test_type = test_type,
                        IC_hist = IC, iter_num = R, trend = x - x_c  )
      }
  } # end the boosted HP

  return(result)
}
