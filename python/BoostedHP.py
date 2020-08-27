#' Boosting the Hodrick-Prescott Filter
#'
#' Coded by Mei Ziwei
#' Documented by Shi Zhentao
#'
#' All in one function of conducting the boosted HP-filter.
##############################################################################
#' Parameters: 
#'
#' x - a raw time series to be filtered.
#' lam - turning parameter, default value is 1600,
#'   as recommended by Hodrick and Prescott (1997) for quarterly data.
#' iter - logical, True (default) to conduct the boosted HP filter.
#'   False does not iterated, which is exactly the original HP filter.
#' stopping - stopping criterion.  "BIC" (default), or  "adf", or  "nonstop"  means keeping
#'    iteration until the maximum number of iteration, specified by Max_Iter is reached.
#' sig_p - a threshold of the p-value for the ADF test, with default value 0.050.
#'    Only effective when stopping = "adf".
#' Max_Iter - maximal number of iterations. The default is 100.
##############################################################################
#' The function returns a dictionary containing the following items:
#'
#' cycle - The cyclical component in the final iteration.
#' trend - The trend component in the final iteration.
#' trend_hist - The estimated trend in each iteration.
#' iter_num - The total number of iterations when it stops.
#' BIC_hist - The path of the BIC up to the final iterations.
#' adf_p_hist - The path of the ADF test p-value up to the final iteration.
##############################################################################
#' Details:
#'
#' This is the main function of implementing the boosted HP filter (Phillisp and
#' Shi, 2019). The arguments accommendate the orginal HP filter (iter =
#' False), the boosted HP filter with the BIC stopping criterion (stopping = "BIC"),
#' or ADF test stopping criterion (stopping = "adf"), 
#' or keep going until the maximum number of iterations is reached (stopping = "nonstop").
#'
#' Either the original HP filter or the bHP filter requires lambda to
#' control the strength of the weak learner for in-sample fitting. 
#' The default is lambda = 1600, 
#' which is recommended by Hodrick and Prescott (1997) for quarterly data. 
#' lambda should be adjusted for different frequencies.
#' For example, lambda = 129600 for monthly data and
#' lambda = 100 or 6.25 for annual data.
#'
#' See the vignette with a brief introduction of the idea of bHP.
#'
##############################################################################
#' References: 
#'
#' Phillips, Peter CB, and Zhentao Shi. "Boosting: Why you can use the hp
#' filter." arXiv: 1905.00175, Cowles Foundation Discussion Paper No.2192,
#' (2019).
#'
##############################################################################

import numpy as np 
import statsmodels.tsa.stattools as ts
import math 
import logging  

def BoostedHP(x, lam = 1600, iter = True, stopping = "BIC", \
              sig_p = 0.050, Max_Iter = 100):
    
    
    x = np.array(x)
    
    ## generating trend operator matrix "S：        
    raw_x = x # save the raw data before HP
    n = len(x) # data size
    
    I_n = np.eye(n)
    D_temp = np.vstack((np.zeros([1,n]),np.eye(n-1,n)))
    D_temp= np.dot((I_n-D_temp),(I_n-D_temp))
    D = D_temp[2:n].T
    S = np.linalg.inv(I_n+lam*np.dot(D,D.T)) # Equation 4 in PJ
    mS = I_n - S
    
    ##########################################################################  
    
    ## the simple HP-filter
    if not iter:
        
        print("Original HP filter.")
        x_f = np.dot(S,x)
        x_c = x - x_f
        result = {"cycle": x_c, "trend_hist" : x_f, \
                   "stopping" : "nonstop", "trend" : x - x_c, "raw_data" : raw_x}
            
    ##########################################################################
            
    ## The Boosted HP-filter 
    if iter:
        ### ADF test as the stopping criterion
        if stopping == "adf":
            
            print("Boosted HP-ADF.")
            
            r = 1
            stationary = False
            x_c = x
            
            x_f = np.zeros([n,Max_Iter])
            adf_p = np.zeros([Max_Iter,1])
            
            while (r <= Max_Iter) and (not stationary):
                
                x_c = np.dot(mS,x_c)
                x_f[:,[r-1]] = x-x_c
                adf_p_r = ts.adfuller(x_c, maxlag = math.floor(pow(n-1,1/3)), autolag=None, \
                                      regression = "ct")[1]

                # x_c is the residual after the mean and linear trend being removed by HP filter
                # we use the critical value for the ADF distribution with
                    # the intercept and linear trend specification
                    
                adf_p[[r-1]] = adf_p_r
                stationary = adf_p_r <= sig_p
                
                # Truncate the storage matrix and vectors
                if stationary:
                    R = r
                    x_f = x_f[:,0:R]
                    adf_p = adf_p[0:R]
                    break
                
                r += 1
            
            if r > Max_Iter:
                R = Max_Iter
                logging.warning("The number of iterations exceeds Max_Iter. \
                The residual cycle remains non-stationary.")
                
            result = {"cycle" : x_c, "trend_hist" : x_f,  "stopping" : stopping,
                     "signif_p" : sig_p, "adf_p_hist" : adf_p, "iter_num" : R,
                    "trend" : x - x_c, "raw_data" : raw_x}
        
        
        else: # either BIC or nonstopping
            
            # assignment 
            r = 0
            x_c_r = x
            x_f = np.zeros([n,Max_Iter])
            IC = np.zeros([Max_Iter,1])
            # IC_decrease = True
            
            I_S_0 = I_n - S
            c_HP = np.dot(I_S_0, x)
            I_S_r = I_S_0
            
            while r < Max_Iter:
                
                r += 1
                
                x_c_r = np.dot(I_S_r, x)
                x_f[:,[r-1]] = x - x_c_r
                B_r = I_n - I_S_r 
                IC[[r-1]] =  np.var(x_c_r)/np.var(c_HP) + \
                    np.log(n)/(n-np.sum(np.diag(S))) * np.sum(np.diag(B_r))
                
                I_S_r = np.dot(I_S_0, I_S_r) # update for the next round
                
                if r >= 2 and stopping == "BIC":
                    if IC[[r-2]] < IC[[r-1]]:
                        break
            
            # final assignment
            R = r-1
            x_f = x_f[:, list(range(0,R))]
            x_c = x - x_f[:, [R-1]]
            
            if stopping == "BIC":
                
                print("Boosted HP-BIC.")
                # save the path of BIC till iter+1 times to keep the "turning point" of BIC history.
                result = {"cycle" : x_c, "trend_hist" : x_f,  "stopping" : stopping, 
                       "BIC_hist" : IC[0:(R+1)], "iter_num" : R, "trend" : x- x_c, "raw_data" : raw_x}
            
            if stopping == "nonstop":
                
                print('Boosted HP-BIC with stopping = "nonstop".')
                result = {"cycle" : x_c, "trend_hist" : x_f,  "stopping" : stopping, 
                       "BIC_hist" : IC, "iter_num" : Max_Iter - 1, "trend" : x- x_c, "raw_data" : raw_x}
            
    return result 

### function ends 
##############################################################################
# Examples of Boosted HP
    
import pandas as pd     
import os
os.chdir(os.path.split(os.path.realpath(__file__))[0]) # set current path

IRE = np.array(pd.read_csv("IRE.csv", header = None))  # load the data 'IRE'
lam = 100 # tuning parameter for the annual data

#
#' # raw HP filter
bx_HP = BoostedHP(IRE, lam = lam, iter = False)
# bx_HP_cycle = bx_HP["cycle"]  # The cyclical component 
# bx_HP_trend = bx_HP["trend"]  # The trend component 
#'
#' # by BIC
bx_BIC = BoostedHP(IRE, lam = lam, iter = True, stopping = "BIC")
#'
#' # by ADF
#' # CAVEAT: Results may be slightly different from other languages 
#' #    due to different critical values of ADF test.  
bx_ADF = BoostedHP(IRE, lam = lam, iter = True, stopping = "adf")
#'
#' # If stopping = "nonstop",
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
bx_nonstop = BoostedHP(IRE, lam = lam, iter = True, stopping = "nonstop")

