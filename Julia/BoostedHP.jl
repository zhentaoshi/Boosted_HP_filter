#' Boosting the Hodrick-Prescott Filter
#'
#' Coded by Mei Ziwei
#' Documented by Shi Zhentao
#'
#' All in one function of conducting the boosted HP-filter.
#' tesing example is at the end of the script
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

using LinearAlgebra
using Statistics
using HypothesisTests
function BoostedHP(x,λ=1600,iter=true,stopping="BIC",sig_p=0.050,Max_Iter=100)

    ## Examine data format
    ## generating trend operator matrix "S"
    raw_x = x;
    n = length(x);

    I_n = Matrix(I,n,n);
    D_temp = [zeros(1,n);Matrix(I,n-1,n)];
    D_temp = (I_n-D_temp)*(I_n-D_temp);
    D = D_temp[3:n, :]';

    S = inv(I_n+λ*(D*D'));
    mS = I_n - S ;

    # the Simple HP Filter
    if ~iter

        # get the trend and cycle
        x_f = S*x;
        x_c = x - x_f;

        # Results
        result = Dict("cycle" => x_c, "trend_hist" => x_f,
                   "stopping" => "nonstop", "trend" => x - x_c, "raw_data" => raw_x);
    end

    # the Boosted HP-filter
    if iter

        ## ADF test as the stopping criterion
        if stopping == "adf"

            r = 1;
            stationary = false;
            x_c = x;

            x_f = zeros(n,Max_Iter);
            adf_p = zeros(Max_Iter);

            while (r <= Max_Iter) & ~stationary

                x_c = mS * x_c; # update
                x_f[:,r] = x - x_c;

                adf_p_r = pvalue(ADFTest(x_c,:trend,Int(floor((n-1)^(1/3)))));
                # x_c is the residual after the mean and linear trend being removed by HP filter
                # we use the critical value for the ADF distribution with
                # the intercept and linear trend specification

                adf_p[r] = adf_p_r;
                stationary = (adf_p_r <= sig_p);
                # Truncate the storage matrix and vectors

                if stationary
                    R = r;
                    x_f = x_f[:,1:R];
                    adf_p = adf_p[1:R];
                    break
                end

                r += 1
            end  # end the while loop

            if r > Max_Iter
                R = Max_Iter;
                warning("The number of iterations exceeds Max_Iter.
                The residual cycle remains non-stationary.")

            end
            result = Dict("cycle" => x_c, "trend_hist" => x_f,  "stopping" => stopping,
                 "signif_p" => sig_p, "adf_p_hist" => adf_p, "iter_num" => R,
                 "trend" => x - x_c, "raw_data" => raw_x);

        else # either

            #assignment
            r = 0;
            x_c_r = x;
            x_f = zeros(n,Max_Iter);
            IC = zeros(Max_Iter);
            # IC_decrease = true;

            I_S_0 = I_n - S;
            c_HP = I_S_0 * x;
            I_S_r = I_S_0;

            while r < Max_Iter

                r = r + 1;

                x_c_r = I_S_r * x;# this is the cyclical component after m iterations
                x_f[:,r] = x - x_c_r;
                B_r = I_n - I_S_r;
                IC[r] = var(x_c_r)/var(c_HP) + log(n)/(n-sum(diag(S)))*sum(diag(B_r));

                I_S_r = I_S_0 * I_S_r; # update for the next round

                if (r>=2) & (stopping == "BIC")
                    if IC[r-1] < IC[r]
                        break
                    end
                end

            end # end the shile loop

            # Final assignment
            R = r - 1;
            x_f = x_f[:, 1:R]
            x_c = x - x_f[:,R]

            if stopping == "BIC"
                # save the path of BIC till iter+1 times to keep the "turning point" of BIC history.
                result = Dict("cycle" => x_c, "trend_hist" => x_f,  "stopping" => stopping,
                       "BIC_hist" => IC[1:(R+1)], "iter_num" => R, "trend" =>  x - x_c, "raw_data" => raw_x);
            end

            if stopping == "nonstop"

                result = Dict("cycle" => x_c, "trend_hist" => x_f,  "stopping" => stopping,
                       "BIC_hist" => IC, "iter_num" => Max_Iter-1, "trend" => x - x_c, "raw_data" => raw_x);
            end

        end

    end  # end the Boosted HP

    return result

end  ## end function
###############################################################################
## Examples

using CSV
using DelimitedFiles
IRE = readdlm("IRE.csv", ',')[:]; # laod the data "IRE"
λ = 100;  # tuning the parameter for annumal data
#
#' # raw HP filter
bx_HP = BoostedHP(IRE, λ, false);
# bx_HP_cycle = bx_HP["cycle"]  # The cyclical component
# bx_HP_trend = bx_HP["trend"]  # The trend component
#'
#' # by BIC
bx_BIC = BoostedHP(IRE, λ, true,"BIC");
#'
#' # by ADF
#' # CAVEAT: Results may be slightly different from other languages
#' #    due to different critical values of ADF test.
bx_ADF = BoostedHP(IRE, λ, true,"adf");
#'
#' # If stopping = "nonstop",
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
bx_nonstop = BoostedHP(IRE, λ, true,"nonstop");
