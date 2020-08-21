% Boosting the Hodrick-Prescott Filter
%
% code by Mei Ziwei
% documented by Shi Zhentao
%
% All in one function of conducting the boosted HP-filter.
%
% Inputs:
%    x - a raw time series to be filtered.
%    lambda - the turning parameter, default value is 1600,
%               as recommended by Hodrick and Prescott (1997) for quarterly data.
%    iter - logical,  true (default) to conduct the boosted HP filter.
%                 false does not iterated, which is exactly the original HP filter.
%    stopping - stopping criterion. "BIC" (default), or "adf", or "nonstop" means keeping
%              iteration until the maximum number of iteration, specified by Max_iter is reached.
%    sig_p - a threshold of the p-value for the ADF test, with default value 0.050.
%              Only effective when stopping = "adf".
%    Max_Iter - maximal number of iterations. The default is 100.
% 
% Outputs: 
% The function returns a list containing the following items:
%   cycle - The cyclical component in the final iteration.
%   trend - The trend component in the final iteration.
%   trend_hist-  The estimated trend in each iteration.
%   iter_num - The total number of iterations when it stops.
%   iter_hist - The path of the BIC up to the final iterations when
%       stopping = "BIC" or "nonstop", and the path of the ADF test p-value up to the final
%       iteration when stopping = "ADF". 
%  
% Details:
%
% This is the main function of implementing the boosted HP filter (Phillisp and
% Shi, 2019). The arguments accommendate the orginal HP filter (iter =FALSE), 
%  the boosted HP filter with the BIC stopping criterion (stopping = "BIC")
% or ADF test stopping criterion (stopping = "adf"),  
% or keep going until the maximum number of iterations is reached (stopping = "nonstop"). 
%
% Either the original HP filter or the bHP filter requires lambda to
% control the strength of the weak learner for in-sample fitting. 
% The default is lambda=1600, 
% which is recommended by Hodrick and Prescott (1997) for quarterly data. 
% lambda should be adjusted for different frequencies. 
% For example, lambda = 129600 for monthly data
% and lambda = 100 or 6.25 for annual data.
%
% See the vignette with a brief introduction of the idea of bHP.
%
% References: 
%
% Phillips, Peter CB, and Zhentao Shi. "Boosting: Why you can use the hp
% filter." arXiv: 1905.00175, Cowles Foundation Discussion Paper No.2192,
% (2019).


function [cycle, trend, trend_hist, iter_num, iter_hist] = BoostedHP(x, lambda, iter, stopping, sig_p, Max_Iter)
    
    %% Examine data format
    
    if ~isnumeric(x) || any(ismissing(x))
        error("The raw time series is not numeric or it contains missing values.")
    end 

        
    %% Default settings  
    if nargin <= 1 || isempty(lambda)
        lambda = 1600;
    end
    
    if  nargin <= 2 || isempty(iter)  
        iter = true;
    end
    
    if nargin <= 3 || isempty(stopping) 
        stopping = "BIC";
    end
    
    if nargin <= 4  || isempty(sig_p) 
        sig_p = 0.050;
    end
    
    if nargin <= 5 || isempty(Max_Iter) 
        Max_Iter = 100;
    end
    
    %% generating trend operator matrix "S"
    % raw_x = x;  % save the raw data before HP
    n = length(x);  % data size
    
    I_n = eye(n);
    D_temp = [zeros(1, n); eye(n-1,n)];
    D_temp = (I_n -D_temp)*(I_n -D_temp);
    D = (D_temp(3:n, :))';
    
    S = inv( I_n + lambda *( D * D' )); % Equation 4 in PJ
   mS = I_n - S; 
    
   %% the simple HP-filter
   if ~iter 
       disp("Original HP filter.")

       % get the trend and cycle
       x_f = S * x;
       x_c = x - x_f;
       
       % Results 
       cycle = x_c;
       trend_hist = x_f;
       %stopping = "nonstop";
       trend = x - x_c;
       % raw_data = raw_x;
       iter_hist = []; 
       iter_num = []; 
   end
   
    %% the boosted HP filter
    if iter
        if stopping == "adf" 
            r = 1;
            stationary = false;
            x_c = x;
            x_f = zeros(n, Max_Iter);
            adf_p = zeros(1, Max_Iter);
            warning('off')
            while (r <= Max_Iter) && (~stationary)
                x_c = (I_n-S) * x_c; % update 
                x_f(:,r) = x-x_c;
                [~,adf_p_r] = adftest(x_c,"model","TS",'lags',floor((n-1)^(1/3)));
                % x_c is the residual after the mean and linear trend being removed by HP filter
                % we use the critical value for the ADF distribution with
                % the intercept and linear trend specification

                adf_p(r) = adf_p_r;
                stationary = adf_p_r <= sig_p;
                
                if stationary 
                    R = r;
                    x_f = x_f(:,1:R);
                    adf_p = adf_p(1:R);
                    break
                end
                
                r = r+1; 
            end   % end the while loop 
            warning('on')
           
            if r > Max_Iter
                R = Max_Iter; 
                warning("The number of iterations exceeds Max_Iter. The residual cycle remains non-stationary.\n")
            end
            
            % Results 
            disp("Boosted HP-ADF.")
            disp(strcat("Threshold of the p-value: ",num2str(sig_p)))
            cycle = x_c;
            trend_hist = x_f;

            iter_hist= adf_p;
            iter_num = R;
            trend  = x - x_c;
            %raw_data = raw_x;
            
        else % Either BIC ot nonstopping
            
            % assignment
            r = 0;
            % x_c_r=x;
            x_f = zeros(n,Max_Iter);
            IC = zeros(1,Max_Iter);
            % IC_decrease = true; 
            
            I_S_0 = I_n - S;
            c_HP = I_S_0 * x;
            I_S_r = I_S_0;
            
            while r < Max_Iter
                r = r+1;
                
                x_c_r = I_S_r * x;  % This is the cyclical component after m iterations 
                x_f(:,r) = x - x_c_r;
                B_r = I_n - I_S_r;
                IC(r) = var(x_c_r)/var(c_HP) + log( n )/(n-sum(diag(S)))*sum(diag(B_r)); 
                
                I_S_r = I_S_0 * I_S_r; % update for the next round
                
                if (r>=2) && (stopping == "BIC")
                    if IC(r-1)<IC(r)
                        break
                    end
                end
            end  % end of the while loop
            
            % final assignment
            R = r-1;
            x_f = x_f(:,1:R);
            x_c = x - x_f(:,R);
            
            if stopping == "BIC"
                % save the path of BIC till iter+1 times to keep the "turning point" of BIC history.
                disp("Boosted HP-BIC.")
                cycle = x_c;
                trend_hist = x_f;
                iter_hist = IC(1:(R+1));
                iter_num = R;
                trend =  x- x_c;
                % raw_data = raw_x; 
            end
            
            if stopping == "nonstop"
                disp('Boosted HP-BIC with stopping = "nonstop". ')
                cycle = x_c;
                trend_hist = x_f;
                iter_hist = IC;
                iter_num = Max_Iter-1;
                trend =  x- x_c;
                % raw_data = raw_x;
            end
        end  
    end% end the boosted HP