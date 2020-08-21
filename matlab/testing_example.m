% Examples of Boosted HP Filter 
clear; clc; 
IRE = csvread("IRE.csv"); % load the data 'IRE'
lambda = 100;                % tuning parameter for the annual data

%% Original HP Filter
iter = false; 
[cycle_HP, trend_HP] = BoostedHP(IRE, lambda, iter);

%% by BIC
iter = true;
stopping ="BIC" ; 
[cycle_BIC, trend_BIC] = BoostedHP(IRE, lambda, iter, stopping);

%% by ADF
iter = true;
stopping ="adf" ; % this option requires Matlab's econometrics package
[cycle_ADF, trend_ADF] = BoostedHP(IRE, lambda, iter, stopping);

%% If stopping = "nonstop",
%%  Iterated HP filter until Max_Iter and keep the path of BIC.
iter = true;
stopping ="nonstop" ; 
[cycle_nonstop, trend_nonstop] = BoostedHP(IRE, lambda, iter, stopping);

