{smcl}
{* *! version 1.0.0  05Nov2022}{...}
{p2colset 1 19 23 2}{...}
{p2col:{bf:[TS] boostedhp} {hline 2}}Boosted Hodrick-Prescott Filter{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:boostedhp}
{help varname:{it:varname}}
{ifin} [{cmd:,} {it:options}]

{synoptset 25 tabbed}{...}
{synopthdr:options}
{synoptline}
{syntab:New variables}
{synopt :{opth cy:cle(newvar)}}save the cyclical component in new variable{p_end}
{synopt :{opth tr:end(newvar)}}save the trend component in new variabl{p_end}
{synopt :{opth trendh:ist(newvar)}}save the estimated trends of each iteration in new variables{p_end}

{syntab:Parameters}
{synopt:{opt sm:ooth(#)}}smoothing parameter for the boosted Hodrick-Prescott filter; the default is 1600{p_end}
{synopt:{opt max:iter(#)}}maximal number of iterations; the default is 100{p_end}

{syntab:Model}
{synopt :{opt nst:op}}keep iteration until the maximum number of iteration {opt max:iter(#)} is reached{p_end}
{synopt :{opt ori:ginal}}conduct the original HP-filter{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{it:varname} may contain time-series operators; see
{help tsvarlist}.{p_end}

{marker description}{...}
{title:Description}

{pstd}
{cmd:boostedhp} uses to implement the boosted HP filter ({help boostedhp##PS2021:Phillisp and Shi, 2021}). 
The arguments accommendate the original HP filter ({opt ori:ginal}), the boosted
HP filter with the BIC stopping criterion (default), or keep going until 
the maximum number of iterations is reached ({opt nst:op}). 

{pstd}
Either the original HP filter or the boosted HP filter requires {opt sm:ooth(#)} 
to control the strength of the weak learner for in-sample fitting. The 
default is {opt sm:ooth} = 1600, which is recommended by {help boostedhp##HP1997:Hodrick and Prescott (1997)} for quarterly data. {opt sm:ooth(#)} should be adjusted for different 
frequencies. For example, {opt sm:ooth} = 129600 for monthly data and
 {opt sm:ooth} = 6.25 for annual data. 

{pstd}
See the {browse "https://github.com/zhentaoshi/bHP_R_pkg/blob/master/vignettes/vignette.pdf":vignette} with a brief introduction of the idea of boosted HP.

{marker options}{...}
{title:Options}

{dlgtab:New variables}

{phang}
{cmd:cycle({newvar})} saves the cyclical component in the new variable specified by {it:newvar}.

{phang}
{cmd:trend({newvar})} saves the trend component in the new variable specified by {it:newvar}.

{phang}
{cmd:trendhist({newvar})} saves the estimated trends of each iteration in new variables specified by {it:newvar}.

{dlgtab:Options}

{phang}
{opt smooth(#)} sets the smoothing parameter for the Hodrick-Prescott filter. The default value is {cmd:smooth(1600)}, which is recommended by Hodrick and Prescott (1997) for quarterly data. The smoothing parameter must be greater than 0.

{phang}
{opt maxiter(#)} sets maximal number of iterations. The default value is {cmd:maxiter(100)}. The number of iterations must be greater than 0.

{phang}
{opt nstop} means keeping iteration until the maximum number of iteration {opt max:iter(#)} is reached

{phang}
{opt original} conducts the original HP-filter.

{marker example}{...}
{title:Example}

{pstd}Setup{p_end}
{phang2}{inp:.} {stata "webuse gdp2, clear":webuse gdp2, clear}{p_end}

{pstd}Use the boosted Hodrick-Prescott filter to estimate the cyclical and trend components of the log of quarterly U.S. GDP{p_end}
{phang2}{inp:.} {stata "boostedhp gdp_ln, smooth(1600)":boostedhp gdp_ln, smooth(1600)}{p_end}
{phang2}{inp:.} {stata "boostedhp gdp_ln, smooth(1600) cycle(gdp_cycle) trend(gdp_trend) trendhist(gdp_trendhist)":boostedhp gdp_ln, smooth(1600) cycle(gdp_cycle) trend(gdp_trend) trendhist(gdp_trendhist)}{p_end}

{pstd}Keep going until the maximum number of iterations is reached{p_end}
{phang2}{inp:.} {stata "boostedhp gdp_ln, nstop smooth(1600)":boostedhp gdp_ln, nstop smooth(1600)}{p_end}
{phang2}{inp:.} {stata "boostedhp gdp_ln, nstop smooth(1600) cycle(gdp_cycle_nst) trend(gdp_trend_nst) trendhist(gdp_trendhist_nst)":boostedhp gdp_ln, nstop smooth(1600) cycle(gdp_cycle_nst) trend(gdp_trend_nst) trendhist(gdp_trendhist_nst)}
{p_end}

{pstd}Use the original Hodrick-Prescott filter{p_end}
{phang2}{inp:.} {stata "boostedhp gdp_ln, original smooth(1600)":boostedhp gdp_ln, original smooth(1600)}{p_end}
{phang2}{inp:.} {stata "boostedhp gdp_ln, original smooth(1600) cycle(gdp_cycle_ori) trend(gdp_trend_ori)":boostedhp gdp_ln, original smooth(1600) cycle(gdp_cycle_hp) trend(gdp_trend_hp)}{p_end}

{pstd}Compare with {manhelp hpfilter TS:hpfilter}{p_end}
{phang2}{inp:.} {stata "hpfilter gdp_ln, trend(gdp_trend_hpfilter) cycle(gdp_cycle_hpfilter)":hpfilter gdp_ln, trend(gdp_trend_hpfilter) cycle(gdp_cycle_hpfilter)}{p_end}


{marker results}{...}
{title:Stored results}

{pstd}
{cmd:boostedhp} stores the following in {cmd:e()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:e(smooth)}}smoothing parameter lambda{p_end}
{synopt:{cmd:e(iteration)}}the number of iterations{p_end}

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Matrices}{p_end}
{synopt:{cmd:e(cycle)}}the cyclical component{p_end}
{synopt:{cmd:e(trend)}}the trend component{p_end}
{synopt:{cmd:e(trendhist)}}the estimated trends of each iteration{p_end}
{synopt:{cmd:e(bichist)}}the path of the BIC up to the final iterations{p_end}
{p2colreset}{...}

{marker author}{...}
{title:Author}

{pstd}
Shu SHEN{p_end}
{pstd}
The Chinese University of Hong Kong{p_end}
{pstd}
Email: shushen710@gmail.com{p_end}

{marker references}{...}
{title:References}

{marker PS2021}{...}
{phang}
Peter C.B. Phillips and Zhentao Shi. 2021.
{browse "https://arxiv.org/abs/1905.00175":{it:Boosting: Why You Can Use the HP Filter}.}
International Economic Review, 62(2), 521-570.

{marker HP1997}{...}
{phang}
Hodrick R.J. and Prescott E.C. 1997.
{browse "https://www.jstor.org/stable/2953682":{it:Postwar US business cycles: an empirical investigation}.}
Journal of Money, credit, and Banking, 1-16.

{p2colreset}{...}
