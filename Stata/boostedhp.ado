* Version: 1.0.0
* Updated: 05.11.2022
* Author: Shu SHEN, Email: shushen710@gmail.com

* ==============================================================================

program define boostedhp, eclass

	version 16.0
	
	syntax varlist(min=1 max=1) [if] [in] , [CYcle(string) TRend(string) TRENDHist(string)] [SMooth(real 1600) MAXiter(int 100)] [NSTop ORIginal]
	
	set type double
	ereturn clear
	
	if `smooth' < 0 {
		di as result "smooth()" as err " must be positive"
		exit 198
	}
	
	if `maxiter' < 0 {
		di as result "maxiter()" as err " must be positive"
		exit 198
	}
	
	if `"`nstop'"' != "" & `"`original'"' != "" {
		di as err "Only " as result "nstop" as err " or " as result "original" as err " option can be specified"
		exit 198
	}
	
	if `"`original'"' != "" & `"`trendhist'"' != "" {
		di as result "trendhist" as err " option is not allowed"
		exit 198
	}

	* =============================== Boosted HP ===============================
	
	local x = `"`varlist'"'
	
	tempname raw_x I_n D_temp D S 
	
	mkmat `x'
	matrix `raw_x' = `x'
	qui sum
	local n = r(N)
	matrix `I_n' = I(`n')
	matrix `D_temp' = [ J(1, `n', 0) \ [I(`n'-1), J(`n'-1, 1, 0)] ]
	matrix `D_temp' = (`I_n' - `D_temp') * (`I_n' - `D_temp')
	matrix `D' = `D_temp'[3..`n', ....]'
	matrix `S' = inv( `I_n' + `smooth' * (`D' * `D'') )
	
	di as text "The smoothing parameter is " `smooth'
	
	if `"`original'"' != `""' {
	    
		tempname x_f x_c x_t
		
	    matrix `x_f' = `S' * `x'
		matrix `x_c' = `x' - `x_f'
		matrix `x_t' = `x' - `x_c'
		
		di as text "The original HP is done. "
	}
	
	if `"`original'"' == `""' {
	    
		tempname x_c_r IC I_S_0 c_HP I_S_r B_r x_f x_c x_t x_b
		
	    local r = 0
		matrix `x_c_r' = `x'
		matrix `x_f' = J(`n', `maxiter', 0)
		matrix `IC' = J(1, `maxiter', 0)
		
		matrix `I_S_0' = I(`n') - `S'
		matrix `c_HP' = `I_S_0' * `x'
		matrix `I_S_r' = `I_S_0'
		
		while `r' < `maxiter' {
			
			local r = `r' + 1
			
			matrix `x_c_r' = `I_S_r' * `x'
			matrix `x_f'[1, `r'] = `x' - `x_c_r'
			matrix `B_r' = I(`n') - `I_S_r'
			matrix `IC'[1, `r'] = `x_c_r''*`x_c_r' * inv( `c_HP''*`c_HP' ) +  J(1, 1, ln(`n')) * inv( (J(1, 1, `n') - trace(`S')) )* trace(`B_r') 
			
			matrix `I_S_r' = `I_S_0' * `I_S_r'
			
			if `r' >= 2 &  `"`nstop'"' == `""' {
				if `IC'[1, `r'-1] < `IC'[1, `r'] {
					continue, break
				}
			}
		}
		
		local R = `r' - 1
		matrix `x_f' = `x_f'[...., 1..`R']
		matrix `x_c' = `x' - `x_f'[....,`R']
		matrix `x_t' = `x' - `x_c'
		matrix `x_b' = `IC'[1, 1..`r']
		
		di as text "The number of iteration is " `R'
	}

	* ================================= Result =================================
	
	if `"`cycle'"' != "" {
		svmat `x_c', names(`cycle')
		rename `cycle'* `cycle'
		label var `cycle' "Cycle of `varlist' .  `original' `nstop'"
	}
	
	if `"`trend'"' != "" {
		svmat `x_t', names(`trend')
		rename `trend'* `trend'
		label var `trend' "Trend of `varlist' .  `original' `nstop'"
	}
	
	if `"`original'"' == `""' & `"`trendhist'"' != "" {
		svmat `x_f', names(`trendhist')
	}
	
	* ================================= Return =================================
	
	ereturn scalar smooth = `smooth'
	ereturn matrix cycle = `x_c'
	ereturn matrix trend = `x_t'
	
	if `"`original'"' == `""' {
		ereturn scalar iteration = `R'
		ereturn matrix trendhist = `x_f'
		ereturn matrix bichist = `x_b'
	}
	
end
