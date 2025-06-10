clear
cd "[DTA data folder]"
use "RUTG0003_OUTPUT.dta", clear

*******************
** ANALYSIS PREP **
*******************

* recode treatment variable(s) *

gen tx=applicant_cond
recode applicant_cond (3 4 7 8 = 1) (nonm = 0), gen(txrecord)
recode applicant_cond (1 2 3 4 = 1) (nonm = 0), gen(txblack)
recode applicant_cond (2 4 6 8 = 1) (nonm = 0), gen(txgoogle)

lab def TXRECORD 1 "Criminal Record" 0 "Clean Record"
lab def TXBLACK 1 "Black Applicant" 0 "White Applicant"
lab def TXGOOGLE 1 "Google Criminal" 0 "Google Clean"
lab val txrecord TXRECORD
lab val txblack TXBLACK
lab val txgoogle TXGOOGLE

* recode outcome variable (7 = very likely; 1 = very unlikely) *

recode q1 (98 = .), gen(callback)
replace callback=8-callback

* recode pretest covariates for balance diagnostics (X) *

gen Xage=2021-birthyr+1
gen Xmale=gender==1
recode race (2/max = 1) (1 = 0), gen(Xnwhite)
recode educ (1 2 3 4 = 1) (5 = 2) (6 = 3), gen(educ3cat)
recode marstat (5 6 = 1) (1 4 = 2) (2 3 = 3), gen(marr3cat)
gen Xmarr1=marr3cat==1
gen Xmarr2=marr3cat==2
gen Xmarr3=marr3cat==3
gen Xchild=child18==1
gen Xeduc1=educ3cat==1
gen Xeduc2=educ3cat==2
gen Xeduc3=educ3cat==3
recode employ (2 = 0), gen(Xftwork)
recode faminc_new (97 = .), gen(Xfaminc)
*https://www.bls.gov/iag/tgs/iag_index_naics.htm
recode industrynaics (1 2 = 1 "good, nat") ///
					 (3 = 2 "good, con") ///
					 (4 = 3 "good, man") ///
					 (5 6 7 = 4 "serv, trd") ///
					 (8 = 5 "serv, inf") ///
					 (9 10 11 = 6 "serv, fin") ///
					 (13 14 15 = 7 "serv, pro") ///
					 (16 17  = 8 "serv, edu") ///
					 (18 19 = 9 "serv, hos") ///
					 (12 20 21 = 10 "serv, oth") ///
					 (22 23 = 11 "othr") ///
					 , gen(naicscat)
gen Xnaics1_2_3=naicscat>=1 & naicscat<=3
gen Xnaics4=naicscat==4
gen Xnaics5_6_7=naicscat>=5 & naicscat<=7
gen Xnaics8=naicscat==8
gen Xnaics9_10=naicscat>=9 & naicscat<=10
gen Xnaics11=naicscat==11
recode pid7 (8 = .), gen(Xrepub)
gen Xvoted=presvote16post<7 & presvote20post<6
recode newsint (4 = 1) (3 = 2) (2 = 3) (1 = 4) (7 98 = .), gen(Xpolintrst)
recode pew_churatd (6 = 1) (5 = 2) (4 = 3) (3 = 4) (2 = 5) (1 = 6) (7 = .), gen(church1)
recode pew_religimp (4 = 1) (3 = 2) (2 = 3) (1 = 4), gen(church2)
recode pew_prayer (7 = 1) (6 = 2) (5 = 3) (4 = 4) (3 = 5) (2 = 6) (1 = 7) (8 = .), gen(church3)
factor church*, pcf
predict Xchurch

*************************************
** TABLE 2: DESCRIPTIVE STATISTICS **
*************************************

tabstat txrecord txblack txgoogle callback X*, s(n mean sd min max) c(s) format(%9.3f)

******************************
** TABLE 3: FACTORIAL ANOVA **
******************************

foreach out of varlist callback {
	qui: anova `out' txrecord##txblack##txgoogle
	local i=1
	local f=e(F)
	local p=Ftail(e(df_m), e(df_r), e(F))
	foreach j of numlist 1 2 4 3 5 6 7 {
		local f`i'=e(F_`j')
		local p`i'=Ftail(e(df_`j'), e(df_r), e(F_`j'))
		local i=`i'+1
	}
	qui: estat esize
	local i=1
	local e`i'=r(esize)[1,1]
	foreach j of numlist 2 3 5 4 6 7 8 {
		local e`i'=sqrt(r(esize)[`j',1])
		local i=`i'+1
	}
	di as result ""
	di "1-way, record: " 					_col(35) %9.2f `f1' %9.4f `p1' %10.2f `e1'
	di "1-way, race: " 						_col(35) %9.2f `f2' %9.4f `p2' %10.2f `e2' 
	di "1-way, google: " 					_col(35) %9.2f `f3' %9.4f `p3' %10.2f `e3'
	di "2-way, record X race: " 			_col(35) %9.2f `f4' %9.4f `p4' %10.2f `e4'
	di "2-way, record X google: " 			_col(35) %9.2f `f5' %9.4f `p5' %10.2f `e5'
	di "2-way, race X google: " 			_col(35) %9.2f `f6' %9.4f `p6' %10.2f `e6'
	di "3-way, record X race X google: "	_col(35) %9.2f `f7' %9.4f `p7' %10.2f `e7'
	di "full model: " 						_col(35) %9.2f `f'  %9.4f `p'  %10.2f `e'
}

*******************************
** TABLE 4: REGRESSION MODEL **
*******************************

local v1 "txrecord txgoogle txgoogle"
local v2 "txblack txblack txrecord"
local w1 "txrecord txgoogle txgoogle txrecord"
local w2 "txblack txblack txrecord txgoogle"
foreach out of varlist callback {
	local i=1
	qui: reg `out' txrecord##txblack##txgoogle, vce(robust)
	qui: margins txrecord txblack txgoogle, contrast(eff)
	forvalues j=1/3 {
		local b`i'=r(table)[1,`j']
		local s`i'=r(table)[2,`j']
		local p`i'=r(table)[4,`j']
		local i=`i'+1
	}
	forvalues k=1/3 {
		local trt : word `k' of `v1'
		local at : word `k' of `v2'
		qui: margins `trt', at(`at'=(1 0)) contrast(eff)
		forvalues j=1/2 {
			local b`i'=r(table)[1,`j']
			local s`i'=r(table)[2,`j']
			local p`i'=r(table)[4,`j']
			local i=`i'+1
		}
	}
	local i=1
	qui: margins txrecord txblack txgoogle
	forvalues j=1(2)5 {
		local l=`j'+1
		local n`i'0=(r(table)[1,`j']-1)/(7-1)
		local n`i'1=(r(table)[1,`l']-1)/(7-1)
		local d`i'=100*(`n`i'1'/`n`i'0'-1)
		local i=`i'+1
	}
	forvalues k=1/4 {
		local trt : word `k' of `w1'
		local at : word `k' of `w2'
		qui: margins `trt', at(`at'=(1 0))
		forvalues j=1(2)3 {
			local l=`j'+1
			local n`i'0=(r(table)[1,`j']-1)/(7-1)
			local n`i'1=(r(table)[1,`l']-1)/(7-1)
			local d`i'=100*(`n`i'1'/`n`i'0'-1)
			local i=`i'+1
		}
	}
	di as result ""
	di "record: " 				_col(25) %9.2f `b1'  %8.2f `s1'  %9.4f `p1'  %8.1f `d1'
	di "race: " 				_col(25) %9.2f `b2'  %8.2f `s2'  %9.4f `p2'  %8.1f `d2'
	di "google: " 				_col(25) %9.2f `b3'  %8.2f `s3'  %9.4f `p3'  %8.1f `d3'
	di "record | black: " 		_col(25) %9.2f `b4'  %8.2f `s4'  %9.4f `p4'  %8.1f `d4'
	di "record | white: " 		_col(25) %9.2f `b5'  %8.2f `s5'  %9.4f `p5'  %8.1f `d5'
	di "google | black: " 		_col(25) %9.2f `b6'  %8.2f `s6'  %9.4f `p6'  %8.1f `d6'
	di "google | white: "		_col(25) %9.2f `b7'  %8.2f `s7'  %9.4f `p7'  %8.1f `d7'
	di "google | record: "		_col(25) %9.2f `b8'  %8.2f `s8'  %9.4f `p8'  %8.1f `d8'
	di "google | no record: " 	_col(25) %9.2f `b9'  %8.2f `s9'  %9.4f `p9'  %8.1f `d9'
	di "record | google: " 		_col(51)                                     %8.1f `d10'
	di "record | no google: " 	_col(51)                                     %8.1f `d11'
}

* randomization inference *

*net install ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/) replace
*which ritest // version 1.21, March 2024

program margin_marg_main
    syntax, command(string)
	`command'
    qui: margins, dydx(txrecord txblack txgoogle) post
end

program margin_marg_inter1
    syntax, command(string)
	`command'
    qui: margins, dydx(txrecord) at(txblack=(1 0)) pwcompare post
end

program margin_marg_inter2
    syntax, command(string)
	`command'
    qui: margins, dydx(txgoogle) at(txblack=(1 0)) pwcompare post
end

program margin_marg_inter3
    syntax, command(string)
	`command'
    qui: margins, dydx(txgoogle) at(txrecord=(1 0)) pwcompare post
end

foreach tx of varlist txrecord txblack txgoogle {
	qui: ritest `tx' _b[1.`tx'], reps(5000) seed(20230415): margin_marg_main, command(qui: reg callback txrecord##txblack##txgoogle, vce(robust))
	di as result "`tx': " _col(15) %7.2f r(b)[1,1] %5.0f r(c)[1,1] %9.4f r(p)[1,1]
}

local j "txrecord txgoogle txgoogle"
local k "txblack txblack txrecord"
forvalues i=1/3 {
	local trt : word `i' of `j'
	local at : word `i' of `k'
	qui: ritest `trt' _b[1.`trt':1bn._at] _b[1.`trt':2._at] (_b[1.`trt':2._at] - _b[1.`trt':1bn._at]), reps(5000) seed(20230415): margin_marg_inter`i', command(qui: reg callback txrecord##txblack##txgoogle, vce(robust))
	di as result ""
	di "`trt' | `at' = 1: " _col(25) %9.2f r(b)[1,1] %5.0f r(c)[1,1] %9.4f r(p)[1,1]
	di "`trt' | `at' = 0: " _col(25) %9.2f r(b)[1,2] %5.0f r(c)[1,2] %9.4f r(p)[1,2]
}

*********************
** APPENDIX OUTPUT **
*********************

* appendix A *

foreach var of varlist txrecord txblack txgoogle {
	forvalues k=0/1 {
		qui: sum callback if `var'==`k', detail
		local n=r(N)
		local mn=r(mean)
		local sd=r(sd)
		local md=r(p50)
		local norm=(`mn'-1)/(7-1)
		di as result "`var'==`k': " _col(15) %9.0f `n' %8.2f `mn' %8.2f `sd' %7.1f `md' %10.1f 100*`norm'
	}
}

local s "txrecord txblack txrecord"
local t "txblack txgoogle txgoogle"
forvalues i=1/3 {
	local v1 : word `i' of `s'
	local v2 : word `i' of `t'
	forvalues k=0/1 {
		forvalues j=0/1 {
			qui: sum callback if `v1'==`j' & `v2'==`k', detail
			local n=r(N)
			local mn=r(mean)
			local sd=r(sd)
			local md=r(p50)
			local norm=(`mn'-1)/(7-1)
			di as result "`i'  `v1'==`j' & `v2'==`k': " _col(35) %5.0f `n' %8.2f `mn' %8.2f `sd' %7.1f `md' %10.1f 100*`norm'
		}
	}
}

forvalues j=0/1 {
	forvalues k=0/1 {
		forvalues l=0/1 {
			qui: sum callback if txrecord==`k' & txblack==`j' & txgoogle==`l', detail
			local n=r(N)
			local mn=r(mean)
			local sd=r(sd)
			local md=r(p50)
			local norm=(`mn'-1)/(7-1)
			di as result "txrecord==`k' & txblack==`j' & txgoogle==`l': " _col(25) %9.0f `n' %8.2f `mn' %8.2f `sd' %7.1f `md' %10.1f 100*`norm'
		}
	}
}

* appendix B *

foreach cov of varlist X* {
	qui: anova `cov' txblack##txrecord##txgoogle
	di as result "`cov': " _col(20) %5.0f e(N) %9.2f e(F) %9.4f Ftail(e(df_m), e(df_r), e(F)) 
}

* appendix C *

foreach out of varlist callback {
	qui: reg callback 1.txrecord##1.txblack##1.txgoogle, vce(robust)
	local i=1
	foreach j of numlist 1 2 4 3 5 6 7 8 {
		local b`i'=r(table)[1,`j']
		local s`i'=r(table)[2,`j']
		local p`i'=r(table)[4,`j']
		local i=`i'+1
	}
	di as result ""
	di "1-way, record: " 					_col(35) %9.2f `b1' %9.2f `s1' %9.4f `p1'
	di "1-way, black: " 					_col(35) %9.2f `b2' %9.2f `s2' %9.4f `p2'
	di "1-way, google: " 					_col(35) %9.2f `b3' %9.2f `s3' %9.4f `p3'
	di "2-way, record X black: " 			_col(35) %9.2f `b4' %9.2f `s4' %9.4f `p4'
	di "2-way, record X google: " 			_col(35) %9.2f `b5' %9.2f `s5' %9.4f `p5'
	di "2-way, black X google: " 			_col(35) %9.2f `b6' %9.2f `s6' %9.4f `p6'
	di "3-way, record X black X google: "	_col(35) %9.2f `b7' %9.2f `s7' %9.4f `p7'
	di "constant: " 						_col(35) %9.2f `b8' %9.2f `s8' 
	di "r-square: "							_col(44) %9.2f e(r2)
}
