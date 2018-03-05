

********************************************************************************
********** FIGURE: DIE ROLL REPORTED BY BEHAVIORAL TYPE ************************
********************************************************************************


local fname= "`path'subjects_2018.dta"
use "`fname'", clear
drop if include_data==0

egen nnn=count(russia), by(realdie ind_typenew2)
keep ind_typenew2 realdie nnn
duplicates drop
sort realdie ind_typenew2
gen t=_n
drop if realdie==.
replace t=t+1 in 5/24
replace t=t+1 in 9/24
replace t=t+1 in 13/24
replace t=t+1 in 17/24
replace t=t+1 in 21/24
rename ind_typenew2 cc
egen nnt=sum(nnn), by(cc)
gen c0=nnn/nnt
twoway (bar c0 t if cc==1, color(red) barw(.8)) (bar c0 t if cc==2, color(dkgreen) barw(.8))  (bar c0 t if cc==3, color(blue) barw(.8)) (bar c0 t if cc==4, color(gray) barw(.8)), xlabel(2.5 "1" 7.5 "2" 12.5 "3" 17.5 "4" 22.5 "5" 27.5 "6", noticks) legend(row(2) order(1 "Consistent maximal" 2 "Consistent partial" 3 "Consistent honest" 4 "Other") size(small)) title("") xtitle("") ytitle("Fraction") yline(0.1666, lcolor(black)) note("The graph shows the relative frequencies of reported die rolls for different behavioral types." "The horizontal line corresponds to 0.1666=1/6.")
graph export "`path'die_type.eps", as(eps) preview(off) replace
use "`fname'", clear



*******************************************************************************
******** FIGURE: DISTRIBUTION OF DECLARATIONS BY COUNTRY ***************
*******************************************************************************

local fname= "`path'subjects_2018.dta"
use "`fname'", clear

keep if include_data==1
egen nnn=count(russia), by( declared_0_n declared_1_n)
keep declared_0_n declared_1_n nnn
quietly tabstat nnn, stats(n) save
matrix l1=r(StatTotal)
local n=l1[1,1]/100
gen nfreq=nnn/`n'
format %4.1f nfreq
graph drop _all
scatter declared_0_n declared_1_n, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(All) name(All)

use "`fname'", clear
keep if include_data==1
egen nnn=count(russia), by( declared_0_n declared_1_n country_code)
keep declared_0_n declared_1_n nnn country_code
egen ccount=count(country_code), by(country_code)
gen nfreq= nnn*100/ ccount
format %4.1f nfreq
scatter declared_0_n declared_1_n if country_code==1, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(Chile) name(Chile)
scatter declared_0_n declared_1_n if country_code==2, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(Russia) name(Russia)
scatter declared_0_n declared_1_n if country_code==3, msymbol(none) mlabel(nfreq ) mlabposition(0) xtitle(Declared 100%) ytitle(Declared 0%) title(UK) name(UK)
gr combine Chile Russia UK All, note("The figures show the percent of subjects for each number of rounds with 0% and 100% declarations ")
graph export "`path'\declared_freq_all.eps", as(eps) preview(off) replace
use "`fname'", clear




**********************************************************************************
***************** FIGURE: DISTRIBUTION OF BEHAVIOR TYPE BY PERFORMANCE ***********
**********************************************************************************

local fname= "`path'mastern_final2018.dta"
use "`fname'", clear

drop hightype
gen hightype= ncorrect_rank>.5

keep if include_data==1
collapse (count) c0=russia, by(country_code ind_typenew2 hightype)
rename offerdg_0 hightype
gsort country_code ind_typenew2 -hightype
gen t=_n
replace t=t+1 in 9/24
replace t=t+1 in 17/24

rename ind_typenew2 cc

egen nntot=sum(c0), by(country_code hightype)
replace c0=c0/nntot
generate hi = c0 + invttail(n-1,0.025)*(sqrt(c0*(1-c0)) / sqrt(nntot))
generate lo = max(c0 - invttail(n-1,0.025)*(sqrt(c0*(1-c0)) / sqrt(nntot)),0)

rename hightype type
twoway (bar c0 t if type==1&cc==1, color(red) barw(.8)) (bar c0 t if type==0&cc==1, color(red) barw(.8) fi(50)) (bar c0 t if type==1&cc==2, color(dkgreen) barw(.8)) (bar c0 t if type==0&cc==2, color(dkgreen) barw(.8) fi(50)) (bar c0 t if type==1&cc==3, color(blue) barw(.8)) (bar c0 t if type==0&cc==3, color(blue) barw(.8) fi(50)) (bar c0 t if type==1&cc==4, color(gray) barw(.8)) (bar c0 t if type==0&cc==4, color(gray) barw(.8) fi(50)) (rcap hi lo t, color(black)), xlabel(4.5 "Chile" 13.5 "Russia" 22.5 "UK", noticks) xsize(6) ylabel(0(.2)1) legend(row(2) order(1 "Consistent maximal" 3 "Consistent partial" 5 "Consistent honest" 7 "Other") size(small)) note("Distribution of behavior types by subject performance" "Dark shades correspond to high performance subjects, light shapes - to low-performance subjects") xtitle("")
graph export "`path'\ind_typenew2_hightype.eps", as(eps) preview(off) replace
use "`fname'", clear


**********************************************************************************
***************** FIGURE: DISTRIBUTION OF BEHAVIOR TYPE BY DG=0 ******************
**********************************************************************************

local fname= "`path'mastern_final2018.dta"
use "`fname'", clear

keep if include_data==1
collapse (count) c0=russia, by(country_code ind_typenew2 offerdg_0)
rename offerdg_0 hightype
gsort country_code ind_typenew2 -hightype
gen t=_n
replace t=t+1 in 9/24
replace t=t+1 in 17/24

rename ind_typenew2 cc

egen nntot=sum(c0), by(country_code hightype)
replace c0=c0/nntot
generate hi = c0 + invttail(n-1,0.025)*(sqrt(c0*(1-c0)) / sqrt(nntot))
generate lo = max(c0 - invttail(n-1,0.025)*(sqrt(c0*(1-c0)) / sqrt(nntot)),0)

rename hightype type
twoway (bar c0 t if type==1&cc==1, color(red) barw(.8)) (bar c0 t if type==0&cc==1, color(red) barw(.8) fi(50)) (bar c0 t if type==1&cc==2, color(dkgreen) barw(.8)) (bar c0 t if type==0&cc==2, color(dkgreen) barw(.8) fi(50)) (bar c0 t if type==1&cc==3, color(blue) barw(.8)) (bar c0 t if type==0&cc==3, color(blue) barw(.8) fi(50)) (bar c0 t if type==1&cc==4, color(gray) barw(.8)) (bar c0 t if type==0&cc==4, color(gray) barw(.8) fi(50)) (rcap hi lo t, color(black)), xlabel(4.5 "Chile" 13.5 "Russia" 22.5 "UK", noticks) xsize(6) ylabel(0(.2)1) legend(row(2) order(1 "Consistent maximal" 3 "Consistent partial" 5 "Consistent honest" 7 "Other") size(small)) note("Distribution of behavior types by dictator game donations" "Dark shades correspond to subjects with DG=0, light shapes - to subjects with DG>0") xtitle("")
graph export "`path'ind_typenew2_dg0.eps", as(eps) preview(off) replace
use "`fname'", clear

*******************************************************************************
************** TABLE: INITIAL PREDICTION OF GROUP RANK ******************
*******************************************************************************
tab ind_typenew2  init_pred if include_data==1&period2==1, co
ttest ncorrect_rank if include_data==1&period2==1&inlist(init_pred,1,2), by(init_pred) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(init_pred,1,2), by(init_pred) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(init_pred,3,2), by(init_pred) welch
ttest rank_withingroup if include_data==1&period2==1&inlist(init_pred,3,4), by(init_pred) welch
tabstat rank_withingroup if period2==1&include_data==1, by(init_pred) stats(mean sd)


**************************************************************************************
************** TABLE: PREDICTING DIE ROLL FROM BEHAVIOR TYPE ******************
**************************************************************************************

use "`path'mastern_final2018.dta", clear
local fname="`path'table_dieroll_pred.tex"

estimates clear
forval i=1/6 {
logit realdie_`i' ncorrect_rank male age int_typenew_1 int_typenew_2 declared_part_av ind_typenew_4 russia oxford if period2==1
margin, dydx(*)
est store m`i'
}
esttab m1 m2 m3 m4 m5 m6 using "`fname'", label mtitle(1 2 3 4 5 6) drop(ncorrect_rank male age_subject) compress nonum replace note("Logistic regression, marginal coefficients. Individual controls not shown.")

 
*******************************************************************************
************** TABLE: PARTICIPANT TYPE ************************************
*******************************************************************************

use "`path'mastern_final2018.dta", clear
local fname="`path'table_parttype.tex"
local note = "Average marginal effects for multinomial logistic regression. Dependent variable is whether the subject is a consistent maximal cheater, consistent partial cheater, is consistently honest, or neither of those. Robust standard errors. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE."


local varlist = "ncorrect_rank i.male age i.offerdg_0 offerdg i.tax_20 i.tax_30 i.tax_40 i.tax_50 i.deadweight i.mpcr i.shock i.status i.status_H i.non_fixed"
local varlist2 = " "
local varlist3 = "0.male 0.offerdg_0 0.tax_20 0.tax_30 0.tax_40 0.tax_50 0.deadweight 0.mpcr 0.shock 0.status 0.status_H 0.non_fixed 0.russia 0.oxford"


file open mf using "`fname'", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{lcccccccc}" _n
file write mf "\hline\hline" _n
file close mf

	mlogit ind_typenew2 `varlist' `varlist2' i.russia i.oxford if include_data==1&period2==1, robust 
	est store m
	forval i=1/4 {
		est res m
		margins, dydx(*) predict(outcome(`i')) post 
		test 1.tax_20=1.tax_30
		test 1.tax_20=1.tax_40
		test 1.tax_20=1.tax_50
		test 1.tax_30=1.tax_40		
		test 1.tax_30=1.tax_50
		test 1.tax_40=1.tax_50
		*test status_H=status_L
		est store m`i'
	}
	
	esttab m1 m2 m3 m4 using "`fname'", drop(`varlist3') label mtitle("Consistent maximal" "Consistent partial" "Consistently honest" "Other") wide compress nonum title(`tt') append fragment  prehead(&\multicolumn{6}{c}{\bf `tt'}\\) postfoot(\hline\hline) se star(* 0.10 ** 0.05 *** 0.01)
file open mf using "`fname'", write append
file write mf "\multicolumn{9}{p{17cm}}{\tiny `note'}\\" _n
file write mf "\multicolumn{9}{l}{\tiny \sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" _n
file write mf "\end{tabular}"
file close mf


*******************************************************************************
*******************************************************************************
*************** TABLES: CHOICE  *********************************************
*******************************************************************************
*******************************************************************************

use "`path'mastern_final2018.dta", clear
local varlist_3_all "i.tax_40 i.tax_50 i.deadweight i.mpcr i.russia i.oxford" 
local varlist_3_uk "i.tax_40 i.tax_50 i.deadweight i.mpcr"



********************************************************************************
*************** PERIODS 1-10 *************************************************
********************************************************************************
local fname="`path'table_reduced1_10.tex"
local conds "1==1 country_code==1 country_code==2 country_code==3"
local varlist4="i.shock i.shock_H i.status i.status_H i.non_fixed"
local varlist = "ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg i.tax_20 i.tax_30"
local varlist2_0 = "0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local varlist2_4 = "0.tax_20 0.tax_30 0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local varlist2_1 = "0.russia 0.oxford 0.tax_20 0.tax_30 0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local note = "Average marginal effects for multinomial logistic regression. Dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE."
              
********************************************************************************
**************** PERIODS 2-10, PAST ACTIONS ********************************
********************************************************************************
local fname="`path'table_reduced2_10.tex"
local conds "1==1&period2>1 country_code==1&period2>1 country_code==2&period2>1 country_code==3&period2>1"
local varlist4="i.shock i.shock_H i.status i.status_H i.non_fixed i.l0 i.lf l_declim l_others"
local varlist = "ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg i.tax_20 i.tax_30"
local varlist2_0 = "0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local varlist2_4 = "0.tax_20 0.tax_30 0.tax_40 0.tax_50 1.tax_20 1.tax_30 1.tax_40 1.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local varlist2_1 = "0.russia 0.oxford 0.tax_20 0.tax_30 0.tax_40 0.tax_50 1.tax_20 1.tax_30 1.tax_40 1.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local note = "Average marginal effects for multinomial logistic regression. Dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE. Deduction controls not shown."

********************************************************************************
**************** PERIOD 1 ****************************************************
********************************************************************************

local fname="`path'table_reduced1.tex"
local conds "1==1&period2==1 country_code==1&period2==1 country_code==2&period2==1 country_code==3&period2==1"
local varlist4="i.shock i.shock_H i.status i.status_H i.non_fixed"
local varlist = "ncorrect_rank ncorrect_dev2 i.male age i.offerdg_0 offerdg i.tax_20 i.tax_30"
local varlist2_0 = "0.tax_20 0.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local varlist2_4 = "0.tax_20 0.tax_30 0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local varlist2_1 = "0.russia 0.oxford 0.tax_20 0.tax_30 0.tax_40 0.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0"
local note = "Average marginal effects for multinomial logistic regression. Dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE."


********************************************************************************
**************** PERIODS 1-10,  MORE CONTROLS ******************************
********************************************************************************
local fname="`path'table_reduced1_10_more.tex"
local conds "1==1 country_code==1 country_code==2 country_code==3"
local varlist4="i.shock i.shock_H i.status i.status_H i.non_fixed norms i.trust safechoices ideology income2"
local varlist = "ncorrect_rank ncorrect_dev2 i.male age period2 i.offerdg_0 offerdg i.tax_20 i.tax_30"
local varlist2_0 = "0.tax_20 0.tax_30 1.tax_20 1.tax_30 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0 0.trust"
local varlist2_4 = "0.tax_20 0.tax_30 0.tax_40 0.tax_50 1.tax_20 1.tax_30 1.tax_40 1.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0 0.trust"
local varlist2_1 = "0.russia 0.oxford 0.tax_20 0.tax_30 0.tax_40 0.tax_50 1.tax_20 1.tax_30 1.tax_40 1.tax_50 0.mpcr 0.deadweight 0.shock 0.shock_H 0.status 0.status_H 0.non_fixed 0.male 0.offerdg_0 0.trust"
local note = "Average marginal effects for multinomial logistic regression. Dependent variable is whether the subject declared 0\%, 100\%, or something in between, in a given round. Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE.  Deduction controls not shown."

*********************************************************************************
********** TABLES: CHOICE, COMMON PART **************************************
*********************************************************************************

local titles="All Chile Russia UK"
local aplist="append append append append"
file open mf using "`fname'", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{lcccccc}" _n
file write mf "\hline\hline" _n
file close mf

forval ii=1/4 {
*forval ii=1/3 {
*forval ii=4 {
	local cc `: word `ii' of `conds''
	local tt `: word `ii' of `titles''
	local ap `: word `ii' of `aplist''
	estimates clear
	if `ii'==1 {
		local varlist3 `varlist_3_all' 
		local varlist2 `varlist2_1'
	} 
	else if `ii'==4 {
		local varlist3 `varlist_3_uk' 
		local varlist2 `varlist2_4'
	}
	else {
		local varlist3=" "
		local varlist2 `varlist2_0'
	}	
	mlogit declared_cat `varlist' `varlist3' `varlist4' if `cc'&include_data==1, cluster(subj_id)
	est store m
	forval i=1/3 {
		est res m
		margins, dydx(*) predict(outcome(`i')) post
		test 1.tax_20=1.tax_30
		if `ii'==1|`ii'==4 {
			test 1.tax_20=1.tax_40
			test 1.tax_20=1.tax_50
			test 1.tax_30=1.tax_40		
			test 1.tax_30=1.tax_50
		}	
		*test status_H=status_L
		*test shock_H=shock_L
		est store m`i'
	}
	
	esttab m1 m2 m3 using "`fname'", drop(`varlist2') label mtitle("Maximal cheating" "Partial cheating" "Honest") wide compress nonum title(`tt') `ap' fragment  prehead(&\multicolumn{6}{c}{\bf `tt'}\\) postfoot(\hline\hline) se star(* 0.10 ** 0.05 *** 0.01)
}

file open mf using "`fname'", write append
file write mf "\multicolumn{7}{p{12cm}}{\tiny `note'}\\" _n
file write mf "\multicolumn{7}{l}{\tiny \sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" _n
file write mf "\end{tabular}"
file close mf

******************************************************************
*******  END OF TABLE WITH MULTINOMIAL CHOICE **************
******************************************************************



*******************************************************************************
************** TABLES: LIMIT CHEATERS TYPE ********************************
*******************************************************************************

use "`path'mastern_final2018.dta", clear
local fname="`path'table_parttype_lim.tex"

gen declared_frac_p=declared_frac
replace declared_frac_p=. if declared_cat!=2
egen declared_part_av=mean(declared_frac_p) if include_data==1, by(subj_id)
replace declared_part_av=0 if declared_part_av==.

estimates clear
local varname="ncorrect_rank male age offerdg_0 offerdg tax_20 tax_30 status status_H non_fixed"
reg declared_part_av `varname' if ind_typenew2==2&include_data==1&country_code==1&period2==1, robust
est store m1
reg declared_part_av `varname' if ind_typenew2==2&include_data==1&country_code==2&period2==1, robust
est store m2
local varname="ncorrect_rank male age offerdg_0 offerdg tax_20 tax_30 tax_40 tax_50 deadweight mpcr status status_H non_fixed"
reg declared_part_av `varname' if ind_typenew2==2&include_data==1&country_code==3&period2==1, robust
est store m3
reg declared_part_av `varname' russia oxford if ind_typenew2==2&include_data==1&period2==1, robust
est store m4

esttab m1 m2 m3 m4 using "`fname'", label mtitle("Chile" "Russia" "UK" "All") wide compress nonum replace note("OLS regressions for consistent partial cheaters. Robust standard errors. Dependent variable is the average fraction of income declared, excluding 0\% and 100\% declarations. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task.") r2 se star(* 0.10 ** 0.05 *** 0.01)

graph drop _all
hist declared_part_av if ind_typenew2==2&country_code==1, title(Chile) xtitle("") name(Chile)
hist declared_part_av if ind_typenew2==2&country_code==2, title(Russia) xtitle("") name(Russia)
hist declared_part_av if ind_typenew2==2&country_code==3, title(UK) xtitle("") name(UK)
gr combine Chile Russia UK, r(1) xsize(5) ysize(2) note("Distribution of average fracion of income declared, excluding 0% and 100% declarations, for consistent partial cheaters")
 

 
 
 
 
 


*******************************************************************************
*******************************************************************************
*************** TABLES: LIMIT CHEATING **************************************** 
*******************************************************************************
*******************************************************************************

use "`path'mastern_final2018.dta", clear
local varlist2 "deadweight mpcr shock shock_H status status_H non_fixed" 
local varlist4 ""



********************************************************************************
*************** CHILE **************************************************
********************************************************************************

local fname="`path'table_limit_chile.tex"
local vv1=""
local vv2=""
local cc1 "country_code==1"


********************************************************************************
*************** RUSSIA **************************************************
********************************************************************************

local fname="`path'table_limit_russia.tex"
local vv1=""
local vv2=""
local cc1 "country_code==2"


********************************************************************************
*************** UK *************************************************************
********************************************************************************

local fname="`path'table_limit_uk.tex"
local vv1="tax_40 tax_50"
local vv2=""
local cc1 "country_code==3"

*********************************************************************************
********** TABLES: LIMIT CHEAT, COMMON PART *************************************
*********************************************************************************

estimates clear
local varlist1 = "ncorrect_rank ncorrect_dev2 male age period2 offerdg_0 offerdg tax_20 tax_30"
local varlist3 = ""
local cc2 "1==1"
reg declared_frac `varlist1' `vv1' `varlist2' `varlist3' `vv2' if `cc1'&`cc2'&include_data==1&declared_cat==2&ind_typenew2==2, cluster(subj_id)
est store m1

local varlist1 = "ncorrect_rank ncorrect_dev2 male age period2 offerdg_0 offerdg tax_20 tax_30"
local varlist3 = "l0 lf l_declim l_others"
local cc2 "period2>1"
reg declared_frac `varlist1' `vv1' `varlist2' `varlist3' `vv2' if `cc1'&`cc2'&include_data==1&declared_cat==2&ind_typenew2==2, cluster(subj_id)
est store m2

local varlist1 = "ncorrect_rank ncorrect_dev2 male age offerdg_0 offerdg tax_20 tax_30"
local varlist3 = ""
local cc2 "period2==1"
reg declared_frac `varlist1' `vv1' `varlist2' `varlist3' `vv2' if `cc1'&`cc2'&include_data==1&declared_cat==2&ind_typenew2==2, cluster(subj_id)
est store m3

local varlist1 = "ncorrect_rank ncorrect_dev2 male age period2 tax_20 tax_30"
local varlist3 = "l_others norms trust safechoices ideology income2"
local cc2 "1==1"
reg declared_frac `varlist1' `vv1' `varlist2' `varlist3' `vv2' if `cc1'&`cc2'&include_data==1&declared_cat==2&ind_typenew2==2, cluster(subj_id)
est store m4



local varlist1 = "ncorrect_dev2 period2 shock_H"
local cc2 "1==1"
reg declared_frac `varlist1' i.subj_id if `cc1'&`cc2'&include_data==1&declared_cat==2&ind_typenew2==2, cluster(subj_id)
est store m6

esttab m1 m2 m3 m4 m6 using "`fname'", drop(`varlist4' *subj_id) label mtitle("Periods 1-10" "Periods 2-10" "Period 1" "Periods 1-10" "Periods 1-10, FE") wide compress nonum replace  note("OLS regressions for consistent partial cheaters. Standard errors are clustered by subject. Consistent partial cheaters. Dependent variable is the fraction of income declared in a given round, excluding 0\% and 100\% declarations. t-values in parenthesis." "RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE.") r2 se star(* 0.10 ** 0.05 *** 0.01)


******************************************************************
*******  END OF TABLE WITH LIMIT CHEATING ************************
******************************************************************



******************************************************************
***********FIGURE: CUMULATIVE DISTRIBUTION OF REACTION TIME ******
******************************************************************


local fname= "`path'mastern_final2018.dta"
use "`fname'", clear
graph drop _all

drop time_declare_*	
gen time_declare_1=time_declare+uniform()
replace time_declare_1=. if include_data==0
	sort time_declare_1 declared_cat

	cumul time_declare_1 if declared_cat==1, gen(time_declare_cu_1)
	cumul time_declare_1 if declared_cat==2, gen(time_declare_cu_2)
	cumul time_declare_1 if declared_cat==3, gen(time_declare_cu_3)

	label variable time_declare_1 "seconds"

	twoway (line time_declare_cu_1 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black)) (line time_declare_cu_2 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern(dash)) (line time_declare_cu_3 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern("-.")), legend(label(1 "Maximal cheating") label(2 "Partial cheating") label(3 "Honest") row(1) size(small)) name(RT) xsize(5) note("Cumulative distribution functions of RT for different decisions")
	
graph export "`path'response.eps", as(eps) preview(off) replace
use "`fname'", clear
 

*****************************************************************
********** FIGURE: RT BY PREVIOUS CHOICE, BY COUNTRY ************
*****************************************************************

local fname= "`path'mastern_final2018.dta"
use "`fname'", clear

graph drop _all

gen time_declare_1=.
forval i=1/3 {
	drop time_declare_*	
	gen time_declare_1=time_declare+uniform()
	replace time_declare_1=. if country_code!=`i'&include_data==0
	sort time_declare_1 declared_cat

	cumul time_declare_1 if declared_cat==1, gen(time_declare_cu_1)
	cumul time_declare_1 if declared_cat==2, gen(time_declare_cu_2)
	cumul time_declare_1 if declared_cat==3, gen(time_declare_cu_3)

	label variable time_declare_1 "seconds"
	if `i'==1 {
		twoway (line time_declare_cu_1 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black)) (line time_declare_cu_2 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern(dash)) (line time_declare_cu_3 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern("-.")), legend(label(1 "Maximal cheating") label(2 "Partial cheating") label(3 "Honest") row(1) size(small)) name(Chile) title(Chile)
	} 
	else if `i'==2 {
		twoway (line time_declare_cu_1 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black)) (line time_declare_cu_2 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern(dash)) (line time_declare_cu_3 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern("-.")), legend(label(1 "Maximal cheating") label(2 "Partial cheating") label(3 "Honest") row(1) size(small)) name(Russia) title(Russia)
	}
	else if `i'==3 {
		twoway (line time_declare_cu_1 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black)) (line time_declare_cu_2 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern(dash)) (line time_declare_cu_3 time_declare_1 if time_declare<31&include_data==1, c(J) lcolor(black) lpattern("-.")), legend(label(1 "Maximal cheating") label(2 "Partial cheating") label(3 "Honest") row(1) size(small)) name(UK) title(UK)
	}

	}
gr combine Chile Russia UK, row(1) xsize(8) ysize(3) note("Cumulative distribution functions of RT for different decisions")
graph export "`path'response_country.eps", as(eps) preview(off) replace
use "`fname'", clear
 

 

*******************************************************************************
*************** TABLE: NEAR-MAXIMAL CHEATING ****************************** 
*******************************************************************************

local fname="`path'table_nearmax.tex"
local byvar="hightype"
local llist="Low High p"

local fname="`path'table_nearmax_male.tex"
local byvar="male"
local llist="Female Male p"

local fname="`path'table_nearmax_dg.tex"
local byvar="offerdg_0"
local llist="DG>0 DG=0 p"



local uplim="1 10 20 30 40 50 60 70 80 90"
local clist="Chile Russia UK"
matrix M=J(9,10,.)
drop near_temp
gen near_temp=.
local i=1
local j=2
forval iii=1/3 {
forval ii=1/10 {
	local ul `: word `ii' of `uplim''
	quietly replace near_temp=0
	quietly replace near_temp=1 if declared>0&declared<=`ul'
	quietly tabstat near_temp if include_data==1&country_code==`iii', by(`byvar') stats(mean sd n) save

	matrix l1=r(Stat1)
	matrix l2=r(Stat2)
	*local se=sqrt(l`i'[2,1]^2/l`i'[3,1]+l`j'[2,1]^2/l`j'[3,1])
	*local t=abs(l`i'[1,1]-l`j'[1,1])/`se'
	*local df=min(l`i'[3,1],l`j'[3,1])-1
	*local pp=tprob(`df',`t')
	quietly cc `byvar' near_temp if include_data==1&country_code==`iii', exact
	local pp=  r(p_exact)
	
	mat M[1+3*(`iii'-1),`ii']=l`i'[1,1]
	mat M[2+3*(`iii'-1),`ii']=l`j'[1,1]
	mat M[3+3*(`iii'-1),`ii']=`pp'
}
}


file open mf using "`fname'", write replace
file write mf "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write mf "\begin{tabular}{|ll|cccccccccc|}" _n
file write mf "\hline\hline" _n
file write mf "&"
forval i=1/10 {
	local ul `: word `i' of `uplim''
	file write mf "&1-`ul' ECU"
}
file write mf "\\" _n
file write mf "\hline" _n
forval ii=1/3 {
	local cc `: word `ii' of `clist''
	forval iii=1/3 {
		local ll `: word `iii' of `llist''
		if `iii'==2 {
			file write mf "`cc'"
		}
		file write mf "&`ll'"
		forval j=1/10 {
			local mm=M[`iii'+3*(`ii'-1),`j']
			file write mf "&"
			file write mf %9.6f (`mm')
			
			
		}
		file write mf "\\" _n
	}
	file write mf "\hline"	
}
file write mf "\multicolumn{11}{p{\textwidth}}{\footnotesize For each country, the first two rows report the frequencies of declarations for two groups of subjects. The third row reports the p-value for Fisher's exact test comparing these two frequencies.}\\" _n
file write mf "\end{tabular}"
file close mf


****************************************************************************
*********** FIGURE: PREVALENCE OF NEAR-MAXIMAL CHEATING *************
****************************************************************************

local fname= "`path'mastern_final2018.dta"
use "`fname'", clear
drop declared_near
gen declared_near=declared>0&declared_frac<=.2
collapse (mean) c0=declared_0 cf=declared_f cn=declared_near (sd) c0_sd=declared_0 cf_sd=declared_f cn_sd=declared_near (count) n=cheat if include_data==1, by(country_code hightype)
rename hightype type
replace type=type+1
expand 3
sort country_code type
gen t=_n
gen cc=0
forval i=1/3 {
forval ii=1/3 {
forval iii=1/2 {
local obsn=(`i'-1)*6+(`iii'-1)*3+`ii'
replace cc=`ii' in `obsn'
if `ii'==2 {
replace c0=cf in `obsn'
replace c0_sd=cf_sd in `obsn'
}
else if `ii'==3 {
replace c0=cn in `obsn'
replace c0_sd=cn_sd in `obsn'
}
}
}
}
generate hi = c0 + invttail(n-1,0.025)*(c0_sd / sqrt(n))
generate lo = c0 - invttail(n-1,0.025)*(c0_sd / sqrt(n))
replace t=t+1 in 7/18
replace t=t+1 in 13/18

twoway (bar c0 t if type==1&cc==1, color(red) barw(.8)) (bar c0 t if type==1&cc==2, color(red) barw(.8) fi(60)) (bar c0 t if type==1&cc==3, color(red) barw(.8) fi(30)) (bar c0 t if type==2&cc==1, color(blue) barw(.8)) (bar c0 t if type==2&cc==2, color(blue) barw(.8) fi(60)) (bar c0 t if type==2&cc==3, color(blue) barw(.8) fi(30)) (rcap hi lo t, color(black)), xlabel(3 "Chile" 10 "Russia" 17.5 "UK", noticks) note("Prevalence of cheating depending on subject performance") ylabel(0(.2)1) legend(row(3) order(1 "Low performance, maximal" 4 "High performance, maximal" 2 "Low performance, limited" 5 "High performance, limited" 3 "Low performance, near-maximal" 6 "High performance, near-maximal" ) size(small)) xsize(6) xtitle("")
graph export "`path'\cheat_hilo.eps", as(eps) preview(off) replace
use "`fname'", clear



******************************************************************************
********* FIGURE: DISTRIBUTION OF RET PERFORMANCE ****************************
******************************************************************************

local i=1
graph twoway (kdensity ncorrect_subjav if period2==1&country_code==1&include_data==1, color(dkgreen) range(0 30) bwidth(`i')) (kdensity ncorrect_subjav if period2==1&country_code==2&include_data==1, color(red) range(0 30) bwidth(`i'))  (kdensity ncorrect_subjav if period2==1&country_code==3&include_data==1, color(blue) range(0 30) bwidth(`i')), legend(row(1) order(1 "Chile" 2 "Russia" 3 "UK")) xtitle("") ytitle("") note("Distribution of the number of correct answers. Epanechnikov density, bwidth=1")
graph export "`path'ret_density.eps", as(eps) preview(off) replace


******************************************************************
******* TABLE: RET PERFORMANCE ***********************************
******************************************************************

use "`path'mastern_final2018.dta", clear
local fname="`path'table_ret.tex"

local varlist1 = "male age_subject offerdg_0 offerdg tax_20 tax_30"
local varlist2 = "tax_40 tax_50 deadweight mpcr" 
local varlist4 = "shock status status_H non_fixed norms trust safechoices ideology income2"
local vv = "russia oxford"

estimates clear
reg ncorrect_subjav `varlist1' `varlist4' if period2==1&include_data==1&country_code==1, robust
est store m1
reg ncorrect_subjav `varlist1' `varlist4' if period2==1&include_data==1&country_code==2, robust
est store m2 
reg ncorrect_subjav `varlist1' `varlist2' `varlist4' if period2==1&include_data==1&country_code==3, robust
est store m3 
reg ncorrect_subjav `varlist1' `varlist2' `vv' `varlist4' if period2==1&include_data==1, robust
est store m4
esttab m1 m2 m3 m4 using "`fname'", label mtitle("Chile" "Russia" "UK" "All") wide compress nonum replace order(`varlist1' `varlist2' `vv' `varlist4') note("OLS regression. Robust standard errors. Dependent variable is subject's average performance over 10 rounds.") se r2 star(* 0.10 ** 0.05 *** 0.01)


******************************************************************
******* TABLE: RET PERFORMANCE, PERIOD ***************************
******************************************************************

use "`path'mastern_final2018.dta", clear
local fname="`path'table_ret_per.tex"

local varlist1 = "male age_subject period2 offerdg_0 offerdg tax_20 tax_30"
local varlist2 = "tax_40 tax_50 deadweight mpcr" 
local varlist4 = "shock l_shock status status_H non_fixed l_others norms trust safechoices ideology income2"
local vv = "russia oxford"
local varlist5 = "tax_20 tax_30 tax_40 tax_50" 

estimates clear
reg ncorrectret `varlist1' `varlist4' if period2>1&include_data==1&country_code==1, cluster(subj_id)
est store m1
reg ncorrectret `varlist1' `varlist4' if period2>1.&include_data==1&country_code==2, cluster(subj_id)
est store m2 
reg ncorrectret `varlist1' `varlist2' `varlist4' if period2>1.&include_data==1&country_code==3, cluster(subj_id)
est store m3 
reg ncorrectret `varlist1' `varlist2' `vv' `varlist4' if period2>1.&include_data==1, cluster(subj_id)
est store m4
esttab m1 m2 m3 m4
esttab m1 m2 m3 m4 using "`fname'", label mtitle("Chile" "Russia" "UK" "All") drop(`varlist5') wide compress nonum replace order(`varlist1' `varlist2' `vv' `varlist4') note("Standard errors are clustered by subject. RET rank is the national rank, between 0 and 1, of subject's national performance at the real effort task. RET Deviation is the difference between actual number of correct additions and one predicted from subject and period FE.") r2 se star(* 0.10 ** 0.05 *** 0.01)



*********************************************************************************
************** TESTS IN THE TEXT ************************************************
*********************************************************************************
*\ref{stata:chile_udd}
ranksum ind_typenew2 if country_code==1&period2==1&include_data==1, by(chile_udd)

*\ref{stata:robustcheat}
drop ttt
gen ttt= ind_typenew2
replace ttt=. if ~inlist( ind_typenew2,1,3)
replace ttt=0 if ttt==3
cc ttt realdie_6 if period2==1&include_data==1, exact
cc ttt realdie_5 if period2==1&include_data==1, exact
cc ttt realdie_2 if period2==1&include_data==1, exact

*\ref{stata:robustcheat_ranksum}
* Mann-Whitney U test, Mostly 1-99 vs mostly 100
drop ttt
gen ttt= ind_typenew2
replace ttt=. if inlist(ttt,1,4)
ranksum realdie if period2==1&include_data==1, by(ttt)
* Mann-Whitney U test, Always 1-99 vs always 100
drop ttt
gen ttt=.
replace ttt=1 if declared_f_n==10
replace ttt=2 if declared_1_n==10
ranksum realdie if period2==1&include_data==1, by(ttt)

*\ref{stata:robustcheat_bino}
bitest realdie_5==.16666 if ind_typenew2==3&period2==1&include_data==1
bitest realdie_6==.16666 if ind_typenew2==3&period2==1&include_data==1
bitest realdie_5==.16666 if declared_1_n==10&period2==1&include_data==1
bitest realdie_6==.16666 if declared_1_n==10&period2==1&include_data==1

*\label{stata:whocheats}
drop ttt
gen ttt= ind_typenew2==1
drop hightype
gen hightype=ncorrect_rank>.5
cc hightype ttt if country_code==1&period2==1&include_data==1, exact
cc hightype ttt if country_code==2&period2==1&include_data==1, exact
cc hightype ttt if country_code==3&period2==1&include_data==1, exact
drop ttt
gen ttt= ind_typenew2==2
cc hightype ttt if country_code==1&period2==1&include_data==1, exact
cc hightype ttt if country_code==2&period2==1&include_data==1, exact
cc hightype ttt if country_code==3&period2==1&include_data==1, exact

*\label{stata:whocheats_dg}
tab ind_typenew2 offerdg_0 if include_data==1&period2==1, co
drop ttt
gen ttt=ind_typenew2==1
cc offerdg_0 ttt if country_code==1&include_data==1&period2==1, exact
cc offerdg_0 ttt if country_code==2&include_data==1&period2==1, exact
cc offerdg_0 ttt if country_code==3&include_data==1&period2==1, exact
cc offerdg_0 realdie_6 if country_code==2&include_data==1&period2==1, exact
cc offerdg_0 realdie_6 if country_code==3&include_data==1&period2==1, exact

*\label{stata:init_pred}
tab ind_typenew2 init_pred if period2==1&include_data==1, co
egen rank_withingroup=rank( ncorrect_subjav) if period2==1, by( session group)
replace rank_withingroup=5- rank_withingroup
ttest rank_withingroup if inlist(init_pred,1,2)&include_data==1, by(init_pred) welch
ttest rank_withingroup if inlist(init_pred,2,3)&include_data==1, by(init_pred) welch
ttest rank_withingroup if inlist(init_pred,3,4)&include_data==1, by(init_pred) welch
drop ttt
gen ttt=ind_typenew2==1
drop ppp
gen ppp=inlist(init_pred,1,2)
cc ppp ttt if init_pred!=.&include_data==1&period2==1, exact

*\label{stata:nearmaxfig}
drop declared_near
gen declared_near=declared>0&declared_frac<=.2
replace declared_near=1 if declared_frac>0&declared_frac<=.2
cc hightype declared_near if country_code==1&include_data==1, exact
cc hightype declared_near if country_code==2&include_data==1, exact
cc hightype declared_near if country_code==3&include_data==1, exact

*\label{stata:ret}
tabstat ncorrect_subjav if period2==1&include_data==1, by(country_code) stats(mean sd)
ttest ncorrect_subjav if period2==1&include_data==1&inlist(country_code,2,3), by(country_code) welch
ttest ncorrect_subjav if period2==1&include_data==1&country_code==1, by(offerdg_0) welch
ttest ncorrect_subjav if period2==1&include_data==1&country_code==2, by(offerdg_0) welch
ttest ncorrect_subjav if period2==1&include_data==1&country_code==3, by(offerdg_0) welch

*\label{stata:part}

tabstat declared_part_av if ind_typenew2==2&period2==1&include_data==1, by(country_code) stats(mean sd n)
ttest declared_part_av if ind_typenew2==2&period2==1&include_data==1&inlist(country_code,1,3), by(country_code) welch

ttest declared_part_av if declared_f_n==10 &include_data==1&inlist(country_code,1,3)&period2==1, by(country_code)
ttest declared_part_av if declared_f_n==10 &include_data==1&inlist(country_code,2,3)&period2==1, by(country_code)
tabstat declared_part_av if declared_f_n==10 &period2==1&include_data==1, by(country_code) stats(mean sd n)
ttest declared_part_av if declared_f_n==10 &period2==1&include_data==1&inlist(country_code,1,3), by(country_code) welch
ttest declared_part_av if declared_f_n==10 &period2==1&include_data==1&inlist(country_code,2,3), by(country_code) welch
