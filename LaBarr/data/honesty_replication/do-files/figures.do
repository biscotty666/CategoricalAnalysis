** =============================================================================
** Figures for Civic Honesty Around the Globe
** =============================================================================

** Set colors
** ==========================================

global color_nomoney "orange*0.8"
global color_money "red"
global color_bigmoney "red*2"



** Loading Data
** ==========================================
version 14.2
snapshot erase _all

// primary data set
use "../data/behavioral data.dta", clear 
snapshot save

// non-expert prediction data
use "../data/non-expert data.dta", clear 
generate id = _n
reshape long prediction selfinterest altruism theftaversion, i(id) j(cond)
label define condl 1 "No Money" 2 "Money" 3 "Big Money"
label val cond condl
snapshot save

// expert prediction data
use "../data/expert data.dta", clear 
generate id = _n
reshape long prediction, i(id) j(cond)
label define condl 1 "No Money" 2 "Money" 3 "Big Money"
label val cond condl
snapshot save



** Make graph directory
** ==========================================
cap mkdir "../graphs"



** Figure 1: Share of wallets reported
** ==========================================

// Defined offset for right panel
local offset = 95
local offset_m1 = `offset' - 1
local offset_p5 = `offset' + 5
local offset_p10 = `offset' + 10
local offset_p15 = `offset' + 15
local offset_max = `offset' + 18
local mlab_y = 0.2
local mlab_x = 0

// Right panel
snapshot restore 1
keep if inlist(cond,0,1)
separate response, by(cond)
collapse (mean) NM = response0, by(country)
egen rank = rank(NM), unique
tempfile ranks
save "`ranks'"

snapshot restore 1
keep if inlist(cond,0,1)
qui merge m:1 country using "`ranks'"
qui generate qrt = floor((rank+9)/10)
qui parmby "qui areg response cond, a(city) r",by(qrt) label command norestore
gen se_low = estimate - stderr
gen se_high = estimate + stderr
keep if parm == "cond"
tempfile te
save "`te'"

// Left panel
snapshot restore 1
keep if inlist(cond,0,1)
separate response, by(cond)
collapse (mean) NoMoney = response0 Money = response1, by(country)
egen rank = rank(NoMoney), unique

append using `te'
gen ypos = 10*(qrt-1) + 5.5
foreach var of varlist estimate se_low se_high {
	replace `var' = 1*`var' + `offset'
}
gen whitebar = `offset' if estimate != .

// Position labels
gen lab_ypos = rank + `mlab_y'
gen lab_xpos = NoMoney
replace lab_xpos = Money if Money <= NoMoney
replace lab_xpos = lab_xpos + `mlab_x'

#delimit ;
twoway 	(bar estimate ypos, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(2)) 
		(bar whitebar ypos, sort lcolor(white) color(white) horizontal barw(4)) 
		(rcap se_high se_low ypos, col(gs4) lwidth(medium) horizontal)
		(scatteri 0.75 `offset' 10.25 `offset', c(l) lcolor(gs13) msym(none))
		(scatteri 10.75 `offset' 20.25 `offset', c(l) lcolor(gs13) msym(none))
		(scatteri 20.75 `offset' 30.25 `offset', c(l) lcolor(gs13) msym(none))
		(scatteri 30.75 `offset' 40.25 `offset', c(l) lcolor(gs13) msym(none))
		(pcspike rank NoMoney rank Money, lcolor(gs13)) /// creates connecting line
		(scatter rank Money, sort mcolor($color_money) msize(large)) /// creates orange markers
		(scatter rank NoMoney, sort mcolor($color_nomoney) msize(large)) /// creates yellos markers
		(scatter rank NoMoney if Money >= NoMoney, msymbol(none) ) /// adds labels for most countries
		(scatter rank Money if Money <= NoMoney, msymbol(none)) /// adds labels for inverted countries
		(scatter lab_ypos lab_xpos, sort m(i) mlabcolor(black) mlabel(country) mlabsize(*1.15) mlabposition(9) mlabgap(medsmall))
		, yscale(off) yscale(range(0.5 40.5)) yscale(noline) ylabel(none) 
		xtitle("                                            Reporting rate (%)                                                          Treatment effect (%)", color(black) size(medsmall)) 
		xscale(range(-5 `offset_max')) xscale(lcolor(black) lwidth(medium) line)
		xlabel(0 "0" 10 "10" 20 "20" 30 "30" 40 "40" 50 "50" 60 "60" 70 "70" 80 "80" `offset' "0" `offset_p10' "10", 
			labcolor(black) noticks nogrid labsize(medsmall)) 
		text(5.5 `offset_m1' "4{superscript:th} Quartile", orient("vertical") place(w) size(medsmall)) 
		text(15.5 `offset_m1' "3{superscript:rd} Quartile", orient("vertical") place(w) size(medsmall)) 
		text(25.5 `offset_m1' "2{superscript:nd} Quartile", orient("vertical") place(w) size(medsmall))
		text(35.5 `offset_m1' "1{superscript:st} Quartile", orient("vertical") place(w) size(medsmall))
		legend(on order(10 "NoMoney" 9 "Money") rows(2) region(lcolor(white)) position(11) ring(0) bmargin(small) size(medsmall)) 
		graphregion(fcolor(white) lcolor(white)) ysize(13) xsize(15) name(fig_1, replace)
		scale(0.7) 
;
#delimit cr 
graph export "../graphs/Fig1.png", replace  



** Figure 2: Reporting rates and stakes
** ==========================================

snapshot restore 1
keep if inlist(Country,"Poland","UK","USA")
keep if inlist(cond,0,1,2)
collapse (mean) response, by(cond Country)
gen mlab = ""
replace mlab = Country if cond == 2
twoway 	(scatter response cond if Country == "Poland", c(l) msize(large) lwidth(medthick) color($color_bigmoney) mlabcolor($color_bigmoney) mlabel(mlab) mlabposition(3) mlabsize(*1.3) mlabgap(tiny)) ///
		(scatter response cond if Country == "UK", c(l) msize(large) lwidth(medthick) color($color_money) mlabcolor($color_money) mlabel(mlab) mlabposition(3) mlabsize(*1.3) mlabgap(tiny)) ///
		(scatter response cond if Country == "USA", c(l) msize(large) lwidth(medthick) color($color_nomoney) mlabcolor($color_nomoney) mlabel(mlab) mlabposition(3) mlabsize(*1.3) mlabgap(tiny)) ///
		, xscale(range(-.5 2.5)) yscale(range(17 100) lcolor(black) lwidth(medium)) ///
		xlabel(0 "NoMoney" 1 "Money" 2 "BigMoney", notick) ///
		ylabel(20(10)100, labsize(medium) angle(horizontal) gmin notick nogrid) ///
		xtitle("") ytitle("Reporting Rate (%)", margin(esubhead) size(medium)) xscale(lstyle(none)) ///
		legend(off) graphregion(fcolor(white) lcolor(white)) ysize(4) xsize(3) name(fig_2, replace)
graph export "../graphs/Fig2.png", replace



** Figure 3: Actual vs. predicted reporting 
** ==========================================

// left panel
snapshot restore 1
keep if inlist(Country,"USA")
keep if inlist(cond,0,1,2)
collapse (mean) response (semean) se = response, by(cond)
generate se_low = response - se
generate se_high = response + se
label define labcond  0 "NoMoney" 1 "Money" 2 "BigMoney"
label values cond labcond 
twoway 	(bar response cond if cond == 0, sort fcolor($color_nomoney) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9))  ///
		(bar response cond if cond == 1, sort fcolor($color_money) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9)) ///
		(bar response cond if cond == 2, sort fcolor($color_bigmoney) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9)) ///
		(rcap se_high se_low cond, col(black) lwidth(medium)), ///
		ytitle("Reporting Rate (%)", size(medium)) yscale(range(20 80) lcolor(black) lwidth(medium)) ///
		ylabel(20(10)80, labsize(medium) angle(horizontal) notick gmin nogrid) ///
		legend(off) xlabel(0 1 2, valuelabel labsize(medium) noticks labgap(*1.3)) xscale(lstyle(none)) ///
		xtitle("Actual Reporting Rate" "(United States)", margin(medlarge) size(vlarge) color(black)) ///
		graphregion(fcolor(white) lcolor(white)) ysize(5) xsize(3.5) name(fig_3_left, replace)
		
// middle panel
snapshot restore 2
collapse (mean) prediction (semean) se = prediction, by(cond) 
generate hiprediction = prediction + se 
generate loprediction = prediction - se
label define labcond  1 "NoMoney" 2 "Money" 3 "BigMoney"
label values cond labcond 
twoway 	(bar prediction cond if cond == 1, sort fcolor($color_nomoney) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9))  ///
		(bar prediction cond if cond == 2, sort fcolor($color_money) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9)) ///
		(bar prediction cond if cond == 3, sort fcolor($color_bigmoney) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9)) ///
		(rcap hiprediction loprediction cond, col(black) lwidth(medium)), ///
		ytitle("Reporting Rate (%) ", size(medium)) yscale(range(20 80) lcolor(black) lwidth(medium)) ///
		ylabel(20(10)80, labsize(medium) angle(horizontal) notick gmin nogrid) ///
		legend(off) xlabel(1 2 3, valuelabel labsize(medium) noticks labgap(*1.3)) xscale(lstyle(none)) ///
		xtitle("Predicted Reporting Rate" "(Non-Expert Sample)", margin(medlarge) size(vlarge) color(black)) ///
		graphregion(fcolor(white) lcolor(white)) ysize(5) xsize(3.5) name(fig_3_middle, replace)
	
// right panel
snapshot restore 3
collapse (mean) prediction (semean) se = prediction, by(cond) 
generate hiprediction = prediction + se 
generate loprediction = prediction - se
label define labcond  1 "NoMoney" 2 "Money" 3 "BigMoney"
label values cond labcond 
twoway 	(bar prediction cond if cond == 1, sort fcolor($color_nomoney) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9))  ///
		(bar prediction cond if cond == 2, sort fcolor($color_money) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9)) ///
		(bar prediction cond if cond == 3, sort fcolor($color_bigmoney) 	lcolor(white)  lwidth(vvvthin) barwidth(0.9)) ///
		(rcap hiprediction loprediction cond, col(black) lwidth(medium)), ///
		ytitle("Reporting Rate (%) ", size(medium)) yscale(range(20 80) lcolor(black) lwidth(medium)) ///
		ylabel(20(10)80, labsize(medium) angle(horizontal) notick gmin nogrid) ///
		legend(off) xlabel(1 2 3, valuelabel labsize(medium) noticks labgap(*1.3)) xscale(lstyle(none)) ///
		xtitle("Predicted Reporting Rate" "(Expert Sample)", margin(medlarge) size(vlarge) color(black)) ///
		graphregion(fcolor(white) lcolor(white)) ysize(5) xsize(3.5) name(fig_3_right, replace)
			
// combine figure
graph combine fig_3_left fig_3_middle fig_3_right, rows(1) ycommon graphregion(fcolor(white) lcolor(white)) ysize(5) xsize(9)
graph export "../graphs/Fig3.png", replace



** Figure S2: Distribution of response times
** ==========================================

snapshot restore 1
keep if inlist(cond,1,2)
expand 2, gen(avg)
replace country = 100 if avg == 1
bysort country: cumul responsetime, gen(cumresponse)
sort cumresponse
bysort country: gen n = _n
expand 2 if n==1, gen(new)
replace cumresponse = 1 if new == 1
replace responsetime = 100 if new == 1
sort cumresponse
local call
foreach nr of numlist 1(1)40 {
   local call `call' || line cumresponse responsetime if country == `nr', lc(gs14)
}

#delimit ;
twoway 	`call' || line cumresponse responsetime if country == 40, lcolor($color_bigmoney) lw(*1.5) || line cumresponse responsetime if country == 39, lcolor($color_money) lw(*1.5)  ///
		|| line cumresponse responsetime if country == 27, lcolor($color_nomoney) lw(*1.5) || line cumresponse responsetime if country == 35, lcolor($color_bigmoney) lp(dash) /// 
		|| line cumresponse responsetime if country == 100, lcolor($color_money) lp(dash) || line cumresponse responsetime if country == 6, lcolor($color_nomoney) lp(dash) ///
		legend(on region(lcolor(white)) position(3) ring(0) bmargin(zero) size(small) rowgap(.5) ///
			order(41 "USA (21{superscript:th})" 44 "Switzerland (1{superscript:st})" 42 "UK (22{superscript:th})" 45 "AVERAGE" 43 "Poland (6{superscript:th})" 46 "China (40{superscript:th})")) ///
		ylab(, noticks grid glcolor(gs14) angle(horizontal)) yscale(lstyle(none)) ytitle("Cumulative density") ///
		xlab(, noticks) xscale(lstyle(none)) xtitle("Response time in days") ///
		ysize(4) graphregion(fcolor(white) lcolor(white)) name(fig_S2, replace) 
;
#delimit cr	
graph export "../graphs/FigS2.png", replace 



** Figure S5: Correlates of civic honesty
** ==========================================

// values
snapshot restore 1
gen title_geo = .
lab var title_geo "{bf:Geography}"
gen title_culture = .
lab var title_culture "{bf:Culture}"
gen title_instituions = .
lab var title_instituions "{bf:Institutions}"
gen space = .
lab var space " "
generate var_name = ""
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist title_geo c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 space title_culture c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties space title_instituions c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
	quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
	quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
	quietly cap regress response `var' i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
	if _rc == 0 {
		quietly lincom _b[`var']
		quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
		quietly replace stderr = r(se) in `i' // grabs standard error for each variable
		quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
		quietly test `var' = 0
		quietly replace p = r(p) in `i'
	}
	local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(var_name)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// graph
gen var_nr = _N + 1 - _n
replace var_name = var_name + " ("+string(N_countries)+")" if coefficient != .
labmask var_nr, values(var_name)

gen p_pos = 18
gen high = coefficient + stderr
gen low = coefficient - stderr
tostring p_FDR, gen(pvalue) format(%-9.3f) force
replace pvalue = "{it: p = }"+substr(pvalue,1,5) if coefficient != .
levelsof var_nr if var_name != " ", local(var_nrs)
#delimit ;
twoway (scatter var_nr p_pos if p_FDR != ., sort mcolor(none) msize(tiny) mlabel(pvalue) mlabc(black))
	(bar coefficient var_nr if p_FDR <=0.05 , sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(bar coefficient var_nr if p_FDR >0.05, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(rcap high low var_nr, col(black) lwidth(medium) horizontal),
	yscale(noline) ylabel(`var_nrs',valuelabel angle(horizontal) notick nogrid) ytitle("")
	xtitle("Change in reporting rate (%)") xtitle(, color(black)) xscale(range(-17 17)) 
	xscale(lcolor(black) lwidth(medium) line) xlabel(-15(5)15, labcolor(black) noticks nogrid) 
	legend(off) ysize(20) xsize(15) graphregion(fcolor(white) lcolor(white) margin(r+7)) scale(0.75) name(fig_S5, replace);
#delimit cr
graph export "../graphs/FigS5.png", replace 



** Figure S6: Explaning Variation R2
** ==========================================

// generating principal components
snapshot restore 1
collapse c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_GPLS_plurality  c_GPLS_prop_representation c_PIV_exconst c_PIV_years_democracy c_DEEP_adjstatehist c_HIST_prim_educ_1920, by(country)
pca c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9, comp(1)
predict pc_geography, score
label variable pc_geography "Geography"
pca c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest, comp(1) blanks(0.1)
predict pc_culture, score
label variable pc_culture "Culture"
pca c_GPLS_plurality c_GPLS_prop_representation c_PIV_exconst c_PIV_years_democracy c_DEEP_adjstatehist c_HIST_prim_educ_1920, comp(1) blanks(0.1)
predict pc_institutions, score
label variable pc_institutions "Institutions"
keep country pc_*
tempfile dataset_pc
save "`dataset_pc'", replace
snapshot restore 1
merge m:1 country using "`dataset_pc'"
snapshot save

// Examining contribution of each principal component
snapshot restore 4
keep if inlist(cond,0,1)
drop if missing(pc_institutions)
regress response i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
predict dv, residual
collapse dv pc_geography pc_culture pc_institutions,by(country)
sum *
spearman dv pc_geography pc_culture pc_institutions

gen r2_a = .
regress dv pc_geography, robust // PC Geography
display e(r2_a)
gen geo_r2_a = 100*e(r2_a) if _n <= 4
replace r2_a = 100*e(r2_a) if _n == 1
regress dv pc_geography pc_culture, robust // PC Geography + Culture
display e(r2_a)
replace r2_a = 100*e(r2_a) if _n == 2
regress dv pc_geography pc_institutions, robust // PC Geography + Institutions
display e(r2_a)
replace r2_a = 100*e(r2_a) if _n == 3
regress dv pc_geography pc_culture pc_institutions, robust // All factors
display e(r2_a)
replace r2_a = 100*e(r2_a) if _n == 4

* -- Graph
gen model = _n if _n <= 4
label def model 1 "PC Geography" 2 "PC Culture" 3 "PC Institutions" 4 "All factors"    
label values model model 
gen inc_r2_a = r2_a - geo_r2_a
graph hbar geo_r2_a inc_r2_a, stack over(model, axis(lcolor(gs14)) label(labsize(medlarge) noticks labgap(*1.3))) ///
	yscale(range(0 100) lcolor(gs14) lwidth(medium))  ylabel(0(20)100,  noticks glcolor(gs14)) ///
	ytitle("Variance explained, adj. R{superscript:2} (%)", margin(medium) size(medlarge))  ///
	bar(1, fcolor($color_money) lcolor(white) lwidth(vvvthin)) bar(2, fcolor($color_nomoney) lcolor(white) lwidth(vvvthin)) ///
	legend(on label(1 "PC Geography") label(2 "Add. factors") region(lcolor(white)))  graphregion(fcolor(white) lcolor(white)) name(fig_S6, replace)
graph export "../graphs/FigS6.png", replace 



** Figure S7: Correlates: original data only
** ==========================================

snapshot restore 1

// remove manually updated data
foreach var of varlist c_AG_* c_GO_* c_DEEP_* c_HIST_* {
	quietly replace `var' = . if inlist(Country,"Serbia")
} 
foreach var of varlist c_PIV_* {
	quietly replace `var' = . if inlist(Country,"Serbia","Croatia")
} 
foreach var of varlist c_TAB_* {
	quietly replace `var' = . if inlist(Country,"Serbia","Croatia","Morocco")
} 
foreach var of varlist c_LANG_* {
	quietly replace `var' = . if inlist(Country,"Brazil","Morocco","Peru","Serbia","South Africa")
	quietly replace `var' = . if inlist(Country,"Indonesia","Ghana","Kenya","Kazakhstan","India","UAE")
}

// values
gen title_geo = .
lab var title_geo "{bf:Geography}"
gen title_culture = .
lab var title_culture "{bf:Culture}"
gen title_instituions = .
lab var title_instituions "{bf:Institutions}"
gen space = .
lab var space " "
generate var_name = ""
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist title_geo c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 space title_culture c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties space title_instituions c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
	quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
	quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
	quietly cap regress response `var' i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
	if _rc == 0 {
		quietly lincom _b[`var']
		quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
		quietly replace stderr = r(se) in `i' // grabs standard error for each variable
		quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
		quietly test `var' = 0
		quietly replace p = r(p) in `i'
	}
	local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(var_name)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// graph
gen var_nr = _N + 1 - _n
replace var_name = var_name + " ("+string(N_countries)+")" if coefficient != .
labmask var_nr, values(var_name)

gen p_pos = 18
gen high = coefficient + stderr
gen low = coefficient - stderr
tostring p_FDR, gen(pvalue) format(%-9.3f) force
replace pvalue = "{it: p = }"+substr(pvalue,1,5) if coefficient != .
levelsof var_nr if var_name != " ", local(var_nrs)
#delimit ;
twoway (scatter var_nr p_pos if p_FDR != ., sort mcolor(none) msize(tiny) mlabel(pvalue) mlabc(black))
	(bar coefficient var_nr if p_FDR <=0.05 , sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(bar coefficient var_nr if p_FDR >0.05, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(rcap high low var_nr, col(black) lwidth(medium) horizontal),
	yscale(noline) ylabel(`var_nrs',valuelabel angle(horizontal) notick nogrid) ytitle("")
	xtitle("Change in reporting rate (%)") xtitle(, color(black)) xscale(range(-17 17)) 
	xscale(lcolor(black) lwidth(medium) line) xlabel(-15(5)15, labcolor(black) noticks nogrid) 
	legend(off) ysize(20) xsize(15) graphregion(fcolor(white) lcolor(white) margin(r+7)) scale(0.75) name(fig_S7, replace);
#delimit cr
graph export "../graphs/FigS7.png", replace 



** Figure S8: Correlates: NoMoney
** ==========================================

// values
snapshot restore 1
keep if cond == 0
gen title_geo = .
lab var title_geo "{bf:Geography}"
gen title_culture = .
lab var title_culture "{bf:Culture}"
gen title_instituions = .
lab var title_instituions "{bf:Institutions}"
gen space = .
lab var space " "
generate var_name = ""
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist title_geo c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 space title_culture c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties space title_instituions c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
	quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
	quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
	quietly cap regress response `var' i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
	if _rc == 0 {
		quietly lincom _b[`var']
		quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
		quietly replace stderr = r(se) in `i' // grabs standard error for each variable
		quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
		quietly test `var' = 0
		quietly replace p = r(p) in `i'
	}
	local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(var_name)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// graph
gen var_nr = _N + 1 - _n
replace var_name = var_name + " ("+string(N_countries)+")" if coefficient != .
labmask var_nr, values(var_name)

gen p_pos = 18
gen high = coefficient + stderr
gen low = coefficient - stderr
tostring p_FDR, gen(pvalue) format(%-9.3f) force
replace pvalue = "{it: p = }"+substr(pvalue,1,5) if coefficient != .
levelsof var_nr if var_name != " ", local(var_nrs)
#delimit ;
twoway (scatter var_nr p_pos if p_FDR != ., sort mcolor(none) msize(tiny) mlabel(pvalue) mlabc(black))
	(bar coefficient var_nr if p_FDR <=0.05 , sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(bar coefficient var_nr if p_FDR >0.05, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(rcap high low var_nr, col(black) lwidth(medium) horizontal),
	yscale(noline) ylabel(`var_nrs',valuelabel angle(horizontal) notick nogrid) ytitle("")
	xtitle("Change in reporting rate (%)") xtitle(, color(black)) xscale(range(-17 17)) 
	xscale(lcolor(black) lwidth(medium) line) xlabel(-15(5)15, labcolor(black) noticks nogrid) 
	legend(off) ysize(20) xsize(15) graphregion(fcolor(white) lcolor(white) margin(r+7)) scale(0.75) name(fig_S8, replace);
#delimit cr
graph export "../graphs/FigS8.png", replace 



** Figure S9: Correlates: Money
** ==========================================

// values
snapshot restore 1
keep if cond == 0
gen title_geo = .
lab var title_geo "{bf:Geography}"
gen title_culture = .
lab var title_culture "{bf:Culture}"
gen title_instituions = .
lab var title_instituions "{bf:Institutions}"
gen space = .
lab var space " "
generate var_name = ""
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist title_geo c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 space title_culture c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties space title_instituions c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
	quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
	quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
	quietly cap regress response `var' i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
	if _rc == 0 {
		quietly lincom _b[`var']
		quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
		quietly replace stderr = r(se) in `i' // grabs standard error for each variable
		quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
		quietly test `var' = 0
		quietly replace p = r(p) in `i'
	}
	local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(var_name)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// graph
gen var_nr = _N + 1 - _n
replace var_name = var_name + " ("+string(N_countries)+")" if coefficient != .
labmask var_nr, values(var_name)

gen p_pos = 18
gen high = coefficient + stderr
gen low = coefficient - stderr
tostring p_FDR, gen(pvalue) format(%-9.3f) force
replace pvalue = "{it: p = }"+substr(pvalue,1,5) if coefficient != .
levelsof var_nr if var_name != " ", local(var_nrs)
#delimit ;
twoway (scatter var_nr p_pos if p_FDR != ., sort mcolor(none) msize(tiny) mlabel(pvalue) mlabc(black))
	(bar coefficient var_nr if p_FDR <=0.05 , sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(bar coefficient var_nr if p_FDR >0.05, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(rcap high low var_nr, col(black) lwidth(medium) horizontal),
	yscale(noline) ylabel(`var_nrs',valuelabel angle(horizontal) notick nogrid) ytitle("")
	xtitle("Change in reporting rate (%)") xtitle(, color(black)) xscale(range(-17 17)) 
	xscale(lcolor(black) lwidth(medium) line) xlabel(-15(5)15, labcolor(black) noticks nogrid) 
	legend(off) ysize(20) xsize(15) graphregion(fcolor(white) lcolor(white) margin(r+7)) scale(0.75) name(fig_S9, replace);
#delimit cr
graph export "../graphs/FigS9.png", replace 



** Figure S10: Correlates: control. geography
** ==========================================

// values
snapshot restore 4
gen title_geo = .
lab var title_geo "{bf:Geography}"
gen title_culture = .
lab var title_culture "{bf:Culture}"
gen title_instituions = .
lab var title_instituions "{bf:Institutions}"
gen space = .
lab var space " "
generate var_name = ""
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist title_culture c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties space title_instituions c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
	quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
	quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
	quietly cap regress response `var' c.pc_geography i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
	if _rc == 0 {
		quietly lincom _b[`var']
		quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
		quietly replace stderr = r(se) in `i' // grabs standard error for each variable
		quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
		quietly test `var' = 0
		quietly replace p = r(p) in `i'
	}
	local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(var_name)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// graph
gen var_nr = _N + 1 - _n
replace var_name = var_name + " ("+string(N_countries)+")" if coefficient != .
labmask var_nr, values(var_name)

gen p_pos = 18
gen high = coefficient + stderr
gen low = coefficient - stderr
tostring p_FDR, gen(pvalue) format(%-9.3f) force
replace pvalue = "{it: p = }"+substr(pvalue,1,5) if coefficient != .
levelsof var_nr if var_name != " ", local(var_nrs)
#delimit ;
twoway (scatter var_nr p_pos if p_FDR != ., sort mcolor(none) msize(tiny) mlabel(pvalue) mlabc(black))
	(bar coefficient var_nr if p_FDR <=0.05 , sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(bar coefficient var_nr if p_FDR >0.05, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(rcap high low var_nr, col(black) lwidth(medium) horizontal),
	yscale(noline) ylabel(`var_nrs',valuelabel angle(horizontal) notick nogrid) ytitle("")
	xtitle("Change in reporting rate (%)") xtitle(, color(black)) xscale(range(-17 17)) 
	xscale(lcolor(black) lwidth(medium) line) xlabel(-15(5)15, labcolor(black) noticks nogrid) 
	legend(off) ysize(15) xsize(15) graphregion(fcolor(white) lcolor(white) margin(r+7)) scale(0.75) name(fig_S10, replace);
#delimit cr
graph export "../graphs/FigS10.png", replace 



** Figure S11: Correlates: control. for GDP
** ==========================================

// values
snapshot restore 1
gen title_geo = .
lab var title_geo "{bf:Geography}"
gen title_culture = .
lab var title_culture "{bf:Culture}"
gen title_instituions = .
lab var title_instituions "{bf:Institutions}"
gen space = .
lab var space " "
generate var_name = ""
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist title_geo c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 space title_culture c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties space title_instituions c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
	quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
	quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
	quietly cap regress response `var' c_log_gdp i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
	if _rc == 0 {
		quietly lincom _b[`var']
		quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
		quietly replace stderr = r(se) in `i' // grabs standard error for each variable
		quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
		quietly test `var' = 0
		quietly replace p = r(p) in `i'
	}
	local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(var_name)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// graph
gen var_nr = _N + 1 - _n
replace var_name = var_name + " ("+string(N_countries)+")" if coefficient != .
labmask var_nr, values(var_name)

gen p_pos = 18
gen high = coefficient + stderr
gen low = coefficient - stderr
tostring p_FDR, gen(pvalue) format(%-9.3f) force
replace pvalue = "{it: p = }"+substr(pvalue,1,5) if coefficient != .
levelsof var_nr if var_name != " ", local(var_nrs)
#delimit ;
twoway (scatter var_nr p_pos if p_FDR != ., sort mcolor(none) msize(tiny) mlabel(pvalue) mlabc(black))
	(bar coefficient var_nr if p_FDR <=0.05 , sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(bar coefficient var_nr if p_FDR >0.05, sort lcolor($color_nomoney) color($color_nomoney) horizontal barw(0.75))
	(rcap high low var_nr, col(black) lwidth(medium) horizontal),
	yscale(noline) ylabel(`var_nrs',valuelabel angle(horizontal) notick nogrid) ytitle("")
	xtitle("Change in reporting rate (%)") xtitle(, color(black)) xscale(range(-17 17)) 
	xscale(lcolor(black) lwidth(medium) line) xlabel(-15(5)15, labcolor(black) noticks nogrid) 
	legend(off) ysize(20) xsize(15) graphregion(fcolor(white) lcolor(white) margin(r+7)) scale(0.75) name(fig_S11, replace);
#delimit cr
graph export "../graphs/FigS11.png", replace 



** Figure S12: Regression-adjusted Ranking
** ==========================================

snapshot restore 1
keep if inlist(cond,0,1)
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution"
regress response `controls', cluster(country)
predict response_p, residual
qui sum response // adding mean value to the residuals
replace response_p = response_p + r(mean)
separate response_p, by(cond)
collapse (mean) NoMoney = response_p0 Money = response_p1, by(country)
list country NoMoney Money
egen rank = rank(NoMoney), unique
#delimit ;
twoway 	(pcspike rank NoMoney rank Money, lcolor(gs13)) /// creates connecting line
		(scatter rank Money, sort mcolor($color_money) msize(large)) /// creates red markers
		(scatter rank Money if Money <= NoMoney, sort mcolor($color_money) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		(scatter rank NoMoney, sort mcolor($color_nomoney) msize(large)) // creates orange markers
		(scatter rank NoMoney if Money >= NoMoney, sort mcolor($color_nomoney) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		, legend(on rows(2) order(4 "NoMoney" 2 "Money") region(lcolor(white)) position(11) ring(0))  /// legend
		yscale(off) yscale(range(0.5 41)) yscale(noline) ylabel(none) xtitle("Reporting rate, regression adjusted (%)", color(black)) /// axis
		xscale(range(-10 100) lcolor(black) lwidth(medium) line) xlabel(0(10)100, labcolor(black) noticks nogrid) 
		scale(0.6) graphregion(fcolor(white) lcolor(white)) ysize(4) xsize(4) name(fig_S12, replace) /// misc
;
#delimit cr 
graph export "../graphs/FigS12.png", replace 



** Figure S13: Country ranking for hotels
** ==========================================

snapshot restore 1
keep if inlist(cond,0,1) & hotel == 1
separate response, by(cond)
collapse (mean) NoMoney = response0 Money = response1, by(country)
egen rank = rank(NoMoney), unique
#delimit ;
twoway 	(pcspike rank NoMoney rank Money, lcolor(gs13)) /// creates connecting line
		(scatter rank Money, sort mcolor($color_money) msize(large)) /// creates red markers
		(scatter rank Money if Money <= NoMoney, sort mcolor($color_money) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		(scatter rank NoMoney, sort mcolor($color_nomoney) msize(large)) // creates orange markers
		(scatter rank NoMoney if Money >= NoMoney, sort mcolor($color_nomoney) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		, legend(on rows(2) order(4 "NoMoney" 2 "Money") region(lcolor(white)) position(11) ring(0))  /// legend
		yscale(off) yscale(range(0.5 41)) yscale(noline) ylabel(none) xtitle("Reporting rate (%)", color(black)) /// axis
		xscale(range(-10 100) lcolor(black) lwidth(medium) line) xlabel(0(10)100, labcolor(black) noticks nogrid) 
		scale(0.6) graphregion(fcolor(white) lcolor(white)) ysize(4) xsize(4) name(fig_S13, replace) /// misc
;
#delimit cr 
graph export "../graphs/FigS13.png", replace 



** Figure S14: Reg. Adj. Ranking: Email Usage
** ==========================================

snapshot restore 1
keep if inlist(cond,0,1)
drop if missing(c_GES_email)
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution"
regress response c_GES_email `controls', cluster(country)
predict response_p, residual
qui sum response // adding mean value to the residuals
replace response_p = response_p + r(mean)
separate response_p, by(cond)
collapse (mean) NoMoney = response_p0 Money = response_p1, by(country)
list country NoMoney Money
egen rank = rank(NoMoney), unique
#delimit ;
twoway 	(pcspike rank NoMoney rank Money, lcolor(gs13)) /// creates connecting line
		(scatter rank Money, sort mcolor($color_money) msize(large)) /// creates red markers
		(scatter rank Money if Money <= NoMoney, sort mcolor($color_money) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		(scatter rank NoMoney, sort mcolor($color_nomoney) msize(large)) // creates orange markers
		(scatter rank NoMoney if Money >= NoMoney, sort mcolor($color_nomoney) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		, legend(on rows(2) order(4 "NoMoney" 2 "Money") region(lcolor(white)) position(11) ring(0))  /// legend
		yscale(off) yscale(range(0.5 28)) yscale(noline) ylabel(none) xtitle("Reporting rate, regression adjusted (%)", color(black)) /// axis
		xscale(range(-10 100) lcolor(black) lwidth(medium) line) xlabel(0(10)100, labcolor(black) noticks nogrid) 
		scale(0.6) graphregion(fcolor(white) lcolor(white)) ysize(4) xsize(4) name(fig_S14, replace) /// misc
;
#delimit cr 
graph export "../graphs/FigS14.png", replace 



** Figure S15: Reg. Adj. Ranking: Country GDP
** ==========================================

snapshot restore 1
keep if inlist(cond,0,1)
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution"
regress response c_log_gdp `controls', cluster(country)
predict response_p, residual
qui sum response // adding mean value to the residuals
replace response_p = response_p + r(mean)
separate response_p, by(cond)
collapse (mean) NoMoney = response_p0 Money = response_p1, by(country)
list country NoMoney Money
egen rank = rank(NoMoney), unique
#delimit ;
twoway 	(pcspike rank NoMoney rank Money, lcolor(gs13)) /// creates connecting line
		(scatter rank Money, sort mcolor($color_money) msize(large)) /// creates red markers
		(scatter rank Money if Money <= NoMoney, sort mcolor($color_money) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		(scatter rank NoMoney, sort mcolor($color_nomoney) msize(large)) // creates orange markers
		(scatter rank NoMoney if Money >= NoMoney, sort mcolor($color_nomoney) mlabcolor(black) msize(large) mlabel(country) mlabsize(medsmall) mlabposition(9) mlabgap(small)) 
		, legend(on rows(2) order(4 "NoMoney" 2 "Money") region(lcolor(white)) position(11) ring(0))  /// legend
		yscale(off) yscale(range(0.5 41)) yscale(noline) ylabel(none) xtitle("Reporting rate, regression adjusted (%)", color(black)) /// axis
		xscale(range(-10 100) lcolor(black) lwidth(medium) line) xlabel(0(10)100, labcolor(black) noticks nogrid) 
		scale(0.6) graphregion(fcolor(white) lcolor(white)) ysize(4) xsize(4) name(fig_S15, replace) /// misc
;
#delimit cr 
graph export "../graphs/FigS15.png", replace 
