** =============================================================================
** Civic Honesty Around the Globe
** =============================================================================

** Set Bootstrap Replications (Mediation)
** ==========================================
global bootreps 10000 // Set to 100 to reduce run time

** Loading and Preparing Data
** ==========================================
version 14.2
snapshot erase _all

// primary data set
use "../data/behavioral data.dta", clear 
snapshot save

// wallet rejection data
use "../data/rejection data.dta", clear 
snapshot save

// wallet pickup data
use "../data/pickup data.dta", clear 
snapshot save

// survey experiment data
use "../data/survey data.dta", clear 
snapshot save

// non-expert prediction data
use "../data/non-expert data.dta", clear
snapshot save // snapshot in wide format
generate id = _n
reshape long prediction selfinterest altruism theftaversion, i(id) j(cond)
label define condl 1 "No Money" 2 "Money" 3 "Big Money"
label val cond condl
snapshot save // snapshot in long format

// expert prediction data
use "../data/expert data.dta", clear
snapshot save // snapshot in wide format
generate id = _n
reshape long prediction, i(id) j(cond)
label define condl 1 "No Money" 2 "Money" 3 "Big Money"
label val cond condl
snapshot save // snapshot in long format



** =============================================================================
** Manuscript
** =============================================================================


** Behavioral Field Experiment
** ==========================================
// No Money vs Money: Main result
snapshot restore 1
keep if inlist(cond,0,1)
replace response = 1 if response == 100
prtest response, by(cond)

// No Money vs Money: Result for each country
snapshot restore 1
keep if inlist(cond,0,1)
replace response = 1 if response == 100
statsby NoMoney=r(P_1) Money=r(P_2) Z=r(z), by(country) clear: prtest response, by(cond)
generate P_value = (1 - normal(abs(Z))) * 2
list

// No Money vs Money vs Big Money vs No-Key
snapshot restore 1
keep if inlist(Country,"Poland","UK","USA")
table cond, c(mean response)
replace response = 1 if response == 100
prtest response if inlist(cond,0,1), by(cond)
prtest response if inlist(cond,0,2), by(cond)
prtest response if inlist(cond,1,2), by(cond)

** Non-expert Predictions
** ==========================================
// Predictions by incentive level
snapshot restore 6
table cond, c(mean prediction sd prediction)

// Predictions compared to actual changes across conditions
snapshot restore 6
regress prediction i.cond, cluster(id)
lincom [2.cond] - (57.33333 - 38.66667)
lincom [3.cond] - (65.50000 - 38.66667)
lincom ([3.cond] - [2.cond]) - (65.50000 - 57.33333)

// Sign test
snapshot restore 5
generate trend = .
replace trend = 1 if (prediction1 >= prediction2) & (prediction2 >= prediction3)
replace trend = -1 if (prediction1 <= prediction2) & (prediction2 <= prediction3)
replace trend = 0 if (prediction1 == prediction2) & (prediction2 == prediction3)
replace trend = 0 if missing(trend)
tabulate trend
signtest trend = 0

** Expert Predictions
** ==========================================
// Predictions by incentive level
snapshot restore 8
table cond, c(mean prediction sd prediction)

// Predictions compared to actual changes across conditions
snapshot restore 8
regress prediction i.cond, cluster(id)
lincom [2.cond] - (57.33333 - 38.66667)
lincom [3.cond] - (65.50000 - 38.66667)
lincom ([3.cond] - [2.cond]) - (65.50000 - 57.33333)

// Sign test
snapshot restore 7
generate trend = .
replace trend = 1 if (prediction1 >= prediction2) & (prediction2 >= prediction3)
replace trend = -1 if (prediction1 <= prediction2) & (prediction2 <= prediction3)
replace trend = 0 if (prediction1 == prediction2) & (prediction2 == prediction3)
replace trend = 0 if missing(trend)
tabulate trend
signtest trend = 0



** =============================================================================
** Materials and Methods: Experimental Design
** =============================================================================

** Lost Wallet Experiments
** ==========================================
// Number of observations
snapshot restore 1
tabulate response

// Number of cities
snapshot restore 1
codebook city

// Number of countries
snapshot restore 1
codebook country

// Number of cities per country
snapshot restore 1
collapse (count) id, by(city country)
collapse (count) n_city = id (sum) N = id, by(country)
table country, c(mean n_city mean N)
summarize n_city, detail

// Fraction of drop-offs per institution type
snapshot restore 1
tabulate institution

// Footnote 2: Black and white names
snapshot restore 1
tabulate response name_owner if Country == "South Africa", chi2

// Footnote 5: Fraction of recipients who refused to accept wallet
snapshot restore 1
collapse rejection_frac, by(country)
summarize rejection_frac, detail

// Footnote 5: Descriptive statistics of Wallet Rejections
snapshot restore 2
collapse rejection, by(country)
summarize rejection, detail
table country, c(mean rejection) format(%9.2f)

// Footnote 5: Pair-wise comparison of rejection rates
snapshot restore 2
keep if inlist(cond,1,2)
tempfile p_values
postfile post float c1 c2 p using "`p_values'", replace
quietly levelsof country, local(countries)
foreach c1 of local countries {
  foreach c2 of numlist `c1'(1)40 {
    quietly cap tabulate rejection country if inlist(country,`c1',`c2'), chi2
    post post (`c1') (`c2') (r(p))
  }
}
postclose post
use `p_values', replace
qqvalue p, method(simes) qvalue(p_FDR)
quietly drop if c1 == c2
quietly generate significant_FDR = (p_FDR < 0.05)
summarize significant_FDR

// Descriptive statistics of Response Times
snapshot restore 1
generate minutes = (responsetime * 60 * 24) // days to minutes
summarize minutes, detail
generate within_1day = (responsetime < 1) if ~missing(responsetime)
tabulate within_1day

// Correlation between response rate and response times
snapshot restore 1
collapse response responsetime, by(country)
pwcorr response responsetime, sig
spearman response responsetime

// Pair-wise comparison of response times
snapshot restore 1
tempfile p_values
postfile post float c1 c2 p using "`p_values'", replace
quietly levelsof country, local(countries)
foreach c1 of local countries {
  foreach c2 of numlist `c1'(1)40 {
    cap ttest responsetime if inlist(country,`c1',`c2'), by(country)
    post post (`c1') (`c2') (r(p))
  }
}
postclose post
use `p_values', replace
qqvalue p, method(simes) qvalue(p_FDR)
quietly drop if c1 == c2
quietly generate significant_FDR = (p_FDR < 0.05)
summarize significant_FDR

// Table S2: Randomization Check for UK
snapshot restore 1
keep if Country == "UK" 
keep cond above40 male computer other_bystanders coworkers hotel bank cultural public postal local_recipient no_english busy
order cond above40 male computer other_bystanders coworkers hotel bank cultural public postal local_recipient no_english busy
tabstat above40-busy, by(cond) stat(mean sd) col(stat) format(%9.3f)
foreach var of varlist above40-no_english {
  tabulate `var' cond, chi
}
kwallis busy, by(cond)

// Table S3: Randomization Check for Poland
snapshot restore 1
keep if Country == "Poland"
keep cond above40 male computer other_bystanders coworkers hotel bank cultural public postal local_recipient no_english busy
order cond above40 male computer other_bystanders coworkers hotel bank cultural public postal local_recipient no_english busy
tabstat above40-busy, by(cond) stat(mean sd) col(stat) format(%9.3f)
foreach var of varlist above40-no_english {
  tabulate `var' cond, chi
}
kwallis busy, by(cond)

// Table S4: Randomization Check for USA
snapshot restore 1
keep if Country == "USA"
keep cond above40 male computer other_bystanders coworkers hotel bank cultural public postal security_cam security_guard local_recipient no_english understood_situation busy
order cond above40 male computer other_bystanders coworkers hotel bank cultural public postal security_cam security_guard local_recipient no_english understood_situation busy
tabstat above40-busy, by(cond) stat(mean sd) col(stat) format(%9.3f)
foreach var of varlist above40-no_english {
  tabulate `var' cond, chi
}
kwallis understood_situation, by(cond)
kwallis busy, by(cond)

// Table S5: Randomization Check for Global Data
snapshot restore 1
keep if inlist(cond,0,1)
keep cond above40 male computer other_bystanders coworkers hotel bank cultural public postal security_cam security_guard local_recipient no_english understood_situation busy
order cond above40 male computer other_bystanders coworkers hotel bank cultural public postal security_cam security_guard local_recipient no_english understood_situation busy
tabstat above40-busy, by(cond) stat(mean sd) col(stat) format(%9.3f)
foreach var of varlist above40-busy {
  tabulate `var' cond, chi
}
kwallis understood_situation, by(cond)
kwallis busy, by(cond)

// Table S6: Analysis of Wallet Rejections
snapshot restore 2
regress rejection i.city i.institution i.cond if inlist(cond,0,1), robust // column 1
regress rejection i.city i.institution i.cond if inlist(Country,"Poland","UK","USA"), robust // column 2
margins cond, pwcompare(effects)
testparm i.cond

// Table S7: Analysis of Response Times
snapshot restore 1
regress responsetime i.city i.institution i.cond if inlist(cond,0,1), robust // column 1
regress responsetime i.city i.institution i.cond if inlist(Country,"Poland","UK","USA"), robust // column 2
margins cond, pwcompare(effects)
testparm i.cond

** Prediction Study: Non-Expert Sample
** ==========================================
// Experimental Design: Demographics
snapshot restore 5
tabulate male
summarize age
tabulate ethnicity
tabulate education
tabulate employment
tabulate income

** Prediction Study: Expert Sample
** ==========================================
// Experimental Design: Demographics
snapshot restore 7
tabulate male
summarize age
tabulate rank



** =============================================================================
** Supplementary Text 2: Additional Results
** =============================================================================

** Behavioral Data from Lost Wallet Experiments
** ==========================================
// No Money vs Money: Main result
snapshot restore 1
keep if inlist(cond,0,1)
replace response = 1 if response == 100
prtest response, by(cond)

// No Money vs Money: Result for each country
snapshot restore 1
keep if inlist(cond,0,1)
replace response = 1 if response == 100
statsby NoMoney=r(P_1) Money=r(P_2) Z=r(z), by(country) clear: prtest response, by(cond)
generate P_value = (1 - normal(abs(Z))) * 2
list
qqvalue P_value, method(simes) qvalue(p_FDR)
quietly generate significant_FDR = (p_FDR <= 0.05)
tabulate significant_FDR

// Table S8: Reporting rates and situational characteristics
snapshot restore 1
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
regress response i.city i.institution i.cond, robust // column 1
regress response i.city i.institution `controls' i.cond, robust // column 2

// No Money vs Money vs Big Money vs No-Key
snapshot restore 1
keep if inlist(Country,"Poland","UK","USA")
table cond, c(mean response)
replace response = 1 if response == 100
prtest response if inlist(cond,0,1), by(cond)
prtest response if inlist(cond,0,2), by(cond)
prtest response if inlist(cond,1,2), by(cond)

// Table S9: Reporting Rates in NoMoney, Money, and BigMoney condition
snapshot restore 1
keep if inlist(Country,"Poland","UK","USA")
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
regress response i.city i.institution `controls' i.cond, robust // column 1
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond if country == 39, robust // column 2 
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond if country == 27, robust // column 3 
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond if country == 40, robust // column 4 
lincom [2.cond] - [1.cond]

// Footnote 8: Altruism Manipulation Check
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
keep if inlist(treatment,1,2)
ttest motive_harm, by(treatment)
esize twosample motive_harm, by(treatment)
ranksum motive_harm, by(treatment)

// Footnote 8: Altruism Manipulation Check - Examining each country separately
ttest motive_harm if country == "UK", by(treatment)
ttest motive_harm if country == "Poland", by(treatment)
ttest motive_harm if country == "USA", by(treatment)
ranksum motive_harm if country == "UK", by(treatment)
ranksum motive_harm if country == "Poland", by(treatment)
ranksum motive_harm if country == "USA", by(treatment)

// Table S10: Reporting Rates in Money-NoKey condition
snapshot restore 1
keep if inlist(Country,"Poland","UK","USA")
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
regress response i.city i.institution `controls' ib1.cond, robust // column 1
regress response i.city i.institution `controls' ib1.cond if country == 39, robust // column 2
regress response i.city i.institution `controls' ib1.cond if country == 27, robust // column 3
regress response i.city i.institution `controls' ib1.cond if country == 40, robust // column 4

** Nationally Representative Survey Data
** ==========================================
// Table S11: Survey Responses across Experimental Conditions
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
regress motive_selfimage i.categorynum i.countryFE ib3.treatment, robust // column 1
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]
regress contact_prob_own i.categorynum i.countryFE ib3.treatment, robust // column 2
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]
regress contact_prob_own motive_selfimage i.categorynum i.countryFE ib3.treatment, robust // column 3
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]
regress contact_prob_own motive_selfimage motive_harm motive_detection i.categorynum i.countryFE ib3.treatment, robust // column 4
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]

// Table S12: Survey Responses across Experimental Conditions (Full Sample)
snapshot restore 4
regress motive_selfimage i.categorynum i.countryFE ib3.treatment, robust // column 1
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]
regress contact_prob_own i.categorynum i.countryFE ib3.treatment, robust // column 2
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]
regress contact_prob_own motive_selfimage i.categorynum i.countryFE ib3.treatment, robust // column 3
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]
regress contact_prob_own motive_selfimage motive_harm motive_detection i.categorynum i.countryFE ib3.treatment, robust // column 4
lincom [4.treatment] - [1.treatment]
lincom [1.treatment] - [2.treatment]

// Optimistic self-reported reporting rate
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
table country, c(mean contact_prob_own)

// Mediation test I
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
generate cond = .
replace cond = 0 if treatment == 3
replace cond = 1 if treatment == 1
replace cond = 2 if treatment == 4
rename contact_prob_own dv
rename motive_selfimage mv
tabulate cond, gen(cond)
quietly compress
capture program drop mediation1
program mediation1, rclass
  sem (mv <- cond2 cond3) (dv <- mv cond2 cond3)
  estat teffects
  matrix bi = r(indirect)
  matrix bd = r(direct)
  return scalar indirect1 = el(bi,1,4)
  return scalar direct1 = el(bd,1,4)
  return scalar indirect2 = el(bi,1,5)
  return scalar direct2 = el(bd,1,5)
end
set seed 987654321
bootstrap r(indirect1) r(direct1) r(indirect2) r(direct2), reps($bootreps) nodots: mediation1
estat boot

// Mediation test II
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
generate cond = .
replace cond = 0 if treatment == 1
replace cond = 1 if treatment == 2
rename contact_prob_own dv
rename motive_selfimage mv1
rename motive_harm mv2
quietly compress
capture program drop mediation2
program mediation2, rclass
  sem (mv1 <- cond) (mv2 <- cond) (dv <- mv1 mv2 cond)
  return scalar indirect1 = _b[mv1:cond]*_b[dv:mv1]
  return scalar indirect2 = _b[mv2:cond]*_b[dv:mv2]
  return scalar direct = _b[dv:cond]
end
set seed 987654321
bootstrap r(indirect1) r(indirect2) r(direct), reps($bootreps) nodots: mediation2
estat boot

** Prediction Data: Non-expert Sample
** ==========================================
// Descriptive statistics
snapshot restore 6
table cond, c(mean prediction sd prediction p50 prediction) format(%9.2f)

// Predictions by incentive level
snapshot restore 6
regress prediction i.cond, cluster(id)
lincom [3.cond] - [2.cond]

// Table S13
snapshot restore 6
eststo m1: regress prediction i.cond, cluster(id)
lincom [3.cond] - [2.cond]
eststo m2: regress prediction selfinterest i.cond, cluster(id)
lincom [3.cond] - [2.cond]
eststo m3: regress prediction altruism i.cond, cluster(id)
lincom [3.cond] - [2.cond]
eststo m4: regress prediction theftaversion i.cond, cluster(id)
lincom [3.cond] - [2.cond]

// Pattern
snapshot restore 5
generate pattern = "non-monotonic"
replace pattern = "decreasing" if (prediction1 >= prediction2) & (prediction2 >= prediction3)
replace pattern = "increasing" if (prediction1 <= prediction2) & (prediction2 <= prediction3)
replace pattern = "flat" if (prediction1 == prediction2) & (prediction2 == prediction3)
tabulate pattern

// Descriptive statistics
snapshot restore 6
table cond, c(mean prediction mean selfinterest mean altruism mean theftaversion) format(%9.2f)

// Effect sizes prediction
snapshot restore 5
generate p_diff1 = prediction1 - prediction2
summarize p_diff1
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'

generate p_diff2 = prediction1 - prediction3
summarize p_diff2
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'

// Predicted motivations
snapshot restore 6
regress selfinterest i.cond, cluster(id)
lincom [3.cond] - [2.cond]
regress altruism i.cond, cluster(id)
lincom [3.cond] - [2.cond]
regress theftaversion i.cond, cluster(id)
lincom [3.cond] - [2.cond]

// Effect size - self-interest
snapshot restore 5
generate si_diff1 = selfinterest2 - selfinterest1
summarize si_diff1
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'
generate si_diff2 = selfinterest3 - selfinterest1
summarize si_diff2
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'

// Effect size - altruism
snapshot restore 5
generate a_diff1 = altruism1 - altruism2
summarize a_diff1
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'
generate a_diff2 = altruism1 - altruism3
summarize a_diff2
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'

// Effect size - theft aversion
snapshot restore 5
generate ta_diff1 = theftaversion2 - theftaversion1
summarize ta_diff1
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'
generate ta_diff2 = theftaversion3 - theftaversion1
summarize ta_diff2
display "Within-subject Cohen's d = " `r(mean)'/`r(sd)'

// Correlation between items
snapshot restore 6
pwcorr selfinterest altruism theftaversion

// Mediation tests
snapshot restore 6
tabulate cond, gen(cond)
quietly compress
capture program drop mediation1
program mediation1, rclass
  sem (selfinterest <- cond2 cond3) (prediction <- selfinterest cond2 cond3)
  estat teffects
  matrix bi = r(indirect)
  matrix bd = r(direct)
  return scalar indirect1 = el(bi,1,4)
  return scalar direct1 = el(bd,1,4)
  return scalar indirect2 = el(bi,1,5)
  return scalar direct2 = el(bd,1,5)
end
set seed 987654321
bootstrap r(indirect1) r(direct1) r(indirect2) r(direct2), cluster(id) reps($bootreps) nodots: mediation1
estat boot

capture program drop mediation2
program mediation2, rclass
  sem (altruism <- cond2 cond3) (prediction <- altruism cond2 cond3)
  estat teffects
  matrix bi = r(indirect)
  matrix bd = r(direct)
  return scalar indirect1 = el(bi,1,4)
  return scalar direct1 = el(bd,1,4)
  return scalar indirect2 = el(bi,1,5)
  return scalar direct2 = el(bd,1,5)
end
set seed 987654321
bootstrap r(indirect1) r(direct1) r(indirect2) r(direct2), cluster(id) reps($bootreps) nodots: mediation2
estat boot

capture program drop mediation3
program mediation3, rclass
  sem (theftaversion <- cond2 cond3) (prediction <- theftaversion cond2 cond3)
  estat teffects
  matrix bi = r(indirect)
  matrix bd = r(direct)
  return scalar indirect1 = el(bi,1,4)
  return scalar direct1 = el(bd,1,4)
  return scalar indirect2 = el(bi,1,5)
  return scalar direct2 = el(bd,1,5)
end
set seed 987654321
bootstrap r(indirect1) r(direct1) r(indirect2) r(direct2), cluster(id) reps($bootreps) nodots: mediation3
estat boot

// Prediction weights of each motivation item
snapshot restore 6
regress prediction i.cond, cluster(id)
regress prediction selfinterest i.cond, cluster(id)
regress prediction altruism i.cond, cluster(id)
regress prediction theftaversion i.cond, cluster(id)

** Prediction Data: Expert Sample
** ==========================================
// Descriptive statistics
snapshot restore 8
table cond, c(mean prediction sd prediction p50 prediction) format(%9.2f)

// Predictions by incentive level
snapshot restore 8
regress prediction i.cond, cluster(id)
lincom [3.cond] - [2.cond]

// Predictions compared to actual changes across conditions
snapshot restore 8
regress prediction i.cond, cluster(id)
lincom [2.cond] - (57.33333 - 38.66667)
lincom [3.cond] - (65.50000 - 38.66667)
lincom ([3.cond] - [2.cond]) - (65.50000 - 57.33333)

// Pattern
snapshot restore 7
generate pattern = "non-monotonic"
replace pattern = "decreasing" if (prediction1 >= prediction2) & (prediction2 >= prediction3)
replace pattern = "increasing" if (prediction1 <= prediction2) & (prediction2 <= prediction3)
replace pattern = "flat" if (prediction1 == prediction2) & (prediction2 == prediction3)
tabulate pattern

** Cross-country Correlates of Civic Honesty
** ==========================================
// Rank-order correlation between NoMoney and Money conditions
snapshot restore 1
keep if inlist(cond,0,1)
separate response, by(cond)
collapse response0 response1, by(country)
spearman response0 response1

// Correlates of Civic Honesty
snapshot restore 1
generate var_name = "."
generate coefficient = .
generate stderr = .
generate dof = .
generate p = .
local i = 1
foreach var of varlist c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920 {
  quietly ereplace `var' = std(`var') // standardizing variable to have mean of 0 and SD of 1
  quietly regress response `var' i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
  quietly lincom _b[`var']
  quietly replace coefficient = r(estimate) in `i' // grabs coefficient for each variable
  quietly replace stderr = r(se) in `i' // grabs standard error for each variable
  quietly replace dof = r(df) in `i' // grabs degree of freedom for each variable
  quietly replace var_name = "`:var l `var''" in `i' // grabs label for each variable
  quietly test `var' = 0
  quietly replace p = r(p) in `i'
  local `i++'
}
keep var_name coefficient stderr dof p
drop if missing(coefficient)
generate N_countries = dof + 1 // number of countries included in the regression
qqvalue p, method(simes) qvalue(p_FDR) // generating p-values which adjust for False Discovery Rate
list var_name coefficient stderr N_countries p_FDR

// Explaining Cross-country Variation
* -- Generating principal components
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

* -- Examining contribution of each principal component
snapshot restore 9
keep if inlist(cond,0,1)
drop if missing(pc_institutions)
regress response i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
predict dv, residual
collapse dv pc_geography pc_culture pc_institutions,by(country)
summarize *
spearman dv pc_geography pc_culture pc_institutions

generate r2_a = .
regress dv pc_geography, robust // PC Geography
display e(r2_a)
regress dv pc_geography pc_culture, robust // PC Geography + Culture
display e(r2_a)
regress dv pc_geography pc_institutions, robust // PC Geography + Institutions
display e(r2_a)
regress dv pc_geography pc_culture pc_institutions, robust // All factors
display e(r2_a)

// Footnote 18: Explaining Cross-country Variation
* -- Generating principal components
snapshot restore 1
collapse c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9 c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920, by(country)
pca c_AG_ln_soilsuit c_AG_ln_abslat c_AG_distcr c_AG_temp c_AG_precip c_AG_elevavg c_AG_rough c_GO_tmpvolatilitymean_aa c_GO_prevolatilitymean_aa c_PAPR_fitted_pathogen_ms_9, comp(1)
predict pc_geography, score
label variable pc_geography "Geography"
pca c_TAB_pronoun_drop c_TAB_pronoun_diff c_LANG_weak_FTR c_AG_pprotest c_WVS_familyties, comp(1) blanks(0.1)
predict pc_culture, score
label variable pc_culture "Culture"
pca c_DEEP_adjstatehist c_PIV_years_democracy c_PIV_exconst c_GPLS_judicial_independence c_GPLS_constitutional_review c_GPLS_plurality c_GPLS_prop_representation c_HIST_prim_educ_1920, comp(1) blanks(0.1)
predict pc_institutions, score
label variable pc_institutions "Institutions"
keep country pc_*
tempfile dataset_pc
save "`dataset_pc'", replace
snapshot restore 1
merge m:1 country using "`dataset_pc'"
snapshot save

// Examining contribution of each principal component
snapshot restore 10
keep if inlist(cond,0,1)
drop if missing(pc_institutions)
drop if missing(pc_culture)
regress response i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution i.cond, cluster(country)
predict dv, residual
collapse dv pc_geography pc_culture pc_institutions,by(country)
summarize *
spearman dv pc_geography pc_culture pc_institutions

generate r2_a = .
regress dv pc_geography, robust // PC Geography
display e(r2_a)
regress dv pc_geography pc_culture, robust // PC Geography + Culture
display e(r2_a)
regress dv pc_geography pc_institutions, robust // PC Geography + Institutions
display e(r2_a)
regress dv pc_geography pc_culture pc_institutions, robust // All factors
display e(r2_a)



** =============================================================================
** Supplementary Text 3: Alternative Explanations
** =============================================================================

** Fear of Punishment
** ==========================================
// Footnote 19: Fear of Punishment
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
regress motive_detection i.categorynum i.countryFE ib3.treatment, robust
lincom [4.treatment] - [1.treatment]

// Table S14: Fear of Punishment - Civic Honesty and Lost Property Law
snapshot restore 1
keep if Country == "USA"
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
regress response ib141.city i.institution `controls' i.cond if r_PL_propertylaw == 0, robust
lincom [2.cond] - [1.cond]
regress response ib141.city i.institution `controls' i.cond if r_PL_propertylaw == 1, robust
lincom [2.cond] - [1.cond]
eststo m1: quietly regress response ib141.city i.institution `controls' i.cond if r_PL_propertylaw == 0
eststo m2: quietly regress response ib141.city i.institution `controls' i.cond if r_PL_propertylaw == 1
quietly suest m1 m2, robust
test [m1_mean = m2_mean]:1.cond
test [m1_mean = m2_mean]:2.cond

// Table S15: Fear of Punishment - Civic Honesty and Presence of Security Cameras
snapshot restore 1
set matsize 1000
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
regress response ib141.city i.institution `controls' i.security_cam i.cond, robust
regress response ib141.city i.institution `controls' i.cond if security_cam == 0, robust
regress response ib141.city i.institution `controls' i.cond if security_cam == 1, robust
eststo m1: quietly regress response ib141.city i.institution `controls' i.cond if security_cam == 0
eststo m2: quietly regress response ib141.city i.institution `controls' i.cond if security_cam == 1
quietly suest m1 m2, robust
test [m1_mean = m2_mean]:1.cond

// Table S16: Fear of Punishment - Civic Honesty and Social Monitoring
snapshot restore 1
local controls  = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
regress response i.city i.institution `controls' i.cond, robust
regress response i.city i.institution `controls' i.cond if coworkers == 0, robust
regress response i.city i.institution `controls' i.cond if other_bystanders == 0, robust
regress response i.city i.institution `controls' i.cond if coworkers == 0 & other_bystanders == 0, robust

scalar drop _all
quietly regress response i.cond i.city i.institution `controls', robust
quietly lincom [1.cond]
scalar b1 = r(estimate)
quietly test 1.cond, matvlc(var1)
quietly regress response i.cond i.city i.institution `controls' if coworkers == 0, robust
quietly lincom [1.cond]
scalar b2 = r(estimate)
quietly test 1.cond, matvlc(var2)
quietly regress response i.cond i.city i.institution `controls' if other_bystanders == 0, robust
quietly lincom [1.cond]
scalar b3 = r(estimate)
quietly test 1.cond, matvlc(var3)
quietly regress response i.cond i.city i.institution `controls' if coworkers == 0 & other_bystanders == 0, robust
quietly lincom [1.cond]
scalar b4 = r(estimate)
quietly test 1.cond, matvlc(var4)
scalar compare_12 = (b1 - b2)^2/(var1[1,1] + var2[1,1])
display "Model 1 vs Model 2: Chi2(1), P =", chi2tail(1,compare_12)
scalar compare_13 = (b1 - b3)^2/(var1[1,1] + var3[1,1])
display "Model 1 vs Model 3: Chi2(1), P =", chi2tail(1,compare_13)
scalar compare_14 = (b1 - b4)^2/(var1[1,1] + var4[1,1])
display "Model 1 vs Model 4: Chi2(1), P =", chi2tail(1,compare_14)

** Returning the Wallet but Pocketing the Money
** ==========================================
// Money recovered from collected wallets
snapshot restore 3
summarize amount_ratio if country=="Switzerland"
summarize amount_ratio if country=="Czech Republic"
ranksum amount_ratio, by(country)

** Finder's Fees
** ==========================================
// Finder's Fees: Descriptive statistics
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
generate no_reward = (exp_reward_USD == 0)
tabulate no_reward
table treatment, c(median exp_reward_USD)

// Table S17: Civic Honesty and Beliefs about Finder’s Fees
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
generate reward0 = (exp_reward_USD == 0)
tabulate reward0
regress exp_reward_USD midmoney nokey highmoney i.categorynum i.countryFE, robust
lincom [highmoney] - [midmoney]
lincom [nokey] - [midmoney]
regress contact_prob_own exp_reward_USD midmoney nokey highmoney i.categorynum i.countryFE, robust
lincom [highmoney] - [midmoney]
lincom [nokey] - [midmoney]

// Footnote 23: Presence of cash as upper bound for reward
snapshot restore 4
keep if RESTRICTED_SAMPLE == 1
generate AboveMoney = 0 // creating proportion of response above amount in Money condition
replace AboveMoney = (reward > 13.45) if country == "USA"
replace AboveMoney = (reward > 8.5) if country == "UK"
replace AboveMoney = (reward > 25) if country == "Poland"
label var AboveMoney "Expected Reward > MidMoney"
table treatment, c(mean AboveMoney)
prtest AboveMoney if inlist(treatment,1,3), by(treatment) // NoMoney vs Money
bysort country: prtest AboveMoney if inlist(treatment,1,3), by(treatment) // NoMoney vs Money (by country)
sdtest reward_USD if inlist(treatment,1,3), by(treatment) // equality of variances: NoMoney vs Money
sdtest reward_USD if inlist(treatment,3,4), by(treatment) // equality of variances: NoMoney vs BigMoney
sdtest reward_USD if inlist(treatment,2,3), by(treatment) // equality of variances: NoMoney vs Money-NoKey



** =============================================================================
** Supplementary Text 4: Robustness Checks
** =============================================================================

** Individual and Situational Factors
** ==========================================
// Individual and Situational Factors: Rank order correlation between regression adjusted and original ranking
snapshot restore 1
keep if inlist(cond,0,1)
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution"
regress response `controls', cluster(country)
predict response_p, residual
collapse response response_p, by(country cond) 
spearman response response_p if cond == 0
spearman response response_p if cond == 1


** Experimenter effects
** ==========================================
// Table S18: Experimenter effects - Drop-offs by Experimenters and Country: Overlaps
snapshot restore 1
table experimenter country, c(n response) format(%9.2f)
snapshot restore 1 // generating overlaps
collapse (count) exp = response (firstnm) country, by(experimenter city)
reshape wide exp, i(city) j(experimenter)
egen overlaps = rownonmiss(exp1 - exp13) 
forval i = 1/13 {
  bysort country: generate overlaps_obs`i' = sum(exp`i') if overlaps != 1
}
collapse (max) overlaps_obs1-overlaps_obs13, by(country)

// Table S19: Experimenter effects
snapshot restore 1
keep if inlist(Country, "France", "Germany", "Italy", "Malaysia", "Poland")
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
levelsof country, local(levels)
foreach l of local levels {
  regress response i.city i.institution `controls' i.cond if country == `l', robust
  display "Adjusted R2 = " e(r2_a)
  regress response i.city i.institution `controls' i.cond i.experimenter if country == `l', robust
  display "Adjusted R2 = " e(r2_a)
  contrast experimenter
}
regress response i.city i.institution `controls' i.cond if Country == "Poland", robust
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond i.experimenter if Country == "Poland", robust
lincom [2.cond] - [1.cond]

// Table S20: Experimenter effects (continued)
snapshot restore 1
keep if inlist(Country, "Spain", "Switzerland", "Turkey", "UK", "USA")
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
levelsof country, local(levels)
foreach l of local levels {
  regress response i.city i.institution `controls' i.cond if country == `l', robust
  display "Adjusted R2 = " e(r2_a)
  regress response i.city i.institution `controls' i.cond i.experimenter if country == `l', robust
  display "Adjusted R2 = " e(r2_a)
  contrast experimenter
}
regress response i.city i.institution `controls' i.cond if Country == "UK", robust
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond i.experimenter if Country == "UK", robust
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond if Country == "USA", robust
lincom [2.cond] - [1.cond]
regress response i.city i.institution `controls' i.cond i.experimenter if Country == "USA", robust
lincom [2.cond] - [1.cond]

// Footnote 24: Experimenter age and gender
snapshot restore 1
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders"
areg response c.age_experimenter i.maleexperimenter i.cond i.institution `controls' , a(city) robust
areg response c.age_experimenter i.maleexperimenter i.male#i.maleexperimenter i.cond i.institution `controls' , a(city) robust

** Differences in Email Usage
** ==========================================
// Differences in Email Usage
snapshot restore 1
keep if inlist(cond,0,1)
drop if missing(c_GES_email)
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution"
regress response c_GES_email `controls', cluster(country)
predict response_p, residual
collapse response response_p, by(country cond) 
spearman response response_p if cond == 0
spearman response response_p if cond == 1

** Differences in Economic Development
** ==========================================
// Differences in Economic Development
snapshot restore 1
keep if inlist(cond,0,1)
local controls = "i.male i.above40 i.computer i.coworkers i.other_bystanders i.institution"
regress response c_log_gdp `controls', cluster(country)
predict response_p, residual
collapse response response_p, by(country cond) 
spearman response response_p if cond == 0
spearman response response_p if cond == 1
