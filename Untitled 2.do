/********************************************************************
Final Paper Stata Code
Author: Prateetya Bajracharya
********************************************************************/

clear all
set more off
capture log close

*-----------------------------*
*  Setup
*-----------------------------*
cap which esttab
if _rc ssc install estout, replace

cap which coefplot
if _rc ssc install coefplot, replace

log using "Prateetya_StataSample.log", replace text

global mainpath "/Users/pratis/Desktop/Final Paper"
use "$mainpath/final_paper_cps.dta", clear

*-----------------------------*
*  Sample restrictions
*-----------------------------*
* Exclude non-relevant worker classifications
drop if inlist(classwkr, 00, 26, 29)

* Remove race codes not used in standard CPS analyses
drop if inlist(race, 820, 830)

* Exclude individuals not in the labor force
drop if labforce == 1

* Drop individuals with ambiguous or inactive work status
drop if inlist(wkstat, 13, 42)

* Restrict to prime working-age population
keep if age >= 18 & age <= 65

* Drop observations without valid CPS weights
drop if missing(wtfinl)

*-----------------------------*
*  Key variables
*-----------------------------*

* Outcome: employed vs unemployed
gen employment_status = .
replace employment_status = 1 if empstat == 10
replace employment_status = 0 if empstat == 21
drop if missing(employment_status)

* Treatment/control (policy stringency grouping)
gen treatment = inlist(state, 11, 34, 24, 25, 15, 6, 36, 53, 51, 27)
gen control   = inlist(state, 30, 56, 16, 46, 28, 5, 54, 38, 40, 45)
keep if treatment | control

* Time index
gen ym_date = ym(year, month)
format ym_date %tm
keep if ym_date >= ym(2019,6) & ym_date <= ym(2021,6)

* Post policy window (March 2020 onset)
gen post_policy = (ym_date >= ym(2020,3))

* Education categories
gen edu_category = .
replace edu_category = 1 if educ <= 71
replace edu_category = 2 if educ == 73
replace edu_category = 3 if inlist(educ, 81, 91, 92)
replace edu_category = 4 if educ == 111
replace edu_category = 5 if inlist(educ, 123, 124, 125)

label define edu_labels ///
  1 "Less than HS" ///
  2 "High school" ///
  3 "Some college" ///
  4 "Bachelor's" ///
  5 "Graduate"
label values edu_category edu_labels

* Non-citizen indicator
gen non_citizen = (citizen == 5)

* Simple demographics often used
gen female = (sex == 2)
gen black  = (race == 200)
gen asian  = (race == 651)

*-----------------------------*
*  Summary stats
*-----------------------------*
* Directory Setting
global outtex "${mainpath}/basic_sum_statistics.tex"

* Post summary stats and store results
estpost tabstat ///
    employment_status treatment post_policy ///
    age female black asian non_citizen ///
    i.edu_category region ///
    , statistics(mean sd min max n) columns(statistics)

* Export to LaTeX
#delimit ;
esttab using "${outtex}",
    replace
    cells("mean(fmt(3)) sd(fmt(3)) min(fmt(2)) max(fmt(2)) count(fmt(0))")
    nonumber
    nomtitle
    booktabs
    noobs
    title("Basic Summary Statistics\label{tab:sumstats}")
    collabels("Mean" "SD" "Min" "Max" "N")
    addnote("Source: Current Population Survey (CPS).")
    coeflabels(
        employment_status "Employment Status"
        treatment "Strict Policy States"
        post_policy "Post March 2020"
        age "Age"
        female "Female"
        black "Black"
        asian "Asian"
        non_citizen "Non-citizen"
        2.edu_category "High School"
        3.edu_category "Some College"
        4.edu_category "Bachelor's"
        5.edu_category "Graduate"
        region "Region"
    )
;
#delimit cr

*-----------------------------*
* Summary Stats By Group-Sample Table
*-----------------------------*
* Directory Setting
global outtexsum2 "${mainpath}/Summary_Stats_Different_Groups.tex"

* Post summary stats and store results
* All Education Groups 
eststo grp1: estpost tabstat employment_status age sex, c(stat) stat(mean sd min max n)

* Less than High School
eststo grp2: estpost tabstat employment_status age sex if edu_category==1, c(stat) stat(mean sd min max n)

* High School Graduates
eststo grp3: estpost tabstat employment_status age sex if edu_category==2, c(stat) stat(mean sd min max n)

* Some College Graduate
eststo grp4: estpost tabstat employment_status age sex if edu_category==3, c(stat) stat(mean sd min max n)

* Bachelor's Degree
eststo grp5: estpost tabstat employment_status age sex if edu_category==4, c(stat) stat(mean sd min max n)

* Graduate Degree
eststo grp6: estpost tabstat employment_status age sex if edu_category==5, c(stat) stat(mean sd min max n)


#delimit ;
 esttab grp* using "${outtexsum2}",
  replace 
  cells("mean(fmt(7))" "sd(par)") 
 nonumber ///Do not put numbers below column titlles
  mtitle("All" "Less than High School" "High School" "Some College" "Bachelors" "Graduate") ///This option mainly for regression tables
  nostar 
  unstack 
  booktabs 
  noobs 
  title("Summary Stats by Group\label{by_group}") 
  collabels(none) /// Name of each column
  addnote("Note: Summary statistics by Education" "Source: Census Population Survey Data.") 
  coeflabels(
			employment_status "Employment Status" 
			age "Age" 
			sex "Sex"
	)
 ;
#delimit cr

*-----------------------------*
*  Balance tests (pre-period only)
*-----------------------------*
preserve
keep if ym_date < ym(2020,3)

estpost ttest age female black asian non_citizen, by(policy_group)
esttab using "table_balance_preperiod.txt", replace ///
    title("Balance Tests (Pre-Period)") ///
    cells("mu_1 mu_2 b se p") nonumber noobs
restore

*-----------------------------*
*  Main DiD regressions (LPM; clustered at state)
*-----------------------------*
* Directory Setting + notes
global outtexreg "${mainpath}/regression_outcomes.tex"
global note1="Note: Robust Standard Errors in Parenthesis"
global sources="Source: Census Population Survey Data"

* Core DiD interaction
gen did = treatment * post_policy

eststo clear
eststo m1: reg employment_status treatment post_policy did [pweight=wtfinl], vce(cluster state)
estadd local controls "No"
estadd local fe "No"

eststo m2: reg employment_status treatment post_policy did ///
    age female black asian i.edu_category non_citizen ///
    [pweight=wtfinl], vce(cluster state)
estadd local controls "Yes"
estadd local fe "No"

eststo m3: reg employment_status treatment post_policy did ///
    age female black asian i.edu_category non_citizen ///
    i.state i.year ///
    [pweight=wtfinl], vce(cluster state)
estadd local controls "Yes"
estadd local fe "Yes"

#delimit ;
 esttab using "${outtexreg}", 
  se(3) 
  b(2) 
  label booktabs nomtitle
  replace 
  title(Regressions \label{tab1}) 
  mgroups("Employment Status", pattern(1 0 0)
  prefix(\multicolumn{@span}{c}{) 
  suffix(}) 
  span
  erepeat(\cmidrule(lr){@span}) 
   ) 
  keep(treatment post_policy did age female black asian pacafic_islander two_more_race 2.edu_category 3.edu_category 4.edu_category 5.edu_category ) /// Include only relevant variables  
  coeflabels(
			treatment "Stringest Policy States" 
			post_policy "Post March 2020" 
			did "Treatment \$\times$ Post" 
			age "Age" 
			female "Female" 
			black "Black" 
			asian "Asian" 
			pacafic_islander "Hawaiian/Pacific Islander" 
			two_more_race "Two or more races" 
			edu_category "Educ Classification" 
			2.edu_category "High School Graduate" 
			3.edu_category "Some College" 
			4.edu_category "Bachelor's Degree" 
			5.edu_category "Graduate Degree"
	)
  scalars("N" "controls Controls" "fixed Fixed Effects") 
  nonotes 
  addnote("$note1" 
    "$sources" 
  ) 
;
#delimit cr

*-----------------------------*
*  Parallel trends plot (pre-period means)
*-----------------------------*
preserve
keep if ym_date < ym(2020,3)

collapse (mean) emp_rate=employment_status [pweight=wtfinl], by(treatment ym_date)

twoway ///
 (line emp_rate ym_date if treatment==1, sort) ///
 (line emp_rate ym_date if treatment==0, sort) ///
 , title("Parallel Trends (Pre-Period)") ///
   legend(order(1 "Treated" 2 "Control")) ///
   ytitle("Employment Rate") xtitle("Year-Month")

graph export "parallel_trends_preperiod.png", replace
restore

*-----------------------------*
*  Event-study style dynamic effects (relative month bins)
*-----------------------------*

* Relative month to March 2020
gen rel_month = ym_date - ym(2020,3)

* Create binned event-time categories for readability (optional)
* Example: keep [-9, +12]
keep if rel_month >= -9 & rel_month <= 12

* Omit baseline month = -1 (common)
char rel_month[omit] -1

* Regression with event-time dummies interacted with treatment
* This is a standard event-study (LPM) with state and year FE
reg employment_status i.rel_month##i.treatment i.state i.year ///
    [pweight=wtfinl], vce(cluster state)

* Plot the treatment effects by rel_month using coefplot
* We keep only interaction terms 1.treatment#rel_month
coefplot, ///
    keep(*.rel_month#1.treatment) ///
    vertical xline(10) yline(0) ///
    title("Event Study: Dynamic Treatment Effects") ///
    xtitle("Months relative to March 2020") ytitle("Effect on Employment Status")

graph export "event_study_coefplot.png", replace

log close

