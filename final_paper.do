// Revisited CPS changes with Metropolitan region

* Filter variables
drop if classwkr == 00
drop if classwkr == 26
drop if classwkr == 29
drop if race == 820
drop if race == 830
drop if labforce == 1
drop if wkstat == 13
drop if wkstat == 42
keep if age >= 18 & age <= 65 
drop if wtfinl == .

// Generate a new binary variable post-pandemic
gen post = .
replace post = 1 if year >= 2020
replace post = 0 if year < 2020

* Generate a binary variable for employment status
gen employment_status = .
replace employment_status = 1 if inlist(empstat, 10)
replace employment_status = 0 if inlist(empstat, 21)

* Step 1: Initialize Treatment Variable
gen treatment = 0  // Initialize treatment variable

* Step 2: Define Treatment States
replace treatment = 1 if inlist(state, 11, 34, 24, 25, 15, 6, 36, 53, 51, 27) // Define treatment states

* Step 3: Initialize Control Variable
gen control = 0  // Initialize control variable
replace control = 1 if inlist(state, 30, 56, 16, 46, 28, 5, 54, 38, 40, 45) // Define control states

* Step 4: Filter to Keep Only Treatment and Control Groups
keep if treatment == 1 | control == 1

gen did = treatment * post 

* Step 5: Create a new education category variable
gen edu_category = .

* Step 5: Recode education variable into categories
replace edu_category = 1 if educ <= 71  // Less than High School
replace edu_category = 2 if educ == 73 // High School Graduate
replace edu_category = 3 if inlist(educ, 81, 91, 92) // Some College or Associate's Degree
replace edu_category = 4 if educ == 111  // Bachelor's Degree
replace edu_category = 5 if inlist(educ, 123, 124, 125) // Graduate Degree

* Step 5: Label the new education category variable
label define edu_labels ///
  1 "Less than High School" ///
  2 "High School Graduate" ///
  3 "Some College" ///
  4 "Bachelor's Degree" ///
  5 "Graduate Degree"

label values edu_category edu_labels

* Save the datset 
save final_paper_cps.dta, replace 

gen ym_date = ym(year, month)

format ym_date %tm

gen keep_flag = ym_date >= ym(2019, 6) & ym_date <= ym(2021, 6)
keep if keep_flag


* Replace treatment differently
gen post_policy = (ym_date >= ym(2020, 3))


* nteraction term for DiD: post-treatment indicator
gen did = post * treatment

* Estimate the logit DiD model
logit employment_status post treatment did i.state i.year, cluster(state)

// Interpret the interaction term `treated` as the causal effect of strict policies

// Compute average marginal effects
margins, dydx(did)

// Predicted probabilities for treated vs. control
margins treatment, at(post=(0 1)) predict(pr)

* only keeping 2020 and certain months .. just looking for short impact
keep if (year == 2020 & inlist(month, 1, 2, 4, 5))

replace post = (inlist(month, 4, 5))    // 1 for April-May, 0 for Jan-Feb
gen treated = post * treatment      // Interaction term

logit employment_status i.treatment##i.post i.state age i.sex i.edu_category i.non_citizen [pweight=wtfinl], cluster(state)


twoway (line employment_status month if treatment == 1, sort) ///
       (line employment_status month if treatment == 0, sort), ///
       title("Parallel Trends: Treated vs Control") ///
       xlabel(1 "January" 2 "February") legend(order(1 "Treated" 2 "Control"))

	   
	   
********************************************************************************

gen post = (year == 2020 & month >= 4) | (year == 2021 & month <= 6)   // April 2020–Mid 2021
gen pre = (year == 2019 & month >= 6) | (year == 2020 & month <= 2)   // Mid 2019–Feb 2020
keep if post == 1 | pre == 1
gen treated = post * treatment    // Interaction term for treatment effect

logit employment_status i.treatment##i.post i.state i.year [pweight=wtfinl], cluster(state) robust

svyset [pweight=wtfinl], strata(state) psu(serial)


svy : logit employment_status i.treatment##i.post i.state i.ym

gen time_trend = month + (year - 2019) * 12

keep if pre == 1

gen treated_time = treatment * time_trend

svy: logit employment_status treatment time_trend treated_time i.state

collapse (mean) employment_status, by(treatment time_trend)


twoway (line employment_status time_trend if treatment == 1, sort) ///
       (line employment_status time_trend if treatment == 0, sort), ///
       title("Parallel Trends Test: Treated vs Control") legend(order(1 "Treated" 2 "Control"))



gen treatment_year = .
gen treatment_month = .

replace treatment_year = 2020 if state == 6
replace treatment_month = 4 if state == 6

replace treatment_year = 2020 if state == 36
replace treatment_month = 4 if state == 36

replace treatment_year = 2020 if state == 17
replace treatment_month = 4 if state == 17

// Repeat for other treated states


gen rel_time = (year - treatment_year) * 12 + (month - treatment_month)


svy : logit employment_status i.treatment##i.post i.sex i.edu_category i.state i.year


********************************************************************************
********************* SUMMRARY STATISTICS **************************************
********************************************************************************

gen policy_group = .
replace policy_group = 1 if treatment == 1  // Stringent policy
replace policy_group = 0 if treatment == 0  // Less strict policy

bysort policy_group: summarize age sex employment_status edu_category wtfinl, detail

ssc install estout

estpost summarize age employment_status edu_category wtfinl if policy_group == 1
estadd scalar policy_group 1
esttab using stringent.tex, replace title("Summary Statistics: Stringent Policy") tex

estpost summarize age employment_status edu_category wtfinl if policy_group == 0
estadd scalar policy_group 0
esttab using lessstrict.tex, replace title("Summary Statistics: Less Strict Policy") tex



* Generate a binary variable called non_citizen
gen non_citizen = 0
replace non_citizen = 1 if inlist(citizen, 5)

* Generate a new variable called race_category
gen race_category = .

replace race_category = 1 if race == 

********************************************************************************
****************** Parallel Trend Check*****************************************
********************************************************************************
keep if pre == 1 

replace time_trend = (year-2019) * 12 + month

svy: logit employment_status treatment##c.time_trend i.state

collapse (mean) employment_status, by(treatment time_trend)

twoway (line employment_status time_trend if treatment == 1, sort lcolor(blue)) ///
       (line employment_status time_trend if treatment == 0, sort lcolor(red)), ///
       title("Parallel Trends Check") legend(order(1 "Treated" 2 "Control"))


save final_paper_parallel_trend_check.dta, replace

use final_paper_cps.dta, replace


*************************
gen event_time = time - treatment_time  // Calculate event time
tab event_time, gen(et_)               // Create dummy variables for each event time


********************************************************************************
************************ COEFFICIENT PLOT **************************************
********************************************************************************

svy: reg employment_status i.treatment##i.post age i.sex i.edu_category i.non_citizen i.state i.year

ssc install parmest, replace

parmest, label for(estimate min95 max95 %8.2f) li(parm label estimate min95 max95) saving(coefficients.dta, replace)

use coefficients.dta, clear

keep if strpos(parm, "treatment") // Adjust to your coefficient names

gen parm_id = _n

twoway (scatter estimate parm_id, mlabel(parm_id) mlabsize(vsmall)) ///
       (rcap min95 max95 parm_id), ///
       ytitle("Coefficient") ///
       xscale(off) ///
       title("Difference-in-Differences Coefficients") ///
       subtitle("95% Confidence Intervals") ///
       note("Horizontal whiskers show confidence intervals.") ///
       legend(off)
	   
svy: reg employment_status i.treatment##i.post age i.sex i.edu_category i.non_citizen i.state i.year 

coefplot, drop(_cons) ci(95) xline(0)

svy: logit employment_status i.treatment##i.post age i.sex i.edu_category i.non_citizen i.state i.month


********************************************************************************
********************** EVENT STUDY GRAPHS **************************************
********************************************************************************

gen rel_time = (year - 2020) * 12 + (month - 3)
gen post_1 = rel_time >= 0

svy: reg employment_status i.treatment##i.post_1 i.state i.year 

tabulate rel_time, gen(rel_time_dummies)

local rel_dummies rel_time_dummies1 rel_time_dummies2 rel_time_dummies3 rel_time_dummies4 rel_time_dummies5 ///
rel_time_dummies6 rel_time_dummies7 rel_time_dummies8 rel_time_dummies9 rel_time_dummies10 rel_time_dummies11 ///
rel_time_dummies12 rel_time_dummies13 rel_time_dummies14 rel_time_dummies15 rel_time_dummies16 rel_time_dummies17 ///
rel_time_dummies18 rel_time_dummies19 rel_time_dummies20 rel_time_dummies21 rel_time_dummies22 rel_time_dummies23 rel_time_dummies24

gen rel_time_cat = .
foreach i of numlist 1/24 {
    replace rel_time_cat = `i' if rel_time_dummies`i' == 1
}

svy: reg employment_status i.rel_time_cat##i.treatment i.state


margins rel_time_cat, dydx(treatment)
marginsplot, xline(0) yline(0) title("Dynamic Effects of COVID-19 Policies") ///
    xtitle("Years Since Intervention") ytitle("Effect on Employment Status")


svy: reg employment_status i.rel_time_cat##i.treatment i.state

estimates table 

parmest, saving(event_coefficients.dta, replace)
use event_coefficients.dta, clear

keep if strpos(parm, "rel_time_cat#treatment")

twoway (scatter estimate parm, mlabel(parm) mlabsize(vsmall)) ///
       (rcap min95 max95 parm), ///
       ytitle("Coefficient") ///
       xscale(off) ///
       title("Event Study: Dynamic Effects of COVID-19 Policies") ///
       subtitle("95% Confidence Intervals") ///
       note("Horizontal whiskers represent confidence intervals.") ///
       legend(off)


tab rel_time_cat treatment

margins rel_time_cat, dydx(treatment) at(rel_time_cat=(1 2 3 4 5))




levelsof state, local(states)
foreach s of local states {
    preserve
    keep if state == "`s'"
    reg employment_status i.rel_time##i.treatment if rel_time < 1, cluster(state)
    display "Results for State: `s'"
    restore
}


twoway (line employment_status rel_time if treatment == 1, sort) ///
       (line employment_status rel_time if treatment == 0, sort), ///
       title("Parallel Trends Test for California") ///
       xtitle("Months Relative to Intervention") ///
       ytitle("Employment Rate")

	   
	   
gen post_policy = 0
replace post_policy = 1 if rel_time > 1


svy: logit employment_status i.treatment##i.post_policy age i.sex i.edu_category i.non_citizen i.state i.month i.year 




pwcorr employment_status treatment post_policy age non_citizen edu_category, obs sig

estpost corr employment_status treatment post_policy age non_citizen edu_category
esttab using "correlation_table.tex", replace ///
    title("Correlation Table") ///
    label booktabs


*****************************************
* Post summary statistics for selected variables
estpost summarize employment_status treatment post_policy age sex edu_category year

* Create and export the descriptive statistics table
esttab using "descriptive_stats.tex", replace ///
    cells("mean sd min max") ///
    label ///
    title("Descriptive Statistics") ///
    align(c) ///
    booktabs
	
	
	
	
sysuse auto, clear
estpost: tabstat employment_status state year sex edu_category treatment post_policy, c(stat) stat(mean sd min max n)  

*******************************************************************************
****************** Summary Statistics Table **********************************
******************************************************************************
cap log close
clear all
eststo clear

***********Directory Settings************
global path "/Users/pratis/Desktop/Research/Final Paper"

global outtex "${path}/basic_sum_statistics.tex"
******************************************


***First, Summarize the data and Store Results
use final_paper_cps.dta

estpost tabstat employment_status treatment post_policy age female black asian pacafic_islander two_more_race region edu_category state, c(stat) stat(mean sd min max n)

***Now, Output
#delimit ;
esttab using "${outtex}",
 replace ///Replace file if already exists
 cells("mean sd min max count") ///Which Stats to Output
 nonumber ///Do not put numbers below column titlles
 nomtitle ///This option mainly for regression tables
 booktabs ///Top, Mid, Bottom Rule
 noobs ///We don't need observation counts because count is N
 title("Basic Summary Statistics\label{tab1}") ///Latex number this for us
 collabels("Mean" "SD" "Min" "Max" "N") /// Name of each column
 addnote("Source: Current Population Survey Data.") ///Note below table
 coeflabels(employment_status "Employment Status" treatment "Strict Policy States" post_policy "Post March 2020" age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander" two_more_race "Two or more races" region "Region" edu_category "Education Classification" state "States") ///Label variables right in command
;



********************************************************************************


eststo clear 

eststo: svy: logit employment_status treatment##post_policy 
 
eststo: svy: logit employment_status treatment##post_policy age i.sex i.edu_category i.non_citizen 
 
eststo: svy: logit employment_status treatment##post_policy age i.sex i.edu_category i.non_citizen i.state i.year

estout 
 
esttab using "basic_sum_stats.tex", r2 ar2 p





********************************************************************************
********************** Balance Tests Table *************************************
********************************************************************************

cap log close
cls
clear all
eststo clear

***********Directory Settings************
global path "/Users/pratis/Desktop/Research/Final Paper"

global outtex "${path}/balancetests.tex"
******************************************


***First, Summarize the data and Store Results
use final_paper_cps.dta

***Estpost specifically allows multiple variables in t-test command
estpost ttest age female black asian pacafic_islander two_more_race edu_category employment_status, by(strict_policy_state)


********Option 1--Main and Aux
#delimit ;
esttab using "${outtex}",
 replace ///Replace file if already exists
 cells("mu_1(fmt(2)) mu_2 b(star) se(par) count(fmt(0))") ///Which Stats to Output
 star(* 0.1 * 0.05 ** 0.01) /// Can Define Custom Stars
 nonumber ///Do not put numbers below column titlles
 booktabs ///Top, Mid, Bottom Rule
 noobs ///We don't need observation counts because count is N
 title("Balance Test by Stringest Policy States\label{balance_test}") ///Latex number this for us
 collabels("Leinient Policy States" "Stringest Policy States" "Difference" "Std. Error" "N") /// Name of each column
 addnote("Note: Difference defined across States." "Source: Census Population Survey Data.") ///Note below table
 coeflabels(age "Age" 
    sex "Sex" 
	black "Black"
	asian "Asian"
	pacafic_islander "Hawaiian/Pacific Islander"
    edu_category "Education Classification" 
    employment_status "Employment Status" 
    );

#delimit cr;

********************************************************************************
***************** Summary Statistics By Group-Sample Table *********************
********************************************************************************
cap log close
clear all
cls
eststo clear

***********Directory Settings************
global path "/Users/pratis/Desktop/Research/Final Paper"

global outtex "${path}/Summary_Stats_Different_Groups.tex"
******************************************


***First, Summarize the data and Store Results
use final_paper_cps.dta
***All Education Groups 
eststo grp1: estpost tabstat employment_status age sex, c(stat) stat(mean sd min max n)

***Less than High School
eststo grp2: estpost tabstat employment_status age sex if edu_category==1, c(stat) stat(mean sd min max n)

***High School Graduates
eststo grp3: estpost tabstat employment_status age sex if edu_category==2, c(stat) stat(mean sd min max n)

*** Some College Graduate
eststo grp4: estpost tabstat employment_status age sex if edu_category==3, c(stat) stat(mean sd min max n)

*** Bachelor's Degree
eststo grp5: estpost tabstat employment_status age sex if edu_category==4, c(stat) stat(mean sd min max n)

*** Graduate Degree
eststo grp6: estpost tabstat employment_status age sex if edu_category==5, c(stat) stat(mean sd min max n)


 #delimit ;
 esttab grp* using "${outtex}",
  replace ///Replace file if already exists
  cells("mean(fmt(7))" "sd(par)") ///Which Stats to Output
 nonumber ///Do not put numbers below column titlles
  mtitle("All" "Less than High School" "High School" "Some College" "Bachelors" "Graduate") ///This option mainly for regression tables
  nostar /// No Stars for Significance
  unstack ///Vertical from Stata to Diff Columns
  booktabs ///Top, Mid, Bottom Rule
  noobs ///We don't need observation counts because count is N
  title("Summary Stats by Group\label{by_group}") ///Latex number this for us
  collabels(none) /// Name of each column
  addnote("Note: Summary statistics by Education" "Source: Census Population Survey Data.") ///Note below table
  coeflabels(employment_status "Employment Status" age "Age" sex "Sex") ///Label variables right in command
 ;

********************************************************************************
**************** Regression ***********************
**************************************************
clear all
cls


global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/regression_outcomes.tex"

use final_paper_cps.dta

est clear
eststo: svy: logit employment_status treatment post_policy did 
estadd local controls "No"
estadd local fixed "No"
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category
estadd local controls "Yes"
estadd local fixed "No"
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year
estadd local controls "Yes"
estadd local fixed "Yes"

global note1="Note: Robust Standard Errors in Parenthesis"

global sources="Source: Census Population Survey Data"


global coef_label treatment "Stringest Policy States" post_policy "Post March 2020" did "Treatment \$\times$ Post" age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander" two_more_race "Two or more races" edu_category "Educ Classification" 2.edu_category "High School Graduate" 3.edu_category "Some College" 4.edu_category "Bachelor's Degree" 5.edu_category "Graduate Degree"


#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label booktabs nomtitle
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Tabel
  mgroups("Employment Status", pattern(1 0 0)
   prefix(\multicolumn{@span}{c}{) ///Set-up for Group Title
   suffix(}) 
   span
   erepeat(\cmidrule(lr){@span}) ///Make the Line Under Each Group
   ) 
   keep(treatment post_policy did age female black asian pacafic_islander two_more_race 2.edu_category 3.edu_category 4.edu_category 5.edu_category ) /// Include only relevant variables  
  coeflabels($coef_label)
  scalars("N" "controls Controls" "fixed Fixed Effects") ///Which Extra Stuff You Want In 
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" ///Add your Own Note
    "$sources" ///Space/New Line Puts Next Note on New Line
  ) 
;
//   mtitles("No Controls" "With Controls" "With Fixed Effects") /// Column Titles


clear all
est clear


global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/regression_outcome.tex"

use final_paper_cps.dta

est clear
eststo: svy: logit employment_status treatment post_policy did 
estadd local controls "No"
estadd local fixed "No"
eststo: svy: logit employment_status treatment post_policy did age sex edu_category
estadd local controls "Yes"
estadd local fixed "No"
eststo: svy: logit employment_status treatment post_policy did age sex i.edu_category i.state i.year
estadd local controls "Yes"
estadd local fixed "Yes"

global note1="Note: Robust Standard Errors in Parenthesis"

global sources="Source: Census Population Survey Data"


#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Tabel
  mtitles("No Controls" "With Controls") /// Column Titles
  keep(treatment post_policy did age sex edu_category ) /// Include only relevant variables  
  coeflabels(treatment "Stringest Policy States"
  post_policy "Post March 2020" age "Age" sex "Sex" edu_category "Education Classification") /// Label Variables
  scalars("r2 R^{2}" "N" "controls Controls" "fixed Fixed Effects") ///Which Extra Stuff You Want In 
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" ///Add your Own Note
    "$sources" ///Space/New Line Puts Next Note on New Line
  ) 
;


********************************************************************************
********************* Monthly Graphs *******************************************
********************************************************************************
gen unemployment = employment_status == 0

gen ym = ym(year, month)
format ym %tm

collapse (mean) unemployment, by(region month year ym)

gen ym_label = string(year) + "-" + string(month)

twoway (scatter unemployment ym if region == 11, mcolor(blue) msize(vsmall)) ///
	   (scatter unemployment ym if region == 12, mcolor(blue) msize(vsmall)) ///
	   (scatter unemployment ym if region == 21, mcolor(red) msize(vsmall)) ///
 	   (scatter unemployment ym if region == 22, mcolor(red) msize(vsmall)) ///
	   (scatter unemployment ym if region == 31, mcolor(green) msize(vsmall)) ///
	   (scatter unemployment ym if region == 32, mcolor(green) msize(vsmall)) ///
	   (scatter unemployment ym if region == 33, mcolor(green) msize(vsmall)) ///
	   (scatter unemployment ym if region == 41, mcolor(black) msize(vsmall)) ///
	   (scatter unemployment ym if region == 42, mcolor(black) msize(vsmall)), ///
	legend(order(1 "New England Division" 2 "Middle Atlantic" 3 "Midwest Region" 4 "Midwest Region" 5 "South Region" 6 "South Region" 7 "South Region" 8 "West Region" 9 "West Region")) ///
    xtitle("Year-Month", size(small)) ytitle("Unemployment Rate", size(small)) ///
    title("Unemployment Rates by Region", size(medium))


*******************************************************************************
************** Regression By Regions *******************************************
********************************************************************************
gen region_category = .
replace region_category = 1 if inlist(region, 11, 12) 
replace region_category = 2 if inlist(region, 21, 22)
replace region_category = 3 if inlist(region, 31, 32, 33)
replace region_category = 4 if inlist(region, 41, 42)


clear all
cls


global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/regression_outcomes_by region.tex"

use final_paper_cps.dta

est clear
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year 

// Northeast Region
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year if region_category == 1

// Midwest Region 
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year if region_category == 2

// South Region
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year if region_category == 3

// West Region
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year if region_category == 4



global note1="Note: Robust Standard Errors in Parenthesis"

global sources="Source: Census Population Survey Data"


global coef_label treatment "Stringest Policy States" post_policy "Post March 2020" did "Treatment \$\times$ Post" age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander" two_more_race "Two or more races" edu_category "Educ Classification" 2.edu_category "High School Graduate" 3.edu_category "Some College" 4.edu_category "Bachelor's Degree" 5.edu_category "Graduate Degree"


#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label booktabs nomtitle
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Tabel
  mgroups("Employment Status", pattern(1 0 0)
   prefix(\multicolumn{@span}{c}{) ///Set-up for Group Title
   suffix(}) 
   span
   erepeat(\cmidrule(lr){@span}) ///Make the Line Under Each Group
   ) 
   keep(treatment post_policy did age female black asian pacafic_islander two_more_race 2.edu_category 3.edu_category 4.edu_category 5.edu_category ) /// Include only relevant variables  
  coeflabels($coef_label)
  scalars("N") ///Which Extra Stuff You Want In 
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" ///Add your Own Note
    "$sources" ///Space/New Line Puts Next Note on New Line
  ) 
;
//   mtitles("No Controls" "With Controls" "With Fixed Effects") /// Column Titles


********************************************************************************
********************    Regression By Race Groups    ***************************
********************************************************************************
clear all
cls




global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/regression_outcomes_by_race.tex"

use final_paper_cps.dta

// keep if age >= 25 

est clear
// All 
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.edu_category i.state i.year

// Black 
eststo: svy: logit employment_status treatment post_policy did age female i.edu_category i.state i.year if black == 1

// Asian
eststo: svy: logit employment_status treatment post_policy did age female i.edu_category i.state i.year if asian == 1 

// Hawaiian/Pacific Islander
eststo: svy: logit employment_status treatment post_policy did age female i.edu_category i.state i.year if pacafic_islander == 1

// Two or more Races
eststo: svy: logit employment_status treatment post_policy did age female i.edu_category i.state i.year if two_more_race == 1

// // Graduate Degree
// eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.ym if edu_category == 5
// estadd local controls "Yes"
// estadd local fixed "Yes"


global note1="Note: Robust Standard Errors in Parenthesis"

global sources="Source: Current Population Survey Data"


global coef_label treatment "Stringest Policy States" post_policy "Post March 2020" did "Treatment \$\times$ Post" age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander"two_more_race "Two or more races" edu_category "Educ Classification" 2.edu_category "High School Graduate" 3.edu_category "Some College" 4.edu_category "Bachelor's Degree" 5.edu_category "Graduate Degree"

global variables treatment post_policy did age female 2.edu_category 3.edu_category 4.edu_category 5.edu_category

#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label booktabs nomtitle
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Tabel
  mgroups("Employment Status", pattern(1 0 0)
   prefix(\multicolumn{@span}{c}{) ///Set-up for Group Title
   suffix(}) 
   span
   erepeat(\cmidrule(lr){@span}) ///Make the Line Under Each Group
   ) 
   keep($variables) /// Include only relevant variables  
  coeflabels($coef_label)
  scalars("N") ///Which Extra Stuff You Want In 
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" ///Add your Own Note
    "$sources" ///Space/New Line Puts Next Note on New Line
  ) 
;

********************************************************************************
************** Regression Table By Education Level *****************************
********************************************************************************
clear all
cls


global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/regression_outcomes_by_educ.tex"

use final_paper_cps.dta

est clear
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.year 

// Less than High School
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.year if edu_category == 1

// High School 
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.year if edu_category == 2

// Some College
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.year if edu_category == 3

// Bachelor's Degree
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.year if edu_category == 4

// Graduate Degree
eststo: svy: logit employment_status treatment post_policy did age female black asian pacafic_islander two_more_race i.state i.year if edu_category == 5

global note1="Note: Robust Standard Errors in Parenthesis"

global sources="Source: Current Population Survey Data"


global coef_label treatment "Stringest Policy States" post_policy "Post March 2020" did "Treatment \$\times$ Post" age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander" two_more_race "Two or more races" edu_category "Educ Classification" 2.edu_category "High School Graduate" 3.edu_category "Some College" 4.edu_category "Bachelor's Degree" 5.edu_category "Graduate Degree"


#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label booktabs nomtitle
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Tabel
  mgroups("Employment Status", pattern(1 0 0)
   prefix(\multicolumn{@span}{c}{) ///Set-up for Group Title
   suffix(}) 
   span
   erepeat(\cmidrule(lr){@span}) ///Make the Line Under Each Group
   ) 
   keep(treatment post_policy did age female black asian pacafic_islander two_more_race ) /// Include only relevant variables  
  coeflabels($coef_label)
  scalars("N") ///Which Extra Stuff You Want In 
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" ///Add your Own Note
    "$sources" ///Space/New Line Puts Next Note on New Line
  ) 
;
********************************************************************************
*************** Regression Table for States Only *******************************
********************************************************************************




********************************************************************************
*************** Sub Sample Means for Selected Explanatory Variables ************
********************************************************************************
gen female = sex == 2
gen black = race == 200
gen asian = race == 651
gen pacafic_islander = race == 652
gen two_more_race = 0
replace two_more_race = 1 if inlist(race, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816 817, 818, 819)
gen educ1 = 0
replace educ1 = 1 if edu_category == 1
gen educ2 = 0
replace educ2 = 1 if edu_category == 2
gen educ3 = 0
replace educ3 = 1 if edu_category == 3
gen educ4 = 0
replace educ4 = 1 if edu_category == 4
gen educ5 = 0
replace educ5 = 1 if edu_category == 5


clear all
cls

***********Directory Settings************
global path "/Users/pratis/Desktop/Research/Final Paper"

global outtex "${path}/Summary_Stats_Region_Groups.tex"
******************************************

use final_paper_cps.dta

est clear

***All Regions
eststo grp1: estpost tabstat age female black asian pacafic_islander two_more_race educ1 educ2 educ3 educ4 educ5, c(stat) stat(mean sd min max n)

*** Northeast Region
eststo grp2: estpost tabstat age female black asian pacafic_islander two_more_race educ1 educ2 educ3 educ4 educ5 if region_category==1, c(stat) stat(mean sd min max n)

*** Midwest Region
eststo grp3: estpost tabstat age female black asian pacafic_islander two_more_race educ1 educ2 educ3 educ4 educ5 if region_category==2, c(stat) stat(mean sd min max n)

*** South Region
eststo grp4: estpost tabstat age female black asian pacafic_islander two_more_race educ1 educ2 educ3 educ4 educ5 if region_category==3, c(stat) stat(mean sd min max n)

*** West Region
eststo grp5: estpost tabstat age female black asian pacafic_islander two_more_race educ1 educ2 educ3 educ4 educ5 if region_category==4, c(stat) stat(mean sd min max n)



 #delimit ;
 esttab grp* using "${outtex}",
  replace ///Replace file if already exists
  cells("mean(fmt(7))" "sd(par)") ///Which Stats to Output
 nonumber ///Do not put numbers below column titlles
  mtitle("All" "Northeast Region" "Midwest Region" "South Region" "West Region") ///This option mainly for regression tables
  nostar /// No Stars for Significance
  unstack ///Vertical from Stata to Diff Columns
  booktabs ///Top, Mid, Bottom Rule
  noobs ///We don't need observation counts because count is N
  title("Summary Stats by Group\label{by_group}") ///Latex number this for us
  collabels(none) /// Name of each column
  addnote("Note: Summary statistics by Education" "Source: Census Population Survey Data.") ///Note below table
  coeflabels(age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander" two_more_race "Two or more races" educ1 "Less than High School" educ2 "High School Graduate" educ3 "Some College" educ4 "Bachelor's Degree" educ5 "Graduate Degree") ///Label variables right in command
 ;

********************************************************************************
****************** Sub Sample Means By Education *******************************
********************************************************************************
clear all
cls

***********Directory Settings************
global path "/Users/pratis/Desktop/Research/Final Paper"

global outtex "${path}/Summary_Stats_education_Groups.tex"
******************************************

use final_paper_cps.dta

est clear

***All Education Level
eststo grp1: estpost tabstat age female black asian pacafic_islander two_more_race, c(stat) stat(mean sd min max n)

*** Less than High School
eststo grp2: estpost tabstat age female black asian pacafic_islander two_more_race if edu_category == 1, c(stat) stat(mean sd min max n)

*** High School Graduate
eststo grp3: estpost tabstat age female black asian pacafic_islander two_more_race if edu_category == 2, c(stat) stat(mean sd min max n)

*** Some College 
eststo grp4: estpost tabstat age female black asian pacafic_islander two_more_race if edu_category == 3, c(stat) stat(mean sd min max n)

*** Bachelor's Degree 
eststo grp5: estpost tabstat age female black asian pacafic_islander two_more_race if edu_category == 4, c(stat) stat(mean sd min max n)

*** Graduate Degree 
eststo grp6: estpost tabstat age female black asian pacafic_islander two_more_race if edu_category == 5, c(stat) stat(mean sd min max n)

 #delimit ;
 esttab grp* using "${outtex}",
  replace ///Replace file if already exists
  cells("mean(fmt(6))" "sd(par)") ///Which Stats to Output
 nonumber ///Do not put numbers below column titlles
  mtitle("All" "Less than High School" "High School Graduate" "Some College" "Bachelor's Degree" "Graduate Degree") ///This option mainly for regression tables
  nostar /// No Stars for Significance
  unstack ///Vertical from Stata to Diff Columns
  booktabs ///Top, Mid, Bottom Rule
  noobs ///We don't need observation counts because count is N
  title("Summary Stats by Education Level\label{by_group}") ///Latex number this for us
  collabels(none) /// Name of each column
  addnote("Note: Summary statistics by Education" "Source: Current Population Survey Data.") ///Note below table
  coeflabels(age "Age" female "Female" black "Black" asian "Asian" pacafic_islander "Hawaiian/Pacific Islander" two_more_race "Two or more races") ///Label variables right in command
 ;

********************************************************************************
********************** Event Study CoeffPlot Graph *****************************
********************************************************************************

forvalues i = 2/24 {
    drop rel_time_dummies`i'
}


// Create relative time dummies
tab rel_time, gen(rel_time_dummies)

// Exclude the base period (e.g., rel_time == -1)
drop rel_time_dummies1

// Run regression
svy: reg employment_status i.rel_time_dummies*##i.treatment


// Calculate margins for the treatment group across time
margins rel_time_dummies*, dydx(treatment)

// List results to verify
marginsplot, xline(0, lcolor(red) lwidth(medium)) ///
    yline(0, lcolor(black) lwidth(thin)) ///
    title("Event Study Coefficient Plot") ///
    subtitle("Parallel Trend Assumption") ///
    xtitle("Relative Time") ytitle("Coefficient")





********************************************************************************

gen education1 if edu_category = 1
gen education2 if edu_category = 2
gen education3 if edu_category = 3
gen education4 if edu_category = 4
gen education5 if edu_category = 5

clear all
est clear

global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/regression_outcome.tex"

use final_paper_cps.dta

* Model 1: No controls
eststo: svy: logit employment_status treatment post_policy did
estadd local controls "No"
estadd local fixed "No"

* Model 2: With controls
eststo: svy: logit employment_status treatment post_policy did age sex edu_category*
estadd local controls "Yes"
estadd local fixed "No"

* Model 3: With fixed effects (states and years)
eststo: svy: logit employment_status treatment post_policy did age sex edu_category* state* year*
estadd local controls "Yes"
estadd local fixed "Yes"

global note1="Note: Robust Standard Errors in Parenthesis"
global sources="Source: Census Population Survey Data"

#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Table
  mtitles("No Controls" "With Controls" "With Fixed Effects") /// Column Titles
  keep(treatment post_policy did age sex edu_category* state*) /// Explicitly Include Variables
  coeflabels(
      treatment "Stringent Policy States"
      post_policy "Post March 2020"
      did "DiD (Treatment × Post)"
      age "Age"
      sex "Sex"
      edu_category_1 "Less than High School"
      edu_category_2 "High School Graduate"
      edu_category_3 "Some College"
      edu_category_4 "Bachelor's Degree"
      edu_category_5 "Graduate Degree"
  ) 
  scalars("r2 R^{2}" "N" "controls Controls" "fixed Fixed Effects") /// Include Summary Stats
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" /// Add your own note
    "$sources" /// Add source information
  ) 
;
#delimit cr


********************************************************************************
****************** Parallel Trend Check ****************************************
********************************************************************************
gen event_time = time - treatment_time  // Calculate event time
tab event_time, gen(et_)               // Create dummy variables for each event time


gen treatment_time = ym_int == 723
keep if ym_int < 723
gen event_time_pre = ym_int - 713

reg employment_status i.event_time_pre##i.treatment, robust cluster(state)
testparm i.treatment#c.year



svy: logit employment_status i.event_time_positive##i.treatment 

estimates store event_study

matrix mean_b = e(b)
gen diff_from_mean = employment_status - mean_b[1, 1]

coefplot (diff_from_mean, label("Excess Prop Firms")) ///
    xline(0) ylabel("Difference from Mean") ///
    xlabel("Year relative to treatment") ///
    title("Event Study: Excess Prop Firms Over Time")
	
coefplot event_study keep(event_time_pre#c.treatment) xlab(1/19) vert  ytitle(Coefficient)

coefplot, drop(1.event_time_positive#1.treatment) vertical ci /// 
    xlabel(0(1)15, angle(0)) ///       // Adjust x-axis labels to match the range and avoid overlap
    xtitle("Event Time") ///          // Add a clear title for the x-axis
    ytitle("Coefficient Estimate") /// // Add a clear title for the y-axis
    yline(0, lcolor(black) lwidth(medium)) /// // Make the reference line bold
    msize(medium) lwidth(thick) ///    // Adjust marker size and line width
    color(blue) lcolor(blue) ///      // Set a consistent color scheme
    legend(off)       // Turn off the legend for a cleaner look
	
	
coefplot, drop(1.event_time_positive#1.treatment) vertical ci /// 
    xlabel(0(6)24, angle(0)) ///       // Show x-axis labels every 2 units
    xtitle("Event Time") ///          // Add a clear title for the x-axis
    ytitle("Coefficient Estimate") /// // Add a clear title for the y-axis
    yline(0, lcolor(black) lwidth(medium)) /// // Make the reference line bold
    msize(medium) lwidth(thick) ///    // Adjust marker size and line width
    color(black) lcolor(blue)          // Set a consistent color scheme

coefplot, drop(1.event_time_positive#1.treatment) vertical ci /// 
    xlabel(0 "May 2019" 5 "Oct 2019" 10 "Mar 2020" 15 "Aug 2020" 20 "Jan 2021" 24 "May 2021", angle(45)) ///
    xtitle("Event Time (Months)") ///
    ytitle("Coefficient Estimate") ///
    yline(0, lcolor(black) lwidth(medium)) ///
    msize(medium) lwidth(thick) ///
    color(blue) lcolor(blue)
	
twoway ///
    (rarea lower upper event_time_positive, color(blue%20)) ///  // Shaded confidence intervals
    (scatter coef event_time_positive, msymbol(O) msize(medium) lcolor(blue) lwidth(medium)), ///  // Scatter points
    yline(0, lcolor(black) lwidth(medium) lpattern(dash)) ///  // Reference line at y=0
    xtitle("Event Time (Months)") ///  // X-axis title
    ytitle("Coefficient Estimate") ///  // Y-axis title
    xlabel(0 "May 2019" 5 "Oct 2019" 10 "Mar 2020" 15 "Aug 2020" 20 "Jan 2021" 24 "May 2021", angle(45)) ///  // Simplified x-axis labels
    legend(off) ///  // Remove legend for clarity
    title("Impact of Policies on Employment") ///  // Add a descriptive title
    note("Shaded regions show 95% confidence intervals.") ///  // Add a note explaining shaded areas
    graphregion(color(white)) ///  // Set graph background color
    bgcolor(white) 
	// Set plot area background color
	
coefplot, ///
    drop(1.event_time_positive#1.treatment) vertical ci ///
    xlabel(0(6)24, format(%td) labsize(small)) ///  // Show labels every 6 months, with smaller label size
    xtitle("Event Time (Months)") ///  // Add a meaningful x-axis title
    ytitle("Coefficient Estimate") /// // Add a meaningful y-axis title
    yline(0, lcolor(black) lwidth(medium)) /// // Add a reference line at 0
    title("Impact of Policies on Employment") ///  // Add a clear and descriptive title
    legend(order(1 "95% Confidence Interval")) /// // Simplify the legend
    msize(medium) lwidth(thick) ///    // Make markers and lines more visible
    color(blue) lcolor(blue)          // Apply a consistent color scheme




	
gen lower = coef - 1.96 * std_err   // Lower bound of 95% CI
gen upper = coef + 1.96 * std_err   // Upper bound of 95% CI

gen pre_post = (event_time_positive < 10) // 1 for pre-treatment, 0 for post-treatment

gen pre_coef = coef if event_time_positive < 10
gen post_coef = coef if event_time_positive >= 10
gen pre_lower = lower if event_time_positive < 10
gen pre_upper = upper if event_time_positive < 10
gen post_lower = lower if event_time_positive >= 10
gen post_upper = upper if event_time_positive >= 10

twoway ///
(rarea pre_lower pre_upper event_time_positive if event_time_positive < 10, color(red%30)) /// 
(line pre_coef event_time_positive if event_time_positive < 10, lcolor(red) lwidth(medium) lpattern(solid)) ///
(rarea post_lower post_upper event_time_positive if event_time_positive >= 10, color(blue%30)) /// 
(line post_coef event_time_positive if event_time_positive >= 10, lcolor(blue) lwidth(medium) lpattern(solid)) ///
, ///
yline(0, lcolor(black) lwidth(medium) lpattern(dash)) ///
xline(10, lcolor(black) lwidth(medium) lpattern(dash)) ///
xtitle("Days since March 2020") ///
ytitle("Coefficients") ///
legend(order(1 "Pre-trend coefficients" 2 "Treatment effects") position(6)) ///
title("Impact of Policies on Employment") ///
xlabel(0(5)25, format(%td))





*********************************************

margins event_time_positive, at(treatment=(0 1)) post // Marginal effects at treatment = 0 and 1



********************************************************************************
******************* Parallel Trend Check ***************************************
********************************************************************************
gen parallel_did = event_time_pre*treatment
clear all 


est clear

global path "/Users/pratis/Desktop/Research/Final Paper"
global outtex "${path}/parallel_trend_regression_outcome.tex"

use final_paper_cps.dta

* Model 1: No controls
eststo: svy: logit employment_status event_time_pre treatment parallel_did 


global note1="Note: Robust Standard Errors in Parenthesis"
global sources="Source: CurrentPopulation Survey Data"

#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label
  replace /// Replace File
  title(Statistical Results For Parallel Trend Check \label{tab1}) /// Title of Table
  mtitles(Employment Status) /// Column Titles
  keep(event_time_pre treatment parallel_did) /// Explicitly Include Variables
  coeflabels(
	  event_time_pre "Pre March 2020"
      treatment "Stringent Policy States"
      parallel_did "DiD (Treatment $\time$ Pre-March 2020)"
     
  ) 
  scalars("N" ) /// Include Summary Stats
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" /// Add your own note
    "$sources" /// Add source information
  ) 
;

#delimit ;
esttab using "${outtex}", 
  se(3) /// SE with 3 Decimal Places
  b(2) /// Coefficients with 2 Decimal Places
  label
  replace /// Replace File
  title(Regressions \label{tab1}) /// Title of Table
  mtitles() /// Column Titles
  keep(event_time_pre treatment 1.parallel 2.parallel~d 3.parallel~d 4.parallel~d 5.parallel~d 6.parallel~d 7.parallel~d 8.parallel~d) /// Explicitly Include Variables
  coeflabels(
	  event_time_pre "Pre March 2020"
      treatment "Stringent Policy States"
      parallel_did "DiD (Treatment $\time$ Pre-March 2020)"
	  1.parallel~d "July"
	  2.parallel~d "August"
	  3.parallel~d "September"
	  4.parallel~d "October"
	  5.parallel~d "November"
	  6.parallel~d "December"
	  7.parallel~d "January"
	  8.parallel~d "February"
     
  ) 
  scalars("N" ) /// Include Summary Stats
  nonotes /// Suppress Automatically Generated Notes
  addnote("$note1" /// Add your own note
    "$sources" /// Add source information
  ) 
;


