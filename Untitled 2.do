/// 

// Just keeping year from 2017 to 2022
drop if year == 2023 

// set treatment 
gen treat = 1 if inlist(state, 11, 34, 24, 25, 15, 6, 36, 53, 51, 27)

gen control = 1 if inlist(state, 30, 56, 16, 46, 28, 5, 54, 38, 40, 45)

keep if treat == 1 | control == 1

gen treatment = treat == 1

tab treatment


sum age educ employment_rate if treatment == 1
sum age educ employment_rate if treatment == 0


collapse (mean) employment_rate, by(year treatment)
twoway (line employment_rate year if treatment == 1, lcolor(blue)) ///
       (line employment_rate year if treatment == 0, lcolor(red)), ///
       legend(label(1 "Treatment") label(2 "Control"))
	   
ttest age, by(treatment)
ttest educ, by(treatment)
ttest employment_rate, by(treatment)


reghdfe employment_rate treatment##post age educ, absorb(state year) cluster(state)


gen did = treatment * post

didregress (employment_rate age sex non_college_degree non_citizen non_white) (did), group(state) time(year)


gen diff_employment_rate = employment_rate if post == 0

graph box diff_employment_rate, over(treatment) ///
    title("Box Plot of Pre-Treatment Differences in Employment Rates")


	
save state_cps.dta, replace 

collapse (mean) employment_rate, by(year treatment)
gen diff_employment_rate = employment_rate[treatment == 1] - employment_rate[treatment == 0]


gen year2017 = (year == 2017)
gen year2018 = (year == 2018)
gen year2019 = (year == 2019)

gen treat_year2017 = year2017 * treatment
gen treat_year2018 = year2018 * treatment
gen treat_year2019 = year2019 * treatment


reghdfe employment_rate treat_year2017 treat_year2018 treat_year2019, absorb(state year) cluster(state)














