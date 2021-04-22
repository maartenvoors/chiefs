*************************************************
/*
	Replication file for
	
	Voors, M., T. Turley, E. Bulte, A. Kontoleon, and J. List
	Chief for a Day! Participatory Development and Elite Capture in Sierra Leone, 
	Management Science
	
*/

*************************************************
*************	HOUSEKEEPING		*************
*************************************************

	* STATA 13
	* cd "ENTER YOUR PATH TO data FOLDER"
	set more off
	
	* install eststo
	* install mhtexp 
	* for List, Shaikh, Xu (2016) MULTIPLE HYPOTHESIS TESTING IN EXPERIMENTAL ECONOMICS, NBER WORKING PAPER SERIES, Working Paper 21875
	* http://www.nber.org/papers/
	
*************************************************
***********			ANALYSIS		*************
*************************************************

********************************
*TABLE 1. DESCRIPTIVE STATISTICS
********************************

* copy output and edit 

** Panel A
* Project type (56 villages)
	use "chiefs_data.dta", clear 
	
	foreach var in construction mosque barri ghouse value_US late {
	preserve
	include copylabels.do
	collapse `var'  treat, by(vill_id) 	
	include attachlabels.do
	eststo Difference: estpost ttest `var', by (treat)
	ttest `var', by (treat)
	matrix sd1=r(sd_1)
	matrix sd2=r(sd_2)
	matrix colnames sd1=`var'
	matrix colnames sd2=`var'
	estadd matrix sd1
	estadd matrix sd2
	esttab Difference using "../Tables/Table1.rtf", replace label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0))" "sd1(par) sd2(par) p(par)") collabels("Chief (SD)" "Committee (SD)" "Difference (p-value)" "Obs villages") noobs nonote nomtitle nonumber ///
			addnotes("Data source: (b) Endline data, 56 villages")
	eststo clear
	matrix drop _all
	restore
	}

** Panel B
	*baseline respondent data (34 villages)
	use "otherdata/baseline_hh.dta", clear
		foreach var in bl_sex bl_age bl_mende bl_muslim bl_chief {	
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' treat, cluster(vill_id)
		replace diff_`var'=_b[t]
		replace se_`var'=_se[t]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var', by(treat)
		ttest `var', by (treat)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf",append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chief (SD)" "Committee (SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
				addnotes("Standard errors clustered at village level. Data source: (a) Baseline survey data, 34 villages")
	eststo clear
	matrix drop _all
	}
	
	*endline responent time invariant data
	use "chiefs_data.dta", clear
	
	foreach var in male age stranger rice_ac {
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' treat, cluster(vill_id)
		replace diff_`var'=_b[t]
		replace se_`var'=_se[t]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var', by(treat)
		ttest `var', by (treat)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf",append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chief (SD)" "Committee (SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
			addnotes("Standard errors clustered at village level. Data source: (b) Endline data, 56 villages")
	eststo clear
	matrix drop _all
	}
	
** Panel C: community level data
	* chiefly households
	use "otherdata/baseline_hh.dta", clear
	
	collapse (sum) hhchf_sum =bl_chief, by(vill_id)
	save "temp/chiefbl.dta", replace

	use "chiefs_data.dta", clear
	merge m:1 vill_id using "temp/chiefbl.dta"
	
	preserve	
		collapse hhchf_sum treat, by(vill_id)
	la var hhchf_sum "Number of Chief Households"
	
	foreach var in hhchf_sum{	
		eststo Difference: estpost ttest `var', by (treat)
		ttest `var', by (treat)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
	esttab Difference using "../Tables/Table1.rtf", append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0))" "sd1(par) sd2(par) p(par)") collabels("Chief (SD)" "Committee (SD)" "Difference (p-value)" "Obs villages") noobs nonote nomtitle nonumber ///
			addnotes("Data source: (a) Baseline survey data, 34 villages")
	eststo clear
	matrix drop _all
		}
	restore

	* non stranger households, village size, distance to chiefdom headquarter town, social capital (2010 survey (n=41))
	use "chiefs_data.dta", clear
	preserve
	
	include copylabels.do
		collapse treat cfam hh_est chftown_dist soccapbl, by (vill_id)
	include attachlabels.do

	foreach var in cfam hh_est chftown_dist soccapbl {
		eststo Difference: estpost ttest `var', by (treat)
		ttest `var', by (treat)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
	esttab Difference using "../Tables/Table1.rtf", append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0))" "sd1(par) sd2(par) p(par)") collabels("Chief (SD)" "Committee (SD)" "Difference (p-value)" "Obs villages") noobs nonote nomtitle nonumber ///
			addnotes("Data source: (b) Endline data, 56 villages")
	eststo clear
	matrix drop _all
		}
	restore 	


** Panel D: Chief Committee vs Villager Committee 
	use "chiefs_data.dta", clear

	foreach var in male age rice_ac stranger {
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' chf_vs_comm1, cluster(vill_id)
		replace diff_`var'=_b[chf_vs_comm1]
		replace se_`var'=_se[chf_vs_comm1]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var', by(chf_vs_comm1)
		ttest `var', by (chf_vs_comm1)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf", append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chiefs (SD)" "Villagers (SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
			addnotes("Standard errors clustered at village level. Data source: (b) Endline data, 56 villages")
	eststo clear
	matrix drop _all
	}
		
	drop diff* se* p*
	
** Panel E: Chiefs vs Villager Committee 
	
	foreach var in male age rice_ac stranger {
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' chief_vs_avcomm, cluster(vill_id)
		replace diff_`var'=_b[chief_vs_avcomm]
		replace se_`var'=_se[chief_vs_avcomm]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var', by(chief_vs_avcomm)
		ttest `var', by (chief_vs_avcomm)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf",append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chief (SD)" "Villagers (SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
				addnotes("Standard errors clustered at village level. Data source: (b) Endline data, 56 villages")
	eststo clear
	matrix drop _all
		}
		
	drop diff* se* p*

** Panel F: differences strangers / non strangers in committee		
	
	eststo clear
	foreach var in male age rice_ac {
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' stranger if chf_vs_comm1==1, cluster(vill_id)
		replace diff_`var'=_b[stranger]
		replace se_`var'=_se[stranger]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var' if chf_vs_comm1==1, by(stranger)
		ttest `var' if chf_vs_comm1==1, by (stranger)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf",append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Non stranger (SD)" "Stranger (SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
				addnotes("Standard errors clustered at village level. Data source: (b) Endline data, 56 villages")
	eststo clear
	matrix drop _all
		}

** Panel G: Chiefs vs Villagers (Bulte et al 2015 data)
	use "otherdata/bulte2017.dta", clear
	
	include copylabels.do
	collapse math_score pict12_score school_years trader01 chief01, by(grouploc_id id1)
	include attachlabels.do

	* math score, picture score, education
	eststo clear
	foreach var in math_score pict12_score school_years trader01 {
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' chief01, cluster(grouploc_id)
		replace diff_`var'=_b[chief01]
		replace se_`var'=_se[chief01]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var', by(chief01)
		ttest `var', by (chief01)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf", append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chiefs (SD)" "Villagers(SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
					addnotes("Standard errors clustered at village level. Data source: (c) Data from Bulte et al (2015), 33 villages")
		eststo clear
		matrix drop _all
		}

** Panel H: Chiefs vs Villagers
	* Primary income source and arabic education (2010 survey data)
	use "otherdata/hhsurvey2010.dta", clear
	
	foreach var in agric arabic{
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg `var' chief if id==1 & FECEndline==1, cluster(vcode)
		replace diff_`var'=_b[chief]
		replace se_`var'=_se[chief]
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=`var'
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=`var'
		eststo Difference: estpost ttest `var' if id==1 & FECEndline==1, by(chief)
		ttest `var' if id==1 & FECEndline==1, by (chief)
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=`var'
		matrix colnames sd2=`var'
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf", append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chiefs (SD)" "Villagers(SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
				addnotes("Standard errors clustered at village level. Data source: (d) Household Survey 2010, 41 villages, overlapping with FEC sample")
	eststo clear
	matrix drop _all
	}

	* education differences (Casey et al 2012 data)	
	use "otherdata/GBF_2005_HH.dta", clear	
		
		* people from chiefly family
		ta s1cq5

		* limit sample to hh heads 
		keep if  hhid1==1 
		keep if s2bd1==1 

		* education 
		gen educ = 0
		replace educ = 1 if s2bl1>0
		replace educ = . if s2bl1==. 
		replace educ = 0 if s2bl1==15 
		la var educ "Any formal schooling (1=yes)"

		* test difference any education household head for hh who can stand for chief vs not
		* test differences any education for hheads in leadership positions and not
		gen leader = 0
			replace leader = 1 if s2bk21 > 0 & s2bk21 !=.
			replace leader = 1 if s2bk31 > 0 & s2bk31 !=.
			replace leader = 1 if s2bk41 > 0 & s2bk41 !=.
		recode leader (0=2)
		
	foreach var in leader s1cq5 {
		matrix pvalues = J(1, 1, .)
		matrix clusters = J(1, 1, .)
		gen diff_`var'=.
		gen se_`var'=.
		gen p_`var'=.
		reg educ `var', cluster(id_vill)
		replace diff_`var'=_b[`var']
		replace se_`var'=_se[`var']
		replace p_`var'=2*(1-normal(abs(diff_`var'/ se_`var')))
		local pvalue=p_`var'
		local cluster=e(N_clust)
		matrix pvalues[1,1]=`pvalue'
		matrix colnames pvalues=educ
		matrix clusters[1,1]=`cluster'
		matrix colnames clusters=educ
		eststo Difference: estpost ttest educ, by(`var')
		ttest educ, by (`var')
		matrix sd1=r(sd_1)
		matrix sd2=r(sd_2)
		matrix colnames sd1=educ
		matrix colnames sd2=educ
		estadd matrix sd1
		estadd matrix sd2
		estadd matrix pvalues
		estadd matrix clusters
	esttab Difference using "../Tables/Table1.rtf",append label cells("mu_1 (fmt(2)) mu_2 b count(fmt(0)) clusters(fmt(0))" "sd1(par) sd2(par) pvalues(par)") collabels("Chiefs (SD)" "Villagers (SD)" "Difference (p-value)" "Obs households" "Obs villages") noobs nonote nomtitle nonumber ///
			addnotes("Standard errors clustered at village level. Data source: (e) Casey et al replication files available through Harvard Dataverse (*) households that can stand for Chief vs households that cannot stand for Chief.")
		eststo clear
		matrix drop _all
		}

***************************************
*TABLE 2A, AFE OUTCOMES, AID DIVERSION
***************************************

use "chiefs_data.dta", clear
	
	global rhs "hh_est chftown_dist value_US late" 
	
	preserve
	include copylabels.do
			collapse valdiff sidepr sidemat treat $rhs, by(vill_id)
	include attachlabels.do
	
	*PANEL A
	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat, robust
	}	
	
	esttab using "../Tables/Table2.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table 2A. Project Outcomes, Aid diversion NO CONTROLS") nonotes keep(treat) ///
			addnotes("OLS regressions. Robust standard errors in parentheses" "* p < 0.10, ** p < 0.05, *** p < 0.01")

	* MHT corrections (not reported in paper)
	* mhtexp valdiff sidepr sidemat, treatment(treat)
	* See remarkts in List, Shaikh, Xu (2016) MULTIPLE HYPOTHESIS TESTING IN EXPERIMENTAL ECONOMICS, NBER WORKING PAPER SERIES, Working Paper 21875

	*PANEL B
	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean= r(mean)
	*quietly: summarize `var'
	scalar min=r(min)
	estadd scalar min=min
	scalar max=r(max)
	estadd scalar max=max
	}
	
	esttab using "../Tables/Table2.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table 2A. Project Outcomes, Aid diversion WITH CONTROLS") nonotes keep(treat) ///
			addnotes("OLS regressions. Robust standard errors in parentheses. Regression includes village size , distance to Chiefdom headquarter town and project grant value (USD) (except for column (1)) and NGO performance." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))

	restore

***************************************
*TABLE 2B ATE OUTCOMES, IMPLEMENTION
***************************************

use "chiefs_data.dta", clear

	preserve
	include copylabels.do
		collapse treat end_start end_mcomplete constructed maintained $rhs, by(vill_id)
	include  attachlabels.do

	*PANEL A			
	eststo clear
	foreach var in end_start end_mcomplete constructed maintained {
	eststo: reg `var' treat, robust
	}

	* MHT corrections (not reported in paper)
	* mhtexp end_start end_mcomplete constructed maintained, treatment(treat)
	
	restore 	
	
	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	eststo: reg `var' treat, robust cluster(vill_id) 
	}
	
	* MHT corrections (not reported in paper)
	* mhtexp lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP,  treatment(treat)
	
	esttab using "../Tables/Table2.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table 2B. Project Outcomes, Implementation NO CONTROLS") nonotes keep(treat) ///
			addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10)" "* p < 0.10, ** p < 0.05, *** p < 0.01")

	*PANEL B
	preserve
	include copylabels.do
		collapse treat end_start end_mcomplete constructed maintained $rhs, by(vill_id)
	include  attachlabels.do

	eststo clear
	foreach var in end_start end_mcomplete constructed maintained {
	eststo: reg `var' treat $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}

	* mean chief group
	su end_start end_mcomplete constructed maintained if treat==0 
	
	restore
	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	qui eststo: reg `var' treat $rhs, robust cluster(vill_id) 
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	
	* mean chief group
	su lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP if treat==0   
	
	esttab using "../Tables/Table2.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table 2B. ATE Project Outcomes, Implementation, PANEL B: WITH CONTROLS") nonotes keep(treat) ///
			addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10). Regression in Panel B include controls: village size, distance to Chiefdom headquarter town and project grant value (USD) and NGO performance." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))


***************************************
*TABLE 3A, INTERACTION EFFECTS, COMMITTEE CHIEF POWER, AID DIVERSION
***************************************

use "chiefs_data.dta", clear

	global inter1 "cfam tcfam" 
	global rhs "hh_est chftown_dist value_US late" 
	
	* PANEL A
	preserve
	include copylabels.do
		collapse treat $inter1 valdiff sidepr sidemat $rhs, by(vill_id)
	include attachlabels.do

	reg 
	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat $inter1, robust
	}
	restore	
	esttab using "../Tables/Table3.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table 3A, Heterogeneity Chief Power, Aid diversion, NO CONTROLS") nonotes keep(treat $inter1) ///
			addnotes("OLS regressions. Robust standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01")

	* PANEL B		
	preserve
	include copylabels.do
		collapse treat $inter1 valdiff sidepr sidemat $rhs, by(vill_id)
	include attachlabels.do

	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat $inter1 $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	restore

	esttab using "../Tables/Table3.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table 3A, Heterogeneity Chief Power, Aid diversion WITH CONTROLS") nonotes keep(treat $inter1) ///
			addnotes("OLS regressions. Robust standard errors in parentheses. Regressions in Panel B include village size, distance to Chiefdom headquarter town, project value (USD) (except for column (1)) and NGO performance" "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))

***************************************			
*TABLE 3B, INTERACTION EFFECTS, COMMITTEE CHIEF POWER, IMPLEMENTATION
***************************************
	
	* PANEL A
	preserve
	include copylabels.do
		collapse treat $inter1 end_start end_mcomplete constructed maintained, by(vill_id)
	include attachlabels.do

	eststo clear
	foreach var in end_start end_mcomplete constructed maintained {
	eststo: reg `var' treat $inter1, robust
	}
	restore 
	
	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	eststo: reg `var' treat $inter1, robust cluster(vill_id) 
	}
	esttab using "../Tables/Table3.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table 3B, Heterogeneity Chief Power, Implementation NO CONTROLS") nonotes keep(treat $inter1) ///
			addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10). * p < 0.10, ** p < 0.05, *** p < 0.01")

	* PANEL B
	preserve
	include copylabels.do
		collapse treat $inter1 end_start end_mcomplete constructed maintained $rhs, by(vill_id)
	include attachlabels.do

	eststo clear
	foreach var in end_start end_mcomplete constructed maintained {
	eststo: reg `var' treat $inter1 $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	restore 

	* mean chief group
	*su end_start end_mcomplete constructed maintained if treat==0 
	
	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	eststo: reg `var' treat $inter1 $rhs, robust cluster(vill_id) 
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}

	* mean chief group
	*su lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP if treat==0 

	esttab using "../Tables/Table3.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table 3B, Heterogeneity Chief Power, Implementation WITH CONTROLS") nonotes keep(treat $inter1) ///
			addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10). Regressions on Panel B include village size, distance to Chiefdom headquarter town, project value (USD) and NGO performance." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))


			
******************************************************************************************************************************************************			
														*** APPENDIX **
******************************************************************************************************************************************************			

***************************************		
*Table A1. Project Outcomes, Cash requested without controls
***************************************		
	
	use "chiefs_data.dta", clear
	
	global rhs "hh_est chftown_dist value_US late" 
	
	*cash percentage
	gen cash_gfp_p = ((cash_gfp+1)/value)*100
			label var cash_gfp_p "Percent of grant requested as cash" 
	
	preserve
	include copylabels.do
		collapse cash_gfp_p $rhs treat, by(vill_id)
	include  attachlabels.do
	
	*PANEL A
	eststo clear
	foreach var in cash_gfp_p {
	eststo: reg `var' treat, robust
	}
		
	su cash_gfp_p if treat==0
	
	esttab using "../Tables/AppTables.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A1. Project Outcomes, Cash Requested, no controls") nonotes keep(treat) ///
			addnotes("OLS regressions. Robust standard errors in parentheses." "* p < 0.10, ** p < 0.05, *** p < 0.01")
	
	*PANEL B
	eststo clear
	foreach var in cash_gfp_p {
	eststo: reg `var' treat $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A1. Project Outcomes, Cash Requested, with controls") nonotes keep(treat) ///
			addnotes("OLS regressions. Robust standard errors in parentheses. Regressions in Panel B include controls: village size, distance to Chiefdom headquarter town and project value (USD) and NGO performance." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))

	restore
	
***************************************		
*Table A2. Project Outcomes, Implementation Midline
***************************************		

	use "chiefs_data.dta", clear

	global rhs "hh_est chftown_dist value_US late" 

	preserve
	include copylabels.do
		collapse mid_start $rhs treat, by(vill_id)
	include  attachlabels.do
	
	*PANEL A
		eststo clear
		la var mid_start "Project started midline" 
		foreach var in mid_start  {
		eststo: reg `var' treat, robust
		}
		
		esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A2. Project Outcomes, Implementation Midline, no controls") nonotes keep(treat) ///
				addnotes("OLS regressions. Robust standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01")
	
	*PANEL B
		eststo clear
		la var mid_start "Project started midline" 
		foreach var in mid_start  {
		eststo: reg `var' treat $rhs, robust
		quietly: summarize `var' if treat==0 
		estadd scalar mean=r(mean)
		quietly: summarize `var'
		estadd scalar min=r(min)
		estadd scalar max=r(max)
		}
		
		su mid_start if treat==0	

	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table A2. Project Outcomes, Implementation Midline, with controls") nonotes keep(treat) ///
				addnotes("OLS regressions. Robust standard errors in parentheses. Regression includes village size, distance to Chiefdom headquarter town and project value (USD) and NGO performance.* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))

		restore

***************************************		
*Table A3. Project Outcomes, Implementation, Alternative Satisfaction Proxies
***************************************		
	
	use "chiefs_data.dta", clear
	
	* Panel A
		eststo clear
		foreach var in benefit_GFP report 	farmchange 	foresthealthy 	satisfy_organise 	satisfy_meterials usebenefit {
		eststo: reg `var' treat, robust cluster(vill_id)  
		}
		esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A3. Project Outcomes, Implementation, Alternative Satisfaction Proxies, no controls") nonotes keep(treat ) ///
				addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level. * p < 0.10, ** p < 0.05, *** p < 0.01")

	* Panel B
		eststo clear
		foreach var in benefit_GFP report 	farmchange 	foresthealthy 	satisfy_organise 	satisfy_meterials usebenefit {
		eststo: reg `var' treat $rhs, robust cluster(vill_id) 
		quietly: summarize `var' if treat==0 
		estadd scalar mean=r(mean)
		quietly: summarize `var'
		estadd scalar min=r(min)
		estadd scalar max=r(max)
		}
		
		su benefit_GFP report 	farmchange 	foresthealthy 	satisfy_organise 	satisfy_meterials usebenefit if treat==0
		
		esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A3. Project Outcomes, Implementation, Alternative Satisfaction Proxies, with controls") nonotes keep(treat ) ///
				addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level. Regression includes village size, distance to Chiefdom headquarter town and project value (USD) and NGO performance. * p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("Observations" "Mean Chief group" "Minimum" "Maximum"))


***************************************		
*Table A4a. Heterogeneity Chief Power, Aid diversion, Human Capital controls
***************************************		
	
	use "chiefs_data.dta", clear

	global inter1 "cfam tcfam" 
	global inter2 "malecomm tmalecomm agecomm tagecomm rice_accomm trice_accomm strangercomm tstrangercomm "
	global rhs "hh_est chftown_dist value_US late" 
	
	preserve
	include copylabels.do
		collapse treat  $inter1  $inter2  valdiff sidepr sidemat $rhs, by(vill_id)
	include attachlabels.do

	*su valdiff  if treat==0
	
	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat $inter1 $inter2 $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var' if treat==0 
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	restore	
	
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table A4a. Heterogeneity Chief Power, Aid diversion, Human Capital controls") nonotes keep(treat $inter1 $inter2) ///
			addnotes("OLS regressions. Robust standard errors in parentheses. Regression includes village size, distance to Chiefdom headquarter town, project value (USD) (except for column (1)) and NGO performance and chief/villager committee gender, age, income and stranger and interactions)." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("observations" "mean Chief group" "minimum" "maximum"))

***************************************			
*Table A4b. Heterogeneity Chief Power, Implementation, Human Capital controls
***************************************

	preserve
	include copylabels.do
		collapse treat $inter1 $inter2  end_start end_mcomplete constructed maintained $rhs, by(vill_id)
	include attachlabels.do

	eststo clear
	foreach var in end_start end_mcomplete constructed maintained {
	eststo: reg `var' treat $inter1 $inter2 $rhs, robust
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	restore 
	
	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	eststo: reg `var' treat $inter1 $inter2 $rhs, robust cluster(vill_id) 
	quietly: summarize `var' if treat==0 
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A4b. Heterogeneity Chief Power, Implementation, Human Capital controls") nonotes keep(treat $inter1 $inter2) ///
			addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10). Regression includes village size, distance to Chiefdom headquarter town, project value (USD) and NGO performance, and chief/villager committee gender, age, income and stranger and interactions." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3)labels("observations" "mean Chief group" "minimum" "maximum"))

			
***************************************		
*Table A5a. Heterogeneity Chief Power, Aid diversion. Social Capital 
***************************************		

use "chiefs_data.dta", clear


* uses trust data from 2010 survey n=41
	
	global inter2 "soccapbl tsoccapbl"
	
	preserve
	include copylabels.do
		collapse treat  $inter1  $inter2  valdiff sidepr sidemat $rhs, by(vill_id)
	include attachlabels.do

	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat $inter1 $inter2 $rhs, robust
	su `var' if treat==0 & soccapbl!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	restore	
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se  label title("Table A5a. Heterogeneity Chief Power, Aid diversion. Social Capital al") nonotes keep(treat $inter1 $inter2) ///
			addnotes("OLS regressions. Robust standard errors in parentheses. Regression includes village size, distance to Chiefdom headquarter town, project value (USD) (except for column (1)) and NGO performance, social capital and social capital*commitee.)" "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("observations" "mean Chief group" "minimum" "maximum"))


***************************************			
*Table A5b. Heterogeneity Chief Power, Aid diversion. Social Capital 
***************************************
		
	global inter2 "soccapbl tsoccapbl"
	
	preserve
	include copylabels.do
		collapse treat $inter1 $inter2  end_start end_mcomplete constructed maintained $rhs, by(vill_id)
	include attachlabels.do

	eststo clear
	foreach var in end_start end_mcomplete constructed maintained {
	eststo: reg `var' treat $inter1 $inter2 $rhs, robust
	su `var' if treat==0 & soccapbl!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	restore 
	
	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	eststo: reg `var' treat $inter1 $inter2 $rhs, robust cluster(vill_id) 
	su `var' if treat==0 & soccapbl!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A5b. Heterogeneity Chief Power, Aid diversion. Social Capital ") nonotes keep(treat $inter1 $inter2) ///
			addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10). Regression includes village size, distance to Chiefdom headquarter town, project value (USD) and NGO performance, social capital and social capital*commitee." "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("observations" "mean Chief group" "minimum" "maximum"))

						
			
**************************************		
*Table A6a. Heterogeneity Chief Power, Aid diversion. Alternative Chief Power Proxy (subsample)
***************************************		
	* How many HH can stand for chiefs?
	* Baseline asks "5. Family could have a chief?"

	use "otherdata/baseline_hh.dta", clear
	
	collapse (sum) hhchf_sum =bl_chief (mean) hhchf_fr = bl_chief, by(vill_id)
	sort vill_id 
	la var hhchf_sum "Number of hh can produce chief"
	la var hhchf_fr "Fraction of hh can produce chief"
	save "temp/chiefbl.dta", replace

	use "chiefs_data.dta", clear
	
	preserve 
		collapse stranger, by (vill_id)
		ren stranger stranger_fr
		merge 1:1 vill_id using "temp/chiefbl.dta"
		la var stranger "Fraction stranger hh"
		reg  hhchf_fr stranger_fr /* the correlation is large -0.5, pval=0.01) */
		su hhchf_fr stranger_fr
	restore 
	
	* now repeat heterogeneity with this subsample 
	sort vill_id 
	merge vill_id using "otherdata/rulingfam.dta"
	drop _merge
	sort vill_id 
	merge vill_id using "temp/chiefbl.dta"
	sort vill_id hh_id
	gen thhchf_sum = hhchf_sum * treat
	la var thhchf_sum "Chief families * Committee"

	global inter "hhchf_sum thhchf_sum"
	global rhs "hh_est chftown_dist value_US late" 

	preserve
	include copylabels.do
	collapse treat $inter valdiff sidepr sidemat $rhs, by(vill_id)
	include attachlabels.do
	
	su hhchf_sum if treat==1
	su hhchf_sum if treat==0
	reg hhchf_sum treat, robust
	
	eststo clear
	foreach var in valdiff sidepr sidemat {
	eststo: reg `var' treat $inter $rhs, robust
	su `var' if treat==0 & hhchf_sum!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	su valdiff sidepr sidemat if treat==0 & hhchf_sum!=.
	
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A6a. Heterogeneity Chief Power, Aid diversion. Alternative Chief Power Proxy (subsample)") nonotes  keep (treat $inter) ///
		addnotes("OLS regressions. Robust standard errors in parentheses. Regression includes village size, distance to Chiefdom headquarter town and project value (USD) (except for column (1)) and NGO performance" "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("observations" "mean Chief group" "minimum" "maximum"))

	restore 

***************************************		
*Table A6b. Heterogeneity Chief Power, Aid diversion. Alternative Chief Power Proxy (subsample)
***************************************		

	preserve
	include copylabels.do
	collapse treat $inter end_start end_mcomplete constructed maintained hh_est chftown_dist value_US late, by(vill_id)
	include attachlabels.do
	
	eststo clear
	foreach var in end_start end_mcomplete  {
	eststo: reg `var' treat $inter $rhs, robust
	su `var' if treat==0 & hhchf_sum!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}

	foreach var in  constructed maintained {
	eststo: reg `var' treat $inter $rhs, robust
	su `var' if treat==0 & hhchf_sum!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}

	su end_start end_mcomplete constructed maintained if treat==0 & hhchf_sum!=.
	restore 

	foreach var in lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP {
	eststo: reg `var' treat $inter $rhs, robust cluster(vill_id) 
	su `var' if treat==0 & hhchf_sum!=.
	estadd scalar mean=r(mean)
	quietly: summarize `var'
	estadd scalar min=r(min)
	estadd scalar max=r(max)
	}
	
	su lnhours_hh satisfy_man benefit betteroff satisfy_GFP support_GFP if treat==0 & hhchf_sum!=.
	
	esttab using "../Tables/AppTables.rtf", append star(* 0.10 ** 0.05 *** 0.01) b(3) se label title("Table A6b. Heterogeneity Chief Power, Aid diversion. Alternative Chief Power Proxy (subsample)") nonotes keep (treat $inter) ///
	addnotes("OLS regressions. Robust standard errors in parentheses, clustered at village level for Column (5)-(10). Regression includes village size, distance to Chiefdom headquarter town and project value (USD) and NGO performance" "* p < 0.10, ** p < 0.05, *** p < 0.01") stats(N mean min max, fmt(a3) labels("observations" "mean Chief group" "minimum" "maximum"))

***********
*Claims in paper
***********
	*can produce chief p. 9
	use "otherdata/baseline_hh.dta", clear
		ta bl_chief
		collapse (sum) hhchf_sum =bl_chief (mean) hhchf_fr = bl_chief, by(vill_id)
		sort vill_id 
		la var hhchf_sum "Number of hh can produce chief"
		la var hhchf_fr "Fraction of hh can produce chief"
		ta hhchf_fr
	*share of strangers p. 10
	use "chiefs_data.dta", clear
	preserve 
		collapse stranger, by(vill_id)
		ren stranger stranger_fr
				la var stranger_fr "Fraction stranger hh"
		su stranger_fr
	restore
	*correlation chief power proxies p. 10
	preserve 
		collapse stranger, by (vill_id)
		ren stranger stranger_fr
		sort vill_id
		merge vill_id using "temp/chiefbl.dta"
		la var stranger_fr "Fraction stranger hh"
		reg  hhchf_fr stranger_fr /* the correlation is large -0.5, pval=0.01) */
		su hhchf_fr stranger_fr
	restore 
	*grant value p. 11
	preserve
		collapse value value_US, by(vill_id)
		su value
		su value_US
	restore
	*construction project p. 11
	preserve
		collapse construction, by(vill_id)
		su construction
	restore
	*cash grant p. 21
	preserve
		gen cash_gfp_p = ((cash_gfp+1)/value)*100
			label var cash_gfp_p "Percent of grant requested as cash" 
		collapse treat cash01 cash_US cash_gfp_p, by(vill_id)
		ttest cash01, by(treat)
		ttest cash_US, by(treat)
		ttest cash_gfp_p, by(treat)
	restore
	*project never implemented p. 24
	preserve
		collapse end_start, by(vill_id)
		su end_start
	restore
