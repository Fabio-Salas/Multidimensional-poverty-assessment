**** OPHI Summer School 2023 ****

*********************************
*** REVIEWED IN PREVIOUS SESSIONS
*********************************

clear all
* Adjust path

*cd $path
*use template_dataprep, clear
global path_in "D:\" 	  
global path_out "D:\Estimation" // The in and out paths can be different
use "$path_in/ZWE_2019_merged_v3_dataprep.dta", clear


svyset psu [w=hhweight], strata(stratum) singleunit(scaled)
* Adjust psu, weight, and strata variables as needed

set type double // Specifies precision for decimal points; don't need to adjust

/* Disaggregation: To disaggregate, one just has to repeat the necessary 
estimation commands for all relevant subgroups in the disaggregation variable

For this dofile, you only need to manually adjust the following, all in the 
"setting parameters" section:
(1) disaggregation variables for population subgroups of interest. If these are 
not already available in the dataset, you will need to create them now, before 
running the rest of the code
(2) list of indicators
(3) poverty cutoffs
 */
 
* -----------------------------------------------------------------------------
* Set parameters
* -----------------------------------------------------------------------------

*recode hh7 (0=10), gen(region) //create a variable for each province (we'll named them regions)
* Adjust disaggregation variables

*Age variable to analyse children 
gen agecat_child=1 if hl6<18
replace agecat_child=2 if hl6>18
label var agecat_child "Age categorised (Children and adult)"
label define agecat_child 1 "Children (0-18)" 2 "Adult (18+)"
label value agecat_child agecat_child

*Generate a broader age range 
gen agecat=1 if hl6<=17 
replace agecat=2 if hl6>=18 & hl6<=39
replace agecat=3 if hl6>=40 & hl6<=59
replace agecat=4 if hl6>=60
label var agecat "Age categorised"
label define agecat 1 "0-17" 2 "18-39" 3 "40-59" 4 "60+"
label value agecat agecat


rename agecat region
global groups hh7 region


global indic hh_d_school hh_d_edgap hh_d_ict hh_d_elec hh_d_housing hh_d_overcrowd hh_d_asset hh_d_water hh_d_sanitation hh_d_cookingfuel
* Adjust list of indicators

global sel_k 25 33 50 
* Adjust selected poverty cutoffs (k)

* -----------------------------------------------------------------------------
* Uncensored Headcount Ratios: Percentage of the population deprived in each 
* indicator
* -----------------------------------------------------------------------------
/* These ratios are estimated first because they are independent of indicator 
weights and the chosen multidimensional povery cutoff */
	
foreach g of global groups {
	svy: mean $indic, over(`g')
}
	
* -----------------------------------------------------------------------------
* Setting indicator weights
* -----------------------------------------------------------------------------
/* Change the below according to the desired specification. Remember that the 
sum of weights MUST be equal to 1 or 100% */

foreach var in hh_d_elec hh_d_housing hh_d_overcrowd hh_d_asset {	
	gen	w_`var' = 1/12
	lab var w_`var' "Weight `var'"
}


foreach var in hh_d_water hh_d_sanitation hh_d_cookingfuel  {	
	gen	w_`var' = 1/9
	lab var w_`var' "Weight `var'"
}

foreach var in hh_d_school hh_d_edgap hh_d_ict{	
	gen	w_`var' = 1/9
	lab var w_`var' "Weight `var'"
}
* -----------------------------------------------------------------------------
* Weighted deprivation matrix 
* -----------------------------------------------------------------------------
/* Multiply the deprivation matrix by the weight of each indicator */ 

foreach var of global indic {
	gen	g0_w_`var' = `var' * w_`var'
	lab var g0_w_`var' "Weighted Deprivation of `var'"
}

* -----------------------------------------------------------------------------
* Counting vector
* -----------------------------------------------------------------------------
/* Generate the vector of individual weighted deprivation scores, 'c' */

egen	c_vector = rowtotal(g0_w_*)
lab var c_vector "Counting Vector"

* -----------------------------------------------------------------------------
* Identification of the poor 
* -----------------------------------------------------------------------------
/* Identify the poor at different poverty cutoffs (i.e. different k) */

forvalue k = 1(1)100 {
	gen	mdp_`k' = (c_vector >= `k'/100)
	lab var mdp_`k' "Poverty Identification with k=`k'%"
}

* -----------------------------------------------------------------------------
* Censored counting vector
* -----------------------------------------------------------------------------
/* Generate the censored counting vector of individual weighted deprivation 
score, 'c(k)', providing a score of zero if a person is not poor */

forvalue k = 1(1)100  {
	gen	cens_c_vector_`k' = c_vector
	replace cens_c_vector_`k' = 0 if mdp_`k'==0 
}

* -----------------------------------------------------------------------------
* Censored deprivation matrix 
* -----------------------------------------------------------------------------
/* Generate the censored deprivation matrix, replacing deprivations as 0 if the 
person is non-poor */

foreach k of global sel_k {
	foreach var of global indic {
		gen	g0_`k'_`var' = `var'
		replace g0_`k'_`var' = 0 if mdp_`k'==0
	}
}

*********************************
*** ROBUSTNESS COMMANDS
*********************************

* -----------------------------------------------------------------------------
* Robustness analysis. We will focus here on the stability to k values
* We will first create dataset with the test-based poverty orderings for each 
* k value
* -----------------------------------------------------------------------------

codebook region // 4 subnational regions
cap install distinct
distinct region
local n_reg=r(ndistinct) // Number of distinct subnational regions

local kval 25 33 50 // Set of plausible k-values. We will compare k=25; k=33; k=50
levelsof region, local(lev)
foreach k of local kval { 
	mat C=J(1,5,.) // Initialize Matrix
		foreach p1 of numlist 1/`n_reg' {
			foreach p2 of numlist 1/`n_reg' {
				if `p1'<`p2' { // Avoid unnecessary computations
					gen pair=1 if region==`p1'
					replace pair=2 if region==`p2'
					svy: mean cens_c_vector_`k', over(pair) // computes the MPI for each district
					mat E=[e(b)]
					*test _b[cens_c_vector_`k':1]=_b[cens_c_vector_`k':2] // Stata 15 or below
					test c.cens_c_vector_`k'@1.pair=c.cens_c_vector_`k'@2.pair  // Stata 16 or above (MPI are statistically indistinguable)
					mat B=[`p1',`p2',E[1,1...],r(p)]
					mat C=[C\B] // adds columns to matrix C
					drop pair
				}
			}
		}
	preserve //purely chosmetic
		mat colnames C=snr_1_`k' snr_2_`k' MPI_1_`k' MPI_2_`k' pvalue_`k' // assigns names to columns of matrix C
		svmat C, names(col) // Transforms matrix C in a dataset
		drop if snr_1_`k'==. & snr_2_`k'==. // We only need one observation per pair of subnational regions
		sort snr_1_`k' snr_2_`k'
		gen id=_n
		keep id snr_1_`k' snr_2_`k' MPI_1_`k' MPI_2_`k' pvalue_`k'
		order id snr_1_`k' snr_2_`k' MPI_1_`k' MPI_2_`k' pvalue_`k'
		save "$path_out/pairwise_`k'.dta", replace // change path to store matrices as wished
	restore
}

* -----------------------------------------------------------------------------
* Join k-varying datasets for analysis
* -----------------------------------------------------------------------------

clear all
global path "$path_out" // change path to where datasets were stored
cd "$path"
use pairwise_33, clear // Preferred k value. This is the baseline k-value

foreach k of numlist 25 33 50 { // This is the alternative k values that one wishes to assess
	merge 1:1 id using pairwise_`k'.dta
	drop _merge
}

* -----------------------------------------------------------------------------
* Create robustness statistics/measures
* -----------------------------------------------------------------------------

* Compute rank correlation coefficients
preserve
	keep if snr_1_33==1 // drops unncessesary observations for simple rank correlation coefficients
	sum snr_2_33
	local n_reg=r(max) // identify the number of subnational regions
	set obs `n_reg' // this line and the loop reinsert the MPI value for region 1
	foreach k of numlist 25 33 50 {
		sum MPI_1_`k' // recover MPI value for region 1
		local reg1=r(mean)
		replace MPI_2_`k'=`reg1' if _n==`n_reg' // append MPI value for region 1 with the rest of MPI values
		replace snr_2_`k'=1 // Identify the newly recovered MPI value
	}
	ktau MPI_2_25 MPI_2_33 MPI_2_50
	spearman MPI_2_25 MPI_2_33 MPI_2_50
restore
		
foreach k of numlist 25 33 50 {
	gen diff_`k'=MPI_1_`k'-MPI_2_`k' // Creates difference between MPI values
	gen sig_poorer_`k'=1 if pvalue_`k'<0.05 //we can establish a significant poverty ordering
	replace sig_poorer_`k'=0 if pvalue_`k'>=0.05 //we cannot stablish a significant poverty ordering
}

gen rob_all_k=1 if sig_poorer_33==sig_poorer_25 ///
				   & sign(diff_33)==sign(diff_25) ///
				   & sig_poorer_33==sig_poorer_50 ///
				   & sign(diff_33)==sign(diff_50) 
			   
replace rob_all_k=0 if (sig_poorer_33!=sig_poorer_25) ///
					  | (sign(diff_33)!=sign(diff_25)) ///
					  | (sig_poorer_33!=sig_poorer_50) ///
					  | (sign(diff_33)!=sign(diff_50)) 
					  
gen rob_sig_k=1 if sig_poorer_33==sig_poorer_25 ///
				    & sign(diff_33)==sign(diff_25)  ///
					& sig_poorer_33==sig_poorer_50 ///
				   & sign(diff_33)==sign(diff_50) & sig_poorer_33==1
				   
replace rob_sig_k=0 if ((sig_poorer_33!=sig_poorer_25) ///
					  | (sign(diff_33)!=sign(diff_25)) ///
					  | (sig_poorer_33!=sig_poorer_50) ///
					  | (sign(diff_33)!=sign(diff_50))) & sig_poorer_33==1 				   

tab sig_poorer_33 // Significant at baseline
tab rob_all_k // Robust taking into account all possible comparisons
tab rob_sig_k // Robust taking into account only comparisons that are robust at baseline



test c.cens_c_vector_`k'@1.pair=c.cens_c_vector_`k'@2.pair
