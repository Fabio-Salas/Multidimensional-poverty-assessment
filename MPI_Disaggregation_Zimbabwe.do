**** OPHI Summer School 2023 ****
**** Working group session on disaggregation ****

clear all
* Adjust path

*cd $path
*use template_dataprep, clear

global path_in "D:\" 	  
global path_out "D:\Estimation" // The in and out paths can be different
use "$path_in\ZWE_2019_merged_v3_dataprep.dta", clear

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

/*We define a variable for area of resdidence (Urban/Rural)
gen area=hh6
replace area=0 if hh6==2
lab def ar 0 "Rural" 1 "Urban"
label var area "Area of residence: Urban or Rural"
lab val area ar
*/

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

tab hh7 //is the variable that identifies the province 

global groups hh6 hh7 agecat_child agecat
* Adjust disaggregation variables

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

* -----------------------------------------------------------------------------
* Calculate MPI, H, A
* -----------------------------------------------------------------------------
/* Calculate the Multidimensional Poverty Index/Adjusted Headcount Ratio (MPI/M0),
the Incidence/Headcount Ratio (H), and the Average Intensity of Poverty among 
the Poor (A) for selected poverty cutoff(s) (k). */

foreach g of global groups {
    display("Group: `g'")
	foreach k of global sel_k {
	display("k: `k'")
		svy: mean cens_c_vector_`k', over(`g') // MPI for specific k cutoffs (one or more)
	display("________________________________")
	}

	foreach k of global sel_k {
	display("k: `k'")
		svy: mean mdp_`k', over(`g') // H for specific k cutoffs (one or more)
	}
	
	foreach k of global sel_k {
	display("k: `k'")
		svy, subpop(mdp_`k'): mean cens_c_vector_`k', over(`g') // A for specific k cutoffs (one or more)
	}
}
	
* -----------------------------------------------------------------------------
* Censored Headcount Ratios: Percentage of the population poor and deprived in
* each indicator
* -----------------------------------------------------------------------------
/* Calculate the censored headcount ratios, as the mean of each column of the 
censored deprivation matrix */

foreach g of global groups {
	foreach k of global sel_k {
		svy: mean g0_`k'_*, over(`g')
	}
}
	
* -----------------------------------------------------------------------------
* Population shares
* -----------------------------------------------------------------------------
/* Calculate the percentage of the population in each group of the 
disaggregation variable */
	
foreach g of global groups {
	svy: prop `g'
}

* -----------------------------------------------------------------------------
* Absolute and Percentage Contributions
* -----------------------------------------------------------------------------
/* Absolute contributions are each indicator's contribution to the MPI level 
(calculated as the censored headcount ratio * weight). Percentage contributions 
are the % of the MPI that each indicator contributes (calculated as the censored 
headcount ratio * weight divided by the MPI). Note that all percentage 
contributions MUST sum to 1 or 100% */

/* Create variables for each censored headcount ratio and k cutoff */
local gn=0 // group number counter
foreach g of global groups {
	local gn=`gn'+1 // update group counter
	levelsof `g', local(lev)
	foreach l of local lev {
		foreach k of global sel_k {
			foreach var of global indic {
				svy: mean g0_`k'_`var' if `g'==`l'
				gen c_`k'_g`gn'_`var'_`l'=_b[g0_`k'_`var']
			}
		}
	}
}

/* Create variables for MPI at each k cutoff */
local gn=0 // group number counter
foreach g of global groups {
	local gn=`gn'+1 // update group counter
	levelsof `g', local(lev)
	foreach l of local lev {	
		foreach k of global sel_k {
			svy: mean cens_c_vector_`k' if `g'==`l'
			gen M_`k'_g`gn'_`l'=_b[cens_c_vector_`k']
		}
	}
}

/* Contributions (absolute and percentage) */
local gn=0 // group number counter
foreach g of global groups {
	local gn=`gn'+1 // update group counter
	levelsof `g', local(lev)
	foreach l of local lev {
		foreach var of global indic {
			foreach k of global sel_k {
				gen	pctr_`k'_g`gn'_`var'_`l' = c_`k'_g`gn'_`var'_`l'*w_`var'/M_`k'_g`gn'_`l'
				lab var pctr_`k'_g`gn'_`var'_`l'  "Relative contribution of Indicator `var' to M0 for k-value `k'"
				gen	actr_`k'_g`gn'_`var'_`l' = c_`k'_g`gn'_`var'_`l'*w_`var'
				lab var actr_`k'_g`gn'_`var'_`l'  "Absolute contribution of Indicator `var' to M0 for k-value `k'"
			}
		}
	}
}
	
/* Test and display results */
local gn=0 // group number counter
foreach g of global groups {
	local gn=`gn'+1 // update group counter
	foreach k of global sel_k {
		sum pctr_`k'_g`gn'* if _n==1 // Note: can change "sum" to "fsum" to better see variable names
		sum actr_`k'_g`gn'* if _n==1 // Note: can change "sum" to "fsum" to better see variable names
	}
}

/* Note: if you want to use fsum but it doesn't run, install using: 
"ssc install fsum" */

* -----------------------------------------------------------------------------
* Generating Output Dataset
* -----------------------------------------------------------------------------
/* Creating a DTA file with all relevant indicators for each subgroup as 
variables. The structure of this dataset allows us to combine it with other 
results to be produced subsequently. It can all be run together and does not
need manual adjustment */

preserve
	use "$path_out\ag_nat", clear
	save "$path_out\disag_nat_data.dta", replace // This data will contain national and subgroup estimates
restore 

foreach g of global groups { 
	local estim b se lb ub // We will estimate mean points (b), standard errors (se), and confidence interval 	lower and upper bounds (lb ub) for all results
	local n_ind: list sizeof global(indic) // n_ind contains the number of indicators
	levelsof `g', local(lev) // Levels of the subgroup are stored here
	local lbe : value label `g' // lbe contains the labels of the subgroup variables
	local l_n=0 // Counter of group level 
	foreach l of local lev { // We loop the same commands for all levels
		local l_n=`l_n'+1 //Update group level counter
		local l_lab: label `lbe' `l' // Store subgroup label

       /* Uncensored headcount ratios */
		foreach e of local estim {
			mat `e'=J(1,`n_ind',.) // Initialize row vectors
		}

		/* Population shares */
		local i=0 // Indicator counter
			
		local varn_b = ""
		local varn_se = ""
		local varn_lb = ""
		local varn_ub = ""
		foreach var of global indic {  
			local i=`i'+1 // Update indicator counter
			local aux=substr("`var'",6,.) // Extract indicator name
			svy: mean `var' if `g'==`l'
			mat E=r(table)
			mat b[1,`i'] = E[1,1]*100 // Estimated coefficient
			local varn_b="`varn_b' "+"u_`aux'_b"
			mat se[1,`i'] = E[2,1]*100 // SE
			local varn_se="`varn_se' "+"u_`aux'_se"
			mat lb[1,`i'] = E[5,1]*100 // CI LB
			local varn_lb="`varn_lb' "+"u_`aux'_lb"
			mat ub[1,`i'] = E[6,1]*100 // CI UB
			local varn_ub="`varn_ub' "+"u_`aux'_ub"
		}
		mat colnames b=`varn_b'
		mat colnames se=`varn_se'
		mat colnames lb=`varn_lb'
		mat colnames ub=`varn_ub'
		mat U=[b,se,lb,ub] // Matrix to be stored and treated subsequently

		/* H for selected values */
		foreach k of global sel_k {
			foreach e of local estim {
				mat `e'=J(1,1,.) // Re-nitialize row vectors
			}
			svy: mean mdp_`k' if `g'==`l'
			mat E=r(table)
			mat b[1,1] = E[1,1]*100 // Estimated coefficient
			mat se[1,1] = E[2,1]*100 // SE
			mat lb[1,1] = E[5,1]*100 // CI LB
			mat ub[1,1] = E[6,1]*100 // CI UB
			foreach e of local estim { // Name columns for readibility 
				mat colnames `e'="H_`k'_`e'" 
			}
			mat H_`k'=[b,se,lb,ub] // Matrix to be stored and treated subsequently
		}
			
		/* A for selected values */
		foreach k of global sel_k {
			foreach e of local estim {
				mat `e'=J(1,1,.) // Re-nitialize row vectors
			}
			svy, subpop(mdp_`k'): mean cens_c_vector_`k' if `g'==`l'
			mat E=r(table)
			mat b[1,1] = E[1,1]*100 // Estimated coefficient
			mat se[1,1] = E[2,1]*100 // SE
			mat lb[1,1] = E[5,1]*100 // CI LB
			mat ub[1,1] = E[6,1]*100 // CI UB
			foreach e of local estim { // Name columns for readibility 
				mat colnames `e'="A_`k'_`e'" 
			}
			mat A_`k'=[b,se,lb,ub] // Matrix to be stored and treated subsequently
		}
			
		/* MPI for selected values */
		foreach k of global sel_k {
			foreach e of local estim {
				mat `e'=J(1,1,.) // Re-nitialize row vectors
			}
			svy: mean cens_c_vector_`k' if `g'==`l'
			mat E=r(table)
			mat b[1,1] = E[1,1] // Estimated coefficient
			mat se[1,1] = E[2,1] // SE
			mat lb[1,1] = E[5,1] // CI LB
			mat ub[1,1] = E[6,1] // CI UB
			foreach e of local estim { // Name columns for readibility 
				mat colnames `e'="MPI_`k'_`e'" 
			}
			mat M_`k'=[b,se,lb,ub] // Matrix to be stored and treated subsequently
		}
			
		/* Censored headcount ratio for selected values */
		foreach k of global sel_k {
			foreach e of local estim {
				mat `e'=J(1,`n_ind',.) // Initialize row vectors
			}

			local varn_b = ""
			local varn_se = ""
			local varn_lb = ""
			local varn_ub = ""
				
			local i=0 // Indicator counter
				
			foreach var of global indic {  
				local i=`i'+1 // Update indicator counter
				svy: mean g0_`k'_`var' if `g'==`l'
				local aux=substr("`var'",6,.) // Extract indicator name
				mat E=r(table)
				mat b[1,`i'] = E[1,1]*100 // Estimated coefficient
				local varn_b="`varn_b' "+"c_`k'_`aux'_b"
				mat se[1,`i'] = E[2,1]*100 // SE
				local varn_se="`varn_se' "+"c_`k'_`aux'_se"
				mat lb[1,`i'] = E[5,1]*100 // CI LB
				local varn_lb="`varn_lb' "+"c_`k'_`aux'_lb"
				mat ub[1,`i'] = E[6,1]*100 // CI UB
				local varn_ub="`varn_ub' "+"c_`k'_`aux'_ub"
			}
			mat colnames b=`varn_b'
			mat colnames se=`varn_se'
			mat colnames lb=`varn_lb'
			mat colnames ub=`varn_ub'
			mat C_`k'=[b,se,lb,ub] // Matrix to be stored and treated subsequently
		}
			
		/* Contributions (much easier after creating matrix R above) */
		foreach k of global sel_k {
			mat RCO_`k'=J(1,`n_ind',.) // Initialize row vectors
			mat ACO_`k'=J(1,`n_ind',.) // Initialize row vectors
			local i=0 // Indicator counter
			local varn_rb = "" // Initialize variable name
			local varn_ab = "" // Initialize variable name
			foreach var of global indic {
				local i=`i'+1 // Update indicator counter
				local aux=substr("`var'",6,.) // Extract indicator name
				local a=C_`k'[1,`i']*w_`var'/M_`k'[1,1]
				mat RCO_`k'[1,`i'] = `a'	
				local varn_rb="`varn_rb'"+" pctr_`k'_`aux'"
				local b=C_`k'[1,`i']*w_`var'/100
				mat ACO_`k'[1,`i'] = `b'	
				local varn_ab="`varn_ab'"+" actr_`k'_`aux'"
			}
			mat colnames RCO_`k'=`varn_rb'
			mat colnames ACO_`k'=`varn_ab'
		}
			
		/* Population share and sample size */
		svy: prop `g'
		mat A=r(table)
		mat S=A[1,`l_n']*100
		mat colnames S="pop_share"
		tab `g', matcell(B)
		mat UN=B[`l_n',1]
		mat colnames UN="sample_size"
			
		/* Merge all indicators and create subnational-level datasets */
		mat R=[UN,S]
		foreach k of global sel_k {
			mat R=[R,M_`k',H_`k',A_`k',C_`k',RCO_`k',ACO_`k']
		}
		mat R=[R,U]

		/* Export as line as temporary DTA file and append to national DTA */
		preserve
			clear 
			svmat R, names(col)
			gen loa="`g'"
			gen subgroup="`l_lab'"
			order loa subgroup
			keep if _n==1
			save "$path_out\temp", replace
			use "$path_out\disag_nat_data"
			append using "$path_out\temp"
			erase "$path_out\temp.dta"
			save "$path_out\disag_nat_data.dta", replace
		restore
	}
}


*We visualize the dataset
use "$path_out\disag_nat_data.dta" , clear

keep loa subgroup pop_share sample_size H_33_* MPI_33* A_33_*

ttesti 30079 0.0378973 0.006 14393 0.452 0.009 // urban vs. rural (MPI)
ttesti 30079 0.5611 0.049 14393 0.4340 0.0111 // urban vs. rural (A)

ttesti 21663 0.355 0.009 21806 0.289 0.007 // age group 1 (MPI)
ttesti 21663 0.5638 0.0051 21806 0.5456 0.0047 // age group 1 (A)


keep loa subgroup c_33_* //see the censored headcount by indicator







