/*
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
======================== OPHI Summer School 2023 =========================
-Developing an MPI for Zimbawe
-Group 7 
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
*/


*============= DEFINING INDICATORS AND DEPRIVATIONS =====================

clear all

*Import data 
use  "D:/ZWE_2019_merged_v3.dta"

svyset psu [w=hhweight], strata(stratum) singleunit(scaled)

lab def dep 0 "Non Deprived" 1 "Deprived"


* -----------------------------------------------------------------------------
* EDUCATION 
* -----------------------------------------------------------------------------

*Define the eligible age range 
lookfor age

sum hl6 //estimate statistics for age 

*====================================
* Indicator 1: Education level
*====================================

/* 
Deprived if none of the adult members (18-65) of the household have completed primary education or if when they completed primary education cannot read all the sentence of question WB14 
*/
 
gen age_adult= (hl6>11 & hl6<66) if hl6!=. 
tab age_adult, m
bysort hh_id: egen hh_age_adult = max(age_adult) //sort by household, then make sure we are counting the household who has at least one person 16 and over 
tab hh_age_adult, m //3% of people live in households with no member between 18 and 65

lookfor education //search for variables wich description refer to education
tab ed4 // Ever attended school or any Early Childhood Education programme
tab ed5a  //highest level of education attended
tab ed10a //Level of education attended current school year
tab wb14 //can read part of the sentence

tab ed5b ed5a if age_adult==1, m //crosstab to see the number of people in a given grade and level of education
tab ed5b ed5a if age_adult==1, nol m

codebook ed4, tab(20) // 2 -> never attended school or any childhood education programme

gen d_scho = 0 if age_adult==1 //we start by stating that everyone age older than 17 and younger thatn 65 is non-deprived (applicable population)
replace d_scho = 1 if ((ed5a==1 & ed5b<7) | (ed5a==0) | (ed5a==1 & ed5b==7 & ed6==1 & wb14<3)) & age_adult==1 //Zimbawes primary education takes 7 years (replace everyone who has attended primary school but has not complete 7 years of education or people which only attended ECE (Pree-school) or people that have completed primary school but cannot read completely 
replace d_scho = . if (ed5a==. & ed5b==.) & age_adult==1 //replace those with missing values
replace d_scho = . if ed5a>19 & age_adult==1  //replace people who answer "don't know" (DK) or who have missing values
replace d_scho = . if ed5b>7 & ed5b!=. & age_adult==1 //replace those who "don't know" (DK) or have inconsistent values for 
replace d_scho = 1 if ed4==2 & age_adult==1
lab var d_scho "Has not completed primary school or cannot read having completed it"
lab val d_scho dep
tab d_scho if age_adult==1, m
tab ed5b ed5a if age_adult==1 & d_scho==1, m 


lab var d_scho "Adults between 18 and 65 have not completed primary school or while completing primary school cannot read"
lab val d_scho dep
tab d_scho if age_adult==1, m
tab ed5b ed5a if age_adult==1 & d_scho==1, m //ECE: Early Childhood Education

bys hh_id: egen hh_d_school = min(d_scho) //we should count at the household level (one would indicate that everyone within the household is deprived)
tab hh_d_school, m // Sample statistics
replace hh_d_school=0 if hh_age_adult==0
lab var hh_d_school "Household deprived in Schooling Achievement"
lab val hh_d_school dep


*br hh_id ind_id hl6 ed5a ed5b d_scho hh_d_school wb14

tab hh_d_school, miss // Sample statistics
svy: tab hh_d_school, miss // Population statistics (<ind the differen

*==========================
*Indicator 2: Schooling gap 
*==========================
gen age_children= (hl6>8 & hl6<24) if hl6!=. 
tab age_children, m
bysort hh_id: egen hh_age_children = max(age_children) //we identify households with at least one children (age <24)
tab hh_age_children, m

tab ed10a if age_children==1 //Level of education attended current school year

codebook ed5b //highest grade attended at that level
codebook ed5a //highest level of education attended
tab ed5a,nolabel
tab ed5b
tab ed5b,nolabel

gen edu_all=ed5b if ed5a<5 & ed5b<8 //create a variable to account for the year of schooling
tab ed5b if ed5a==3 //lower secondary and upper secondary 
tab ed5b if ed5a==4 //post-secondary 

replace edu_all=8 if ed5a==3 & ed5b==1 //grade 8 (first year of lower secondary)
replace edu_all=9 if ed5a==3 & ed5b==2 //grade 9 (second year of lower secondary )
replace edu_all=10 if ed5a==3 & ed5b==3 //grade 10 (first year of upper secondary )
replace edu_all=11 if ed5a==3 & ed5b==4 //grade 11 (second year of upper secondary )


gen ed_gap=(hl6-edu_all)-5 if age_children==1 & ed5a<5 //education gap: Difference between age and year of schooling for children until upper secondary (0 means that the child is at the school year that corresponds to its age)

/*
If the child is 9 years (hl6=9) old he should be in the fourth year of primary school (edu_all=4), but if he is in the first year (edu_all=1), its ed_gap would be equal to 8
If the child is 17 years (hl6=17) old he should be in the last year of upper secondary (edu_all=11), but if he is in the first year of lower (edu_all=8), its ed_gap would be 4
*/


/*
Education        School/Level            Grades              Age               Years              Notes
Primary	        Primary School      	  1–7	             6–12           	  7	 
Middle	         Junior High	          8–9	             13–14	              2	           Zimbabwe Junior Certificate
Secondary	      Secondary	             10–11	             15–17	              2            General Certificate of Education: "O" Level Studies
Post-secondary	"A" Level Courses	     12–13	             15–17	              2	
*/

gen d_edgap = 0 if age_children==1 //to start, everyone age younger than 15 non-deprived
replace d_edgap=1 if ed_gap>2 & age_children==1
replace d_edgap = . if (ed_gap==.) & age_children==1 //replace those who have missing
replace d_edgap = . if ed5a>19 & age_children==1  //replace people who answer "don't know" (DK) or who have missing values
replace d_edgap = . if ed5b>7 & ed5b!=. & age_children==1 //replace those who "don't know" (DK) or have inconsistent values for 

lab var d_edgap "at least one child is over two years delayed with respect to his/her schooling grade according to age"
lab val d_edgap dep
tab d_edgap if age_children==1, m
tab ed5b ed5a if age_children==1 & d_edgap==1, m 


bys hh_id: egen hh_d_edgap = max(d_edgap) //we should count at the household level (one would indicate that at least one child)
tab hh_d_edgap, m // Sample statistics
replace hh_d_edgap=0 if hh_age_children==0
lab var hh_d_edgap "Household deprived in Children Educational Gap "
lab val hh_d_edgap dep


br hh_id ind_id hl6 ed_gap d_edgap hh_d_edgap

tab hh_d_edgap, miss // Sample statistics
svy: tab hh_d_edgap, miss // Population statistics (<ind the differen


* -----------------------------------------------------------------------------
* Indicator 3: ACCESS TO ICT
* -----------------------------------------------------------------------------


gen hh_d_ict=0
replace hh_d_ict=1 if hc11==2 & hc13==2
replace hh_d_ict=. if hc11==. | hc13==. 

lab var  hh_d_ict "Household deprived in Access to ICT"
lab val hh_d_ict dep

tab hc13 hh_d_ict, miss


// Creating variable at the household level
tabulate hh_d_ict, miss // Sample statistics

svy: tab hh_d_ict, miss // Population statistics (<ind the difference with the line above)



* ---------------------------------------------------------------
* LIVING STANDARD
* ---------------------------------------------------------------

*======================================
*   Indicator 1: Access to electricity
*======================================

/*
A household is deprived if the household does not have access to electricity*
*/

codebook hc8, tab(20)
tab hc8, m


recode hc8 (1=0) (2=0) (3=1) (9=.), gen(d_elec) //create a variable that shows the deprivation 

tab hc8 d_elec, miss

*Check missing values

bys hh_id: egen hh_d_elec = max(d_elec)
lab var  hh_d_elec "Household deprived in Access to electricity"
lab val hh_d_elec dep

// Creating variable at the household level
tabulate hh_d_elec, miss // Sample statistics
svy: tab hh_d_elec, miss // Population statistics (<ind the difference with the line above)

*======================================
*  Indicator 2:  Housing 
*======================================

*A household is deprived if the household does not furnished floor, furnished wall and furnished roof*

codebook hc4, tab(20)
codebook hc5, tab(20)
codebook hc6, tab(20)
tab hc4, m
tab hc5, m
tab hc6, m

recode hc4 (11=1) (12=1) (21=1) (31/35=0) (96=.), gen(floor)
tab hc4 floor, m

recode hc5 (11=1) (12=1) (21=1) (23=1) (24=1) (31/37=0) (96=.), gen(roof)
tab hc5 roof, m

recode hc6 (12/26=1) (31/36=0) (96=.), gen(wall)
tab hc6 wall, m


gen d_housing=0
replace d_housing=1 if floor==1 | roof==1 | wall==1
tab d_housing, m

bys hh_id: egen hh_d_housing = max(d_housing)

lab var  hh_d_housing "Household deprived in housing"
lab val hh_d_housing dep
tab hc8 hh_d_housing, miss
tab hh_d_housing, m

// Creating variable at the household level
tabulate hh_d_housing, miss // Sample statistics
svy: tab hh_d_housing, miss // Population statistics (<ind the difference with the line above)


*======================================
* Indicator 3: Overcrowding
*======================================
*A household is deprived if household have 3 or more persons per sleeping room*

codebook hc3, tab(20)
tab hc3, m
tab hh48, m
tab hh51, m

*remove children aged 5 and under from the hh48 (household size)
gen hhsize=hh48-hh51 //we are not considering the children while counting the number of persons in the household

gen roomratio=hhsize/hc3

gen d_overcrowd=0
replace d_overcrowd=1 if roomratio>=3

bys hh_id: egen hh_d_overcrowd= max(d_overcrowd)

lab var  hh_d_overcrowd "Household deprived in overcrowding"
lab val hh_d_overcrowd dep
tab hh_d_overcrowd, m

*br hh48 hh51 hhsize hc3 roomratio hh_d_overcrowd

// Creating variable at the household level
tabulate hh_d_overcrowd, miss // Sample statistics
svy: tab hh_d_overcrowd, miss // Population statistics (<ind the difference with the line above)

*=======================================
*  Indicator 4: Assets
*=======================================

/* Householders are considered non-deprived in assets if the household own more than one of each assets in each of the three categories (home assets, transport assets, land or livestock assets). */

tab hc7a

recode hc12 (2=0) (9=.), gen(mobile)
recode hc7a (2=0) (9=.), gen(telephone)
recode hc9a (2=0) (9=.), gen(television)
recode hc9b (2=0) (9=.), gen(refrigerator)
recode hc10b (2=0) (9=.), gen(bicycle)
recode hc10c (2=0) (9=.), gen(scooter)
recode hc10d (2=0) (9=.), gen(animalcart)
recode hc10e (2=0) (9=.), gen(car)
recode hc15 (2=0) (9=.), gen(agriland)
recode hc17 (2=0) (9=.), gen(livestock)

egen n_home_asset=rowtotal(mobile television bicycle)
lab var n_home_asset "home assets owned by hh"
codebook n_home_asset
ta  n_home_asset, m

gen d_asset=0
replace d_asset=1 if (n_home_asset<2 & (car==1 | bicycle==1))
replace d_asset=1 if (n_home_asset<2 & (livestock==1 | agriland==1)) 

bys hh_id: egen hh_d_asset= max(d_asset)

lab var  hh_d_asset "Household deprived in asset"
lab val hh_d_asset dep
tab hh_d_asset, m

tab hh6 hh_d_asset, col


// Creating variable at the household level
tabulate hh_d_asset, miss // Sample statistics
svy: tab hh_d_asset, miss // Population statistics (<ind the difference with the line above)



* -----------------------------------------------------------------------------
*   HEALTH      
* -----------------------------------------------------------------------------


************************************
* Indicator 1: Acces to quality drinking water
************************************

*1. Access to quality drinking water (WS1)// What is the main source of drinking water used by members of your household?

*Piped water-Agua Tubería; Dug well-Pozo; Spring-Agua de manantial; Packaged water. 

/* 
A household is non deprived if: it has piped water, dug well protected, protected spring. 
A household is deprived if: it gets water from an unprotected well, 
unprotected spring, rainwater, tanker-truck, cart with small tank, surface water, OTHER. 
*/

lookfor  water
codebook ws1, tab(20)

// Recoding
recode   ws1 (11/31=0)(32=1)(41=0)(42=1)(51/81=1)(91=0)(96=1), gen(hh_d_water)
lab var  hh_d_water "Household deprived in Access to Safe Water"
lab val hh_d_water dep
tab ws1 hh_d_water, miss

// Creating variable at the household level
tabulate hh_d_water, miss // Sample statistics
svy: tab hh_d_water, miss // Population statistics (<ind the difference with the line above)

*******************************
*        Indicator 2: Sanitation 
*******************************

*3. Sanitation (WS11)// What kind of toilet facility do members of your household usually use?

lookfor toilet
codebook ws11

// Recoding
gen hh_d_sanitation=0
replace hh_d_sanitation=1 if ws11==23 | ws11==23 | ws11==31 | ws11==95 | ws11==96 

lab var  hh_d_sanitation "Household deprived in Sanitation"
lab val hh_d_sanitation dep
tab ws11 hh_d_sanitation, miss

// Creating variable at the household level
tabulate hh_d_sanitation, miss // Sample statistics
svy: tab hh_d_sanitation, miss // Population statistics (<ind the difference with the line above)



*=======================================
* Indicator 3: Cooking fuel
*=======================================
* A household is non-deprived if the household uses
* A household is deprived if the household uses coal, charcoal, wood, crop residue, dung, woodchips, garbage/plastic,  sawdust*

codebook eu1, tab(20)
tab eu1, m

recode eu1 (1/3=0) (4/11=1) (96=1) (97=0), gen(fuelsource)

recode eu2 (1=0) (2=1) (9=.), gen(chimney)

recode eu5 (1=1) (2=0) (3=0) (4=0) (5=0) (9=.), gen(kitchen)

gen hh_d_cookingfuel=.
replace hh_d_cookingfuel=0 if fuelsource==0
replace hh_d_cookingfuel=1 if fuelsource==1
replace hh_d_cookingfuel=0 if chimney==0
lab var  hh_d_cookingfuel "Household deprived in cooking fuel"
lab val hh_d_cookingfuel dep
tab hh_d_cookingfuel, m

// Creating variable at the household level
tabulate hh_d_cookingfuel, miss // Sample statistics
svy: tab hh_d_cookingfuel, miss // Population statistics (<ind the difference with the line above)

desc hh_d_cookingfuel //number of missings values



save "D:/ZWE_2019_merged_v3_dataprep.dta", replace


*============= ASSOCIATION AND REDUNDANCY ANALYSIS ===================== 

use "D:/ZWE_2019_merged_v3_dataprep.dta", clear
* Cramer's V (Association between deprivation in schooling achievement and children educational gap)
	tab hh_d_school hh_d_edgap, V // This does not take into account survey sampling

	svy: tab  hh_d_school hh_d_edgap
	local denom = e(r)-1
	di "Cramer's V: " sqrt(e(cun_Pear)/(e(N)*`denom'))



tab2 hh_d_school hh_d_edgap hh_d_ict hh_d_elec hh_d_housing hh_d_overcrowd hh_d_asset hh_d_water hh_d_sanitation hh_d_cookingfuel, V chi2


local vlist hh_d_school hh_d_edgap hh_d_ict hh_d_elec hh_d_housing hh_d_overcrowd hh_d_asset hh_d_water hh_d_sanitation hh_d_cookingfuel
local nvar: word count `vlist'
mat V = J(`nvar', `nvar', .)
forval i = 1/`=`nvar' -1' {
   forval j = `=`i' + 1'/`nvar' {
     local v1 = word("`vlist''", `i')   
     local v2 = word("`vlist'", `j')   
     tabulate `v1' `v2', V
     mat V[`i', `j'] = r(CramersV)
     mat V[`j', `i'] = r(CramersV)
   }
}   
mat list V


tab2 hh_d_discrimination hh_d_victim, V chi2



* -----------------------------------------------------------------------------
* Programme to compute both measures for several pairs of indicators
* run NSuppa's `assoc_program.do' dofile once and execute the one-line command:
* The underlying calculations are largely explained above. This programme has
* the virtue of simplifying output that can be easily copy-pasted elsewhere
* -----------------------------------------------------------------------------

*run the assoc program

assoc [w=hhweight], dep(hh_d_s*)



/*=====================================================================
                       DEPRIVATION BUNDLES
=======================================================================*/

*Primero generamos variables operativas que nos ayuden con las estimaciones:

*variables: hh_d_school hh_d_edgap hh_d_elec hh_d_housing hh_d_overcrowd hh_d_cookingfuel hh_d_itequip hh_d_intaccess hh_d_discrimination hh_d_victim hh_d_water hh_d_health hh_d_sanitation hh_d_childmortality hh_safety hh_d_asset

gen vinteres_1=hh_d_school
gen vinteres_2=hh_d_edgap
gen vinteres_3=hh_d_elec
gen vinteres_4=hh_d_housing
gen vinteres_5=hh_d_overcrowd
gen vinteres_6=hh_d_cookingfuel
gen vinteres_7=hh_d_itequip
gen vinteres_8=hh_d_intaccess
gen vinteres_9=hh_d_discrimination
gen vinteres_10=hh_d_victim
gen vinteres_11=hh_d_water
gen vinteres_12=hh_d_health
gen vinteres_13=hh_d_sanitation
gen vinteres_14=hh_d_childmortality
gen vinteres_15=hh_safety
gen vinteres_16=hh_d_asset


*De la siguiente forma calculamos cuantas personas sufren de privaciones en la variabla j y i (simultaneamnete) para todas las combinaciones de los 24 indicadores
     
preserve
	 forvalues j=1(1)16{
		display("Begin: vinteres_`j'")
		forvalues i=1(1)16{
	   	count if vinteres_`j'==1 & vinteres_`i'==1
		                  }
	   display("End: vinteres_`j'")
	                   }
restore




*============================= VARIABLES FOR DISAGREGATION =============================
lookfor area

codebook hh6
clonevar area = hh6
replace area=0 if area==2 // urban(1) and rural (2)



