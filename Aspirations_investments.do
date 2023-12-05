

*************************************************************************************************************************************************************************************************
*****************************************************************DO FILE FOR ASPIRATIONS AND INVESTMENTS IN CAMEROON AND KENYA*******************************************************************
*************************************************************************************************************************************************************************************************


*************************************************************************************************************************************************************************************************

* Project : 	ASPIRATIONS
* Program:		ASPIRATIONS AND FUTURE-ORIENTED BEHAVIOURS

*************************************************************************************************************************************************************************************************

*Note this code was implemented in STATA version 17


clear matrix
clear mata
set more off
set logtype text
capture log close


***setting work directory***


*******************************************************************************************************************************************************************
* SET FILE PATHS
*******************************************************************************************************************************************************************


local path "C:\Users\MPTabe-Ojong\Dropbox (IFPRI)\Tabe-Ojong\B01\Aspirations_investments\" 

gl data "`path'Tabe-Ojong\B01\Aspirations_investments\"

gl results "`path'Tabe-Ojong\B01\Aspirations_investments\"

**************************************************************************************************************************************************************************************************
***LOADING THE DATA****
use "aspirations_investments.dta", clear
**************************************************************************************************************************************************************************************************
**Hope**

gen gap = (income_future - income)/income_future
replace gap = 0 if gap < 0
gen gap_sq = gap * gap

gen ihs_income = asinh( income ) /*IHS transformation*/
gen ihs_income_future = asinh( income_future ) /*IHS transformation*/

sum hope1 hope2 hope3 hope4 hope5 hope6
gen agency = (hope2+ hope4+ hope6)/3
gen pathway = (hope1+ hope3+ hope5)/3
sum agency pathway
gen hope = (hope1+ hope2+ hope3+ hope4+ hope5+ hope6)/6
sum hope
cou if agency<=5&pathway<=5
cou if agency>5&pathway<=5
cou if agency<=5&pathway>5
cou if agency>5&pathway>5

foreach var of varlist improved_seeds fert pest machines crop_rotation intercropping fallowing animal_manure green_manure SWC conservation_tillage{
replace `var' = 0 if `var' ==.a 
 }
 
replace agency = 5.75 if agency==.

gen manure = (animal_manure==1| green_manure==1)



*********************************************************************************************************************************************************
****************************************************************lABELING VARIABLES***********************************************************************
*********************************************************************************************************************************************************

la var gap "Aspiration gap (0-1)"
la var gap_sq " Gap squared (0-1)"
la var Offfarm_participation "Off-farm activity (1/0)"
la var hh_size "Household size (num)"
la var credit_access "Credit access (1/0)"
la var age_hh "Age of head (years)"
la var educ_hh "Educational level (years)"
la var coop_membership "Cooperative membership (1/0)"
la var extension_contact "Extension access (1/0)"
la var ethnic_group "Ethnicity"
la var gender_hh "Head is male (1/0)"
la var ihs_income "Income (IHS)"
la var agency "Agency (score)"
la var pathway "Pathway (score)"
la var ihs_income_future "Aspirations"
la var hope "Hope (score)"
la var pc1 "Asset index"
la var manure "Organic soil amendments (1/0)"
la var crop_rotation "Crop rotation (1/0)"
la var intercropping "Intercropping (1/0)"
la var fallowing "Fallowing (1/0)"



************************************************************************************************************************************************************************************************
**************************************************************************************SUMMARY STATISTICS****************************************************************************************
************************************************************************************************************************************************************************************************


global interest ihs_income_future agency pathway gap gap_sq hope

gen income_ppp = income
replace income_ppp = income/232.801 if country==0
replace income_ppp = income/40.185 if country==1


gen income_future_ppp = income_future
replace income_future_ppp = income_future/232.801 if country==0
replace income_future_ppp = income_future/40.185 if country==1

global xlist Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1
***Summary statistics***
su $xlist 

/*
asdoc tabstat crop_rotation intercropping fallowing manure income_ppp income_future_ppp gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, stats(mean sd) dec(2)  

asdoc ttable2 crop_rotation intercropping fallowing manure income_ppp income_future_ppp gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, by(country)

 */

************************************************************************************************************************************************************************************************
**************************************************************************************MAIN REGRESSIONS******************************************************************************************
************************************************************************************************************************************************************************************************

***ASPIRATION***
** Figure 2. Estimates of aspirations and CSA practices
areg crop_rotation ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure2.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("CR") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab replace
outreg2 using stata_outputs/SM_tables/Table_SM2.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab replace

areg intercropping ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure2.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("IC") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM2.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

areg fallowing ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure2.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FA") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM2.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

areg manure ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure2.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("OSA") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM2.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

***ASPIRATION FAILURE***

* Figure 3 . Estimates of aspiration failure and CSA practices
areg crop_rotation gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("CR") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab replace
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab replace


asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) replace

areg intercropping gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("IC") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg fallowing gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FA") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append


asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg manure gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("OSA") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

************************************************************************************************************************************************************************************************
*****************************************************************************************CAMEROON***********************************************************************************************
************************************************************************************************************************************************************************************************

***ASPIRATION***
areg crop_rotation ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("CR") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab replace
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, No) replace

areg intercropping ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("IC") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, No) append

areg fallowing ihs_income_future  Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)

outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FA") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, No) append

areg manure ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("OSA") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, No) append


***ASPIRATION FAILURE***
areg crop_rotation gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("CR") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab replace
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab replace


asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg intercropping gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("IC") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg fallowing gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FA") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg manure gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==0, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("OSA") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend


************************************************************************************************************************************************************************************************
******************************************************************************************KENYA*************************************************************************************************
************************************************************************************************************************************************************************************************
***ASPIRATION***
areg crop_rotation ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("CR") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append
areg intercropping ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("IC") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append
areg fallowing ihs_income_future  Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FA") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append
areg manure ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure4.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("OSA") keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append


***ASPIRATION FAILURE***
areg crop_rotation gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("CR") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg intercropping gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("IC") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append


asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg fallowing gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FA") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend

areg manure gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 if country==1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/figures_table/Figure5.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("OSA") keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab append
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append

asdoc utest gap gap_sq, level(`fiel') fieller save(utest.doc) rowappend


************************************************************************************************************************************************************************************************
*****************************************************************************NON PARAMETRIC ESTIMATIONS*****************************************************************************************
************************************************************************************************************************************************************************************************
***SEMI-PARAMETRIC ESTIMATIONS OF ASPIRATION FAILURE***
lpoly crop_rotation gap, degree(0) kernel(gaussian) noscatter  graphregion(color(white)) bgcolor(white) ci title((a) Crop rotation) legend(off) ///
ytitle(Crop rotation) xtitle(Aspiration Gap) saving(CR, replace)

lpoly intercropping gap, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title((b) Intercropping)  legend(off) ///
ytitle(Intercropping) xtitle(Aspiration Gap) saving(IC, replace)

lpoly fallowing gap, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title((c) Fallowing)  legend(off) ///
ytitle(Fallowing) xtitle(Aspiration Gap) saving(FA, replace)

lpoly manure gap, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title ((d) Organic soil amendments)  legend(off)  ///
ytitle(OSA) xtitle(Aspiration Gap) saving(OSA, replace)

gr combine CR.gph IC.gph FA.gph OSA.gph, col(2) iscale(.7) commonscheme graphregion(color(white)) 
 
graph export "figures/semipar.pdf", as(pdf) replace


***SEMI-PARAMETRIC ESTIMATIONS OF ASPIRATION FAILURE***
lpoly crop_rotation gap if country==0, degree(0) kernel(gaussian) noscatter  graphregion(color(white)) bgcolor(white) ci title((a) Crop rotation)  legend(off) ///
ytitle(Crop rotation) xtitle(Aspiration Gap) saving(CR, replace)

lpoly intercropping gap if country==0, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title((b) Intercropping)  legend(off) ///
ytitle(Intercropping) xtitle(Aspiration Gap) saving(IC, replace)

lpoly fallowing gap if country==0, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title((c) Fallowing) legend(off) ///
ytitle(Fallowing) xtitle(Aspiration Gap) saving(FA, replace)

lpoly manure gap if country==0, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title ((d) Organic soil amendments)  legend(off) ///
ytitle(OSA) xtitle(Aspiration Gap) saving(OSA, replace)

gr combine CR.gph IC.gph FA.gph OSA.gph, col(2) iscale(.7) commonscheme graphregion(color(white)) 
 
graph export "figures/semipar_cmr.pdf", as(pdf) replace


***SEMI-PARAMETRIC ESTIMATIONS OF ASPIRATION FAILURE***
lpoly crop_rotation gap if country==1, degree(0) kernel(gaussian) noscatter  graphregion(color(white)) bgcolor(white) ci title((a) Crop rotation)  legend(off) ///
ytitle(Crop rotation) xtitle(Aspiration Gap) saving(CR, replace) scheme(tab3)

lpoly intercropping gap if country==1, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title((b) Intercropping)  legend(off) ///
ytitle(Intercropping) xtitle(Aspiration Gap) saving(IC, replace)

lpoly fallowing gap if country==1, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title((c) Fallowing)  legend(off) ///
ytitle(Fallowing) xtitle(Aspiration Gap) saving(FA, replace)

lpoly manure gap if country==1, degree(0) kernel(gaussian) noscatter graphregion(color(white)) bgcolor(white) ci title ((d) Organic soil amendments)  legend(off) ///
ytitle(OSA) xtitle(Aspiration Gap) saving(OSA, replace)

gr combine CR.gph IC.gph FA.gph OSA.gph, col(2) iscale(.7) commonscheme graphregion(color(white)) 
 
graph export "figures/semipar_kenya.pdf", as(pdf) replace

************************************************************************************************************************************************************************************************
*****************************************************************************NON PARAMETRIC ESTIMATIONS*****************************************************************************************
************************************************************************************************************************************************************************************************
areg crop_rotation ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab replace 
psacalc beta ihs_income_future, rmax(0.444)
psacalc delta ihs_income_future, rmax(0.444)

areg intercropping ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append 
psacalc beta ihs_income_future, rmax(0.310)
psacalc delta ihs_income_future, rmax(0.310)

areg fallowing ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append 
psacalc beta ihs_income_future, rmax(0.504)
psacalc delta ihs_income_future, rmax(0.504)

areg manure ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, absorb(village) vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab append 
psacalc beta ihs_income_future, rmax(0.293)
psacalc delta ihs_income_future, rmax(0.293)

************************************************************************************************************************************************************************************************
*****************************************************************************MVP REGRESSION*****************************************************************************************
************************************************************************************************************************************************************************************************


global interest ihs_income_future

cmp (crop_rotation = $interest $xlist) (intercropping = $interest $xlist   i.village)(fallowing = $interest $xlist i.village)(manure = $interest $xlist i.village),indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" "$cmp_probit") nolrtest difficult nonrtolerance vce(cluster village)

outreg2 using stata_outputs/SM_tables/Table_SM7.tex, keep($interest $xlist ) lab  addtext(Additional controls, Yes, Village FE, Yes, Year FE, Yes)  replace

outreg2 using stata_outputs/figures_table/Figure6.txt, sideway stats(coef se pval aster  ci_low ci_high ) keep (ihs_income_future) long  nocons  nonotes noparen dec(3) quote lab replace


asdoc margins, dydx(ihs_income_future) predict(eq(#1) pr) force noestimcheck save(margins.doc) replace
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
asdoc margins, dydx(ihs_income_future) predict(eq(#2) pr) force noestimcheck save(margins.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
asdoc margins, dydx(ihs_income_future) predict(eq(#3) pr) force noestimcheck save(margins.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
asdoc margins, dydx(ihs_income_future) predict(eq(#4) pr) force noestimcheck save(margins.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


global interest gap gap_sq

cmp (crop_rotation = $interest $xlist) (intercropping = $interest $xlist   i.village)(fallowing = $interest $xlist i.village)(manure = $interest $xlist i.village),indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" "$cmp_probit") nolrtest difficult nonrtolerance vce(cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, keep($interest $xlist) lab  addtext(Additional controls, Yes, Village FE, Yes, Year FE, Yes) replace

outreg2 using stata_outputs/figures_table/Figure7.txt, sideway stats(coef se pval aster  ci_low ci_high ) keep (gap gap_sq) long  nocons  nonotes noparen dec(3) quote lab replace

asdoc margins, dydx(gap gap_sq) predict(eq(#1) pr) force noestimcheck save(margins2.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
asdoc margins, dydx(gap gap_sq) predict(eq(#2) pr) force noestimcheck save(margins2.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
asdoc margins, dydx(gap gap_sq) predict(eq(#3) pr) force noestimcheck save(margins2.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
asdoc margins, dydx(gap gap_sq) predict(eq(#4) pr) force noestimcheck save(margins2.doc) rowappend
*outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


************************************************************************************************************************************************************************************************
**************************************************************************************ROBUSTNESS CHECKS*****************************************************************************************
************************************************************************************************************************************************************************************************
***Generating the count data variable
egen csa = rowtotal (crop_rotation intercropping fallowing manure)


*********POISSON REGRESSION MODEL***********

***ASPIRATION***
poisson csa ihs_income_future, vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) nocons  nonotes  dec(3)  lab addtext(Additional controls, No, Village FE, No) replace

poisson csa ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval) nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, No) append
poisson csa ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 i.village, vce( cluster village)
outreg2 using Result8.doc, append ctitle (CR) dec(3) lab addtext(Additional controls, Yes, Village FE, Yes)
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval)  nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append
***ASPIRATION FAILURE***
poisson csa gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 i.village, vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval) nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) replace
utest gap gap_sq, prefix(csa) level(`fiel') fieller 



*********ORDERED PROBIT REGESSION MODEL***********
***ASPIRATION***
oprobit csa ihs_income_future, vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval)    nocons  nonotes  dec(3)  lab addtext(Additional controls, No, Village FE, No) append

oprobit csa ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1, vce( cluster village)

outreg2 using stata_outputs/SM_tables/Table_SM9.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval)    nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, No) append

oprobit csa ihs_income_future Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 i.village, vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, keep(ihs_income_future $xlist) stats(coef se pval) paren(se) bracket(pval)    nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append

***ASPIRATION FAILURE***
oprobit csa gap gap_sq Offfarm_participation hh_size credit_access age_hh educ_hh coop_membership extension_contact gender_hh pc1 i.village, vce( cluster village)
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, keep(gap gap_sq $xlist) stats(coef se pval) paren(se) bracket(pval)    nocons  nonotes  dec(3)  lab addtext(Additional controls, Yes, Village FE, Yes) append
utest gap gap_sq, prefix(csa) level(`fiel') fieller 


************************************************************************************************************************************************************************************************
**************************************************************************************QUANTILE REGRESSION***************************************************************************************
************************************************************************************************************************************************************************************************
*ssc install qregpd
*ssc install moremata
***QUANTILE REGRESSION FOR INCOME***
sqreg ihs_income adoption, quantile (.1 .2 .3 .4 .5 .6 .7 .8 .9) reps(100)
preserve
gen q = _n*10 in 1/9
foreach var of varlist adoption {
    gen _b_`var'  = .
    gen _lb_`var' = .
    gen _ub_`var' = .
    local i = 1
    foreach q of numlist 10(10)90 {
        replace _b_`var' = _b[q`q':`var'] in `i'
        replace _lb_`var' = _b[q`q':`var'] - _se[q`q':`var']*invnormal(.975) in `i'
        replace _ub_`var' = _b[q`q':`var'] + _se[q`q':`var']*invnormal(.975) in `i++'
    }
}
keep q _b_* _lb_* _ub_*
keep in 1/9
reshape long _b_ _lb_ _ub_, i(q) j(var) string
export excel _b_ _lb_ _ub_ using "extract1.xlsx", firstrow(variables) replace
set scheme s1color
twoway rarea _lb_ _ub_ q, astyle(ci) yline(0) acolor(%90) || ///
   line _b_ q,                                               ///
   by(var, yrescale xrescale note("") legend(at(4) pos(0)))  ///
   legend(order(2 "effect"                                   ///      
                1 "95% confidence" "interval")               ///
          cols(1))                                           ///
   ytitle(effect on percentile of area under adoption)                       ///
   ylab(,angle(0) format(%7.0gc))                            ///    
   xlab(10(10)90) xtitle(area under adoption)
restore


**************************************************************************************************************************************************************************************************
******************************************************************************************END*****************************************************************************************************
**************************************************************************************************************************************************************************************************					  
						  

						  
						
