* time_poverty_DATA.do -- upstream of 001; run by hand, not part of the run order.
*
* Builds the paid/unpaid time-use datasets and writes the
* harmonized_time_poverty_data release that 001_DATA_time_poverty_study.R reads.
* Relocated here from ../time-poverty-assets/ in the 2026-08 consolidation; the
* .dta inputs stayed put, so the bare relative `use`/`save` calls now resolve
* through $TPAssets rather than through Stata's working directory. Run from the
* okwaayeli repo root.
*
* FLAG -- read before trusting tpoor0150. Line ~53 does `keep if s1q3==1` and
* then RECOMPUTES TimPov15 off PaidTime, overwriting the CommTime-based version
* saved a few lines earlier (and overwriting PaidTimepoverty.dta with it). So
* the tpoor0150 that reaches the release is a PAID-time cutoff on a restricted
* subsample, not the "Committed Time" the variable labels still claim. Nothing
* downstream knows this. Left as found -- flagged, not silently changed.

gl Dropbox_Personal "C:/Users/ftsib/Dropbox (Personal)"
gl LabGitHub "$Dropbox_Personal\GitHub\ghana\okwaayeli\data-raw\releases\harmonized_data"
gl TPAssets  "$Dropbox_Personal\GitHub\ghana\okwaayeli\studies\time_poverty\time-poverty-assets"

use "$TPAssets\TimeGLSS7", clear

*log using "EdYevuDav.log",replace text

rename s4aq3 Wage_Salary
rename s4aq5 Domestic
rename s4aq7 FarmEnt
rename s4aq10 NonFarmEnt
rename s4aq13 FHNonFarmEnt
rename s4aq16 NonPdtAgric
rename s4aq18 Apprentice
rename s4aq20 Voluntary
recode  Wage_Salary-Voluntary (.=0)

recode Wage_Salary-Voluntary (.=0)
recode Wage_Salary-Voluntary (999 =0)

***Changing Weekly Hours to Daily Hours
foreach var of varlist Wage_Salary-Voluntary {
gen Day`var'=(`var'/7)
*label var Days`var' "Days`var'"
}

foreach var of varlist Wage_Salary-Voluntary {
replace `var'=84 if `var'>84
}


foreach var of varlist Wage_Salary-Voluntary {
sum `var' if `var'>0
}

egen CommTime=rowtotal(DayWage_Salary- DayVoluntary)
label variable CommTime "Committed Time"
sum CommTime, detail
sca median=`r(p50)'

gen Cutoff125=1.25*median
gen TimPov125=( CommTime >Cutoff125)
label var TimPov125 "Paid Time Poverty based on cutoff of 1.25"

gen Cutoff15=1.5*median
gen TimPov15=( CommTime >Cutoff15)
label var TimPov15 "Paid Time Poverty based on cutoff of 1.5"
save "$TPAssets\PaidTimepoverty", replace

*---------------------------------------------------------------------
****Computing Paid time poverty based on cuttoff of 1.25 and 1.5
*---------------------------------------------------------------------
drop Cutoff125 TimPov15 TimPov125 Cutoff15 TimPov15
keep if s1q3==1
gen PaidTime=DayWage_Salary +DayFarmEnt+ DayNonFarmEnt+ DayFHNonFarmEnt
label variable PaidTime "Time in paid activities"
sum PaidTime, detail
sca median1=`r(p50)'

gen Cutoff125=1.25*median1
gen TimPov125=(PaidTime>Cutoff125)
label var TimPov125 "Paid Time Poverty based on cutoff of 1.25"

gen Cutoff15=1.5*median1
gen TimPov15=(PaidTime>Cutoff15)
label var TimPov15 "Paid Time Poverty based on cutoff of 1.5"
save "$TPAssets\PaidTimepoverty", replace

*---------------------------------------------------------------------
****Computing Unpaid time poverty based on cuttoff of 1.25 and 1.5
*---------------------------------------------------------------------
gen UnpaidTime=DayDomestic +DayNonPdtAgric +DayApprentice +DayVoluntary
label variable UnpaidTime "Time in unpaid activities"
sum UnpaidTime, detail
sca median2=`r(p50)'

gen UPCutoff125=1.25*median2
gen UPTimPov125=(UnpaidTime>UPCutoff125)
label var UPTimPov125 "Unpaid Time Poverty based on cutoff of 1.25"

gen UPCutoff15=1.5*median2
gen UPTimPov15=(UnpaidTime>UPCutoff15)
label var UPTimPov15 "Unpaid Time Poverty based on cutoff of 1.5"
save "$TPAssets\UnpaidTimepoverty", replace

*CommitedTime based on WEAI(paid and unpaidwork)
gen CommTimeWEAI=DayWage_Salary + DayFarmEnt + DayNonFarmEnt+DayVoluntary+DayDomestic+DayFHNonFarmEnt+DayNonPdtAgric
gen TimePovWEAI=(CommTimeWEAI>10.5)
label define TimePovWEAI 1"poor" 0"Nonpoor"
label values TimePovWEAI TimePovWEAI
label variable TimePovWEAI "Time povertyWEAI"

ren clust EaId
ren nh HhId
ren pid Mid
gen Surveyx = "GLSS7"
keep Surveyx EaId HhId Mid DayWage_Salary-TimePovWEAI


tab TimePovWEAI TimPov15
tab TimPov15 TimPov125 
tab TimPov15 TimePovWEAI 
 

gen tpoor0150 = TimPov15
gen tpoor0125 = TimPov125
gen tpoorweai = TimePovWEAI

for var tpoor0125 tpoorweai:replace X = . if X != tpoor0150
 
tab tpoor0150 tpoor0125 
tab tpoor0150 tpoorweai 

saveold "$LabGitHub\harmonized_time_poverty_data",replace version(12)
