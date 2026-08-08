*==============================================================================
* 05_offfarm_work.do
*
* Builds the 'Harmonized off-farm work - old' extract.
*
* WRITES NOTHING. The saveold at the end of this script is commented out and
* was already commented out in the source. No release file is produced and no
* study reads one. Kept for the code, excluded from 00_run_all.do's run list.
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 1102-1367.
* Run from the okwaayeli repo root, or from this folder.
*==============================================================================

* --- shared paths ------------------------------------------------------------
* Runs standalone or under 00_run_all.do. Locating _paths.do is separated from
* running it, so a genuine path failure inside _paths.do propagates as itself
* rather than being mistaken for "file not found".
if "$GLSS_PATHS" == "" {
    local _p ""
    capture confirm file "_paths.do"
    if !_rc local _p "_paths.do"
    if "`_p'" == "" {
        capture confirm file "data-raw/scripts/data-prep/glss/_paths.do"
        if !_rc local _p "data-raw/scripts/data-prep/glss/_paths.do"
    }
    if "`_p'" == "" {
        di as err "Cannot locate _paths.do. Run this from the okwaayeli repo root"
        di as err "or from data-raw/scripts/data-prep/glss/, or use 00_run_all.do."
        exit 601
    }
    run "`_p'"
}


tempfile data off_farm_work_data tempd
use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Idividual Demographics.dta", clear
decode Survey,gen(Surveyx)
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
keep Surveyx HhId EaId Mid AgeYr Female 
save `data',replace

use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Household Identification",clear 
decode Survey,gen(Surveyx)
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
keep Surveyx HhId EaId RegName Distcode Locality 
merg 1:m Surveyx HhId EaId using `data'
save `data',replace

use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Occupation\Occupation_Main_7_Days",clear 
decode Survey,gen(Surveyx)
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
keep Surveyx Surveyx EaId HhId Mid manwage_* mandays_*  ISCO_*
decode ISCO_7,gen(xISCO_7)
ren *_7 *_7m
for var EaId HhId Mid:drop if X==.
merg 1:1 Surveyx EaId HhId Mid using `data',nogenerate
save `data',replace

use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Occupation\Occupation_Secondary_7_Days",clear 
decode Survey,gen(Surveyx)
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
keep Surveyx Surveyx EaId HhId Mid manwage_* mandays_* ISCO_* 
decode ISCO_7,gen(xISCO_7)
ren *_7 *_7s
for var EaId HhId Mid:drop if X==.
merg 1:1 Surveyx EaId HhId Mid using `data',nogenerate
save `data',replace


use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Occupation\Occupation_Main_12_Months",clear 
decode Survey,gen(Surveyx)
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
keep Surveyx Surveyx EaId HhId Mid manwage_* mandays_*  ISCO_*
decode ISCO_12,gen(xISCO_12)
ren *_12 *_12m
for var EaId HhId Mid:drop if X==.
merg 1:1 Surveyx EaId HhId Mid using `data',nogenerate
save `data',replace

use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Occupation\Occupation_Secondary_12_Months",clear 
decode Survey,gen(Surveyx)
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
keep Surveyx Surveyx EaId HhId Mid manwage_* mandays_* ISCO_* 
decode ISCO_12,gen(xISCO_12)
ren *_12 *_12s
for var EaId HhId Mid:drop if X==.
merg 1:1 Surveyx EaId HhId Mid using `data',nogenerate
save `data',replace

for var mandays_* manwage_*:replace X=0 if X==.
gen mandays_7  = mandays_7s + mandays_7m
gen manwage_7  = manwage_7s + manwage_7m

gen mandays_12  = mandays_12s + mandays_12m
gen manwage_12  = manwage_12s + manwage_12m

for var manwage_7 manwage_12:replace X=. if X==0

foreach x in 7 12 {
	gen ISCO`x' = 0
	foreach isco in xISCO_`x'm  xISCO_`x's {
		*replace ISCO`x' = 1 if ISCO`x' == 0 & `isco' == "Agricultural/Forestry/Fishery"
		
		replace ISCO`x' = 2 if ISCO`x' == 0 & `isco' == "Elementary Occupations"
		replace ISCO`x' = 2 if ISCO`x' == 0 & `isco' == "Craft And Related Trades"

		replace ISCO`x' = 3 if ISCO`x' == 0 & `isco' == "Plant/Machine Operators/Assemblers"
		replace ISCO`x' = 3 if ISCO`x' == 0 & `isco' == "Service/Sales Workers"
		replace ISCO`x' = 3 if ISCO`x' == 0 & `isco' == "Clerical Support"
		
		replace ISCO`x' = 4 if ISCO`x' == 0 & `isco' == "Armed Forces Occupations"
		replace ISCO`x' = 4 if ISCO`x' == 0 & `isco' == "Technicians/Associate Prof."
		replace ISCO`x' = 4 if ISCO`x' == 0 & `isco' == "Professionals"
		replace ISCO`x' = 4 if ISCO`x' == 0 & `isco' == "Legislators/Officials/Managers"
	}
}

for var mandays_* manwage_*:replace X=0 if X==.
for var mandays_* manwage_*:replace X=X>1
gen  Laborforce = AgeYr >=15
egen Employed = rowmax(mandays_* manwage_*)
keep Surveyx EaId HhId Mid RegName Distcode Locality Laborforce Employed ISCO7 ISCO12
save `data',replace

for var ISCO7 ISCO12:tab X,gen(X_)
collapse (max) Laborforce Employed ISCO*_* ,by(Surveyx EaId HhId Mid)
save `data',replace


tempfile NONFARM
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\NONFARM\g7sec10a.dta",replace
ren clust EaId
ren nh HhId
ren s10aq3 Mid0
ren s10aq3a Mid1
ren s10aq4 Mid2
gen Surveyx = "GLSS7"
keep Surveyx EaId HhId Mid0 Mid1 Mid2
save `NONFARM',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\NONFARM\sec10A.dta",replace
ren clust EaId
ren nh HhId
ren s10aq3 Mid1
ren s10aq4 Mid2
gen Surveyx = "GLSS6"
keep Surveyx EaId HhId Mid1 Mid2
append using `NONFARM'
save `NONFARM',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\NONFARM\sec10a.dta" ,replace
ren clust EaId
ren nh HhId
ren s10aq3 Mid1
ren s10aq4 Mid2
gen Surveyx="GLSS5"
keep Surveyx EaId HhId Mid1 Mid2
append using `NONFARM'
save `NONFARM',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS4\Data\NONFARM\SEC10A.DTA" ,replace
ren clust EaId
ren nh HhId
ren s10aq2 Mid1
ren s10aq3 Mid2
gen Surveyx="GLSS4"
keep Surveyx EaId HhId Mid1 Mid2
append using `NONFARM'
save `NONFARM',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\NONFARM\S10A.DTA" ,replace
ren clust EaId
ren nh HhId
ren s10aq2 Mid1
ren s10aq3 Mid2
gen Surveyx="GLSS3"
keep Surveyx EaId HhId Mid1 Mid2
append using `NONFARM'
save `NONFARM',replace

gen ID = _n
reshape long Mid, i(Surveyx EaId HhId ID) j(Midx)
drop if Mid == .
gen indv_bus = 1
collapse (max) indv_bus,by(Surveyx EaId HhId Mid)

merg 1:1 Surveyx EaId HhId Mid using `data',nogenerate
egen indv_wage_1 = rowmax(ISCO*_1)
egen indv_wage_2 = rowmax(ISCO*_2)
egen indv_wage_3 = rowmax(ISCO*_3)
egen indv_wage_4 = rowmax(ISCO*_4)
egen indv_wage = rowmax(indv_wage_*)

collapse (max) Laborforce Employed indv_bus indv_wage* ,by(Surveyx EaId HhId Mid)
save `data',replace

use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Aggregation\Household Aggregates_WB", clear
decode Survey,gen(Surveyx)
ren INC_NFARM_G INC_NFARM
ren INC_FARM_G INC_FARM
ren INC_TOT_G INC_TOT
ren INC_RMT INC_REMIT 
keep Surveyx HhId EaId INC_FARM INC_NFARM INC_WAGE INC_RENT INC_OTHER INC_REMIT INC_TOT
merg 1:m Surveyx EaId HhId using `data',nogenerate

collapse (max) Laborforce Employed indv_bus indv_wage* (mean) INC_*,by(Surveyx EaId HhId Mid)
merg 1:m Surveyx EaId HhId Mid using "$LabGitHub\harmonized_crop_farmer_data"
drop if _merge==1
collapse (max) Laborforce Employed indv_bus indv_wage* (mean) INC_*,by(Surveyx EaId HhId Mid)

for var Laborforce Employed indv_bus indv_wage* INC_* :replace X=0 if X== .
egen indv_offfarm = rowmax(indv_bus indv_wage)
ren indv_* self_*

keep if inlist(Surveyx,"GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")










drop INC_TOT
egen INC_TOT = rowtotal(INC_WAGE INC_FARM INC_NFARM INC_RENT INC_OTHER INC_REMIT)
drop if INC_TOT == 0
for var INC_FARM INC_NFARM INC_WAGE INC_RENT INC_OTHER INC_REMIT:drop if round(X/INC_TOT) >1 

tabstat INC_FARM INC_NFARM INC_WAGE INC_RENT INC_REMIT INC_OTHER INC_TOT, by(Surveyx)

*INC_WAGE INC_FARM INC_NFARM INC_RENT INC_TOT INC_OTHER INC_REMIT
*Farming combined with any type and combination of off-farm work [correct]
egen IncomeAE_G2   = rowtotal(INC_WAGE INC_NFARM) 
gen  RatioAE_G2    = IncomeAE_G2/INC_TOT   
replace RatioAE_G2 = 1 if RatioAE_G2 >1 
gen Dammy_G2       = IncomeAE_G2>0 

*Farming combined with wage employment, with/without nonfarm business [correct]
gen  IncomeAE_G3   = INC_WAGE
gen  RatioAE_G3    = IncomeAE_G3/INC_TOT
replace RatioAE_G3 = 1 if RatioAE_G3  >1 
gen Dammy_G3       = IncomeAE_G3>0 
for var *_G3:replace X=. if inlist(IncomeAE_G3,0,.)

*Farming combined with non-farm business, with/without wage employment [correct]
gen  IncomeAE_G4   = INC_NFARM
gen  RatioAE_G4    = IncomeAE_G4/INC_TOT
replace RatioAE_G4 = 1 if IncomeAE_G4  >1
gen  Dammy_G4      = RatioAE_G4>0        
for var *_G4:replace X=. if inlist(IncomeAE_G4,0,.)

*Farming combined with wage employment only [correct] 
gen IncomeAE_G5 = cond(INC_WAGE > 0 & INC_NFARM>0 ,.,INC_WAGE)
gen  RatioAE_G5    = IncomeAE_G5/INC_TOT   
replace RatioAE_G5 = 1 if RatioAE_G5 >1 
gen  Dammy_G5      = RatioAE_G5>0      
for var *_G5:replace X=. if Dammy_G2 == 1 & inlist(X,.,0)
for var *_G5:replace X=X*Dammy_G3
for var *_G5:replace X=. if IncomeAE_G2 ==1 & inlist(IncomeAE_G5,0,.)

*Farming combined with non-farm business only [correct]
gen IncomeAE_G6    = cond(INC_WAGE > 0 & INC_NFARM>0 ,.,INC_NFARM)
gen  RatioAE_G6    = IncomeAE_G6/INC_TOT   
replace RatioAE_G6 = 1 if RatioAE_G6 >1 
gen  Dammy_G6      = RatioAE_G6>0       
for var *_G6:replace X=. if Dammy_G2 == 1 & inlist(X,.,0)
for var *_G6:replace X=X*Dammy_G4
for var *_G6:replace X=. if IncomeAE_G2 ==1 & inlist(IncomeAE_G6,0,.)

*Farming combined with either non-farm business or wage employment [correct]
egen IncomeAE_G7   = rowtotal(IncomeAE_G5 IncomeAE_G6)
gen  RatioAE_G7    = IncomeAE_G7/INC_TOT   
replace RatioAE_G7 = 1 if RatioAE_G7 >1 
gen  Dammy_G7      = RatioAE_G7>0       
for var *_G7:replace X=. if Dammy_G2 == 1 & inlist(X,.,0)
for var *_G7:replace X=. if IncomeAE_G2 ==1 & inlist(IncomeAE_G7,0,.)

*Farming combined with both non-farm business and wage employment [correct]
gen IncomeAE_G8    = cond(Dammy_G6*Dammy_G7 == 1 ,INC_WAGE+INC_NFARM,.)
gen  RatioAE_G8    = IncomeAE_G8/INC_TOT   
replace RatioAE_G8 = 1 if RatioAE_G8 >1 
gen  Dammy_G8      = Dammy_G6*Dammy_G7 == 1     
for var *_G8:replace X=. if Dammy_G2 == 1 & inlist(X,.,0)
for var *_G8:replace X=. if IncomeAE_G2 ==1 & inlist(IncomeAE_G8,0,.)

ren Dammy_G* hhcombo*

drop RatioAE_* IncomeAE_* 

for var hhcombo*:replace X=X==1

sum 

compress
*saveold "$LabGitHub\harmonized_offfarm_work_data",replace version(12)
