*==============================================================================
* 03_financial_inclusion.do
*
* Builds the 'Harmonized Financial Inclusion' release.
*
* WRITES: $LabGitHub\harmonized_financial_inclusion_data.dta
*
* REQUIRES: harmonized_crop_farmer_data.dta must already exist -- this script
* reads it back from $LabGitHub. Run 01_crop_farmer.do first, or use
* 00_run_all.do, which orders them correctly.
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 625-852.
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

* --- upstream dependency -----------------------------------------------------
capture confirm file "$LabGitHub/harmonized_crop_farmer_data.dta"
if _rc {
    di as err "This script reads harmonized_crop_farmer_data.dta, which is not in"
    di as err "  $LabGitHub"
    di as err "Run 01_crop_farmer.do first, or run 00_run_all.do."
    exit 601
}


tempfile ParentData
use "$COLLATED\Output\Idividual Demographics",clear 
decode Survey,gen(Surveyx)
save `ParentData',replace

use "$COLLATED\Output\Idividual Education",clear 
decode Survey,gen(Surveyx)
merg 1:1 Surveyx HhId EaId Mid using `ParentData'
keep Surveyx HhId EaId Mid AgeYr Relate YerEdu
save `ParentData',replace

use "$COLLATED\Output\Finances\Finances_Banking",clear 
tab NonBanked_Why,gen(NonBanked_Why_)
tab Bank_Info,gen(Bank_Info_)
decode Survey,gen(Surveyx)
collapse (max) Banked InstTyp_* AccTyp_* PrdTyp_* NonBanked_Why_* Bank_Info_*,by(Surveyx HhId EaId Mid Bank_Info)
merg 1:1 Surveyx HhId EaId Mid using `ParentData'
drop _merge
save `ParentData',replace

use "$COLLATED\Output\Finances\Finances_Loans",clear 
decode Survey,gen(Surveyx)
collapse (max) Loan RePaid Applied Accept Refused Proces Source_* Collateral_* Use_* Refusal_* WhyNoLoan_*,by(Surveyx HhId EaId Mid)
merg 1:1 Surveyx HhId EaId Mid using `ParentData'
drop _merge
save `ParentData',replace

use "$COLLATED\Output\Finances\Finances_Insurance",clear 
decode Survey,gen(Surveyx)
collapse (max) Insured*,by(Surveyx HhId EaId Mid)
merg 1:1 Surveyx HhId EaId Mid using `ParentData'
drop _merge
save `ParentData',replace

use "$COLLATED\Output\Household Identification",clear 
decode Survey,gen(Surveyx)
keep Surveyx HhId EaId RegName Weight Locality
merg 1:m Surveyx HhId EaId using `ParentData'
drop _merge
save `ParentData',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec1_5.dta",clear 
for var s4aq34b s4bq3b s4dq4b: decode X,gen(Xx)
for var s4aq34b s4bq3b s4dq4b:tab X
gen FinWorker     = s4aq34bx      == "10. Financial and insurance activities"
replace FinWorker = 1 if s4bq3bx == "10. Financial and insurance activities"
replace FinWorker = 1 if s4dq4bx == "10. Financial and insurance activities"
gen Surveyx = "GLSS7"
ren clust EaId
ren nh HhId
ren pid Mid
collapse (max) FinWorker ,by(Surveyx EaId HhId Mid)
merg 1:1 Surveyx EaId HhId Mid using `ParentData',nogenerate update force replace
save `ParentData',replace
use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC4a",clear 
merg 1:m clust nh HID PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC4b",nogenerate update force replace
merg 1:m clust nh HID PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC4e",nogenerate update force replace
merg 1:m clust nh HID PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC4f",nogenerate update force replace
gen FinWorker = 0
for var s4aq7 s4bq2 s4eq7 s4fq3: replace FinWorker = 1 if X >= 6400 & X < 6800
ren clust EaId
ren nh HhId
ren PID Mid
gen Surveyx = "GLSS6"
collapse (max) FinWorker ,by(Surveyx EaId HhId Mid)
merg 1:1 Surveyx EaId HhId Mid using `ParentData',nogenerate update force replace
save `ParentData',replace

collapse (sum) FinWorker ,by(Surveyx EaId HhId)
ren FinWorker HHFinWorker
merg 1:m Surveyx EaId HhId using `ParentData',nogenerate update force replace
save `ParentData',replace

for var Banked Applied Accept Refused:replace X=X==1
ren Loan LoanAmt

keep if inlist(Surveyx,"GLSS6","GLSS7")
gen     CPI = 118.687/305.788 if Surveyx == "GLSS6" //2012/13
replace CPI = 305.788/305.788 if Surveyx == "GLSS7" //2016/17
replace LoanAmt= LoanAmt/CPI
replace RePaid= RePaid/CPI

for var InstTyp_* AccTyp_* Bank_Info_*: replace X=. if Banked == 0
for var NonBanked_Why_*: replace X=. if Banked == 1
for var Source_* Collateral_* Use_* LoanAmt RePaid: replace X=. if Applied*Accept == 0
for var Refusal_*: replace X=. if Applied*Refused == 0
for var WhyNoLoan_*: replace X=. if Applied == 1
for var Insured_* :replace X=0 if Insured == 0
for var YerEdu AgeYr Banked Insured:drop if X==.
drop if AgeYr < 18
drop CPI 
bysort *: drop if cond(_N==1,0,_n)>0
compress
save `ParentData',replace

use "$COLLATED\Output\Idividual Demographics", clear
decode Survey,gen(Surveyx)
keep Surveyx EaId HhId Mid Relate FthrID MthrID SpusID
merg 1:1 Surveyx EaId HhId Mid using `ParentData'
keep if _merge==3
drop _merge
decode Relate,gen(Relatex)
save `ParentData',replace

tempfile farmer 
gen credit_self = LoanAmt > 0 & LoanAmt != .
keep Surveyx EaId HhId Mid credit_self
saveold `farmer',replace

use `ParentData',clear
gen credit_child1 = LoanAmt > 0 & LoanAmt != .
collapse (max) credit_*,by(Surveyx EaId HhId MthrID)
ren MthrID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen credit_child2 = LoanAmt > 0 & LoanAmt != .
collapse (max) credit_*,by(Surveyx EaId HhId FthrID)
ren FthrID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen credit_spouse = LoanAmt > 0 & LoanAmt != .
collapse (max) credit_*,by(Surveyx EaId HhId SpusID)
ren SpusID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen credit_close = LoanAmt > 0 & inlist(Relatex,"Child (Adopted)","Child (Son/Daughter)","Spouse") & LoanAmt != .
collapse (max) credit_*,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen credit_close = LoanAmt > 0 & inlist(Relatex,"Child (Adopted)","Child (Son/Daughter)","Spouse") & LoanAmt != .
gen credit_member = LoanAmt > 0 & credit_close == 0 & LoanAmt != .
collapse (max) credit_*,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen credit_hh = LoanAmt > 0 & LoanAmt != .
collapse (max) credit_*,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

egen credit_child  = rowmax(credit_child2 credit_child1)
drop credit_child2 credit_child1
collapse (max) credit_*,by(Surveyx EaId HhId Mid)

merg 1:1 Surveyx EaId HhId Mid using `ParentData'
drop if _merge==1
for var credit_*:replace X=0 if X==.
drop _merge
saveold `ParentData',replace

use "$COLLATED\Output\Community\Comm Economy Infrastructure",clear
decode Survey,gen(Surveyx)
collapse (min) BankKm RoadKm TrnprtKm,by(EaId Surveyx)
merg 1:m Surveyx EaId using `ParentData'
keep if _merge == 3
drop _merge Weight RegName Locality YerEdu AgeYr Relate SpusID FthrID MthrID Relatex
save `ParentData',replace

use "$LabGitHub\harmonized_crop_farmer_data",clear
collapse (min) YerEdu,by(Surveyx EaId HhId Mid)
keep Surveyx EaId HhId Mid
merg 1:1 Surveyx EaId HhId Mid using `ParentData'
keep if _merge == 3
drop _merge

for var *:lab var X ""
order Surveyx EaId HhId Mid HHFinWorker FinWorker LoanAmt credit_* Insured* Banked Bank_Info /*
*/ InstTyp_* AccTyp_* PrdTyp_* BankKm RoadKm TrnprtKm 
save `ParentData',replace

use "$COLLATED\Output\Finances\Finances_Loans", clear 
keep Loan RePaid Applied Accept Refused Proces Source_* Collateral_* Use_* Refusal_* WhyNoLoan_*
keep in 1
gen DD = 1
append using `ParentData'
drop if DD == 1
drop DD
save `ParentData',replace

use "$COLLATED\Output\Finances\Finances_Banking", clear 
tab NonBanked_Why,gen(NonBanked_Why_)
tab Bank_Info,gen(Bank_Info_)
keep Banked InstTyp_* AccTyp_* PrdTyp_* NonBanked_Why_* Bank_Info_*
keep in 1
gen DD = 1
append using `ParentData'
drop if DD == 1
drop DD
save `ParentData',replace

use "$COLLATED\Output\Finances\Finances_Insurance", clear 
keep Insured*
keep in 1
gen DD = 1
append using `ParentData'
drop if DD == 1
drop DD
save `ParentData',replace

for var credit_close credit_member credit_spouse credit_self credit_child:replace X=. if X==0 & credit_hh == 1

compress
saveold "$LabGitHub\harmonized_financial_inclusion_data",replace version(12)

