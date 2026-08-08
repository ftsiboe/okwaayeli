*==============================================================================
* 02_income_transfer.do
*
* Builds the 'harmonized households receiving transfers' release.
*
* WRITES: $LabGitHub\harmonized_income_transfer_data.dta
*
* REQUIRES: harmonized_crop_farmer_data.dta must already exist -- this script
* reads it back from $LabGitHub. Run 01_crop_farmer.do first, or use
* 00_run_all.do, which orders them correctly.
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 220-624.
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


qui{ //GlSS3

tempfile temp final
use "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\partb\S11B2.dta",clear 
gen linno = _n

lab define s11bq5 1 Parent 2 spouse 3 Child 4 "Brother/sister" 5 "Other relative" 6 "Non-relative",replace
lab val s11bq5 s11bq5

lab define s11bq7 1 "Yes, Weekly" 2 "Yes, Monthly" 3 "Yes, Quarterly" 4 "Yes, Annually" 5 No 6 Other,replace
lab val s11bq7 s11bq7

lab define s11bq12 1 "This village/town" 2 Accra 3 Kumasi 4 "Sekondi/Takoradi" 5 Tamale 6 "Other Urban" 7 Rural 8 "Abroad (Africa)" 9 "Abroad (other)",replace
lab val s11bq12 s11bq12

save `temp',replace
ren trinccd pid
drop if pid == 0
drop if pid == .
merg m:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\parta\S1"
lab val rel s11bq5
keep if _merge==3
keep clust nh linno sex rel
merg 1:m clust nh linno using  `temp'
for var s11bq5 rel:decode X,gen(Xx)
gen female = inlist(2,sex,s11bq6)
ren relx relation
replace relation = s11bq5x if relation == ""
ren linno remitterID
decode s11bq7 ,gen(remittance_frequency)
ren s11bq8 remittance_is_loan
ren s11bq9 remittance_amount_cash
ren s11bq10 remittance_amount_food
ren s11bq11 remittance_amount_nonfood
decode s11bq12,gen(remitter_location)
keep clust nh remitterID female relation remittance_*
ren clust EaId
ren nh HhId
gen Survey = "GLSS3"
save `final',replace
}
qui{ //GlSS4

tempfile temp
use "$DATABASE\GLSS\Datasets\GSS\GLSS4\Data\PARTB\SEC11B2.dta",clear 
gen linno = _n

lab define s11bq5 1 Parent 2 spouse 3 Child 4 "Brother/sister" 5 "Other relative" 6 "Non-relative",replace
lab val s11bq5 s11bq5

lab define s11bq7 1 "Yes, Weekly" 2 "Yes, Monthly" 3 "Yes, Quarterly" 4 "Yes, Annually" 5 No 6 Other,replace
lab val s11bq7 s11bq7

lab define s11bq12 1 "This village/town" 2 Accra 3 Kumasi 4 "Sekondi/Takoradi" 5 Tamale 6 "Other Urban" 7 Rural 8 "Abroad (Africa)" 9 "Abroad (other)",replace
lab val s11bq12 s11bq12

save `temp',replace
ren trinccd pid
drop if pid == 0
drop if pid == .
merg m:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS4\Data\parta\SEC1"
lab val rel s11bq5
keep if _merge==3
keep clust nh linno sex rel
merg 1:m clust nh linno using  `temp'
for var s11bq5 rel:decode X,gen(Xx)
gen female = inlist(2,sex,s11bq6)
ren relx relation
replace relation = s11bq5x if relation == ""
ren linno remitterID
decode s11bq7 ,gen(remittance_frequency)
ren s11bq8 remittance_is_loan
ren s11bq9 remittance_amount_cash
ren s11bq10 remittance_amount_food
ren s11bq11 remittance_amount_nonfood
decode s11bq12,gen(remitter_location)
keep clust nh remitterID female relation remittance_*
ren clust EaId
ren nh HhId
gen Survey = "GLSS4"
append using `final'
save `final',replace
}
qui{ //GlSS5

use "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\PARTB\sec11b.dta",clear 
ren s11bq4 pid
drop if pid == 0
drop if pid == .
merg m:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\PARTA\sec1"
keep if _merge==3
keep clust nh linno s1q2 s1q3
merg 1:m clust nh linno using "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\PARTB\sec11b.dta"
for var s1q2 s1q3 s11bq5 s11bq6:decode X,gen(Xx)
gen female = inlist("Female",s1q2x,s11bq6x)
ren s1q3x relation
replace relation = s11bq5x if relation == ""
ren linno remitterID
decode s11bq7 ,gen(remittance_frequency)
ren s11bq8 remittance_is_loan
ren s11bq9 remittance_amount_cash
*ren s11bq9a remittance_cost
decode s11bq10,gen(remittance_mode)
decode s11bq11a,gen(remittance_use01)
decode s11bq11b,gen(remittance_use02)
decode s11bq11c,gen(remittance_use03)
*decode s11bq11o,gen(remittance_use00)
ren s11bq12 remittance_amount_food
ren s11bq13 remittance_amount_nonfood
decode s11bq14,gen(remitter_location)
keep clust nh remitterID female relation remittance_*
ren clust EaId
ren nh HhId
gen Survey = "GLSS5"
append using `final'
save `final',replace
}
qui{ //GlSS6
use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTB\sec11b.dta",clear 
ren pid PID
drop if PID == 0
drop if PID == .
merg m:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\sec1"
keep if _merge==3
keep clust nh s11bq3a s1q2 s1q3
merg 1:m clust nh s11bq3a using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTB\sec11b.dta"
for var s1q2 s1q3 s11bq5 s11bq6:decode X,gen(Xx)
gen female = inlist("Female",s1q2x,s11bq6x)
ren s1q3x relation
replace relation = s11bq5x if relation == ""
ren s11bq3a remitterID
decode s11bq7 ,gen(remittance_frequency)
ren s11bq8 remittance_is_loan
ren s11bq9 remittance_amount_cash
*ren s11bq9a remittance_cost
decode s11bq10,gen(remittance_mode)
decode s11bq11a,gen(remittance_use01)
decode s11bq11b,gen(remittance_use02)
decode s11bq11c,gen(remittance_use03)
*decode s11bq11o,gen(remittance_use00)
ren s11bq12 remittance_amount_food
ren s11bq13 remittance_amount_nonfood
decode s11bq14,gen(remitter_location)
keep clust nh remitterID female relation remittance_*
ren clust EaId
ren nh HhId
gen Survey = "GLSS6"
append using `final'
save `final',replace
}
qui{ //GlSS7
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTB\g7sec11b.dta",clear 
ren s11bq4 pid
drop if pid == 0
drop if pid == .
merg m:1 hid clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec1"
keep if _merge==3
keep hid clust nh s11bq3 s1q2 s1q3
merg 1:1 hid clust nh s11bq3 using "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTB\g7sec11b.dta"
for var s1q2 s1q3 s11bq5 s11bq6:decode X,gen(Xx)
gen female = inlist("Female",s1q2x,s11bq6x)
ren s1q3x relation
replace relation = s11bq5x if relation == ""
ren s11bq3 remitterID
decode s11bq7 ,gen(remittance_frequency)
ren s11bq8 remittance_is_loan
ren s11bq9 remittance_amount_cash
ren s11bq9a remittance_cost
decode s11bq10,gen(remittance_mode)
decode s11bq11a,gen(remittance_use01)
decode s11bq11b,gen(remittance_use02)
decode s11bq11c,gen(remittance_use03)
*decode s11bq11o,gen(remittance_use00)
ren s11bq12 remittance_amount_food
ren s11bq13 remittance_amount_nonfood
decode s11bq14,gen(remitter_location)
keep clust nh remitterID female relation remittance_*
ren clust EaId
ren nh HhId
gen Survey = "GLSS7"
append using `final'
save `final',replace
}
qui{ //Clean
ren (relation female) (remitter_relation remitter_female)

_strip_labels *

qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var remitter_relation remittance_frequency remittance_mode remittance_use*: replace X = `code'(X)
}

gen remittance_for_business = inlist("Business",remittance_use01,remittance_use02,remittance_use03)
gen remittance_for_consumption = inlist("Daily Consumption",remittance_use01,remittance_use02,remittance_use03)
gen remittance_for_education = inlist("Education",remittance_use01,remittance_use02,remittance_use03)
gen remittance_for_ceremonies = inlist("Funerals",remittance_use01,remittance_use02,remittance_use03) | inlist("Other Ceremonies",remittance_use01,remittance_use02,remittance_use03) 
gen remittance_for_health = inlist("Health",remittance_use01,remittance_use02,remittance_use03)
gen remittance_for_housing = inlist("Housing",remittance_use01,remittance_use02,remittance_use03)
*gen remittance_for_other = inlist("Nothing Else",remittance_use01,remittance_use02,remittance_use03) | inlist("Other",remittance_use01,remittance_use02,remittance_use03)
gen remittance_for_savings = inlist("Savings",remittance_use01,remittance_use02,remittance_use03)
drop remittance_use*

replace remittance_mode = "1" if remittance_mode == "Brought Home By Migrant"
replace remittance_mode = "1" if remittance_mode == "Sender Him/Herslf"

replace remittance_mode = "2" if remittance_mode == ""
replace remittance_mode = "2" if remittance_mode == "Other"
replace remittance_mode = "2" if remittance_mode == "Friends/Relations"
replace remittance_mode = "2" if remittance_mode == "Friends/Relatives"
replace remittance_mode = "2" if remittance_mode == "Cash Carried By Someone Else"

replace remittance_mode = "3" if remittance_mode == "Agent/Courier (Dhl)"
replace remittance_mode = "3" if remittance_mode == "Post Office"

replace remittance_mode = "4" if remittance_mode == "Bank Accounts"
replace remittance_mode = "4" if remittance_mode == "Bank Transfers (Cheques, Drafts, Direct Deposits, Etc)"

replace remittance_mode = "5" if remittance_mode == "Fast Money Transfer (Gcb)"
replace remittance_mode = "5" if remittance_mode == "Money Transfer Agency"
replace remittance_mode = "5" if remittance_mode == "Moneygram"
replace remittance_mode = "5" if remittance_mode == "Vigo (Merchant Bank)"    
replace remittance_mode = "5" if remittance_mode == "Western Union"

replace remittance_mode = "6" if inlist(Survey,"GLSS7") & remittance_mode == "Mtn Mmt"
replace remittance_mode = "6" if inlist(Survey,"GLSS7") & remittance_mode == "Airtel Mmt"
replace remittance_mode = "6" if inlist(Survey,"GLSS7") & remittance_mode == "Tigo Mmt"
replace remittance_mode = "6" if inlist(Survey,"GLSS7") & remittance_mode == "Vodafone Mmt"

replace remittance_mode = "2" if ! inlist(remittance_mode,"1","2","3","4","5","6")


gen     Relate_ = "1" if inlist(remitter_relation,"Household Head","Head")
replace Relate_ = "2" if inlist(remitter_relation,"Spouse","Spouse (Wife/Husband)","Spouse (Wife/Husband/Living Together)")
replace Relate_ = "3" if inlist(remitter_relation,"Child","Child (Son/Daughter)","Son/Daughte")
replace Relate_ = "3" if inlist(remitter_relation,"Adopted Child","Adopted / Foster/ Step Child","Adopted/Foster/Stepchild","Adopted, Foster, Or Stepchild","Adopted, Foster/Stepchild","Foster Child","Step Child","Adopted/Foster/Step Child")
replace Relate_ = "3" if inlist(remitter_relation,"Grandchild","Great Grandchild")
replace Relate_ = "4" if inlist(remitter_relation,"Parent/Parent-In-Law","Parent/Parentlaw","Parent Or Parent In Law","Parent","Parent Or Parent-In-Law","Parent-In-Law")
replace Relate_ = "4" if inlist(remitter_relation,"Son/Daughter-In-Law","Son/Daughterlaw","Son/Daughter In Law","Daughter In-Law","Son Or Daughter In-Law","Son-In-Law")
replace Relate_ = "5" if inlist(remitter_relation,"Other Relative","Other Related","Brother/Sister","Sister/Brother")
replace Relate_ = "6" if inlist(remitter_relation,"House Help (Non-Relative)","Househelp","Househelp (Other Relative)")
replace Relate_ = "6" if inlist(remitter_relation,"Other","Non_Relative","Non-Relative","Other Unrelated","Non Relative","Other (Specify)")
tab remitter_relation if Relate_ ==""
drop remitter_relation
ren Relate_ remitter_relation 
replace remitter_relation = "6" if remitter_relation==""


replace remittance_frequency = "52.1429" if remittance_frequency == "Yes Weekly"
replace remittance_frequency = "52.1429" if remittance_frequency == "Yes, Weekly" 
replace remittance_frequency = "12" if remittance_frequency == "Yes, Monthly"
replace remittance_frequency = "12" if remittance_frequency == "Yes,Monthly"
replace remittance_frequency = "4" if remittance_frequency == "Yes, Quarterly"
replace remittance_frequency = "4" if remittance_frequency == "Yes, Quaterly"
replace remittance_frequency = "1" if remittance_frequency == "Yes, Annually" 
replace remittance_frequency = "1" if remittance_frequency == "No"
replace remittance_frequency = "1" if remittance_frequency == "Other"

destring remittance_frequency,replace force

for var remittance_amount_cash remittance_cost remittance_amount_food remittance_amount_nonfood:replace X=. if X<0

replace remittance_is_loan = remittance_is_loan== 1
gen remitters_size = 1

for var remittance_amount_cash remittance_amount_food remittance_amount_nonfood remittance_cost:replace X=X*remittance_frequency


egen remittance_amount_total = rowtotal(remittance_amount_cash remittance_amount_food remittance_amount_nonfood)
gen remittance_as_loan = remittance_is_loan*remittance_amount_total

gen remittance_from_females =remitter_female*remittance_amount_total


egen share_mode = sum(remittance_amount_total) ,by(Survey EaId HhId)
replace share_mode = remittance_amount_total/share_mode

tempfile temp final

save `temp',replace


collapse (sum) remitters_size remittance_from_females remittance_amount_* remittance_as_loan remittance_cost /*
*/ (max) remittance_for_* (mean) remittance_frequency, by(Survey EaId HhId)

replace remittance_from_females = remittance_from_females/remittance_amount_total
replace remittance_as_loan = remittance_as_loan/remittance_amount_total
gen remittance_as_cash = remittance_amount_cash/remittance_amount_total
gen remittance_as_food = remittance_amount_food/remittance_amount_total
gen remittance_as_nonfood = remittance_amount_nonfood/remittance_amount_total
drop remittance_amount_cash remittance_amount_food remittance_amount_nonfood

save `final',replace


use `temp',clear 
collapse (sum) share_mode , by(Survey EaId HhId remitter_relation)
destring remitter_relation, replace force

reshape wide share_mode, i(Survey EaId HhId) j(remitter_relation)
ren share_mode1 remittance_mode_self
ren share_mode2 remittance_mode_other
ren share_mode3 remittance_mode_courier
ren share_mode4 remittance_mode_bank
ren share_mode5 remittance_mode_mta
ren share_mode6 remittance_mode_mmt
replace remittance_mode_mmt = 0 if ! inlist(Survey,"GLSS7")
merg 1:1 Survey EaId HhId using `final'
drop _merge
save `final',replace

use `temp',clear 
collapse (sum) share_mode , by(Survey EaId HhId remittance_mode)
destring remittance_mode, replace force

reshape wide share_mode, i(Survey EaId HhId) j(remittance_mode)
ren share_mode1 remittance_from_head
ren share_mode2 remittance_from_spouse
ren share_mode3 remittance_from_child
ren share_mode4 remittance_from_inlaw
ren share_mode5 remittance_from_relative
ren share_mode6 remittance_from_other

merg 1:1 Survey EaId HhId using `final'
drop _merge
save `final',replace


order Survey EaId HhId remitters_size remittance_from_females remittance_from_* remittance_mode_* /*
*/ remittance_amount_total remittance_frequency remittance_as_* remittance_cost remittance_for_*

for var remittance_from_* remittance_mode_* remittance_as_* remittance_for_* remittance_cost remittance_amount_total:replace X=0 if X==.

drop if inlist(remittance_amount_total,0,.)

for var remittance_cost remittance_amount_total:replace X=X/10000 if inlist(Survey,"GLSS5","GLSS4","GLSS3")

gen     CPI = 1.257/305.788   if Survey == "GLSS1"  //1987/88
replace CPI = 1.574/305.788   if Survey == "GLSS2"  //1988/89
replace CPI = 2.551/305.788   if Survey == "GLSS3"  //1990/91
replace CPI = 15.007/305.788  if Survey == "GLSS4"  //1997/98
replace CPI = 58.705/305.788  if Survey == "GLSS5"  //2005/06
replace CPI = 100.00/305.788  if Survey == "GSPS1" //2009/10
replace CPI = 118.687/305.788 if Survey == "GLSS6"  //2012/13
replace CPI = 152.988/305.788 if Survey == "RISING1" //2014/15
replace CPI = 152.988/305.788 if Survey == "GSPS2" //2014/15
replace CPI = 305.788/305.788 if Survey == "GLSS7"  //2016/17

for var remittance_cost remittance_amount_total:replace X=X/CPI

ren Survey Surveyx

tempfile temp
save `temp',replace
use "$LabGitHub\harmonized_crop_farmer_data",clear

collapse (mean) WeightHH,by(EaId HhId Surveyx)

merg 1:m EaId HhId Surveyx using `temp'

keep if _merge==3

drop WeightHH _merge

for var * : lab var X ""

lab var EaId "Enumeration area identifier"
lab var HhId "Household identifier"
lab var Surveyx "Survey wave identifier"
lab var remitters_size "Number of remitters"
lab var remittance_from_females "share of remittance from females"
lab var remittance_from_head "share of remittance from household head"
lab var remittance_from_spouse "share of remittance from spouse of household head"
lab var remittance_from_child "share of remittance from child of household head"
lab var remittance_from_inlaw "share of remittance from inlaw of household head"
lab var remittance_from_relative "share of remittance from other relatives of household head"
lab var remittance_from_other "share of remittance from non-relatives of household head"
lab var remittance_mode_self "share of remittance transmited via in-person delivery by remitters"
lab var remittance_mode_other "share of remittance transmited via other means"
lab var remittance_mode_courier "share of remittance transmited via Courier"
lab var remittance_mode_bank "share of remittance transmited via Bank Transfers (Cheques, Drafts, Direct Deposits, Etc)"
lab var remittance_mode_mta "share of remittance transmited via Money Transfer Agency (eg. wester union)"
lab var remittance_mode_mmt "share of remittance transmited via Mobile Money Transfer"
lab var remittance_amount_total "total annual cash and inkind remittance amount in real 2016/17 GHC"
lab var remittance_frequency "remittance frequency (weeks)"
lab var remittance_as_loan "share of remittance given as loan"
lab var remittance_as_cash "share of remittance transmited as cash"
lab var remittance_as_food "share of remittance transmited as food"
lab var remittance_as_nonfood "share of remittance transmited as nonfood"
lab var remittance_cost "cost of remittance transmision"
lab var remittance_for_business "remittance partly or fully used for business"
lab var remittance_for_consumption "remittance partly or fully used for consumption"
lab var remittance_for_education "remittance partly or fully used for education"
lab var remittance_for_ceremonies "remittance partly or fully used for ceremonies"
lab var remittance_for_health "remittance partly or fully used for health"
lab var remittance_for_housing "remittance partly or fully used for housing"
lab var remittance_for_savings "remittance partly or fully used for savings"

drop remittance_mode_* remittance_from_*

compress
saveold "$LabGitHub\harmonized_income_transfer_data",replace ver(12)
}
