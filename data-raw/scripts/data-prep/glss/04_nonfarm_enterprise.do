*==============================================================================
* 04_nonfarm_enterprise.do
*
* Builds the 'Harmonized non-farm ent' release.
*
* WRITES: $LabGitHub\harmonized_nonfarm_enterprise_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 853-1101.
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


tempfile farmer ParentData
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\NONFARM\g7sec10a.dta",replace
ren clust EaId
ren nh HhId
ren s10aq3 Mid0
ren s10aq3a Mid1
ren s10aq4 Mid2
decode s10aq5b, gen(ISIC1x)
decode s10aq6b , gen(ISIC2x)
gen Surveyx = "GLSS7"
keep Surveyx EaId HhId Mid1 Mid2 ISIC1x ISIC2x
save `farmer',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\NONFARM\sec10A.dta",replace
ren clust EaId
ren nh HhId
ren s10aq3 Mid1
ren s10aq4 Mid2
gen Survey = "GLSS6"
ren s10aq5 ISIC1
ren s10aq6 ISIC2
global Ent ISIC1
do "$Supplementaries\Programs\ISIC Recodes" 
global Ent ISIC2
do "$Supplementaries\Programs\ISIC Recodes" 
ren Survey Surveyx
keep Surveyx EaId HhId Mid1 Mid2 ISIC1x ISIC2x
append using `farmer'
save `farmer',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\NONFARM\sec10a.dta" ,replace
ren clust EaId
ren nh HhId
ren s10aq3 Mid1
ren s10aq4 Mid2
ren s10aq5 ISIC1
ren s10aq6 ISIC2
gen Survey="GLSS5"
global Ent ISIC1
do "$Supplementaries\Programs\ISIC Recodes" 
global Ent ISIC2
do "$Supplementaries\Programs\ISIC Recodes" 
ren Survey Surveyx
keep Surveyx EaId HhId Mid1 Mid2 ISIC1x ISIC2x
append using `farmer'
save `farmer',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS4\Data\NONFARM\SEC10A.DTA" ,replace
ren clust EaId
ren nh HhId
ren s10aq2 Mid1
ren s10aq3 Mid2
gen Survey="GLSS4"
ren s10aq4 ISIC1
global Ent ISIC1
do "$Supplementaries\Programs\ISIC Recodes" 
ren Survey Surveyx
keep Surveyx EaId HhId Mid1 Mid2 ISIC1x
append using `farmer'
save `farmer',replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\NONFARM\S10A.DTA" ,replace
ren clust EaId
ren nh HhId
ren s10aq2 Mid1
ren s10aq3 Mid2
gen Survey="GLSS3"
ren s10aq4 ISIC1
global Ent ISIC1
do "$Supplementaries\Programs\ISIC Recodes" 
ren Survey Surveyx
keep Surveyx EaId HhId Mid1 Mid2 ISIC1x
append using `farmer'
save `farmer',replace

gen ID = _n
reshape long Mid, i(Surveyx EaId HhId ID) j(Midx)
drop if Mid == .
gen indv_nonfarm = 1
collapse (max) indv_nonfarm,by(Surveyx EaId HhId Mid ISIC1x ISIC2x)

qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var ISIC1x ISIC2x:replace X = `code'(X)
}

gen ISIC1x_ = "xx"
gen ISIC2x_ = "xx"
*Agriculture
foreach Chrt in Farming Crop Animal Fishing Forestry Logging Hunting Aquaculture{
replace ISIC1x_="Agriculture" if ISIC1x_ =="xx" & strpos(ISIC1x , "`Chrt'")!=0
replace ISIC2x_="Agriculture" if ISIC2x_ =="xx" & strpos(ISIC2x , "`Chrt'")!=0
}

*Industry					
*"Mining and Quarrying"	Oil & Gas	Manufacturing	Electricity	"Water & Sewerage"	Construction
foreach Chrt in Manufacturing Mining Quarrying Construction{
replace ISIC1x_="Industry" if ISIC1x_ =="xx" & strpos(ISIC1x , "`Chrt'")!=0
replace ISIC2x_="Industry" if ISIC2x_ =="xx" & strpos(ISIC2x , "`Chrt'")!=0
}

*Services									
*Trade; Repair of Vehicles, Household Goods	Hotels And Restaurants	Transport & Storage	Information & Communication	 Financial & Insurance Activities	Real Estate, Professional, Administrative & other Services Activities	*Public Administration & Defence; Social Security	Education	Health	 Community, Social & Personal Service  Activities
foreach Chrt in Administrative "Public Admin" Health "Social Work" "Real Estate" Accommodation Recreation Entertainment /*
*/ Professional Transportation Storage Information Communication Financial Insurance "Other Service" Wholesale Retail /*
*/ "Agricultural Support" "Education" "Other Business Activities" "Extra-Territorial Organizations" "Service Other" "Utility Supply" /*
*/ "Activities Of Private Households" "Activities Of Extraterritorial" "Electricity, Gas, Stream" "Food Service" Food{
replace ISIC1x_="Services" if ISIC1x_ =="xx" & strpos(ISIC1x , "`Chrt'")!=0
replace ISIC2x_="Services" if ISIC2x_ =="xx" & strpos(ISIC2x , "`Chrt'")!=0
}

tab ISIC1x ISIC1x_ 
tab ISIC2x ISIC2x_ 
gen industry = inlist("Industry",ISIC1x_,ISIC2x_)
gen services = inlist("Services",ISIC1x_,ISIC2x_)
gen agriculture = inlist("Agriculture",ISIC1x_,ISIC2x_)

save `farmer',replace

use "$Dropbox_Personal\Database\Ghana\Surveys\Database\Output\Aggregation\Household Aggregates_WB", clear
decode Survey,gen(Surveyx)
ren INC_NFARM_G INC_NFARM
ren INC_FARM_G INC_FARM
ren INC_TOT_G INC_TOT
ren INC_RMT INC_REMIT 
keep Surveyx HhId EaId INC_FARM INC_NFARM INC_WAGE INC_RENT INC_OTHER INC_REMIT
merg 1:m Surveyx EaId HhId using `farmer',nogenerate

collapse (max) indv_nonfarm industry services agriculture (mean) INC_*,by(Surveyx EaId HhId Mid)
merg 1:m Surveyx EaId HhId Mid using "$LabGitHub\harmonized_crop_farmer_data"
drop if _merge==1
collapse (max) indv_nonfarm industry services agriculture (mean) INC_*,by(Surveyx EaId HhId Mid)

for var indv_nonfarm INC_* industry services agriculture:replace X=0 if X== .

ren indv_nonfarm nonfarm_self
*keep if inlist(Surveyx,"GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")

tabstat nonfarm_self industry services agriculture INC_FARM INC_NFARM INC_WAGE INC_RENT INC_REMIT INC_OTHER, by(Surveyx)

save `farmer',replace

use "$COLLATED\Output\Idividual Demographics", clear
decode Survey,gen(Surveyx)
keep Surveyx EaId HhId Mid Relate FthrID MthrID SpusID
merg 1:1 Surveyx EaId HhId Mid using `farmer'
keep if _merge==3
drop _merge
decode Relate,gen(Relatex)
save `ParentData',replace

use `ParentData',clear
gen nonfarm_child1 = nonfarm_self > 0 & nonfarm_self != .
collapse (max) nonfarm_child1,by(Surveyx EaId HhId MthrID)
ren MthrID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen nonfarm_child2 = nonfarm_self > 0 & nonfarm_self != .
collapse (max) nonfarm_child2,by(Surveyx EaId HhId FthrID)
ren FthrID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen nonfarm_spouse = nonfarm_self > 0 & nonfarm_self != .
collapse (max) nonfarm_spouse,by(Surveyx EaId HhId SpusID)
ren SpusID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen nonfarm_close = nonfarm_self > 0 & inlist(Relatex,"Child (Adopted)","Child (Son/Daughter)","Spouse") & nonfarm_self != .
collapse (max) nonfarm_close,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen nonfarm_close = nonfarm_self > 0 & inlist(Relatex,"Child (Adopted)","Child (Son/Daughter)","Spouse") & nonfarm_self != .
gen nonfarm_member = nonfarm_self > 0 & nonfarm_close == 0 & nonfarm_self != .
collapse (max) nonfarm_member nonfarm_close,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `ParentData',clear
gen nonfarm_hh = nonfarm_self > 0 & nonfarm_self != .
gen nonfarm_hh_n = nonfarm_self > 0 & nonfarm_self != .
collapse (sum) nonfarm_hh_n (max) nonfarm_hh,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

egen nonfarm_child  = rowmax(nonfarm_child*)
drop nonfarm_child2 nonfarm_child1
for var nonfarm_*:replace X=0 if X==.
saveold `farmer',replace

merg 1:m Surveyx EaId HhId Mid using "$LabGitHub\harmonized_crop_farmer_data"
keep if _merge==3
egen INC_TOT = rowtotal(INC_WAGE INC_FARM INC_NFARM INC_RENT INC_OTHER INC_REMIT)
for var INC_FARM INC_NFARM INC_WAGE INC_RENT INC_OTHER INC_REMIT:replace X = . if INC_TOT == 0
drop INC_TOT

gen     CPI = 1.257/305.788   if Surveyx == "GLSS1"  //1987/88
replace CPI = 1.574/305.788   if Surveyx == "GLSS2"  //1988/89
replace CPI = 2.551/305.788   if Surveyx == "GLSS3"  //1990/91
replace CPI = 15.007/305.788  if Surveyx == "GLSS4"  //1997/98
replace CPI = 58.705/305.788  if Surveyx == "GLSS5"  //2005/06
replace CPI = 100.00/305.788  if Surveyx == "GSPS1" //2009/10
replace CPI = 118.687/305.788 if Surveyx == "GLSS6"  //2012/13
replace CPI = 152.988/305.788 if Surveyx == "RISING1" //2014/15
replace CPI = 152.988/305.788 if Surveyx == "GSPS2" //2014/15
replace CPI = 305.788/305.788 if Surveyx == "GLSS7"  //2016/17
for var INC_FARM INC_NFARM INC_WAGE INC_RENT INC_OTHER INC_REMIT:replace X = (X/CPI)/HHSizeAE

keep Surveyx EaId HhId Mid nonfarm_* INC_* industry services agriculture 

keep if inlist(Surveyx,"GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")

tabstat nonfarm_self nonfarm_spouse nonfarm_child nonfarm_close nonfarm_member nonfarm_hh nonfarm_hh_n /*
*/ industry services agriculture INC_FARM INC_NFARM INC_WAGE INC_RENT INC_REMIT INC_OTHER, by(Surveyx)

order Surveyx EaId HhId Mid nonfarm_self nonfarm_spouse nonfarm_child nonfarm_close nonfarm_member nonfarm_hh nonfarm_hh_n /*
*/ industry services agriculture INC_FARM INC_NFARM INC_WAGE INC_RENT INC_REMIT INC_OTHER

sum 

tab nonfarm_self nonfarm_hh_n

collapse (max) nonfarm_* industry services agriculture (mean) INC_* ,by(Surveyx EaId HhId Mid)

for var nonfarm_close nonfarm_member nonfarm_spouse nonfarm_self nonfarm_child:replace X=. if X==0 & nonfarm_hh == 1

compress
saveold "$LabGitHub\harmonized_nonfarm_enterprise_data",replace version(12)
