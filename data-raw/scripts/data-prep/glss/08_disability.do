*==============================================================================
* 08_disability.do
*
* Builds the 'Harmonized Disability data' release.
*
* WRITES: $LabGitHub\harmonized_disability_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 1680-1878.
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


tempfile Health 

*GLSS4
use "$DATABASE\GLSS\Datasets\GSS\GLSS4\Data\parta\SEC4G",clear //ACTIVITY STATUS AND EMPLOYMENT SEARCH IN THE LAST 7 DAYS
gen disabled_workavail07 = s4gq7 == 5 //Why was (NAME) not available for work during the last 7 days or within the next 4 weeks days? [7 DAYS]
egen disabled = rowmax(disabled_*)
ren clust EaId
ren nh HhId
ren pid Mid
gen Surveyx = "GLSS4"
keep Surveyx EaId HhId Mid disabled disabled_* 
saveold `Health',replace

*GLSS5
use "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\parta\sec4d",clear                                                    //UNEMPLOYMENT IN LAST 7 DAYS.
merg 1:1 clust hhid nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\parta\sec4g",nogenerate update force replace  //EMPLOYMENT SEAR CH IN THE PAST 12 MONTHS
gen disabled_workavail07 = s4dq10 == 5 //Why was (NAME) not available for work during the last 7 days or within the next 4 weeks days? [7 DAYS]
gen disabled_workavail12 = s4gq7  == 3 //What was (NAME) doing when not available and not seeking for work? [12 MONTHS]
egen disabled = rowmax(disabled_*)
ren clust EaId
ren nh HhId
ren pid Mid
gen Surveyx = "GLSS5"
keep Surveyx EaId HhId Mid disabled disabled_* 
append using `Health', force
saveold `Health',replace

*GLSS6
use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC2a",clear                                                     //GENERAL EDUCATION
merg 1:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC3a",nogenerate update force replace  //HEALTH CONDITION AND DISABILITY
merg 1:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC4d",nogenerate update force replace  //UNEMPLOYMENT IN LAST 7 DAYS
merg 1:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC4g",nogenerate update force replace  //EMPLOYMENT SEAR CH IN THE PAST 12 MONTHS
gen pid = PID
merg 1:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\GOVERNANCE\gps-sec13c",nogenerate update force replace  //GOVERNANCE - VIOLENCE AND SECURITY
merg 1:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\GOVERNANCE\gps-sec13f",nogenerate update force replace  //GOVERNANCE - POLITICAL ENGAGEMENT
gen disabled_edu   = s2aq1a == 2  //What is/was the main reason why (NAME) has never attended school?
gen disabled_health= s3aq26 == 1  //Does (NAME) have any serious disability that limits his/her full participation in life activities (such as mobility, work, social life, etc.)
decode s3aq27, gen (disability)   //What type of disability does (NAME) have?
gen disabled_workeffort   = s4dq4  == 10 //Why has (NAME) not made any effort to find work or start a business? [7 DAYS] {Disabled or unable to work (handicapped}
gen disabled_workavail07  = s4dq10 == 5  //Why was (NAME) not available for work during the last 7 days or within the next 4 weeks days? [7 DAYS]
gen disabled_workavail12 = s4gq7  == 3  //What was (NAME) doing when not available and not seeking for work? [12 MONTHS]
gen disabled_govc  = s13cq27g == 1 //Regarding the provision of public security services, have you ever been discriminated against because of your Disability
gen disabled_govf  = s13fq9 == 9   //Was not allowed to participate in any community level activities becasue of Disability
egen disabled = rowmax(disabled_*)
replace disability = "None" if disabled == 0
gen dis_emotional = disability=="Emotional"
gen dis_hearing   = disability=="Hearing"
gen dis_intellect = disability=="Intellect"
gen dis_physical  = disability=="Physical"
gen dis_sight     = disability=="Sight"
gen dis_speech    = disability=="Speech"
ren clust EaId
ren nh HhId
ren PID Mid
gen Surveyx = "GLSS6"
keep Surveyx EaId HhId Mid disabled disabled_* disability dis_*
append using `Health', force
saveold `Health',replace


*GLSS7
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec2",clear                                                    //GENERAL EDUCATION
merg 1:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec3a",nogenerate update force replace //HEALTH CONDITION AND DISABILITY
merg 1:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec4",nogenerate update force replace  //ECONOMIC ACTIVITY
merg 1:1 clust nh pid using "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\GOVERNANCE\g7sec13",nogenerate update force replace  //GOVERNANCE
gen disabled_edu   = s2aq1a == 2   //What is/was the main reason why (NAME) has never attended school?
gen disabled_health= s3aq26 == 1   //Does (NAME) have any serious disability that limits his/her full participation in life activities (such as mobility, work, social life, etc.)
decode s3aq27i, gen (disability)   //What type of disability does (NAME) have?
gen disabled_bus   = s4eq3  == 10  //Why has (NAME) not made any effort to find work or start a business? [7 DAYS] {Disabled or unable to work (handicapped}
gen disabled_workavail07  = s4eq10 == 5   //Why was (NAME) not available for work during the last 7 days or within the next 4 weeks days? [7 DAYS]
gen disabled_govc  = s13cq28g == 1 //Regarding the provision of public security services, have you ever been discriminated against because of your Disability
gen disabled_govf  = s13fq9 == 9   //Was not allowed to participate in any community level activities becasue of Disability
egen disabled = rowmax(disabled_*)
replace disability = "None" if disabled == 0
gen dis_emotional = s3aq27f==1
gen dis_hearing   = s3aq27b==1
gen dis_intellect = s3aq27e==1
gen dis_physical  = s3aq27d==1
gen dis_sight     = s3aq27a==1
gen dis_speech    = s3aq27c==1
ren clust EaId
ren nh HhId
ren pid Mid
gen Surveyx = "GLSS7"

keep Surveyx EaId HhId Mid disabled disabled_* disability dis_*
append using `Health', force
replace disability = "Intellect" if disability == "Intellectual"
saveold `Health',replace

use "$COLLATED\Output\Idividual Demographics", clear
decode Survey,gen(Surveyx)
keep Surveyx EaId HhId Mid Relate FthrID MthrID SpusID
merg 1:1 Surveyx EaId HhId Mid using `Health'
keep if _merge==3
decode Relate,gen(Relatex)
drop _merge
gen disabCat1 = disability == "Physical"
gen disabCat2 = disability == "Sight"
gen disabCat3 = disability == "Hearing"
gen disabCat4 = disability == "Speech"
gen disabCat5 = disability == "Intellect"
gen disabCat6 = disability == "Emotional"
gen disabCat7 = disability == "Other" 
saveold `Health',replace


tempfile farmer 
keep Surveyx EaId HhId Mid disabled disabCat*
ren disabled disabled_self
ren disabCat* disabCat*_self
merg 1:m Surveyx EaId HhId Mid using "$LabGitHub\harmonized_crop_farmer_data"
drop if _merge==1
drop _merge
saveold `farmer',replace

use `Health',clear
ren disabled disabled_child1
ren disabCat* disabCat*_child1
collapse (max) disabled_child1 disabCat*_child1,by(Surveyx EaId HhId MthrID)
ren MthrID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `Health',clear
ren disabled disabled_child2
ren disabCat* disabCat*_child2
collapse (max) disabled_child2 disabCat*_child2,by(Surveyx EaId HhId FthrID)
ren FthrID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `Health',clear
ren disabled disabled_spouse
ren disabCat* disabCat*_spouse
collapse (max) disabled_spouse disabCat*_spouse,by(Surveyx EaId HhId SpusID)
ren SpusID Mid
merg 1:m Surveyx EaId HhId Mid using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `Health',clear
gen disabled_close  = disabled == 1 & inlist(Relatex,"Child (Adopted)","Child (Son/Daughter)","Spouse")
ren disabCat* disabCat*_close
for var disabCat*_close:replace X=X*disabled_close
collapse (max) disabled_close disabCat*_close,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace

use `Health',clear
gen disabled_close  = disabled == 1 & inlist(Relatex,"Child (Adopted)","Child (Son/Daughter)","Spouse")
gen disabled_member = disabled & disabled_close == 0
ren disabCat* disabCat*_member
for var disabCat*_member:replace X=X*disabled_member
collapse (max) disabled_member disabCat*_member,by(Surveyx EaId HhId)
merg 1:m Surveyx EaId HhId using `farmer'
drop if _merge==1
drop _merge
saveold `farmer',replace
collapse (max) disabled_* disabCat*_*,by(Surveyx EaId HhId Mid)
for var disabled_* disabCat*_*:replace X=0 if X==.
merg 1:1 Surveyx EaId HhId Mid using `Health'
drop if _merge==1
egen disabled_child  = rowmax(disabled_child2 disabled_child1)
egen disabCat1_child = rowmax(disabCat1_child2 disabCat1_child1)
egen disabCat2_child = rowmax(disabCat2_child2 disabCat2_child1)
egen disabCat3_child = rowmax(disabCat3_child2 disabCat3_child1)
egen disabCat4_child = rowmax(disabCat4_child2 disabCat4_child1)
egen disabCat5_child = rowmax(disabCat5_child2 disabCat5_child1)
egen disabCat6_child = rowmax(disabCat6_child2 disabCat6_child1)
egen disabCat7_child = rowmax(disabCat7_child2 disabCat7_child1)
drop disabled_child2 disabled_child1 _merge disabCat1_child2 disabCat2_child2 disabCat3_child2 disabCat4_child2 disabCat5_child2 disabCat6_child2 disabCat7_child2 disabCat1_child1 disabCat2_child1 disabCat3_child1 disabCat4_child1 disabCat5_child1 disabCat6_child1 disabCat7_child1
drop disabled disabCat1 disabCat2 disabCat3 disabCat4 disabCat5 disabCat6 disabCat7
drop SpusID FthrID MthrID Relate Relatex

for var disabled_* disabCat*_*:replace X=0 if X==.

egen disabled  = rowmax(disabled_*)
egen disabCat1 = rowmax(disabCat1_*)
egen disabCat2 = rowmax(disabCat2_*)
egen disabCat3 = rowmax(disabCat3_*)
egen disabCat4 = rowmax(disabCat4_*)
egen disabCat5 = rowmax(disabCat5_*)
egen disabCat6 = rowmax(disabCat6_*)
egen disabCat7 = rowmax(disabCat7_*)
egen disabCat1_6 = rowmax(disabCat1 disabCat2 disabCat3 disabCat4 disabCat5 disabCat6) 
replace disabCat7 = 1 if disabled == 1 & disabCat1_6 == 0
compress
saveold "$LabGitHub\harmonized_disability_data",replace version(12)
