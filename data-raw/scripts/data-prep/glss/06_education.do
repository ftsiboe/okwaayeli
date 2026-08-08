*==============================================================================
* 06_education.do
*
* Builds the 'Harmonized Education data' release.
*
* WRITES: $LabGitHub\harmonized_education_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 1368-1516.
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

tempfile Education temp temp1
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec1",clear
keep clust nh pid s1q16 s1q20
merg 1:1 clust nh pid using "$OneDrive\Research\Database\Ghana\Surveys\Database\GLSS\Datasets\GSS\GLSS7\Data\PARTA\g7sec2"
gen Surveyx = "GLSS7"
ren clust EaId
ren nh HhId
ren pid Mid
save `temp', replace

use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC1",clear
keep clust nh PID s1q16 s1q20
merg 1:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC2a",nogenerate
merg 1:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC2b",nogenerate
merg 1:1 clust nh PID using "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTA\SEC2c",nogenerate
gen Surveyx = "GLSS6"
ren clust EaId
ren nh HhId
ren PID Mid

for var s1q16 s1q20 :replace X=13 if X==12
ren s2aq3 s2aq3_old
recode s2aq3_old (0 1 =0) (2 3 4=1) (5 6 =3) (7 8=6) (9=8) (10=9) (11=10), gen(s2aq3)
gen s2aq1b = s2aq3_old
append using `temp', force

ren (s1q16 s1q20 s2aq1b) (father_attained mother_attained farmer_attained)

* Formal education attempted

foreach agent in father mother farmer{
	gen attempt_KIN_`agent' = inlist(`agent'_attained,1)                  //at least Pre-school
	gen attempt_PRM_`agent' = inlist(`agent'_attained,1,2)                //at least P1
	gen attempt_JSS_`agent' = inlist(`agent'_attained,1,2,3,4)            //at least JSS1/JHS1/M1
	gen attempt_SSS_`agent' = inlist(`agent'_attained,1,2,3,4,5,6)        //at least SSS1/SHS1/S1/L6/U6 !!!! 
	gen attempt_VTC_`agent' = inlist(`agent'_attained,1,2,3,4,5,6,7)      //at least Voc/Tech/Comm/
	gen attempt_TAN_`agent' = inlist(`agent'_attained,1,2,3,4,5,6,8,9)    //at least Teacher, Agric/Nursing Training/polytechnic
	gen attempt_BSC_`agent' = inlist(`agent'_attained,1,2,3,4,5,6,10)     //at least University(bachelor)
	gen attempt_MSC_`agent' = inlist(`agent'_attained,1,2,3,4,5,6,10,11)  //at least University (post graduate)
	gen attempt_PRF_`agent' = inlist(`agent'_attained,1,2,3,4,5,6,12)     //at least Professional
}

tab farmer_attained attempt_MSC_farmer

* Formal education completed
gen complete_BECE = inlist(s2aq3,1,2)               //completed primary education or its equivalence
gen complete_SSCE = inlist(s2aq3,1,2,3,4,5)         //completed secondary education or its equivalence
gen complete_CERT = inlist(s2aq3,1,2,3,4,5,6)       //completed Voc/Tech/Comm. education or its equivalence
gen complete_HNDC = inlist(s2aq3,1,2,3,4,5,7,8)     //completed Polytechnic/Teacher, Agric/Nursing Training education or its equivalence
gen complete_UBSC = inlist(s2aq3,1,2,3,4,5,9)       //completed Bachelor degree or its equivalence
gen complete_UMSC = inlist(s2aq3,1,2,3,4,5,9,10)    //completed Master degree or its equivalence
gen complete_UPHD = inlist(s2aq3,1,2,3,4,5,9,10,11) //completed PHD degree or its equivalence
gen complete_PROF = inlist(s2aq3,1,2,3,4,5,12)      //completed professional certification (ACCA, ICA, CIMA, IT, CIB, etc) or its equivalence	
	
* currently in school
gen student = s2aq4 ==1 | s2aq5 ==1

* Public or Private?
gen schtype_public    = inlist(s2bq4,1) | inlist(s2bq9,1)
gen schtype_private   = inlist(s2bq4,2,3) | inlist(s2bq9,2,3)
gen schtype_religious = inlist(s2bq4,2) | inlist(s2bq9,2)
gen schtype_quasigovt = inlist(s2bq4,4) | inlist(s2bq9,4)

*literacy
gen read_english = inlist(s2cq1,1)
gen read_french  = inlist(s2cq1,2)

gen read_gh_akan     = inlist(s2cq2,2)
gen read_gh_ewe      = inlist(s2cq2,3)
gen read_gh_ga       = inlist(s2cq2,4)
gen read_gh_dagbani  = inlist(s2cq2,5)
gen read_gh_frafra   = inlist(s2cq2,6)
gen read_gh_nzema    = inlist(s2cq2,7)
gen read_gh_wali     = inlist(s2cq2,8)
gen read_gh_other    = inlist(s2cq2,9)

gen write_english = inlist(s2cq3,1)
gen write_french  = inlist(s2cq3,2)

gen write_gh_akan     = inlist(s2cq4,2)
gen write_gh_ewe      = inlist(s2cq4,3)
gen write_gh_ga       = inlist(s2cq4,4)
gen write_gh_dagbani  = inlist(s2cq4,5)
gen write_gh_frafra   = inlist(s2cq4,6)
gen write_gh_nzema    = inlist(s2cq4,7)
gen write_gh_wali     = inlist(s2cq4,8)
gen write_gh_other    = inlist(s2cq4,9)

gen literacy_course   = inlist(s2cq6,1)

*numeracy
gen numeracy = inlist(s2cq5,1)
	
*apprentice
gen apprentice_present = inlist(s2cq9,1)
gen apprentice_past = inlist(s2cq9,2)

*training
gen train_Clerical    = inlist(s2cq15,1)
gen train_Managerial  = inlist(s2cq15,2)
gen train_ICT         = inlist(s2cq15,3)
gen train_Marketing   = inlist(s2cq15,4)
gen train_Teaching    = inlist(s2cq15,5)
gen train_Leadership  = inlist(s2cq15,6)
gen train_Medicine    = inlist(s2cq15,7)
gen train_Accountancy = inlist(s2cq15,8)
gen train_Trade       = inlist(s2cq15,9)
gen train_Other       = inlist(s2cq15,10)

drop father_attained mother_attained farmer_attained

keep Surveyx EaId HhId Mid attempt_* complete_* student schtype_* read_* write_* literacy_course numeracy apprentice_* train_*
save `temp', replace

use "$COLLATED\Output\Household Identification.dta",clear 
decode Survey,gen(Surveyx)
keep Surveyx EaId HhId Weight Locality district Region
merg 1:m Surveyx EaId HhId using `temp'
keep if _merge==3
drop _merge
save `temp', replace
use "$COLLATED\Output\Idividual Education",clear
decode Survey,gen(Surveyx)
keep HhId EaId Mid YerEdu EduLevel EduWhyNo Surveyx
merg 1:m Surveyx EaId HhId Mid using `temp'
keep if _merge==3
drop _merge

egen any_train = rowmax(train_*)
egen any_read  = rowmax(read_*)
egen any_write = rowmax(write_*)
egen any_literacy = rowmax(write_* read_* literacy_course)
egen local_literacy = rowmax(read_gh_* write_gh_*)
egen fregn_literacy = rowmax(read_english read_french write_english write_french)
egen apprentice = rowmax(train_Clerical train_Managerial train_ICT train_Marketing train_Teaching train_Leadership train_Accountancy train_Trade)
gen any_formal = EduLevel > 0 | YerEdu > 0
egen educated = rowmax(any_train any_read any_write any_literacy local_literacy fregn_literacy apprentice numeracy any_formal student)

keep Surveyx EaId HhId Mid educated numeracy YerEdu EduLevel EduWhyNo any_formal /*
*/ any_read any_write any_literacy local_literacy fregn_literacy any_train apprentice student

foreach xx in numeracy YerEdu EduLevel any_formal /*
*/ any_read any_write any_literacy local_literacy fregn_literacy any_train apprentice student{
	replace `xx'    = . if `xx' == 0 & educated == 1
}

saveold "$LabGitHub\harmonized_education_data",replace version(12)
