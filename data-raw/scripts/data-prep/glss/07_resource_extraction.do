*==============================================================================
* 07_resource_extraction.do
*
* Builds the 'Harmonized Resources extraction data' release.
*
* WRITES: $LabGitHub\harmonized_resources_extraction_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 1517-1679.
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


*GlSS7
tempfile Final
use "$DATABASE/GLSS/Datasets/GSS/GLSS7/Data/COMMUNITY/g7comSEC0",clear
gen ComName = comname
keep supid clusterno region district comname
save `Final',replace
use "$DATABASE/GLSS/Datasets/GSS/GLSS7/Data/COMMUNITY/g7comSEC2",clear 
gen ComName = comname
merg 1:1 supid clusterno comname using `Final'
ren clusterno EaId
gen Surveyx = "GLSS7"
for var cs2q1a cs2q1b cs2q1c cs2q1d cs2q35a cs2q35b:decode X,gen(Xx)
qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var cs2q1ax cs2q1bx cs2q1cx cs2q1dx cs2q35ax cs2q35bx:replace X = `code'(X)
}
for var cs2q1ax cs2q1bx cs2q1cx cs2q1dx cs2q35ax cs2q35bx:tab X
gen mining_comm = 0
gen mining_gala = 0
gen sand        = 0
gen salt        = 0
gen quarrying   = 0
for var cs2q1a cs2q1b cs2q1c cs2q1d cs2q35a cs2q35b:replace mining_comm  = 1 if inlist(Xx,"Gold Mining (Commercial)","Diamond Mining (Commercial)","Bauxite")
for var cs2q1a cs2q1b cs2q1c cs2q1d cs2q35a cs2q35b:replace mining_gala  = 1 if inlist(Xx,"Diamond Mining (Galamsey)","Gold Mining (Galamsey)")
for var cs2q1a cs2q1b cs2q1c cs2q1d cs2q35a cs2q35b:replace sand         = 1 if Xx == "Sand Winning"
for var cs2q1a cs2q1b cs2q1c cs2q1d cs2q35a cs2q35b:replace quarrying    = 1 if Xx == "Quarrying"
for var cs2q1a cs2q1b cs2q1c cs2q1d cs2q35a cs2q35b:replace salt         = 1 if Xx == "Salt Mining"
collapse (max) mining_comm mining_gala sand quarrying salt,by( Surveyx EaId)
replace EaId= EaId+70000 
egen extraction_any = rowmax(mining_comm mining_gala sand quarrying salt)
sum mining_comm mining_gala sand quarrying salt
save `Final',replace
/*
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\NONFARM\g7sec10a",clear
for var s10aq5b s10aq6b:decode X,gen(Xx)
ren clust EaId
gen extraction_any10 = s10aq5bx == "Mining and quarrying" | s10aq6bx == "Mining and quarrying"
collapse (max) extraction_any10,by(EaId)
merg 1:m EaId using `Final'
replace extraction_any = 1 if extraction_any10 ==1
drop if _merge==1
drop _merge extraction_any10
save `Final',replace
*/
*GlSS6
use "$DATABASE/GLSS/Datasets/GSS/GLSS6/Data/COMMUNITY/sec2",clear
ren clust EaId
gen Surveyx = "GLSS6"
for var cs2q1a cs2q1b cs2q1c cs2q1d:decode X,gen(Xx)
qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var cs2q1ax cs2q1bx cs2q1cx cs2q1dx:replace X = `code'(X)
}
for var cs2q1ax cs2q1bx cs2q1cx cs2q1dx:tab X
gen mining_comm = 0
gen mining_gala = 0
gen sand        = 0
gen salt        = 0
gen quarrying   = 0
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace mining_comm  = 1 if Xx == "Commercial Mining"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace mining_gala  = 1 if Xx == "Small Scale Mining"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace sand         = 1 if Xx == "Sand Winning"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace quarrying    = 1 if Xx == "Quarrying"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace salt         = 1 if Xx == "Salt Minning"
collapse (max) mining_comm mining_gala sand quarrying salt,by( Surveyx EaId)
egen extraction_any = rowmax(mining_comm mining_gala sand quarrying salt)
sum mining_comm mining_gala sand quarrying salt
append using `Final', force
save `Final',replace

*GlSS5
use "$DATABASE/GLSS/Datasets/GSS/GLSS5/Data/community/com-sec21",clear
ren clust EaId
gen Surveyx = "GLSS5"
for var cs2q1a cs2q1b cs2q1c cs2q1d:decode X,gen(Xx)
qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var cs2q1ax cs2q1bx cs2q1cx cs2q1dx:replace X = `code'(X)
}
for var cs2q1ax cs2q1bx cs2q1cx cs2q1dx:tab X
gen mining_any  = 0
gen sand        = 0
gen salt        = 0
gen quarrying   = 0
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace mining_any  = 1 if Xx == "Mining"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace sand        = 1 if Xx == "Sand Winning"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace quarrying   = 1 if Xx == "Quarrying"
for var cs2q1a cs2q1b cs2q1c cs2q1d:replace salt        = 1 if Xx == "Salt Winning"
collapse (max) mining_any sand quarrying salt,by( Surveyx EaId)
egen extraction_any = rowmax(mining_any sand quarrying salt)
sum mining_any sand quarrying salt
append using `Final', force
save `Final',replace

*GlSS4
use "$DATABASE/GLSS/Datasets/GSS/GLSS4/Data/community/CS2",clear
gen EaId = 4000+eanum
gen Surveyx = "GLSS4"
gen mining_any  = 0
gen sand        = 0
gen salt        = 0
gen quarrying   = 0
/*
Salt "winning" .............5 
Mining .........................6
Sand winning................7
Quarrying .....................8 
*/
for var s2q1a s2q1b s2q1c s2q1d:replace mining_any  = 1 if X == 6
for var s2q1a s2q1b s2q1c s2q1d:replace sand        = 1 if X == 7
for var s2q1a s2q1b s2q1c s2q1d:replace quarrying   = 1 if X == 8
for var s2q1a s2q1b s2q1c s2q1d:replace salt        = 1 if X == 5
collapse (max) mining_any sand quarrying salt,by( Surveyx EaId)
egen extraction_any = rowmax(mining_any sand quarrying salt)
sum mining_any sand quarrying salt
append using `Final', force
save `Final',replace
/*
*GlSS3
use "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\community\cs1",clear
merg m:m clust using "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\community\cs2"
ren clust EaId
gen Surveyx = "GLSS3"
gen extraction_any  = 0
for var econ1 econ2 econ3 econ4:replace extraction_any  = 1 if X == 7.1
for var econ1 econ2 econ3 econ4:replace extraction_any  = 1 if X == 8.2
keep Surveyx EaId extraction_any 
collapse (max) extraction_any,by( Surveyx EaId)
sum extraction_any 
append using `Final', force
save `Final',replace


*GlSS1 & GLSS2
tempfile GlSS
use "$DATABASE\GLSS\Datasets\GSS\GLSS2\Data\Nada\COMM",clear
gen EaId = 2000+clust 
gen Surveyx = "GLSS2"
save `GlSS', replace
/*
use "$DATABASE\GLSS\Datasets\GSS\GLSS1\Data\Nada\COMM",clear
gen EaId = 1000+clust 
gen Surveyx = "GLSS1"
append using `GlSS', force
*/
gen extraction_any  = 0
for var econ1 econ2 econ3 econ4:replace extraction_any  = 1 if X == 6
collapse (max) extraction_any,by( Surveyx EaId)
sum extraction_any 
append using `Final', force
save `Final',replace
*/
replace mining_any = 1 if mining_comm == 1
replace mining_any = 1 if mining_gala == 1
replace mining_any = 0 if mining_gala == 0 & mining_comm == 0

for var extraction_any mining_any mining_comm mining_gala quarrying sand salt:replace X=-999 if X==.

collapse (max) extraction_any mining_any mining_comm mining_gala quarrying sand salt,by(Surveyx EaId)
for var extraction_any mining_any mining_comm mining_gala quarrying sand salt:replace X=. if X==-999
tabstat extraction_any mining_any mining_comm mining_gala quarrying sand salt,by(Surveyx)

saveold "$LabGitHub\harmonized_resources_extraction_data",replace version(12)
