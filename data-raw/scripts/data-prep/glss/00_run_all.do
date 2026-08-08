*==============================================================================
* 00_run_all.do -- master for the GLSS harmonization scripts
*
* Runs the harmonizers in this folder in an order that respects their one
* dependency, logs the whole run, and stops at the first failure.
*
*   do data-raw/scripts/data-prep/glss/00_run_all.do              // everything
*   do data-raw/scripts/data-prep/glss/00_run_all.do 11           // one script
*   do data-raw/scripts/data-prep/glss/00_run_all.do 01 02 03     // a subset
*
* Run from the okwaayeli repo root, or from this folder.
*
* ORDER IS NOT COSMETIC. 02 and 03 read harmonized_crop_farmer_data.dta back
* from $LabGitHub, so 01 has to have written it. Everything else is
* independent, but the numbering is kept stable so a subset argument means the
* same thing tomorrow.
*
* 05_offfarm_work.do is NOT in the default list. Its saveold is commented out,
* it produces no release, and nothing reads one. Name it explicitly to run it.
*
* A FULL RUN REWRITES EVERY RELEASE IN data-raw/releases/harmonized_data/.
* That is usually what you want and occasionally not. Pass a subset when you
* only meant to rebuild one.
*==============================================================================

clear all
set more off

local todo `0'

* --- paths -------------------------------------------------------------------
local _p ""
capture confirm file "_paths.do"
if !_rc local _p "_paths.do"
if "`_p'" == "" {
    capture confirm file "data-raw/scripts/data-prep/glss/_paths.do"
    if !_rc local _p "data-raw/scripts/data-prep/glss/_paths.do"
}
if "`_p'" == "" {
    di as err "Cannot locate _paths.do. Run this from the okwaayeli repo root"
    di as err "or from data-raw/scripts/data-prep/glss/."
    exit 601
}
run "`_p'"

* --- the run list ------------------------------------------------------------
local f01 "01_crop_farmer.do"
local f02 "02_income_transfer.do"
local f03 "03_financial_inclusion.do"
local f04 "04_nonfarm_enterprise.do"
local f05 "05_offfarm_work.do"
local f06 "06_education.do"
local f07 "07_resource_extraction.do"
local f08 "08_disability.do"
local f09 "09_societal_peace_and_cohesion.do"
local f10 "10_land_tenure.do"
local f11 "11_ag_services.do"
local f12 "12_time_poverty.do"

if "`todo'" == "" local todo "01 02 03 04 06 07 08 09 10 11 12"

* --- validate the whole list BEFORE running anything -------------------------
* A typo in the fourth id should not surface after the first three have already
* rewritten their releases.
foreach n of local todo {
    local f "`f`n''"
    if "`f'" == "" {
        di as err "Unknown script id '`n''. Valid ids: 01 02 03 04 05 06 07 08 09 10 11 12"
        exit 198
    }
    capture confirm file "$GLSS/`f'"
    if _rc {
        di as err "Missing script: $GLSS/`f'"
        exit 601
    }
}

capture log close _all
log using "$GLSS/logs/00_run_all.log", replace text

di as txt "{hline 78}"
di as txt "GLSS harmonization -- started $S_DATE $S_TIME"
di as txt "Scripts: `todo'"
di as txt "Releases go to: $LabGitHub"
di as txt "{hline 78}"

foreach n of local todo {
    local f "`f`n''"
    di as txt _n "{hline 78}"
    di as txt ">>> `f'   (started $S_TIME)"
    di as txt "{hline 78}"

    capture noisily run "$GLSS/`f'"

    if _rc {
        di as err _n "{hline 78}"
        di as err "FAILED: `f'  (Stata return code `=_rc')"
        di as err "Stopped here. Releases written by earlier scripts in this run"
        di as err "are already on disk; nothing after this point ran."
        di as err "{hline 78}"
        log close
        exit _rc
    }

    di as txt _n "<<< `f' OK   ($S_TIME)"
}

di as txt _n "{hline 78}"
di as txt "GLSS harmonization -- finished $S_DATE $S_TIME"
di as txt "Ran: `todo'"
di as txt "{hline 78}"
log close
