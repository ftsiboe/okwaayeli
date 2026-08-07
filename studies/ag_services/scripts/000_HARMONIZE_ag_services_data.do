*==============================================================================
* 000_HARMONIZE_ag_services_data.do
*
* Build the harmonized community-level agricultural service dataset from the
* GLSS4-GLSS7 community questionnaires.
*
* POSITION: runs BEFORE 001_DATA_ag_services_study.R, which reads the release
* this file writes. Numbered 000 alongside 000_initialize.R, following the same
* same-number-different-language convention the repo already uses for
* 100_exhibits.do next to 100_exhibit_descriptive_stats.R.
*
* PROVENANCE: extracted verbatim on 2026-08-07 from the block
* `{ //Harmonized Ag services data }` at data-raw/okwaayeli_DATA.do L2470-2773,
* then scaffolded to run standalone. The parent block should be deleted once
* this file is confirmed to reproduce it.
*
* AUDIT: this block was audited on 2026-08-07 against the four GLSS community
* questionnaires and the GLSS5 Community Manual. Findings and their severities
* are in studies/ag_services/narrative/diagnostics/
* ag_services_harmonization_audit_plan.md. Every fix below carries an
* `AUDIT n.n` tag matching a section of that document.
*
* THE AUDIT_FIXES LEVER WAS REMOVED 2026-08-07, having done its job. While it
* existed this file could reproduce the parent block byte-for-byte (`cf _all`
* PASSED against the then-live release) and the two arms could be diffed to
* measure exactly what the audit changed. Both results are recorded in
* narrative/diagnostics/migration_2026-08-07.md, Addendum 6.
*
* Removed because a lever that keeps known-wrong code executable is a hazard:
* running the old arm and promoting it would republish the GLSS5 MOFA zeros and
* the fabricated compliance values. It also made the release SCHEMA conditional
* -- services*_strict existed on only one arm.
*
* The pre-audit build is preserved as
*   data-raw/releases/harmonized_data/harmonized_ag_services_data_PRE_AUDIT.dta
* so the comparison stays reproducible without keeping the code path alive.
*
* Run from the repo root:  do studies/ag_services/scripts/000_HARMONIZE_ag_services_data.do
*==============================================================================

clear all
set more off

*--- Paths (self-contained; mirrors data-raw/okwaayeli_DATA.do) ----------------
gl OneDrive          "C:/Users/ftsib/OneDrive"
gl Dropbox_Personal  "C:/Users/ftsib/Dropbox (Personal)"
gl DATABASE          "$OneDrive\Research\Database\Ghana\Surveys\Database"
gl REPO              "$Dropbox_Personal\GitHub\ghana\okwaayeli"
gl LabGitHub         "$REPO\data-raw\releases\harmonized_data"

* Paths below are ABSOLUTE, derived from $REPO. Relative paths break when the
* file is launched from Stata's do-file editor, which sets the working
* directory to the script's own folder (or a temp copy) rather than the repo
* root -- producing ".../scripts/studies/ag_services/scripts/logs/..." and
* r(603). Absolute paths make the file runnable from anywhere.
capture mkdir "$REPO/studies/ag_services/scripts/logs"

* Fail loudly rather than writing to a path that does not exist.
capture confirm file "$LabGitHub/nul"
if _rc {
  di as err "LabGitHub does not resolve to an existing directory: $LabGitHub"
  exit 601
}

capture log close _all
log using "$REPO/studies/ag_services/scripts/logs/harmonize.log", replace text


*GlSS7
tempfile Final
use "$DATABASE/GLSS/Datasets/GSS/GLSS7/Data/COMMUNITY/g7comSEC0",clear
gen ComName = comname
keep supid clusterno region district comname
save `Final',replace
use "$DATABASE/GLSS/Datasets/GSS/GLSS7/Data/COMMUNITY/g7comSEC5",clear 
gen ComName = comname
* --- AUDIT 1.2 -------------------------------------------------------------
* The parent merged with no keep()/assert. Every variable it contributes is
* dropped four lines later, so its only surviving effect was to add _merge==2
* rows -- SEC0 communities with no SEC5 record. Those carry no cs5q*, so every
* indicator falls to 0 and they enter the CONTROL GROUP OF ALL FOUR TREATMENTS.
merg 1:1 supid clusterno comname using `Final'
qui count if _merge == 2
di as txt "GLSS7: SEC0-only communities (phantom controls in the parent): " r(N)
drop if _merge == 2
drop _merge
ren clusterno EaId
gen Surveyx = "GLSS7"
decode cs5q9, gen(cs5q9x)
for var cs5q10a cs5q10b cs5q10c cs5q10d:decode X,gen(Xx)
for var cs5q13a cs5q13b cs5q13c:decode X,gen(Xx)
replace EaId= EaId+70000 
keep Surveyx EaId cs5q5 cs5q6 cs5q7 cs5q8 cs5q9x cs5q10*x cs5q11 cs5q12 cs5q13*x cs5q14 cs5q15
save `Final',replace

*GlSS6
use "$DATABASE/GLSS/Datasets/GSS/GLSS6/Data/COMMUNITY/sec52",clear
ren clust EaId
gen Surveyx = "GLSS6"
decode cs5q9, gen(cs5q9x)
for var cs5q10a cs5q10b cs5q10c cs5q10d:decode X,gen(Xx)
for var cs5q13a cs5q13b cs5q13c:decode X,gen(Xx)
keep Surveyx EaId cs5q5 cs5q6 cs5q7 cs5q8 cs5q9x cs5q10*x cs5q11 cs5q12 cs5q13*x cs5q14 cs5q15
append using `Final', force
save `Final',replace

*GlSS5
use "$DATABASE/GLSS/Datasets/GSS/GLSS5/Data/community/com-sec52",clear
ren clust EaId
gen Surveyx = "GLSS5"
decode cs5q9, gen(cs5q9x)
for var cs5q10a cs5q10b cs5q10c cs5q10d:decode X,gen(Xx)
for var cs5q12a cs5q12b cs5q12c:decode X,gen(Xx)
keep Surveyx EaId cs5q5 cs5q6 cs5q7 cs5q8 cs5q9x cs5q10*x cs5q11 cs5q12*x cs5q13 cs5q14
ren cs5q14 cs5q15
ren cs5q13 cs5q14
ren cs5q12* cs5q13*
ren cs5q11 cs5q12
append using `Final', force
save `Final',replace

gen extension_office = cs5q5 == 1
gen extension_distance = cs5q6
gen extension_officer  = cs5q7 == 1
gen extension_officer_visit  = cs5q8 == 1

qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var cs5q9x:replace X = `code'(X)
}

* --- AUDIT 1.4 -------------------------------------------------------------
* GLSS7 re-worded the agency options to "NGO/Non-Profit Organisation (Local)"
* while these literals were never updated; "Mofa" and "Agric Cooperative" match
* no printed option text in ANY wave. The three strings below were the only
* ones in the block with no `levelsof` diagnostic behind them.
*
* The correct literals cannot be written without seeing the stored value
* labels, so this ASSERTS rather than guesses: if a wave matches nothing, it
* stops instead of silently releasing a column of zeros.
di as txt _n "Observed cs5q9x levels, POOLED:"
levelsof cs5q9x, clean
di as txt _n "Observed cs5q9x levels, BY WAVE (label text is not constant):"
levelsof Surveyx, local(_ws)
foreach w of local _ws {
  di as txt "  `w': " _c
  levelsof cs5q9x if Surveyx == "`w'", clean
}

* CONFIRMED 2026-08-07 by running this file with AUDIT_FIXES=0: the pooled
* level set is
*   Agric Cooperative | Ministry Of Food And Agriculture | Mofa |
*   Ngo(Foreign) | Ngo(Local) | Other
* The parent matched only "Mofa", so extension_agency_mofa came back ZERO for
* every GLSS5 community -- a silent all-zero column in the live release, not a
* missing one. Both spellings are now matched. The duplicated "Ngo(Local)" in
* the parent's inlist is dropped; it was a no-op.
* Observed level sets, per wave (from AUDIT_FIXES=0 on 2026-08-07):
*   GLSS5: Ministry Of Food And Agriculture | Ngo(Foreign) | Ngo(Local) | Other
*   GLSS6: Agric Cooperative | Mofa | Ngo(Foreign) | Ngo(Local) | Other
*   GLSS7: Agric Cooperative | Mofa | Ngo(Local) | Other
* GLSS5 spells MOFA out; GLSS6/7 abbreviate. The parent matched only "Mofa",
* so extension_agency_mofa was ZERO for every GLSS5 community in the live
* release -- a wrong zero, not a missing. Both spellings are matched below.
gen extension_agency_mofa = inlist(cs5q9x,"Mofa","Ministry Of Food And Agriculture")
gen extension_agency_ngo  = inlist(cs5q9x,"Ngo(Local)","Ngo(Foreign)")
gen extension_agency_coop = cs5q9x == "Agric Cooperative"

* Any level that no indicator claims is a provider silently dropped to zero.
qui gen byte _claimed = extension_agency_mofa | extension_agency_ngo | ///
                        extension_agency_coop | inlist(cs5q9x,"Other","")
qui count if !_claimed & !missing(cs5q9x)
if r(N) > 0 {
  di as err "AUDIT 1.4: " r(N) " records carry an agency level no indicator claims:"
  levelsof cs5q9x if !_claimed & !missing(cs5q9x), clean
  exit 459
}
drop _claimed

* Wave coverage, informational. A zero here is NOT necessarily a defect: an
* option can be offered by the questionnaire and chosen by no community.
* Confirmed on 2026-08-07 --
*   GLSS5 carries NO "Agric Cooperative" level at all, so extension_agency_coop
*   is legitimately zero for that wave. The questionnaire does offer the option
*   (GLSS5 p.27, "Agricultural Cooperatives ...4"); no GLSS5 community selected
*   it. Likewise GLSS7 carries no "Ngo(Foreign)".
* The real guard is the unclaimed-level check ABOVE, which fails when a level
* exists in the data and no indicator matches it. That is the condition which
* silently produces a wrong zero; this one merely reports sparsity.
di as txt _n "Agency indicator coverage by wave (informational, zeros can be real):"
tabstat extension_agency_mofa extension_agency_ngo extension_agency_coop, ///
        by(Surveyx) stat(mean n) nototal

qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace X = `code'(X)
}

preserve
keep cs5q10ax cs5q10bx cs5q10cx cs5q10dx
gen id = _n
reshape long cs5q10, i(id) j(letter) string
levelsof cs5q10 if !missing(cs5q10)
restore

gen services_planting = 0
gen services_mechanization = 0
gen services_credit = 0
gen services_irrigation = 0
gen services_husbandry = 0
gen services_agchemicals = 0
gen services_post_harvest   = 0
gen services_employment = 0
gen services_records = 0
gen services_labour = 0

for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_planting       = 1 if inlist(X,"Planting","Use Of Seeds")
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_mechanization  = 1 if inlist(X,"Mechanization")
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_credit         = 1 if inlist(X,"Credit Facilitie","Credit Facilities")
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_irrigation     = 1 if inlist(X,"Irrigation")
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_husbandry      = 1 if inlist(X,"Animal Husbandar","Animal Husbandary")
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_agchemicals    = 1 if inlist(X,"Use Of Chemicals (Insecticide, Herbicides Etc.)","Use Of Fertilize","Use Of Fertilizer","Insecticide")
for var cs5q10ax cs5q10bx cs5q10cx cs5q10dx:replace services_post_harvest   = 1 if inlist(X,"Post Harv.Servic","Post Harvest Services","Post Harvest Services (Eg. Marketing, Storage)")

gen extension_compliance      = 1 if cs5q11 == 3
replace extension_compliance  = 2 if cs5q11 == 2
replace extension_compliance  = 3 if cs5q11 == 1

gen farm_association = cs5q12 == 1

qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var cs5q13ax cs5q13bx cs5q13cx:replace X = `code'(X)
}

preserve
keep cs5q13ax cs5q13bx cs5q13cx
gen id = _n
reshape long cs5q13, i(id) j(letter) string
levelsof cs5q13 if !missing(cs5q13)
restore


for var cs5q13ax cs5q13bx cs5q13cx:replace services_employment = 1 if inlist(X,"Prov.Of Employm","Provision Of Employment")
for var cs5q13ax cs5q13bx cs5q13cx:replace services_credit     = 1 if inlist(X,"Credit Facilitie","Provision Of Credit Facilities")
for var cs5q13ax cs5q13bx cs5q13cx:replace services_mechanization = 1 if inlist(X,"Agric Equipment","Provision Of Agric Equipment")
for var cs5q13ax cs5q13bx cs5q13cx:replace services_agchemicals = 1 if inlist(X,"Provsion Of Agric Inputs (Seeds, Fertilizer, Fishing Net Etc.)","Agric. Inputs")
for var cs5q13ax cs5q13bx cs5q13cx:replace services_post_harvest = 1 if inlist(X,"Marketing")
for var cs5q13ax cs5q13bx cs5q13cx:replace services_records    = 1 if inlist(X,"Records/Book Kee","Records/Book Keeping")
for var cs5q13ax cs5q13bx cs5q13cx:replace services_labour     = 1 if inlist(X,"Communal Labour","Provision Of Communal Labour")

gen community_cooperative = cs5q14 == 1

ren cs5q15 community_tractors

replace community_tractors = . if community_tractors >= 99
replace extension_distance = . if extension_distance >= 999

collapse (max) extension_* services_* farm_association community_*,by(Surveyx EaId)

save `Final',replace




*GlSS4
use "$DATABASE/GLSS/Datasets/GSS/GLSS4/Data/community/CS5B",clear
gen EaId = 4000+eanum
gen Surveyx = "GLSS4"
ren s* cs*
ren cs5bq* cs5q*

gen extension_office = cs5q5 == 1
gen extension_distance = cs5q6
gen extension_officer_visit  = cs5q7 == 1

gen extension_agency_mofa = cs5q8 == 1
gen extension_agency_ngo  = inlist(cs5q8,2,3) 
gen extension_agency_coop = cs5q8 == 4

gen services_planting = 0
gen services_mechanization = 0
gen services_credit = 0
gen services_irrigation = 0
gen services_husbandry = 0
gen services_agchemicals = 0
gen services_post_harvest   = 0

for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_planting       = 1 if inlist(X,1)
for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_mechanization  = 1 if inlist(X,3)
for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_credit         = 1 if inlist(X,4)
for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_irrigation     = 1 if inlist(X,5)
for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_husbandry      = 1 if inlist(X,6)
for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_agchemicals    = 1 if inlist(X,2,7)
for var cs5q9a cs5q9b cs5q9c cs5q9d:replace services_post_harvest   = 1 if inlist(X,8)

gen farm_association = cs5q10 == 1

gen services_employment = 0
gen services_records   = 0
gen services_labour   = 0

for var cs5q11a cs5q11b cs5q11c:replace services_employment = 1 if inlist(X,1)
for var cs5q11a cs5q11b cs5q11c:replace services_credit     = 1 if inlist(X,2)
for var cs5q11a cs5q11b cs5q11c:replace services_mechanization  = 1 if inlist(X,3)
for var cs5q11a cs5q11b cs5q11c:replace services_agchemicals     = 1 if inlist(X,4)
for var cs5q11a cs5q11b cs5q11c:replace services_post_harvest  = 1 if inlist(X,5)
for var cs5q11a cs5q11b cs5q11c:replace services_records    = 1 if inlist(X,6)
for var cs5q11a cs5q11b cs5q11c:replace services_labour     = 1 if inlist(X,7)

gen community_cooperative = cs5q12 == 1

ren cs5q13 community_tractors

replace community_tractors = . if community_tractors >= 99
replace extension_distance = . if extension_distance >= 999

collapse (max) extension_* services_* farm_association community_*,by(Surveyx EaId)

gen extension_officer = .
gen extension_compliance = .

append using `Final', force

gen extension = 0
replace extension = 1 if extension_officer == 1
replace extension = 2 if extension_office == 1
replace extension = 3 if extension_officer_visit == 1
replace extension = 4 if extension_officer_visit*extension_officer == 1
replace extension = 5 if extension_officer_visit*extension_office == 1
replace extension = 6 if extension_office*extension_officer*extension_officer_visit == 1

* --- AUDIT 2 ---------------------------------------------------------------
* extension code 7: office + stationed officer, but NO visit. The parent
* collapsed this state into code 2 ("Extension office access only"), which is
* factually wrong and makes the ordinal non-injective (8 states, 7 codes).
* Nothing downstream uses `extension` except `> 2`, so this is safe either way.
* MUST run BEFORE the keep below, which drops the three source flags.
replace extension = 7 if extension_office == 1 & extension_officer == 1 ///
                       & extension_officer_visit == 0

* --- AUDIT 2 (cont.) : CONSEQUENCE OF ADDING CODE 7 ------------------------
* Adding code 7 BREAKS the idiom `extension > 2`. In the parent, every code
* above 2 involved a visit, so `> 2` and "an officer visits" coincided. Code 7
* is office + stationed officer + NO VISIT, and 7 > 2, so the idiom would
* silently reclassify those communities as having active extension.
*
* Measured on 2026-08-07: 32 communities. The first version of this fix shipped
* without this guard and moved all 32 into services3, changing the treatment
* definition -- caught by compare_harmonization.do, not by inspection.
*
* The fix is to stop inferring the visit from the ordinal's numbering and key
* on the flag that means it. `_visit` is dropped before the save.
gen byte _visit = extension_officer_visit == 1

tab extension extension_compliance

* --- AUDIT 2 (cont.) -------------------------------------------------------
* The parent dropped extension_office / extension_officer /
* extension_officer_visit here, after using them to build `extension`. That
* makes the control group unverifiable: `ag_services == 0` includes communities
* with an office and/or a stationed officer (see AUDIT 1.1) and no released
* variable lets a reader see which. They are retained under the fixes.
keep Surveyx EaId extension extension_office extension_officer ///
     extension_officer_visit extension_agency_* services_* farm_association ///
     community_cooperative community_tractors extension_distance ///
     extension_compliance _visit

gen     ag_services = 0
replace ag_services = 1 if farm_association == 1 
replace ag_services = 2 if community_cooperative == 1
replace ag_services = 3 if _visit == 1
replace ag_services = 4 if community_cooperative == 1 & farm_association == 1
replace ag_services = 5 if _visit == 1 & community_cooperative == 1
replace ag_services = 6 if _visit == 1 & farm_association == 1
replace ag_services = 7 if _visit == 1 & farm_association == 1 & community_cooperative == 1

tab ag_services Surveyx  


gen services0 = ag_services > 0
gen services1 = farm_association == 1
gen services2 = community_cooperative == 1
gen services3 = _visit == 1

replace services1 = . if services1 == 0 & services0 == 1
replace services2 = . if services2 == 0 & services0 == 1
replace services3 = . if services3 == 0 & services0 == 1

* --- AUDIT 1.1 : SENSITIVITY ARM -------------------------------------------
* The main-specification control group above is `ag_services == 0`. Because
* ag_services counts extension ONLY via `extension > 2` (a visit), that group
* still contains communities with an extension OFFICE and/or a STATIONED
* OFFICER. Those farms are partially treated, so every technology gap ratio is
* biased toward zero by an amount proportional to how many of them there are.
*
* The main specification is retained unchanged. These _strict variants use a
* comparison group with no extension presence of ANY kind, and the paper
* reports both. See the audit plan section 1.1.
qui count if ag_services == 0 & inlist(extension,1,2,7)
di as txt "Partially-treated communities inside the 'no services' control: " r(N)
tab extension Surveyx if ag_services == 0

gen byte _clean = (extension == 0 & farm_association == 0 & community_cooperative == 0)

gen services0_strict = services0
gen services1_strict = services1
gen services2_strict = services2
gen services3_strict = services3
foreach k in 0 1 2 3 {
  replace services`k'_strict = . if services`k'_strict == 0 & !_clean
  label variable services`k'_strict "Sensitivity: control restricted to communities with no service presence"
}
drop _clean

sum services*

for var * : label variable X ""

* AUDIT 2: `for var * : label variable X ""` above wipes every label, and the
* parent's re-label block omitted the four headline treatments.
label variable services0 "Any agricultural service source in community"
label variable services1 "Agricultural/fishing association service source"
label variable services2 "Agricultural cooperative service source"
label variable services3 "Active extension service source (officer visits)"
label variable farm_association "Community has an agricultural or fishing association"

label variable community_cooperative "Community cooperative"
label variable extension "Community agricultural extension"
label variable ag_services "Community agricultural services"

*label variable extension_office              "Community has an agricultural extension office"
label variable extension_distance            "Distance from community to nearest agricultural extension office"
label variable extension_office        "Community has an agricultural extension office"
label variable extension_officer       "Agricultural extension officer stationed in the community"
label variable extension_officer_visit "Extension officer or agent visits farmers in the community"
*label variable extension_officer             "Community has access to an agricultural extension officer"
*label variable extension_officer_visit       "Extension officer visited the community"

label variable extension_agency_mofa         "Community extension service provider is MOFA"
label variable extension_agency_ngo          "Community extension service provider is NGO"
label variable extension_agency_coop         "Community extension service provider is agricultural cooperative"

label variable services_planting         "Extension services in community include planting or seed-use advice"
label variable services_mechanization    "Extension or association in community provides mechanization support"
* AUDIT 1.5: credit, mechanization, agchemicals and post_harvest are written
* from BOTH cs5q10* (extension roster) and cs5q13* (association roster), so
* they mean "extension OR association provides". The labels now say so.
label variable services_credit           "Extension or association in community provides credit support"
label variable services_irrigation       "Extension services in community include irrigation advice"
label variable services_husbandry        "Extension services in community include animal husbandry advice"
label variable services_agchemicals      "Extension or association in community provides fertilizer, insecticide, or other agrochemical support"
label variable services_post_harvest     "Extension or association in community provides post-harvest or marketing/storage support"

label variable extension_compliance          "Community compliance with extension advice"

* --- AUDIT 1.5 -------------------------------------------------------------
* These three come from cs5q13* -- the ASSOCIATION services roster (gated by
* cs5q12 -> farm_association). The cooperative is a SEPARATE question
* (cs5q14 -> community_cooperative) with no service roster at all, and is a
* separate treatment (services2). The parent's labels named the wrong one of
* two variables sitting side by side in the same file.
label variable services_employment      "Farmer association in community provides employment opportunities"
label variable services_records         "Farmer association in community provides record/book-keeping support"
label variable services_labour          "Farmer association in community provides communal labour"

*label variable cooperative_participation     "Community members participate in cooperative activities"
label variable community_tractors            "Number of tractors in the community"


label define extension_compliance_lbl ///
    0 "None" ///
    1 "Did not comply" ///
    2 "Partially complied" ///
    3 "Fully complied"
	
label values extension_compliance extension_compliance_lbl

label define extension_lbl ///
    0 "No agricultural extension" ///
    1 "Extension officer access only" ///
	2 "Extension office access only" ///
    3 "Extension officer visited community only" ///
	4 "Extension visit plus officer access" ///
    5 "Extension visit plus office access" ///
    6 "Extension office, officer access, and officer visit"

label values extension extension_lbl

/*
label define cooperative_lbl ///
    0 "No cooperative" ///
	1 "Community has cooperative only" ///
    2 "Cooperative participation only" ///
    3 "Community has cooperative and participation"

label values cooperative cooperative_lbl

label define ag_services_lbl ///
    0 "None" ///
	1 "Cooperative" ///
	2 "Extension" ///
    3 "Both"

label values ag_services ag_services_lbl

tab extension ag_services

tab cooperative ag_services
*/
* --- AUDIT 1.3 -------------------------------------------------------------
* GLSS5 has NO compliance question -- it first appears as GLSS6 Q11 / GLSS7 Q11
* (verified against the questionnaires). The parent's unrestricted replace
* assigned a substantive 0 = "None" to GLSS5, so the 0 category spanned all
* waves while categories 1-3 were GLSS6/7 only. It also overwrote OBSERVED
* GLSS6/7 responses wherever extension <= 2.
* Two defects in the parent's single line, both fixed here:
*   (a) WAVE. GLSS4 and GLSS5 never asked the question, so a substantive
*       0 = "None" there is fabricated. Restricted to GLSS6/GLSS7.
*   (b) OVERWRITE. The parent replaced unconditionally, destroying OBSERVED
*       responses from GLSS6/7 communities that have an office and/or a
*       stationed officer but no visit (extension <= 2). The run of
*       2026-08-07 shows 27 such records with a real 1/2/3 answer. Restricted
*       to filling MISSING values only.
* NB cs5q11 itself is gone by this point -- it is dropped by the keep above,
* after being used to build extension_compliance before the collapse. The wave
* restriction is what carries the fix; missing() carries the non-overwrite.
replace extension_compliance = 0 if _visit == 0 ///
      & inlist(Surveyx,"GLSS6","GLSS7") & missing(extension_compliance)

tab extension_compliance Surveyx



*==============================================================================
* RELEASE CONTRACT  (AUDIT 2: the parent block had none; the land_tenure block
* immediately above it does. A silent drop is the failure this prevents.)
*==============================================================================
foreach v in Surveyx EaId extension ag_services services0 services1 services2 ///
             services3 farm_association community_cooperative {
  capture confirm variable `v'
  if _rc {
    di as err "harmonized_ag_services_data: expected variable `v' is missing."
    exit 111
  }
}

foreach v in services0_strict services1_strict services2_strict services3_strict ///
             extension_office extension_officer extension_officer_visit {
  capture confirm variable `v'
  if _rc {
    di as err "harmonized_ag_services_data: sensitivity variable `v' is missing."
    exit 111
  }
}

capture drop _visit

di as txt _n "Release summary:"
tab Surveyx
tabstat services0 services1 services2 services3, by(Surveyx) stat(mean n) nototal

*------------------------------------------------------------------------------
* Writes the live release DIRECTLY. Safe because the release contract above
* runs BEFORE the save: a structurally broken build exits 111 and never reaches
* this line. harmonized_ag_services_data_PRE_AUDIT.dta is never overwritten.
*------------------------------------------------------------------------------
compress
saveold "$LabGitHub\harmonized_ag_services_data", replace version(12)
di as res _n "Wrote $LabGitHub\harmonized_ag_services_data.dta"
log close
