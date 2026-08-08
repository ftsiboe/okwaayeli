*==============================================================================
* 11_ag_services.do
*
* Builds the 'Harmonized Ag services data' release from the GLSS4-GLSS7
* community questionnaires (section CS5 / CS5B).
*
* WRITES: $LabGitHub\harmonized_ag_services_data.dta
*
* Also called directly by studies/ag_services/scripts/001_DATA_ag_services_study.R
* on machines that have Stata. 001 reads this script's log and treats any line
* matching ^r(NNN); as a failure, because Stata batch mode exits 0 even on
* error. That is why the log below is opened unconditionally and given a NAME:
* a named log coexists with the unnamed log 00_run_all.do holds open, so the
* script behaves identically standalone and under the master.
*
* The four questionnaires do not ask these questions the same way. Where a
* harmonization decision was not obvious from the variable names, the reasoning
* is in the comment above it. Those comments are load-bearing -- several encode
* a wave-specific quirk that is invisible in the data.
*
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

capture log close ag_services
log using "$GLSS/logs/11_ag_services.log", replace text name(ag_services)

*GlSS7
tempfile Final
use "$DATABASE/GLSS/Datasets/GSS/GLSS7/Data/COMMUNITY/g7comSEC0",clear
gen ComName = comname
keep supid clusterno region district comname
save `Final',replace
use "$DATABASE/GLSS/Datasets/GSS/GLSS7/Data/COMMUNITY/g7comSEC5",clear 
gen ComName = comname

* SEC0 lists every community; SEC5 is the agriculture section. A community in
* SEC0 with no SEC5 record carries no cs5q* at all, so every indicator built
* below would fall to 0 and it would enter the control group of all four
* treatments as a fully-observed "no services" community. It is not: it is
* unobserved. Dropped rather than merged in silently.
merg 1:1 supid clusterno comname using `Final'
qui count if _merge == 2
di as txt "GLSS7: SEC0-only communities dropped (unobserved, not untreated): " r(N)
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
* GLSS5 numbers the tail of the section one lower than GLSS6/GLSS7: its q12 is
* their q13, its q13 their q14, its q14 their q15, and its q11 their q12. The
* renames below shift GLSS5 onto the GLSS6/7 numbering so the three waves can
* be appended. Order matters -- q14 is renamed before q13 so nothing collides.
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

* --- extension provider ------------------------------------------------------
* The provider is stored as LABEL TEXT, and the label text is not constant
* across waves. GLSS5 spells the ministry out; GLSS6 and GLSS7 abbreviate it.
* Matching one spelling produces an all-zero column for the other wave -- a
* wrong zero, which reads as "no MOFA extension anywhere in GLSS5" rather than
* as missing data. Both spellings are matched.
*
* Observed level sets, by wave:
*   GLSS5: Ministry Of Food And Agriculture | Ngo(Foreign) | Ngo(Local) | Other
*   GLSS6: Agric Cooperative | Mofa | Ngo(Foreign) | Ngo(Local) | Other
*   GLSS7: Agric Cooperative | Mofa | Ngo(Local) | Other
*
* These are printed on every run, because a future wave can add a level and
* nothing else would notice.
di as txt _n "Observed cs5q9x levels, POOLED:"
levelsof cs5q9x, clean
di as txt _n "Observed cs5q9x levels, BY WAVE (label text is not constant):"
levelsof Surveyx, local(_ws)
foreach w of local _ws {
  di as txt "  `w': " _c
  levelsof cs5q9x if Surveyx == "`w'", clean
}

gen extension_agency_mofa = inlist(cs5q9x,"Mofa","Ministry Of Food And Agriculture")
gen extension_agency_ngo  = inlist(cs5q9x,"Ngo(Local)","Ngo(Foreign)")
gen extension_agency_coop = cs5q9x == "Agric Cooperative"

* A level that no indicator claims would be silently dropped to zero. This is
* the guard that fails when a spelling changes; it stops the run rather than
* releasing the column.
qui gen byte _claimed = extension_agency_mofa | extension_agency_ngo | ///
                        extension_agency_coop | inlist(cs5q9x,"Other","")
qui count if !_claimed & !missing(cs5q9x)
if r(N) > 0 {
  di as err "extension agency: " r(N) " records carry a level no indicator claims:"
  levelsof cs5q9x if !_claimed & !missing(cs5q9x), clean
  di as err "Add the level to the inlist() above, or to the 'Other' exemption."
  exit 459
}
drop _claimed

* Coverage by wave, informational. A zero here is NOT necessarily a defect: an
* option can be offered by the questionnaire and chosen by no community. GLSS5
* carries no "Agric Cooperative" level and GLSS7 no "Ngo(Foreign)", both
* legitimately. The unclaimed-level check above is the one that discriminates.
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

* --- extension: the eighth state ---------------------------------------------
* The ladder above assigns seven codes, but the questionnaire admits eight
* states: office and stationed officer with NO visit has no code and would fall
* through to 2 ("office access only"), which is factually wrong. Code 7 names it.
*
* MUST run BEFORE the keep below, which drops the three source flags.
replace extension = 7 if extension_office == 1 & extension_officer == 1 ///
                       & extension_officer_visit == 0

* Because code 7 exists, `extension > 2` no longer means "an officer visits" --
* 7 is office plus stationed officer and NO visit. Anything that needs the visit
* must key on the flag that means it, not on the ordinal's numbering. That is
* what _visit is for; it is dropped before the save.
gen byte _visit = extension_officer_visit == 1

tab extension extension_compliance

* extension_office / extension_officer / extension_officer_visit are RETAINED.
* They are the only way a reader can see which communities inside
* `ag_services == 0` have an extension presence without a visit -- the
* partially-treated controls the _strict variants below exclude. Dropping them
* here would make that group unverifiable from the release alone.
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

* --- sensitivity arm ---------------------------------------------------------
* The main-specification control group is `ag_services == 0`. It counts
* extension only via a visit, so it still contains communities with an
* extension OFFICE and/or a STATIONED OFFICER. Those farms are partially
* treated, which biases every technology gap ratio toward zero in proportion to
* how many of them there are.
*
* The main specification is unchanged. The _strict variants below use a
* comparison group with no extension presence of any kind, and the paper
* reports both.
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

* Wipes every label, then re-applies them. Everything the release ships with a
* label must be re-declared below this line, including the four treatments.
for var * : label variable X ""

label variable services0 "Any agricultural service source in community"
label variable services1 "Agricultural/fishing association service source"
label variable services2 "Agricultural cooperative service source"
label variable services3 "Active extension service source (officer visits)"
label variable farm_association "Community has an agricultural or fishing association"

label variable community_cooperative "Community cooperative"
label variable extension "Community agricultural extension"
label variable ag_services "Community agricultural services"

label variable extension_distance       "Distance from community to nearest agricultural extension office"
label variable extension_office         "Community has an agricultural extension office"
label variable extension_officer        "Agricultural extension officer stationed in the community"
label variable extension_officer_visit  "Extension officer or agent visits farmers in the community"

label variable extension_agency_mofa         "Community extension service provider is MOFA"
label variable extension_agency_ngo          "Community extension service provider is NGO"
label variable extension_agency_coop         "Community extension service provider is agricultural cooperative"

* credit, mechanization, agchemicals and post_harvest are written from BOTH
* cs5q10* (the extension roster) and cs5q13* (the association roster), so they
* mean "extension OR association provides". The labels say so; the other four
* come from the extension roster alone.
label variable services_planting         "Extension services in community include planting or seed-use advice"
label variable services_mechanization    "Extension or association in community provides mechanization support"
label variable services_credit           "Extension or association in community provides credit support"
label variable services_irrigation       "Extension services in community include irrigation advice"
label variable services_husbandry        "Extension services in community include animal husbandry advice"
label variable services_agchemicals      "Extension or association in community provides fertilizer, insecticide, or other agrochemical support"
label variable services_post_harvest     "Extension or association in community provides post-harvest or marketing/storage support"

label variable extension_compliance          "Community compliance with extension advice"

* These three come from cs5q13* -- the ASSOCIATION services roster, gated by
* cs5q12 -> farm_association. The COOPERATIVE is a separate question
* (cs5q14 -> community_cooperative) with no service roster at all, and is a
* separate treatment (services2). The two sit side by side; the labels name the
* association, which is what built them.
label variable services_employment      "Farmer association in community provides employment opportunities"
label variable services_records         "Farmer association in community provides record/book-keeping support"
label variable services_labour          "Farmer association in community provides communal labour"

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

* --- compliance: wave scope and non-overwrite --------------------------------
* GLSS4 and GLSS5 never asked this question -- it first appears as GLSS6 Q11 /
* GLSS7 Q11 (verified against the questionnaires). So a substantive
* 0 = "None" outside GLSS6/7 would be fabricated, and the 0 category would span
* all waves while categories 1-3 were GLSS6/7 only.
*
* The fill is also restricted to MISSING values. GLSS6/7 communities with an
* office and/or a stationed officer but no visit gave real 1/2/3 answers; an
* unconditional replace would destroy them.
*
* NB cs5q11 itself is gone by this point -- dropped by the keep above, after
* building extension_compliance before the collapse. The wave restriction
* carries the scope; missing() carries the non-overwrite.
replace extension_compliance = 0 if _visit == 0 ///
      & inlist(Surveyx,"GLSS6","GLSS7") & missing(extension_compliance)

tab extension_compliance Surveyx


*==============================================================================
* RELEASE CONTRACT
*
* Runs BEFORE the save, so a structurally broken build exits 111 and never
* reaches saveold. A silent drop is the failure this prevents: the study
* scripts would read the release, find the column gone, and fail somewhere
* far away from the cause.
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

compress
saveold "$LabGitHub\harmonized_ag_services_data", replace version(12)
di as res _n "Wrote $LabGitHub\harmonized_ag_services_data.dta"
capture log close ag_services
