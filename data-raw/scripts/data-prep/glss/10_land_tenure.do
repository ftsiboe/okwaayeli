*==============================================================================
* 10_land_tenure.do
*
* Builds the 'Harmonized Land Tenure' release.
*
* WRITES: $LabGitHub\harmonized_land_tenure_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 2082-2469.
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

tempfile Land temp GSPS GlSS
qui{ //GlSS3
use "$DATABASE\GLSS\Datasets\GSS\GLSS3\Data\partb\S8B",clear
ren clust EaId
ren nh HhId
ren s8bq5 Irrigated
ren s8bq1 Mid
gen Survey = "GLSS3"
ren farmcd PlotID
lab define s8bq4c 1 Acres 2 Poles 3 Ropes 4 Other,replace
lab define s8bq6 1 "Yes with deed" 2 "Yes without deed" 3 No,replace
lab define s8bq7 1 Sell 2 Security 3 Both 4 "No right",replace
lab define s8bq9 1 "Rented for cash or kind" 2 "Sharecropped by HH" 3 "Use free of charge" 4 "Distributed by village/family",replace
for var s8bq4c s8bq6 s8bq7 s8bq9:lab val X X
decode s8bq4c,gen(Unit)
decode s8bq6,gen(OwnShp)
decode s8bq7,gen(Right)
decode s8bq9,gen(Obtained)
ren s8bq4 Size
ren s8bq8 Value 
ren s8bq10 Rent
gen ShrCrp = s8bq11/100
drop s8*
replace Size = . if Size >300
save `Land', replace
}
qui{ //GlSS4
use "$DATABASE\GLSS\Datasets\GSS\GLSS4\Data\partb\SEC8B",clear
ren clust EaId
ren nh HhId
ren s8bq1 Mid
ren farmcd PlotID
gen Survey = "GLSS4"
lab define s8bq5 1 "Yes with deed" 2 "Yes without deed" 3 No,replace
lab define s8bq6 1 Sell 2 Security 3 Both 4 "No right",replace
lab define s8bq8 1 "Rented for cash or kind" 2 "Sharecropped by HH" 3 "Use free of charge" 4 "Distributed by village/family",replace
lab define s8bq4b 1 Acres 2 Poles 3 Ropes 4 Other,replace
for var s8bq5 s8bq6 s8bq8 s8bq4b:lab val X X
decode s8bq5,gen(OwnShp)
decode s8bq6,gen(Right)
decode s8bq8,gen(Obtained)
decode s8bq4b,gen(Unit)
ren s8bq7 Value 
ren s8bq9 Rent
ren s8bq4a Size
gen ShrCrp = s8bq10 /100
drop s8*
replace Size = . if Size >300
append using `Land', force
save `Land', replace
}
qui{ //GlSS5
use "$DATABASE\GLSS\Datasets\GSS\GLSS5\Data\partb\sec8b",clear
ren clust EaId 
ren nh HhId
ren s8bq1 Mid
ren s8bq3 PlotID
gen Survey = "GLSS5"
for var s8bq10:decode X,gen(X_x)
decode s8bq5,gen(OwnShp)
decode s8bq4b,gen(Unit)
decode s8bq6,gen(Right)
decode s8bq8,gen(Obtained)
ren s8bq4a Size
ren s8bq7 Value 
ren s8bq9 Rent
*------------------------------------------------------------------------------
* SHARECROP SHARE - fragile parser, and not wave-comparable. (Same loop is
* repeated for GLSS6 and GLSS7 below; this note covers all three.)
*
* FRAGILITY: this matches a SUBSTRING of the decoded value label. "1/2" is a
* substring of "1/20", so the label "5...........1/20" is first hit by "1/2"
* (-> 0.5) and only corrected because "1/20" appears LATER in the list and
* overwrites (-> 0.05). The result is right by ordering, not by construction,
* and "1/10" is listed twice. Any reordering of this loop silently changes the
* data. Safer: match on the numeric code of s8bq10 rather than label text.
*
* NOT WAVE-COMPARABLE: the underlying supports differ.
*   GLSS3 (s8bq11) and GLSS4 (s8bq10): continuous PERCENTAGE (e.g. 33, 50, 67).
*   GLSS5 (s8bq10): coded fractions, options 2/3 1/2 1/3 1/4 1/5 ONLY.
*   GLSS6/GLSS7 (s8bq10): coded fractions, ALSO 3/4, 1/10, 1/20 and 0.
* So ShrCrpCat's 0 / 1-49 / 50-100 bins rest on different option sets before
* and after 2005/06. Cross-wave sharecropping-intensity trends are not
* identified from a constant instrument.
*------------------------------------------------------------------------------
gen ShrCrp = .
foreach shr in "1/2" "1/3" "1/4" "1/5" "1/10" "2/3" "3/4" "1/20" "1/10"{
replace ShrCrp = `shr' if strpos(s8bq10_x , "`shr'")!=0
}
drop s8* hhid region ez weight
replace Size = . if Size >300
append using `Land', force
save `Land', replace
}
qui{ //GlSS6
use "$DATABASE\GLSS\Datasets\GSS\GLSS6\Data\PARTB\sec8b",clear
ren clust EaId
ren nh HhId
ren pid Mid
ren s8bq3 PlotID
gen Survey = "GLSS6"
for var s8bq10:decode X,gen(X_x)
decode s8bq5,gen(OwnShp)
decode s8bq4b,gen(Unit)
decode s8bq6,gen(Right)
decode s8bq8,gen(Obtained)
ren s8bq4a Size
ren s8bq7 Value 
ren s8bq9 Rent
gen ShrCrp = .
foreach shr in "1/2" "1/3" "1/4" "1/5" "1/10" "2/3" "3/4" "1/20" "1/10"{
replace ShrCrp = `shr' if strpos(s8bq10_x , "`shr'")!=0
}
drop s8* region hid
replace Size = . if Size >300
append using `Land', force
save `Land', replace
}
qui{ //GlSS7
use "$DATABASE\GLSS\Datasets\GSS\GLSS7\Data\PARTB\g7sec8b",clear
ren clust EaId
ren nh HhId
ren s8bq1 Mid
ren s8bq3 PlotID
gen Survey = "GLSS7"
for var s8bq10:decode X,gen(X_x)
decode s8bq5,gen(OwnShp)
decode s8bq4b,gen(Unit)
decode s8bq6,gen(Right)
decode s8bq8,gen(Obtained)
ren s8bq4a Size
ren s8bq7 Value 
ren s8bq9 Rent
gen ShrCrp = .
foreach shr in "1/2" "1/3" "1/4" "1/5" "1/10" "2/3" "3/4" "1/20" "1/10"{
replace ShrCrp = `shr' if strpos(s8bq10_x , "`shr'")!=0
}
drop s8* region loc2 WTA_S hid
replace Size = . if Size >300
append using `Land', force
save `Land', replace
}

*------------------------------------------------------------------------------
* UNIT CONVERSION - Poles and Ropes are ANALYST ASSUMPTIONS, not lookups.
*------------------------------------------------------------------------------
* GSS never standardised these units. The GLSS5 interviewer manual
* (GLSS5/Manuals/MANUAL.pdf, Section 8, Questions 3 & 4) instructs:
*   "You should record the local units given by respondents. For example local
*    farmers may use Poles and Ropes."
* That is the ONLY mention of either unit in the entire GLSS3-7 documentation
* set (manuals, codebooks, reports all searched). No official conversion exists.
*
* The surveyor's pole/rod (1/160 acre) is NOT the relevant referent - these are
* local, respondent-defined units. Treating 1 pole ~ 1 acre is defensible and
* empirically supported: under it, pole-reported and acre-reported plots come
* out at near-identical mean size in every wave (e.g. GLSS3 1.64 vs 1.80 ha).
* Under a rod-scaled reading a median report of "2 poles" would be 0.005 ha,
* which is not a farm.
*
* This assumption is NOT innocuous - it governs 11-36% of plots by wave
* (GLSS3 36.4%, GLSS4 26.8%, GLSS5 20.1%, GLSS6 10.7%, GLSS7 18.1%) and feeds
* the land elasticity, the largest in the model. Dropping locally-reported
* plots moves mean farm size by 6-21%. Robustness arm: LocalUnit below.
*------------------------------------------------------------------------------
replace Size = .                 if Size >90
gen byte LocalUnit = inlist(Unit,"Poles","Ropes")   // respondent-defined units
gen byte UnitOther = (Unit == "Other")              // dropped below - counted, not silent
lab var LocalUnit "Plot area reported in a local unit (Poles/Ropes) - conversion assumed"
lab var UnitOther "Plot area unit 'Other' - plot dropped from the sample"
qui count if UnitOther
di as txt "Plots dropped for unit=='Other': " as res r(N)
qui count if LocalUnit
di as txt "Plots on assumed local-unit conversion (Poles/Ropes): " as res r(N)

replace Size = Size*0.404686     if Unit == "Acres"
replace Size = Size*0.404686     if Unit == "Poles"   // assumed acre-equivalent; see above
replace Size = Size*(0.404686/9) if Unit == "Ropes"   // assumed 1/9 acre; see above
replace Size = Size*0.01         if Unit == "Plot"
* NB: Unit=="Hectare" (GLSS6/GLSS7 only) is intentionally left unconverted.
replace Size = .                 if Unit == "Other" //check
drop if Size == .
qui foreach code in strtrim stritrim strltrim strrtrim strproper{
for var OwnShp Right Obtained : replace X = `code'(X)
}

replace Obtained = "Rented" if inlist(Obtained,"Rented For Cash Or In Kind","Rented For Cash Or Kind")
replace Obtained = "Sharecropped" if inlist(Obtained,"Sharecropped By Hh","Sharecropped By Household")
replace Obtained = "Free" if inlist(Obtained,"Use Free Of Charge")
* GLSS7 only: "Inherited" (option 6) is folded into Kinship. In GLSS7 Inherited
* (4,699 plots) is LARGER than Distributed (2,037), so Kinship is not
* wave-comparable. GLSS3-6 Kinship = distributed by village/family only.
replace Obtained = "Kinship" if inlist(Obtained,"Distributed By Village/Family","Inherited")

* Missing ownership -> "No". Harmless in practice: these are blank roster slots
* (GLSS6 7,411 rows; GLSS7 6,534) that carry no Size and are dropped by
* `drop if Size == .` above. Verified: own-missing rows have s8bq4a missing 1:1.
replace OwnShp = "No" if OwnShp == ""

* Rights are STRUCTURALLY ABSENT for non-owners, not measured. Every wave's
* questionnaire skips non-owners past the rights question (GLSS3 Q6 "No...3
* (>>9)"; GLSS4 Q5 "(>>9)"; GLSS5-7 "(>>8)"). This line fills that skip by
* construction. Verified: rights-missing counts match non-owner counts almost
* exactly per wave (GLSS4: 7,160 missing vs 7,161 non-owners).
* CONSEQUENCE: any owner-vs-non-owner contrast ON RIGHTS compares a measured
* category against a definitional one. It is the only design the skip permits,
* but it should not be read as evidence about rights per se.
replace Right = "No Right" if OwnShp == "No"
replace Right = "Other" if Right == ""
replace Obtained = "Other" if Obtained == ""
	   
* LocalUnit carried through: without it in the stat list, collapse would silently
* drop the flag here and the local-unit robustness arm could not be built.
collapse (mean) Size Value Rent (max) ShrCrp LocalUnit,by (Survey EaId HhId Mid PlotID OwnShp Right Obtained)

//Transformed Herfindahl (Simpson) Index (THI)
*Crop diversifiction by crop cat
egen SizeMid   = sum(Size),by(Survey EaId HhId Mid)               //Estimated planted land by Farmer
egen LndFrgMid = sum((Size/SizeMid)^2), by(Survey EaId HhId Mid)  //Estimated land con

gen rate = Value/Size
gen rent = Rent/Size
gen LndAqKin = Size if inlist(Obtained,"Kinship")
gen LndAqBuy = Size if inlist(Obtained,"Bought")
gen LndAqRnt = Size if inlist(Obtained,"Rented")
gen LndAqFre = Size if inlist(Obtained,"Free")
gen LndAqShr = Size if inlist(Obtained,"Sharecropped")
gen LndAqOth = Size if inlist(Obtained,"Other")

gen LndRgtSll = Size if inlist(Right,"Sell")
gen LndRgtSec = Size if inlist(Right,"Security")
gen LndRgtBth = Size if inlist(Right,"Both")
gen LndRgtNon = Size if inlist(Right,"No Right")
gen LndRgtOth = Size if inlist(Right,"Other")

gen LndOwnDed = Size if inlist(OwnShp,"Yes With Deed")
gen LndOwnYes = Size if inlist(OwnShp,"Yes Without Deed")
gen LndOwnNon = Size if inlist(OwnShp,"No")
gen plotn = 1
* (mean) LocalUnit at farmer level = SHARE of the farmer's plots whose area was
* reported in an assumed local unit (Poles/Ropes). Robustness arm: re-estimate
* excluding farmers with LocalUnit > 0, or weight by (1 - LocalUnit).
collapse (sum) LndAq* LndRgt* LndOwn* plotn (mean) LndFrgMid rate rent ShrCrp LocalUnit ,by (Survey EaId HhId Mid)
lab var LocalUnit "Share of farmer's plots with area reported in an assumed local unit"

foreach lnd in LndAq  LndRgt LndOwn{
 egen Size = rowtotal(`lnd'*)
for var `lnd'* :replace X=X/Size
drop Size   
}

gen     CPI = 2.551/305.788   if Survey == "GLSS3"  //1990/91
replace CPI = 15.007/305.788  if Survey == "GLSS4"  //1997/98
replace CPI = 58.705/305.788  if Survey == "GLSS5"  //2005/06
replace CPI = 118.687/305.788 if Survey == "GLSS6"  //2012/13
replace CPI = 305.788/305.788 if Survey == "GLSS7"  //2016/17

qui for var rate rent :replace X=X/10000 if inlist(Survey,"GLSS5","GLSS4","GLSS3") //To New Ghana Cedi
qui for var rate rent :replace X=X/CPI

tabstat LndAq* LndRgt* LndOwn* plotn LndFrgMid rate rent ShrCrp,by(Survey)

*------------------------------------------------------------------------------
* FARMER-LEVEL TENURE: collapse rule
*------------------------------------------------------------------------------
* At this point LndOwn*/LndRgt*/LndAq* are SHARES OF CULTIVATED AREA (normalised
* in the foreach loop above), not areas and not counts.
*
* The farmer is assigned to the category holding the LARGEST AREA SHARE
* ("plurality of area"), NOT to "owns any land". A farmer with 60% of area not
* owned and 40% owned with deed is classified NOT-OWNED. This is deliberate: the
* MSF framework needs one technology group per farmer, the farm is the production
* unit, and the dominant tenure regime is the right object for that. It is also
* immaterial in practice - at most 2.2% of farmers are classified differently
* under "any ownership" (tenure is near-homogeneous within a farm).
*
* Ties resolve toward the MORE SECURE category, because the >= replacements
* cascade Non -> Yes -> Ded. Affects <0.5% of farmers in every wave.
*
* See studies/land_tenure/narrative/diagnostics/tenure_variable_documentation.md
* for the verification against the GLSS3-7 questionnaires and the prevalence table.
*------------------------------------------------------------------------------
egen LndOwnmax = rowmax(LndOwn*)
gen LndOwn     = 1 if round(LndOwnNon*10000) >= round(LndOwnmax*10000)
replace LndOwn = 2 if round(LndOwnYes*10000) >= round(LndOwnmax*10000)
replace LndOwn = 3 if round(LndOwnDed*10000) >= round(LndOwnmax*10000)

egen LndRgtmax = rowmax(LndRgt*)
gen LndRgt     = 1 if round(LndRgtNon*10000) >= round(LndRgtmax*10000)
*replace LndRgt = 5 if round(LndRgtOth*10000) >= round(LndRgtmax*10000)
replace LndRgt = 2 if round(LndRgtSec*10000) >= round(LndRgtmax*10000)
replace LndRgt = 3 if round(LndRgtSll*10000) >= round(LndRgtmax*10000)
replace LndRgt = 4 if round(LndRgtBth*10000) >= round(LndRgtmax*10000)

egen LndAqmax = rowmax(LndAq*)
gen LndAq     = 1 if round(LndAqFre*10000) >= round(LndAqmax*10000)
replace LndAq = 6 if round(LndAqOth*10000) >= round(LndAqmax*10000) & inlist(Survey,"GLSS3","GLSS4")
replace LndAq = 2 if round(LndAqShr*10000) >= round(LndAqmax*10000)
replace LndAq = 3 if round(LndAqRnt*10000) >= round(LndAqmax*10000)
replace LndAq = 4 if round(LndAqBuy*10000) >= round(LndAqmax*10000)
replace LndAq = 5 if round(LndAqKin*10000) >= round(LndAqmax*10000)

replace ShrCrp = 0 if ShrCrp == .
gen     ShrCrpCat = 1 if ShrCrp == 0
replace ShrCrpCat = 2 if inrange(ShrCrp,0.001,0.499)
replace ShrCrpCat = 3 if ShrCrp > 0.499

gen OwnLnd = inlist(LndOwn,2,3)

*------------------------------------------------------------------------------
* Alternative collapse rules - sensitivity arms for the treatment definition.
* OwnLnd (above) is the plurality rule and remains the main specification.
* These are additive: nothing downstream changes unless a script asks for them.
*   OwnLndAny  - "owns any land at all"   (the rule the manuscript used to describe)
*   OwnLndMaj  - "owns more than half the cultivated area"
*   OwnLndFull - "owns all cultivated area"
* Prevalence by wave (verified in Python against the raw Section 8b rosters):
*   GLSS3  plurality 45.2 | any 45.8 | maj 45.1 | full 44.3
*   GLSS4  plurality 31.7 | any 33.8 | maj 31.1 | full 28.0
*   GLSS5  plurality 79.1 | any 80.2 | maj 78.7 | full 77.2
*   GLSS6  plurality 67.2 | any 69.4 | maj 66.9 | full 64.7
*   GLSS7  plurality 62.9 | any 64.8 | maj 62.7 | full 60.2
*------------------------------------------------------------------------------
gen OwnLndAny  = (LndOwnDed + LndOwnYes) > 0        & !missing(LndOwnDed,LndOwnYes)
gen OwnLndMaj  = (LndOwnDed + LndOwnYes) > 0.5      & !missing(LndOwnDed,LndOwnYes)
gen OwnLndFull = (LndOwnDed + LndOwnYes) >= 0.999   & !missing(LndOwnDed,LndOwnYes)
lab var OwnLnd     "Land owner (plurality of cultivated area) - MAIN"
lab var OwnLndAny  "Land owner (any owned area) - sensitivity"
lab var OwnLndMaj  "Land owner (>50% of area owned) - sensitivity"
lab var OwnLndFull "Land owner (100% of area owned) - sensitivity"

*------------------------------------------------------------------------------
* Wave-limited categories - the option sets are NOT constant across GLSS3-7.
* Verified against G3QPartB.pdf / G4QPartB.pdf / G5QPartB.pdf and the GLSS6/7
* value labels. Flags only; no values are altered, because in Stata a missing
* value compares as LARGER than any number, so blanking LndAqBuy would make
* `round(LndAqBuy*10000) >= round(LndAqmax*10000)` fire for every GLSS3/GLSS4
* farmer and misclassify them all as Purchased.
*
*  1. Purchase ("Bought") is not a response option before GLSS5. GLSS3 Q9 and
*     GLSS4 Q8 offer only Rented / Sharecropped / Free / Distributed. A 0%
*     purchased share in GLSS3/GLSS4 therefore means NOT ASKED, not zero.
*  2. GLSS7 alone adds "Inherited" (option 6), which is folded into Kinship
*     above. GLSS3-6 Kinship = "distributed by village/family" only. Inherited
*     is the larger of the two in GLSS7, so the category is not wave-comparable.
*------------------------------------------------------------------------------
gen byte LndAqBuy_notasked = inlist(Survey,"GLSS3","GLSS4")
gen byte LndAqKin_inclinherit = (Survey=="GLSS7")
lab var LndAqBuy_notasked    "Purchase not a response option this wave (GLSS3/GLSS4)"
lab var LndAqKin_inclinherit "Kinship includes 'Inherited' this wave (GLSS7 only)"

lab define LndOwn 1 "Not owned" 2 "Owned w/o deed" 3 "Owned w/ deed",replace
lab define LndAq 1 "Free" 2 "Sharecropping" 3 "Rented" 4 "Purchased" 5 "Kinship" 6 "Other" ,replace
lab define LndRgt 1 "None" 2 "Security" 3 "Sell" 4 "Both" 5 "Other" ,replace
lab define ShrCrpCat 1 "0" 2 "1-49" 3 "50-100" ,replace
for var LndOwn LndAq LndRgt ShrCrpCat:lab val X X
for var LndOwn LndRgt LndAq ShrCrpCat:tab X Survey

drop *max CPI
ren Survey Surveyx
tab Surveyx LndAq
tab Surveyx LndOwn
tab Surveyx LndRgt
drop if LndOwn == .

* NB: this keep is the release contract for harmonized_land_tenure_data. Any new
* variable created above MUST be listed here or it is silently dropped before
* saveold -- the file simply ships without it and downstream scripts see nothing
* to error on. OwnLndAny/Maj/Full, LocalUnit and the wave-limitation flags were
* lost exactly this way on the 2026-07-15 run.
keep EaId HhId Mid Surveyx plotn LndFrgMid LndOwn LndRgt LndAq ShrCrpCat OwnLnd /*
*/ OwnLndAny OwnLndMaj OwnLndFull LocalUnit LndAqBuy_notasked LndAqKin_inclinherit

* Release check: fail loudly if the contract above silently lost a variable.
foreach v in OwnLnd OwnLndAny OwnLndMaj OwnLndFull LocalUnit {
  capture confirm variable `v'
  if _rc {
    di as err "harmonized_land_tenure_data: expected variable `v' is missing."
    exit 111
  }
}
* di as txt "Release variables present. Ownership prevalence by wave:"
* table Surveyx, stat(mean OwnLnd OwnLndAny OwnLndMaj OwnLndFull) nformat(%6.4f)

compress
saveold "$LabGitHub\harmonized_land_tenure_data",replace version(12)
