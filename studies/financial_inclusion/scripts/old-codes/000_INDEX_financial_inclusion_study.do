/**************************************************************************
 * Filename: studies/financial_inclusion/scripts/000_INDEX_financial_inclusion_study.do
 * Author: Francis Tsiaboe (ftsiboe)
 * Date: 2025-04-05
 *
 * Purpose:
 * Builds the financial inclusion index used by the financial_inclusion study:
 * the first principal component of a set of financial-access indicators,
 * estimated within each survey round x locality stratum and rescaled onto a
 * common pooled metric.
 *
 * WRITES
 *   data-raw/releases/harmonized_data/financial_inclusion_index.dta
 *       FinIdx     the rescaled first principal component (unbounded, mean ~0)
 *       FinIdxSi   FinIdx min-max scaled over the pooled sample, [0,1]
 *       FinIdxCat  weighted quintiles of FinIdx over the pooled sample, 1-5
 *       keys: Surveyx EaId HhId Mid
 *
 *   data-raw/releases/harmonized_data/financial_inclusion_index_diagnostics.dta
 *       one row per stratum x indicator: the first-component loading, the
 *       stratum N, and the share of variance explained. See "DIAGNOSTICS".
 *
 * Directions for Citing:
 * When using this script or any part of this analysis in your work, please cite
 * it as follows:
 * Tsiaboe, Francis. "Tech Inefficiency and Financial Inclusion Data Analysis."
 * GitHub, 2025. https://github.com/ftsiboe/ghana/tree/main/okwaayeli
 **************************************************************************/

loc REL "$GitHub\ghana\okwaayeli\data-raw\releases\harmonized_data"

// Load the harmonized crop farmer data and clear the current dataset
use "`REL'\harmonized_crop_farmer_data", clear

// Collapse to one row per household member, averaging the demographics over
// that member's plots. WeightHH is averaged the same way.
collapse (mean) YerEdu EduLevel AgeYr Female WeightHH, by(Surveyx EaId HhId Mid Locality Head)

// Merge on the financial indicators.
//
// keepusing() is deliberate. The financial inclusion release also carries
// Locality, RegName, Weight, Relate and Relatex, and Locality would collide
// with the one carried down from the farmer file above. Naming exactly what is
// needed keeps that collision from arising at all, and keeps this script's
// factor list and its inputs in one-to-one correspondence.
merge 1:1 Surveyx EaId HhId Mid using "`REL'\harmonized_financial_inclusion_data", ///
    keepusing(FinWorker HHFinWorker Insured_* Banked)
keep if _merge==3 // Keep only the matched records
drop _merge

// Decode the 'Locality' variable to create a new variable 'Localityx'
decode Locality, gen(Localityx)

/**************************************************************************
 * THE INDICATOR SET
 *
 * Person     YerEdu FinWorker HHFinWorker
 * Insurance  Insured_*
 * Banking    Banked
 *
 * Community distances (BankKm, RoadKm, TrnprtKm) are deliberately NOT in this
 * list. Two reasons.
 *
 * First, coverage. The community questionnaire is administered per enumeration
 * area and does not cover every EA, so those three are missing for 3,340 of
 * the 16,273 farmers in the release -- and the complete-case rule below would
 * drop every one of them. That loss is not random: it falls disproportionately
 * on GLSS6 urban EAs, which is exactly the imbalance 03_financial_inclusion.do
 * now avoids by left-joining the community block rather than inner-joining it.
 * Keeping the distances here would reintroduce the same selection one stage
 * later.
 *
 * Second, construct. The distances are place characteristics and enter with
 * the opposite sign to everything else, so the index was a mix of "how
 * financially connected is this person" and "how far is this person from a
 * bank". Dropping them makes it unambiguously the former, and removes the need
 * to reason about sign when reading the loadings.
 *
 * If they are ever restored, restore the coverage problem with them: decide
 * explicitly whether to impute the distances or to accept the smaller sample.
 **************************************************************************/
loc Person   YerEdu FinWorker HHFinWorker
loc Insured  Insured_*
loc Banked   Banked

loc Factors `Person' `Insured' `Banked'

// Complete cases across every indicator. On the current release the financial
// indicators are all complete, so this bites only through YerEdu, which comes
// from the farmer file. Report the loss rather than taking it silently.
qui count
loc n_before = r(N)
for var `Factors': drop if X==.
qui count
di as txt "complete-case filter: `n_before' -> " r(N) " rows"

sum HhId `Factors'
tabstat HhId `Factors', by(Surveyx)

/**************************************************************************
 * DIAGNOSTICS
 *
 * The first-component loadings are the evidence that this component measures
 * financial inclusion at all, so they are written out rather than discarded:
 * one row per stratum x indicator, with the stratum N and the share of
 * variance explained by the component.
 *
 * What to look for when reading them back:
 *   - the loadings should all share a sign within a stratum. A mixed sign
 *     pattern means the first component is picking up something other than
 *     inclusion;
 *   - rho (explained variance) should be comfortably above the share a single
 *     component would get by chance. A low rho undermines the construct
 *     whatever the loadings look like;
 *   - the sign of the POOLED component is arbitrary. Check it orients so that
 *     higher means more included before interpreting FinIdx directly.
 **************************************************************************/
tempname pf
postfile `pf' str8 round str8 locality str32 indicator double loading ///
              long N double rho using "`REL'\financial_inclusion_index_diagnostics", replace

// ---- pooled component: the reference metric everything is rescaled onto ----
pca `Factors', vce(nor) com(1)
mat L = e(L)
loc rn : rownames L
forvalues i = 1/`=rowsof(L)' {
    loc vn : word `i' of `rn'
    post `pf' ("POOLED") ("POOLED") ("`vn'") (L[`i',1]) (e(N)) (e(rho))
}
qui predict COM

// ---- stratified components, rescaled onto COM ----
qui foreach sur in "GLSS6" "GLSS7" {
    qui foreach lc in "Rural" "Urban" {
        loc varlist

        // Screen out indicators with no variation IN THIS CELL -- pca cannot
        // use them. Insured_Travel, for one, is constant in both GLSS7 cells.
        // Test on r(sd) explicitly: a missing r(sd) must not pass the screen.
        foreach x in `Factors' {
            qui sum `x' if Surveyx == "`sur'" & Localityx == "`lc'"
            if !missing(r(sd)) & r(sd) > 0 loc varlist `varlist' `x'
        }

        // The stratum is round AND locality. Conditioning on Surveyx alone
        // fits one PCA per round and then applies it to each locality in turn,
        // which makes the Rural and Urban passes within a round identical
        // except for which rows receive predictions -- i.e. not stratified by
        // locality at all, despite the loop saying otherwise.
        pca `varlist' if Surveyx == "`sur'" & Localityx == "`lc'", vce(nor) com(1)

        mat L = e(L)
        loc rn : rownames L
        forvalues i = 1/`=rowsof(L)' {
            loc vn : word `i' of `rn'
            post `pf' ("`sur'") ("`lc'") ("`vn'") (L[`i',1]) (e(N)) (e(rho))
        }

        qui predict temp1 if Surveyx == "`sur'" & Localityx == "`lc'"

        // Regressing the pooled score on the stratum score and keeping the
        // fitted values puts every stratum on COM's scale AND fixes its
        // orientation: if a stratum's component came out sign-flipped, the
        // coefficient is negative and the fitted values still align with COM.
        reg COM temp1 if Surveyx == "`sur'" & Localityx == "`lc'"
        predict FinIdx_`sur'_`lc' if Surveyx == "`sur'" & Localityx == "`lc'"
        drop temp1
  }
}
postclose `pf'

// Each FinIdx_<round>_<locality> is non-missing only inside its own cell, so
// rowmean acts as a coalesce -- every observation has exactly one contributing
// value. This depends on the strata being disjoint; if they ever overlap this
// silently averages instead of erroring.
egen FinIdx = rowmean(FinIdx_*)

// Both transforms below are computed over the POOLED sample, so FinIdxSi and
// FinIdxCat are absolute positions, not within-round ranks. A GLSS6 household
// in quintile 1 and a GLSS7 household in quintile 1 sit at the same absolute
// point on the index. That is what you want for discussing change between
// rounds, and the wrong thing for within-round relative position.
sum FinIdx, detail

// FinIdxSi is min-max scaled, so it is sensitive to a single extreme value:
// one outlier compresses everyone else into a narrow band and blunts the
// variable wherever it is used as a matching covariate. Read the percentiles
// printed above -- if the IQR is a small fraction of the 0-1 range, prefer a
// rank or quantile transform here.
sum FinIdx
gen FinIdxSi = (FinIdx-r(min))/(r(max)-r(min))
sum FinIdxSi, detail

xtile FinIdxCat = FinIdx [pw=WeightHH] , nq(5)
tab FinIdxCat Surveyx, col

keep HhId EaId Mid Surveyx FinIdx FinIdxSi FinIdxCat

lab var FinIdx    "Financial inclusion index (first principal component, rescaled)"
lab var FinIdxSi  "Financial inclusion index, min-max scaled over the pooled sample"
lab var FinIdxCat "Financial inclusion index, weighted pooled quintiles"

compress
saveold "`REL'\financial_inclusion_index", replace ver(12)
