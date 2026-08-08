*==============================================================================
* 09_societal_peace_and_cohesion.do
*
* Builds the 'Harmonized Conflict data' release.
*
* WRITES: $LabGitHub\harmonized_societal_peace_and_cohesion_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 1879-2081.
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



tempfile Temp 
use "$COLLATED\Output\Governance\Peace_and_social_cohesion",clear
for var Survey:decode X,gen(Xx)
save `Temp',replace
use "$COLLATED\Output\Household Identification",clear
decode Survey,gen(Surveyx)
merge 1:m Surveyx EaId HhId using `Temp' 
keep if _merge==3
for var s13eq11a s13eq11b s13eq11c s13eq11d s13eq11e s13eq11f s13eq11g s13eq11h s13eq11i s13eq11j s13eq11k s13eq11l:replace X=. if X==2

* (1) Distrust for certain groups of people
for var s13eq1a s13eq1b s13eq1c s13eq1d s13eq1e s13eq1f s13eq1g s13eq1h:replace X=(X-1)/3
for var s13eq12 s13eq13 s13eq15:replace X=(X-1)/2
loc Distrust s13eq1a s13eq1b s13eq1c s13eq1d s13eq1e s13eq1f s13eq1g s13eq1h s13eq12 s13eq13 s13eq15
sum `Distrust'
mat Distrust=J(1,5,.)
qui foreach x in 1 2 3 4 5 6 7 8 9 10 11{
	pca  `Distrust', vce(nor) com(`x')
	mat A = r(table)'
	mat B = J(5,4,.)
	mat B[1,1] = e(N)        //number of observations 
	mat B[2,1] = e(rho)      //fraction of explained variance
	mat B[3,1] = e(v_rho)    //variance of e(rho)
	mat B[4,1] = e(chi2_i)   //chi-squared statistic for test of independence
	mat B[4,4] = e(p_i)      //significance of test of independence
	mat B[5,1] = e(chi2_s)   //chi-squared statistic for test of sphericity
	mat B[5,4] = e(p_s)      //significance of test of sphericity       
	mat rownames B = N_0 rho_0 v_rho_0 chiI_0 chiS_0
	mat A = A[1....,1..4]\B
	mat A = A,J(rowsof(A),1,`x')
	
	mat Distrust = A\Distrust
	mat drop A B
}

* (2) Conflict and tension
replace s13eq2 = (s13eq2-1)/4
decode s13eq4,gen(s13eq4x)
gen s13eq4a = s13eq4x != "land disputes"
gen s13eq4b = s13eq4x != "chieftancy"
gen s13eq4c = s13eq4x != "ethnic/tribal conflict"
gen s13eq4d = s13eq4x != "political differences"
gen s13eq4e = s13eq4x != "indebtness"
gen s13eq4f = s13eq4x != "religion"
gen s13eq4g = s13eq4x != "marriage"
loc Conflict s13eq2 s13eq4a s13eq4b s13eq4c s13eq4d s13eq4e s13eq4f s13eq4g
sum `Conflict'
mat conflict=J(1,5,.)
qui foreach x in 1 2 3{
	pca `Conflict', vce(nor) com(`x')
	mat A = r(table)'
	mat B = J(5,4,.)
	mat B[1,1] = e(N)        //number of observations 
	mat B[2,1] = e(rho)      //fraction of explained variance
	mat B[3,1] = e(v_rho)    //variance of e(rho)
	mat B[4,1] = e(chi2_i)   //chi-squared statistic for test of independence
	mat B[4,4] = e(p_i)      //significance of test of independence
	mat B[5,1] = e(chi2_s)   //chi-squared statistic for test of sphericity
	mat B[5,4] = e(p_s)      //significance of test of sphericity       
	mat rownames B = N_0 rho_0 v_rho_0 chiI_0 chiS_0
	mat A = A[1....,1..4]\B
	mat A = A,J(rowsof(A),1,`x')
	mat conflict = A\conflict
	mat drop A B
}

* (3) Violence
replace s13eq3 = (s13eq3-1)/3   //Use of force or violence in community or neighbourhood
replace s13eq5 = (s13eq5-1)/5      //Risk of violence in community or town
replace s13eq6 = (s13eq6-1)/3   //Frequency of violence between groups
loc Violence s13eq3 s13eq5 s13eq6
sum `Violence'
mat violence=J(1,5,.)
qui foreach x in 1 2 3{
	pca `Violence', vce(nor) com(`x')
	mat A = r(table)'
	mat B = J(5,4,.)
	mat B[1,1] = e(N)        //number of observations 
	mat B[2,1] = e(rho)      //fraction of explained variance
	mat B[3,1] = e(v_rho)    //variance of e(rho)
	mat B[4,1] = e(chi2_i)   //chi-squared statistic for test of independence
	mat B[4,4] = e(p_i)      //significance of test of independence
	mat B[5,1] = e(chi2_s)   //chi-squared statistic for test of sphericity
	mat B[5,4] = e(p_s)      //significance of test of sphericity       
	mat rownames B = N_0 rho_0 v_rho_0 chiI_0 chiS_0
	mat A = A[1....,1..4]\B
	mat A = A,J(rowsof(A),1,`x')
	mat violence = A\violence
	mat drop A B
}

* (4) Dispute resolution
replace s13eq8 = (s13eq8-1)/3      //Level of confidence in dispute resolution mechanism
replace s13eq9 = (s13eq9-1)/2       //Community help
replace s13eq10 = (s13eq10-1)/4    // Presence of a policing or neighbourhood watchdog
loc Resolution s13eq7 s13eq8 s13eq9 s13eq10
sum `Resolution'
mat resolution=J(1,5,.)
qui foreach x in 1 2 3 4{
	pca `Resolution', vce(nor) com(`x')
	mat A = r(table)'
	mat B = J(5,4,.)
	mat B[1,1] = e(N)        //number of observations 
	mat B[2,1] = e(rho)      //fraction of explained variance
	mat B[3,1] = e(v_rho)    //variance of e(rho)
	mat B[4,1] = e(chi2_i)   //chi-squared statistic for test of independence
	mat B[4,4] = e(p_i)      //significance of test of independence
	mat B[5,1] = e(chi2_s)   //chi-squared statistic for test of sphericity
	mat B[5,4] = e(p_s)      //significance of test of sphericity       
	mat rownames B = N_0 rho_0 v_rho_0 chiI_0 chiS_0
	mat A = A[1....,1..4]\B
	mat A = A,J(rowsof(A),1,`x')
	mat resolution = A\resolution
	mat drop A B
}

* (5) Concerns of threats of various nature and incidents
loc Threats s13eq11a s13eq11b s13eq11c s13eq11d s13eq11e s13eq11f s13eq11g s13eq11h s13eq11i s13eq11j s13eq11k s13eq11l
sum `Threats'
mat Threats=J(1,5,.)
qui foreach x in 1 2 3 4 5 6 7 8 9 10 11 12{
	pca  `Threats', vce(nor) com(`x')
	mat A = r(table)'
	mat B = J(5,4,.)
	mat B[1,1] = e(N)        //number of observations 
	mat B[2,1] = e(rho)      //fraction of explained variance
	mat B[3,1] = e(v_rho)    //variance of e(rho)
	mat B[4,1] = e(chi2_i)   //chi-squared statistic for test of independence
	mat B[4,4] = e(p_i)      //significance of test of independence
	mat B[5,1] = e(chi2_s)   //chi-squared statistic for test of sphericity
	mat B[5,4] = e(p_s)      //significance of test of sphericity       
	mat rownames B = N_0 rho_0 v_rho_0 chiI_0 chiS_0
	mat A = A[1....,1..4]\B
	mat A = A,J(rowsof(A),1,`x')
	
	mat Threats = A\Threats
	mat drop A B
}
mat li Threats

* (6) Vacate
loc Vacate s13eq14
sum `Vacate'

unab d1: `Distrust'
unab d2: `Conflict'
unab d3: `Violence'
unab d4: `Resolution'
unab d5: `Threats'
unab d6: `Vacate'

for var `d1' `d2' `d3' `d4' `d5' `d6':drop if X==.

foreach d in 1 2 3 4 5{
	qui pca `d`d'' 
	mata:st_matrix("w`d'",(st_matrix("e(Ev)"):/sum(st_matrix("e(Ev)")))*st_matrix("e(L)"):^2)
	loc n : word count `d`d'' 
	forvalue i=1/`=`n''{
		loc vv : word `i' of `d`d'' 
		gen w`d'_`vv' = `=w`d'[1,`=`i'']'
		gen ws`d'_`vv' = w`d'_`vv'*`vv'
	}
	egen index`d' = rowtotal(ws`d'_*)
}

gen index6 = s13eq14

unab d0: index1 index2 index3 index4 index5 index6
qui pca `d0' 
mata:st_matrix("w0",(st_matrix("e(Ev)"):/sum(st_matrix("e(Ev)")))*st_matrix("e(L)"):^2)
loc n : word count `d0' 
forvalue i=1/`=`n''{
	loc vv : word `i' of `d0' 
	gen w0_`vv' = `=w0[1,`=`i'']'
	gen ws0_`vv' = w0_`vv'*`vv'
}
egen index0 = rowtotal(ws0_*)

sum index*

sum index0
xtile index0CAT = index0 [pw=Weight] , nq(3) 
xtile index1CAT = index1 [pw=Weight] , nq(3) 
gen index2CAT = index2 ==1
gen index3CAT = index3 ==1
xtile index4CAT = index4 [pw=Weight] , nq(3) 
xtile index5CAT = index5 [pw=Weight] , nq(3) 
for var index0CAT index1CAT index2CAT index3CAT index4CAT index5CAT:replace X=X-1
gen   index6CAT = index6

keep EaId HhId Mid Surveyx index0 index1 index2 index3 index4 index5 index6 index*CAT /*
*/ `Distrust' `Conflict' `Violence' `Resolution' `Threats' `Vacate' /*
*/ w0_* w1_* w2_* w3_* w4_* w5_* 

order EaId HhId Mid Surveyx index0 index1 index2 index3 index4 index5 index6 index*CAT /*
*/ `Distrust' `Conflict' `Violence' `Resolution' `Threats' `Vacate' /*
*/ w0_* w1_* w2_* w3_* w4_* w5_* 
compress
saveold "$LabGitHub\harmonized_societal_peace_and_cohesion_data",replace version(12)
