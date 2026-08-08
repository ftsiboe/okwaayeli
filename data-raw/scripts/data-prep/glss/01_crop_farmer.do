*==============================================================================
* 01_crop_farmer.do
*
* Builds the 'Harmonized Crop farmer data' release.
*
* WRITES: $LabGitHub\harmonized_crop_farmer_data.dta
*
* Source: extracted 2026-08-08 from data-raw/okwaayeli_DATA.do, lines 36-219.
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
use "$COLLATED\Output\Agric\Crop Farmer Level Data",clear
drop if inlist(HrvstKg,.,0)
for var CropCatID CropID Region Survey Ecozon Season:decode X,gen(Xx)
gen     Source = 0 if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
replace Source = 1 if inlist(Surveyx,"GSPS1","GSPS2")
keep if inlist(Source,0)
split Seasonx, p("/") limit(2)
destring Seasonx1,gen(TrendY)
/*
keep if Surveyx == "GLSS7"
keep Surveyx EaId HhId Mid CropIDx Area LndFrgMid LndAqKin LndAqBuy LndAqRnt LndAqFre LndAqOthr LndRgtSll LndRgtSec LndRgtBth LndRgtNone LndRgtOthr LndDed LndOwn LndNOwn
order Surveyx EaId HhId Mid CropIDx Area LndFrgMid LndAqKin LndAqBuy LndAqRnt LndAqFre LndAqOthr LndRgtSll LndRgtSec LndRgtBth LndRgtNone LndRgtOthr LndDed LndOwn LndNOwn
sort Surveyx EaId HhId Mid CropIDx
*/
drop EqipMech
egen EqipMech=rowmax(EqipTrcE EqipTrct)
replace Ecozon = 4 if Ecozon == 5
lab define Ecozon 0 "National" 1 "Sudan Savanah" 2 "Guinea Savanah" 3 "Transitional Zone" 4 "Forest Zone" /*
*/ 5 "Forest Zone" 6 "Coastal Savanna" 7 "Meta", replace
//Transformed Herfindahl (Simpson) Index (THI)
*Crop diversifiction by crop cat
egen AreaT   = sum(Area),by(Survey EaId HhId)            //Estimated planted land by Household
egen CrpMix  = sum((Area/AreaT)^2), by(Survey EaId HhId) //Estimated crop mix by Household
qui for var CrpMix:replace X=1-X
qui for var CrpMix:replace X=0 if X < 0
qui for var CrpMix:replace X=1 if X > 1
*keep if CropCatIDx=="Cereal"
*drop if inlist(CropIDx,"Cereal")
save `Temp',replace
keep if inlist(CropIDx,"Sorghum/Millet")
replace CropIDx = "Millet"
gen SorMill = 1
append using `Temp'
replace CropIDx = "Sorghum" if inlist(CropIDx,"Sorghum (Grain)","Sorghum/Millet")
replace CropIDx = "Millet" if inlist(CropIDx,"Millet (Grain)")
replace CropIDx = "Maize" if inlist(CropIDx,"Maize (Grain)")
replace CropIDx = "Palm" if inlist(CropIDx,"Palm Nut")
replace CropIDx = "Beans" if inlist(CropIDx,"Dry Beans")
replace CropIDx = "Cashew" if inlist(CropIDx,"Cashew Nut")

for var SeedKg HirdHr YerEdu HH_Female:replace X=0 if X==.
egen SaleKg = rowtotal(SalOtrKg SalMktKg SalMbdKg SalGatKg SalCrtKg SalConKg)

tempfile Temp
save `Temp',replace

use "$COLLATED\Output\Idividual Education",clear
decode Survey,gen(Surveyx)
keep HhId EaId Mid YerEdu EduLevel EduWhyNo Surveyx
merg 1:m Surveyx EaId HhId Mid using `Temp'
keep if _merge==3
drop Surveyx _merge
lab define EduLevel 0  None 1 Primary 2 JSS 3 SSS 4 "Post SSS" 990 National 991 Meta,replace
lab val EduLevel EduLevel 
gen EduCat=EduLevel>0
replace EduCat = 0 if YerEdu ==0
lab define EduCat 0 "None" 1 "Educated", replace
lab val EduCat EduCat 

gen maize_price = HrvstV/HrvstKg if CropIDx == "Maize"
egen maize_price_median = median(maize_price),by(Regionx Survey)

tempfile Temp
keep Yield SaleKg HrvstV HrvstKg Area HirdHr FertKg PestLt SeedKg HHLaborAE /*
*/Female HH_Female HHSizeAE Depend AgeYr YerEduAE Extension Credit EqipIrig EqipMech Bicycle SorMill /*
*/ Radio Phone Telephone Motorbike Subsidy FmleAERt Marital Relate WlthIndx Locality Marital Relate Ethnic Religion WlthCat /*
*/ CrpMix Region Survey OwnLnd HhId Mid EaId Ecozon EcozonOld welfare Poverty Source Season CropIDx Ecozonx TrendY LndFrgMid Distcode WeightHH /*
*/ YerEdu EduLevel EduWhyNo EduCat maize_price_median RentHa /*
*/ LndAqKin LndAqBuy LndAqRnt LndAqFre LndAqOthr LndRgtSll LndRgtSec LndRgtBth LndRgtNone LndRgtOthr LndDed LndOwn LndNOwn

save `Temp',replace
replace SeedKg=(HrvstV/HrvstKg)*SeedKg
replace HrvstKg=HrvstV/maize_price_median
replace SeedKg=SeedKg/maize_price_median

qui foreach x in Cassava Maize Peanut Plantain Pepper Rice Millet Sorghum Beans Tomatoe Yam Cocoyam Cocoa Okra Palm{
	gen Area_`x' = Area if CropIDx == "`x'"
}

collapse (sum) SaleKg HrvstV HrvstKg Area HirdHr FertKg PestLt SeedKg HHLaborAE Area_* /*
*/ LndAqKin LndAqBuy LndAqRnt LndAqFre LndAqOthr LndRgtSll LndRgtSec LndRgtBth LndRgtNone LndRgtOthr LndDed LndOwn LndNOwn , /*
*/ by(YerEdu EduLevel EduWhyNo EduCat Female HH_Female HHSizeAE Depend AgeYr YerEduAE Extension Credit EqipIrig EqipMech Bicycle SorMill /*
*/ Radio Phone Telephone Motorbike Subsidy FmleAERt Marital Relate WlthIndx Locality Marital Relate Ethnic Religion WlthCat /*
*/ CrpMix LndFrgMid Region Survey OwnLnd HhId Mid EaId Ecozon EcozonOld welfare Poverty Source Season Ecozonx TrendY Distcode WeightHH RentHa)
egen Area_Other = rowtotal(Area_*)
replace Area_Other = Area - Area_Other
qui foreach x in Cassava Maize Peanut Plantain Pepper Rice Millet Sorghum Beans Tomatoe Yam Cocoyam Cocoa Okra Palm Other{
	replace Area_`x' = Area_`x'/Area 
}
sum Area_*
replace Credit = Credit>0
gen Yield = HrvstKg/Area
gen CropIDx = "Pooled"

append using `Temp'

drop if AgeYr<15 | AgeYr ==.
drop if inlist(HHLaborAE,.,0)
drop if Area <0.01
drop if Area >50

drop if inlist(Yield,.,0)
sca Low = 2.5
sca Hig = 97.5
egen Yield_lo=pctile(Yield), p(`=Low') by(Survey CropIDx)
egen Yield_hi=pctile(Yield), p(`=Hig') by(Survey CropIDx)
drop if Yield<Yield_lo | Yield>Yield_hi 

egen Group = group(Survey CropIDx)
egen GroupOBS = count(Group),by(Group)
drop if GroupOBS<30

lab drop CropID

egen TechSeaN =count(Survey),by(CropIDx Ecozon Season)

/*
foreach x in Extension Credit EqipMech Ecozon OwnLnd EqipIrig{
	egen TechSeaN =count(Survey),by(CropID Ecozon Season)
}
*/

keep Yield SaleKg HrvstV HrvstKg Area HirdHr FertKg PestLt SeedKg HHLaborAE /*
*/Female HH_Female HHSizeAE Depend AgeYr YerEduAE Extension Credit EqipIrig EqipMech Bicycle SorMill /*
*/ Radio Phone Telephone Motorbike Subsidy FmleAERt Marital Relate WlthIndx WlthCat /*
*/ RentHa /* LndFrgMid LndAqKin LndAqBuy LndAqRnt LndAqFre LndAqOthr LndRgtSll LndRgtSec LndRgtBth LndRgtNone LndRgtOthr LndDed LndOwn LndNOwn 
*/ CrpMix Region Survey OwnLnd HhId Mid EaId Ecozon EcozonOld welfare Poverty Source Season CropIDx Ecozonx TrendY /*
*/ Locality Marital Relate Ethnic Religion WlthCat LndFrgMid Distcode WeightHH YerEdu EduLevel EduWhyNo EduCat Area_*

decode Relate,gen(Relatex)
gen Head     = 2 if Relatex == "Head"
replace Head = 1 if Relatex == "Spouse"
replace Head = 0 if Head == .
lab define Head 0 "Member" 1 "Spouse of Head" 2 "Head", replace
lab val Head Head 
drop Relatex Relate

decode Religion,gen(Religionx)
drop Religion
gen     Religion = inlist(Religionx,"Catholic","Protestant","Christian")
replace Religion = 2 if inlist(Religionx,"Islam")
replace Religion = 3 if inlist(Religionx,"Traditional")
replace Religion = 4 if inlist(Religionx,"Other","Missing","")
lab define Religion 0 "None" 1 "Christian" 2 "Islam" 3 "Traditional" 4 "Other", replace
lab val Religion Religion 
drop Religionx

decode Marital,gen(Maritalx)
drop Marital
gen     Marital = inlist(Maritalx,"Married","Union")
replace Marital = 2 if inlist(Maritalx,"Divorced/Separated/Widowed")
replace Marital = 0 if inlist(Maritalx,"Missing","")
lab define Marital 0 "None" 1 "Married/Union" 2 "Divorced/Separated/Widowed", replace
lab val Marital Marital 
drop Maritalx

encode CropIDx,gen(CropID) 

replace FertKg = 0 if inlist(CropIDx,"Cassava","Yam","Cocoyam","Plantain","Beans","Peanut")

gen AgeCat     = 1 if AgeYr <= 35
replace AgeCat = 2 if AgeYr > 35 
replace AgeCat = 3 if AgeYr >= 60 

for var SeedKg HHLaborAE HirdHr PestLt FertKg YerEdu:drop if ln(X+0.00001) ==.
for var HrvstKg AgeYr Area:drop if ln(X) ==.
for var Female Extension Credit OwnLnd Ecozon Head Religion Marital CropID AgeCat EqipMech:drop if X ==.

gen UID  = _n
gen Seas = Season
gen Crop = CropID
replace Credit = Credit>0
egen SeasN =group(Seas)
drop CropIDx SorMill Ecozonx
drop HH_Female Motorbike Bicycle Radio Phone Telephone welfare Poverty Subsidy SaleKg WlthIndx WlthCat EcozonOld
drop Source //Region EaId HhId Mid
drop if inlist(HrvstKg,.,0)
decode Survey,gen(Surveyx)
compress
saveold "$LabGitHub\harmonized_crop_farmer_data",replace version(12)
