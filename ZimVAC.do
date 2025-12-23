use hh_id hh_age hh_sex hh_marital province_code hh_religion hheduclevel reseduclevel hh_employ draughtpowercat proxy_income hh_size cattleyn donkeyyn sheepyn goatsyn pigsyn poultry_yn rabbitsyn marketinformation organisedmarketing farmerorganisation marketdistribution hdds lon lat using "data/2024_ZimLAC_rural_SF.dta"

gen year=2024

gen hhh_age=hh_age
drop if hhh_age==998
gen hhh_sex =  hh_sex
tabulate hhh_sex,gen(hhh_female_d)
drop if hh_marital==5 /*Remove those who are cohabiting to ensure consistency with 2022*/
recode hh_marital (6=5), gen (hhh_marital) /*recoding to ensure consistency with 2022*/
tabulate hhh_marital, gen (hhh_marital_d)
tabulate province_code,gen(province_d)
tabulate  hh_religion, gen(hhh_religion_d) 
replace hheduclevel=reseduclevel if hheduclevel==.
tabulate hheduclevel,gen(hhh_education_d)
gen hhh_employ =hh_employ
tabulate hhh_employ, gen(hhh_employ_d)
replace hhh_age=. if hhh_age==998

tabulate draughtpowercat, gen(draughtpowercat_d)

gen lnproxy_income = ln(1+proxy_income)

egen marketing_sum = rowtotal(marketinformation organisedmarketing farmerorganisation marketdistribution)
gen marketing_d=1
replace  marketing_d=0 if marketing_sum==0
tabulate marketing_sum, gen(marketing_sum_d)

generate marketing_sum_any=1
replace marketing_sum_any=0 if marketing_sum_d1==1

gen marketing_sum_a2=1
replace marketing_sum_a2=0 if marketing_sum<=1
gen marketing_sum_a3=1
replace marketing_sum_a3=0 if marketing_sum<=2
gen marketing_sum_a4=0
replace marketing_sum_a4=1 if marketing_sum==4

generate hhh_educated =1
replace hhh_educated=0 if hhh_education_d1==1

* Spatial lag variables not used in R analysis - commented out to save time
* spgen marketinformation, lon(lon) lat(lat) swm(bin) dist(10) dunit(km) largesize
* spgen organisedmarketing, lon(lon) lat(lat) swm(bin) dist(10) dunit(km) largesize
* spgen farmerorganisation, lon(lon) lat(lat) swm(bin) dist(10) dunit(km) largesize
* spgen marketdistribution, lon(lon) lat(lat) swm(bin) dist(10) dunit(km) largesize
* spgen marketing_sum_any, lon(lon) lat(lat) swm(bin) dist(10) dunit(km) largesize


keep hh_id hhh_educated hhh_age hhh_female_d2 hhh_marital_d1-hhh_marital_d5 hhh_religion_d1-hhh_religion_d10 proxy_income hh_size cattleyn donkeyyn sheepyn goatsyn pigsyn poultry_yn rabbitsyn province_d1-province_d8 marketing_sum_any marketinformation organisedmarketing farmerorganisation marketdistribution marketing_sum_a2 marketing_sum_a3 marketing_sum_a4 marketing_sum hdds lnproxy_income year

save "ZIMVAC24Clean", replace

*2022 ZIMVAC
use HH_ID RESPONDENT_AGE hhsex hhmarital PROVINCE_CODE hh_religion hheducation CATTLEYN DONKEYYN SHEEPYN GOATSYN PIGSYN POULTRY_YN RABBITSYN proxy_income hh_size HDDS_HH DistPCode WARD_NUMBER EA_NUMBER VILLAGE_NAME HOUSEHOLD_NUMBER agric_marketing_1 agric_marketing_2 agric_marketing_3 LONGITUDE LATITUDE dcattlecat donkeycat using "data/2022_ZimVAC_RLA_SF.dta"
gen year = 2022

gen hhh_age= RESPONDENT_AGE
drop if hhh_age ==.

gen hhh_sex = hhsex
drop if hhh_sex ==.
tabulate hhh_sex,gen(hhh_female_d)

gen hhh_marital = hhmarital
drop if hhh_marital ==.
tabulate hhh_marital, gen (hhh_marital_d)

tabulate PROVINCE_CODE,gen(province_d)

tabulate  hh_religion, gen(hhh_religion_d)
drop if hh_religion==. 

tabulate hheducation,gen(hhh_education_d)
drop if hheducation==.
generate hhh_educated =1
replace hhh_educated=0 if hhh_education_d1==1

egen draughtpower = rowtotal( dcattlecat donkeycat)
recode draughtpower (0=1 "Zero") (1 2=2 "One to Two") (3 4 5=3 "Three to Five") (6 7 8 9 10 =4 "More than Five"), gen(draughtpowercat)
tab draughtpowercat, gen(draughtpowercat_d)

gen lnproxy_income = ln(1+proxy_income)

rename HH_ID hh_id
rename PROVINCE_CODE province_code
rename DistPCode district_code
rename WARD_NUMBER ward_number
rename EA_NUMBER ea_number
rename VILLAGE_NAME village_name
rename HOUSEHOLD_NUMBER household_number
rename CATTLEYN cattleyn
rename DONKEYYN donkeyyn
rename SHEEPYN sheepyn
rename GOATSYN goatsyn
rename PIGSYN pigsyn
rename POULTRY_YN poultry_yn
rename RABBITSYN rabbitsyn
rename HDDS_HH hdds

drop if cattleyn==.
drop if donkeyyn==.
drop if sheepyn==.
drop if goatsyn==.
drop if pigsyn==.
drop if poultry_yn==.
drop if rabbitsyn==.

gen marketinformation=0
replace marketinformation=1 if agric_marketing_1==2 | agric_marketing_2==2

gen organisedmarketing=0
replace organisedmarketing=1 if agric_marketing_1==3 | agric_marketing_2==3 | agric_marketing_3==3

gen farmerorganisation=0
replace farmerorganisation=1 if agric_marketing_1==4 | agric_marketing_2==4 | agric_marketing_3==4

gen marketdistribution=0
replace marketdistribution=1 if agric_marketing_1==1 

egen marketing_sum = rowtotal(marketinformation organisedmarketing farmerorganisation marketdistribution)
gen marketing_d=1
replace  marketing_d=0 if marketing_sum==0
tabulate marketing_sum, gen(marketing_sum_d)

generate marketing_sum_any=1
replace marketing_sum_any=0 if marketing_sum_d1==1


gen marketing_sum_a2=1
replace marketing_sum_a2=0 if marketing_sum<=1
gen marketing_sum_a3=1
replace marketing_sum_a3=0 if marketing_sum<=2
gen marketing_sum_a4=0
replace marketing_sum_a4=1 if marketing_sum==4

* Longitude check not needed since spgen is commented out
* count if LONGITUDE>180 /*spgen command won't run if the longitude is not within -180 to 180*/
* drop if LONGITUDE>180

* Spatial lag variables not used in R analysis - commented out to save time
* spgen marketinformation, lon(LONGITUDE) lat(LATITUDE) swm(bin) dist(10) dunit(km) largesize
* spgen organisedmarketing, lon(LONGITUDE) lat(LATITUDE) swm(bin) dist(10) dunit(km) largesize
* spgen farmerorganisation, lon(LONGITUDE) lat(LATITUDE) swm(bin) dist(10) dunit(km) largesize
* spgen marketdistribution, lon(LONGITUDE) lat(LATITUDE) swm(bin) dist(10) dunit(km) largesize
* spgen marketing_sum_any, lon(LONGITUDE) lat(LATITUDE) swm(bin) dist(10) dunit(km) largesize

keep hh_id hhh_educated hhh_age hhh_female_d2 hhh_marital_d1-hhh_marital_d5 hhh_religion_d1-hhh_religion_d10 proxy_income hh_size  cattleyn donkeyyn sheepyn goatsyn pigsyn poultry_yn rabbitsyn province_d1-province_d8 marketing_sum_any marketinformation organisedmarketing farmerorganisation marketdistribution marketing_sum_a2 marketing_sum_a3 marketing_sum_a4 marketing_sum hdds lnproxy_income year

save "ZIMVAC22Clean", replace

append using ZIMVAC22Clean ZIMVAC24Clean

save "ZIMVACCombined", replace
