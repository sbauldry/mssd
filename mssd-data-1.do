*** Purpose: to prepare data for mssd
*** Author: S Bauldry
*** Date: April 1, 2016

*** paths to data
global w1 "[path to data]"
global w2 "[path to data]"
global wt "[path to data]"

*** extracting wave 1 data
use aid imonth iday iyear h1gi1m h1gi1y bio_sex h1gi4 h1gi5c h1gi6a-h1gi6e ///
  pa12 pb8 h1rm1 h1rf1 pa55 h1gi3 h1pr1-h1pr4 h1hr3* h1hr6* h1nf1 h1nm1    ///
  using "$w1", replace

*** setting missing data
recode h1gi1m h1gi1y (96 = .)
recode bio_sex h1gi4 h1gi5c h1gi6a-h1gi6e (6 7 8 9 = .)
recode pa12 pb8 h1rm1 h1rf1 (11 12 96/99 = .)
recode pa55 (9996 = .)
recode h1gi3 (96 98 99 = .)
recode h1pr1-h1pr4 (6 96 98 99 = .)
recode h1hr3* h1hr6* (96 97 98 99 = .)

*** preparing variables
recode iyear (94 = 1994) (95 = 1995)
gen age = floor( ( mdy(imonth, iday, iyear) - mdy(h1gi1m, 15, 1900 + h1gi1y) )/364.25 )
lab var age "w1 age"

gen female = (bio_sex == 2) if !mi(bio_sex)
lab var female "w1 female"

gen race = 3 if h1gi4 == 1
replace race = 2 if h1gi6b == 1 & mi(race)
replace race = 4 if (h1gi6c == 1 | h1gi6d == 1 | h1gi6e == 1) & mi(race)
replace race = 1 if h1gi6a == 1 & mi(race)
lab def r 1 "white" 2 "black" 3 "hispanic" 4 "other"
lab val race r
lab var race "w1 race"

gen biofather = .
gen biomother = .
gen othfather = .
gen othmother = .
foreach x in a b c d e f g h i j k l m n o p q r s t {
	replace biofather = 1 if h1hr3`x' == 11 & h1hr6`x' == 1
	replace biofather = 1 if h1hr3`x' == 15 & h1nf1 == 7 
	
	replace biomother = 1 if h1hr3`x' == 12 & h1hr6`x' == 7
	replace biomother = 1 if h1hr3`x' == 14 & h1hr6`x' == 7
	replace biomother = 1 if h1hr3`x' == 12 & h1nm1 == 7
	
	replace othfather = 1 if h1hr3`x' == 11 & (h1hr6`x' > 1 & h1hr6`x' < 7)
	replace othfather = 1 if h1hr3`x' == 15 & h1nf1 != 7
	replace othfather = 1 if h1hr3`x' == 16 & h1nf1 != 7
	
	replace othmother = 1 if h1hr3`x' == 14 & (h1hr6`x' > 7 & h1hr6`x' < 13)
	replace othmother = 1 if h1hr3`x' == 12 & h1nm1 != 7
	replace othmother = 1 if h1hr3`x' == 13 & h1nm1 != 7
}
gen famstr = 1 if biofather == 1 & biomother == 1
replace famstr = 2 if (biofather == 1 & othmother == 1) | ///
                      (othfather == 1 & biomother == 1) | ///
					  (othfather == 1 & othmother == 1)
replace famstr = 3 if (biomother == 1 | othmother == 1) & ///
                       mi(biofather) & mi(othfather)
replace famstr = 4 if (biofather == 1 | othfather == 1) & ///
                       mi(biomother) & mi(othmother)
replace famstr = 5 if mi(famstr)
lab def fs 1 "two bio parents" 2 "two parents" 3 "single mother" ///
           4 "single father" 5 "other"
lab val famstr fs
lab var famstr "w1 family structure"

recode pa12 pb8 h1rm1 h1rf1 (10 = 1)
gen paredu = max(pa12,pb8)
gen cparedu = max(h1rm1,h1rf1)
replace paredu = cparedu if missing(paredu)
recode paredu (1 2 3 = 1) (4 5 = 2) (6 7 = 3) (8 = 4) (9 = 5)
lab var paredu "w1 parent education"

gen lninc = log(pa55 + 1)
lab var lninc "w1 parent income (logged)"

recode h1gi3 (0 = 0) (1/12 = 1) (13/19 = 2), gen(moved)
lab def m 0 "never" 1 "move age < 13" 2 "move age 13+"
lab val moved m
lab var moved "w1 moved"

rename (h1pr1 h1pr2 h1pr3 h1pr4) (ssadult ssparent ssteacher ssfriend)

keep aid age female race famstr paredu lninc moved ss*
tempfile wv1
save `wv1', replace


*** extracting wave 2 data
use aid h2fs3 h2fs6 h2fs11 h2fs16 h2fs19 using "$w2", replace

recode h2fs* (6 8 = .)

rename (h2fs3 h2fs6 h2fs11 h2fs16 h2fs19) (blues depress happy sad life)

tempfile wv2
save `wv2', replace


*** merging data
use `wv1', replace
merge 1:1 aid using `wv2'
keep if _merge == 3
drop _merge

merge 1:1 aid using "$wt"
keep if _merge == 3
drop _merge


*** saving data for analysis
keep if !mi(gswgt2)
save mssd-data-1, replace

