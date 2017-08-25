*** Purpose: to conduct mssd analysis
*** Author: S Bauldry
*** Date: August 20, 2017

*** load and survey set the data
use mssd-data-1
svyset psuscid [pw = gswgt2]


*** descriptive statistics
foreach x of varlist w2dep w2blu w2hap w2sad w2lfe {
	svy: mean `x'
}

svy: prop w1mov

foreach x of varlist w1adl w1tch w1par w1frd {
	svy: mean `x'
}

svy: mean age
foreach x of varlist female race famstr paredu {
	svy: prop `x'
}

svy: mean lninc

*** prepare indicators
qui tab w1mov, gen(m)
qui tab race, gen(r)
qui tab famstr, gen(f)
qui tab paredu, gen(p)

*** estimate parameters for measurement model
svy: sem (Depress -> w2dep w2blu w2hap w2sad w2lfe), method(mlmv)
estat eqgof

sem (Depress -> w2dep w2blu w2hap w2sad w2lfe), method(mlmv)
estat gof, stats(all)
estat eqgof

*** estimate model parameters
local CVs age female r2-r4 f2-f5 p2-p5 lninc
svy: sem (Depress -> w2dep w2blu w2hap w2sad w2lfe)                 ///
  (`CVs' m2 m3 -> w1adl w1tch w1par w1frd)                          ///
  (`CVs' m2 m3 w1adl w1tch w1par w1frd -> Depress),                 ///
  method(mlmv) cov(e.w1adl*e.w1par e.w1par*e.w1tch e.w1tch*e.w1frd  ///
  e.w1adl*e.w1tch e.w1adl*e.w1frd e.w1par*e.w1frd)
	
*** check equation goodness-of-fit
estat eqgof

*** check indirect effects
estat teffects, nodir

* obtain standard errors for specific indirect effects
sem, coeflegend
nlcom _b[Depress:w1par]*_b[w1par:m3]
nlcom _b[Depress:w1frd]*_b[w1frd:m3]
nlcom _b[Depress:w1adl]*_b[w1adl:m3]
nlcom _b[Depress:w1tch]*_b[w1tch:m3]


*** program to bootstrap indirect effects
capture progrom drop BSI
program BSI, rclass
	local CVs age female r2-r4 f2-f5 p2-p5 lninc
	svy: sem (Depress -> w2dep w2blu w2hap w2sad w2lfe)                 ///
	  (`CVs' m2 m3 -> w1adl w1tch w1par w1frd)                          ///
	  (`CVs' m2 m3 w1adl w1tch w1par w1frd -> Depress),                 ///
	  method(mlmv) cov(e.w1adl*e.w1par e.w1par*e.w1tch e.w1tch*e.w1frd  ///
	  e.w1adl*e.w1tch e.w1adl*e.w1frd e.w1par*e.w1frd)
	estat teffects
	mat bi = r(indirect)
	
	nlcom _b[Depress:w1par]*_b[w1par:m3]
	mat b1 = r(b)
	nlcom _b[Depress:w1frd]*_b[w1frd:m3]
	mat b2 = r(b)
	nlcom _b[Depress:w1adl]*_b[w1adl:m3]
	mat b3 = r(b)
	nlcom _b[Depress:w1tch]*_b[w1tch:m3]
	mat b4 = r(b)
	
	return scalar bi1 = bi[1,67]
	return scalar bi2 = bi[1,68]
	return scalar b1  = b1[1,1]
	return scalar b2  = b2[1,1]
	return scalar b3  = b3[1,1]
	return scalar b4  = b4[1,1]
end

log using "mssd-bs.txt", replace text
set seed 13571113
bootstrap r(bi1) r(bi2) r(b1) r(b2) r(b3) r(b4), reps(500): BSI
log close

*** Do more recent moves have stronger relationships social support
gen ymv = age - w1agm
table w1mov, c(mean ymv sd ymv)
svy: mean ymv

local CVs age female r2-r4 f2-f5 p2-p5 lninc
svy: sem (Depress -> w2dep w2blu w2hap w2sad w2lfe)                ///
  (`CVs' ymv m2 m3 -> w1adl w1tch w1par w1frd)                     ///
  (`CVs' ymv m2 m3 w1adl w1tch w1par w1frd -> Depress),            ///
  method(mlmv) cov(e.w1adl*e.w1par e.w1par*e.w1tch e.w1tch*e.w1frd ///
  e.w1adl*e.w1tch e.w1adl*e.w1frd e.w1par*e.w1frd)

*** Does moving between Waves 1 and 2 matter
local CVs age female r2-r4 f2-f5 p2-p5 lninc
svy: sem (Depress -> w2dep w2blu w2hap w2sad w2lfe)                 ///
  (`CVs' w2mov -> w1adl w1tch w1par w1frd)                          ///
  (`CVs' w2mov w1adl w1tch w1par w1frd -> Depress),                 ///
  method(mlmv) cov(e.w1adl*e.w1par e.w1par*e.w1tch e.w1tch*e.w1frd  ///
  e.w1adl*e.w1tch e.w1adl*e.w1frd e.w1par*e.w1frd)
		 
*** DiD analysis
preserve
keep w1adl w1tch w1par w1frd w1dps w2adl w2tch w2par w2frd w2dps w2mov
foreach x in adl tch par frd dps {
  gen d`x' = w2`x' - w1`x'
}

foreach y of varlist ddps dadl dtch dpar dfrd {
	regress `y' w2mov 
}
regress ddps w2mov dadl dtch dpar dfrd
restore
 
*** Check results with scale rather than latent variable
local CVs age female r2-r4 f2-f5 p2-p5 lninc
svy: sem (`CVs' m2 m3 -> w1adl w1tch w1par w1frd)                  ///
  (`CVs' m2 m3 w1adl w1tch w1par w1frd -> w2dps),                  ///
  method(mlmv) cov(e.w1adl*e.w1par e.w1par*e.w1tch e.w1tch*e.w1frd ///
  e.w1adl*e.w1tch e.w1adl*e.w1frd e.w1par*e.w1frd)

