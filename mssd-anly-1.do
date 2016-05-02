*** Purpose: to conduct mssd analysis
*** Author: S Bauldry
*** Date: April 11, 2016

*** load and survey set the data
use mssd-data-1
svyset psuscid [pw = gswgt2]


*** descriptive statistics
foreach x of varlist depress blues happy sad life {
	svy: mean `x'
}

svy: prop moved

foreach x of varlist ss* {
	svy: mean `x'
}

svy: mean age
foreach x of varlist female race famstr paredu {
	svy: prop `x'
}

svy: mean lninc

*** prepare indicators
qui tab moved, gen(m)
qui tab race, gen(r)
qui tab famstr, gen(f)
qui tab paredu, gen(p)

*** estimate parameters for measurement model
svy: sem (Depress -> depress blues happy sad life), method(mlmv)
estat eqgof

sem (Depress -> depress blues happy sad life), method(mlmv)
estat gof, stats(all)
estat eqgof

*** estimate model parameters
svy: sem (Depress -> depress blues happy sad life)                         ///
	     (age female r2-r4 f2-f5 p2-p5 lninc m2 m3 -> ss*)                 ///
	     (age female r2-r4 f2-f5 p2-p5 lninc m2 m3 ss* -> Depress),        ///
	     method(mlmv) cov(e.ssadult*e.ssparent e.ssparent*e.ssteacher      ///
		 e.ssteacher*e.ssfriend e.ssadult*e.ssteacher e.ssadult*e.ssfriend ///
		 e.ssparent*e.ssfriend)
	
*** check equation goodness-of-fit
estat eqgof

*** check indirect effects
estat teffects, nodir

* obtain standard errors for specific indirect effects
sem, coeflegend
nlcom _b[Depress:ssparent]*_b[ssparent:m3]
nlcom _b[Depress:ssfriend]*_b[ssfriend:m3]
nlcom _b[Depress:ssadult]*_b[ssadult:m3]
nlcom _b[Depress:ssteacher]*_b[ssteacher:m3]
