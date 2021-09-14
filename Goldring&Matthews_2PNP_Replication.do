*****************************************
*** To Purge or Not to Purge
*** Goldring&Matthews
*** British Journal of Political Science
*** Replication Files
*****************************************



**************
*** Load data
**************
use "Goldring&Matthews_2PNP_Replication.dta", clear



********************************
*** Table 2: Summary statistics
********************************
sum logit_purge ml_purge firstgen milsec exitage exittenure military monarchy personal party, det
local var "ml_purge firstgen milsec military monarchy personal party"
foreach v of local var {
	tab `v', m
}



************************************
*** Table 3: Descriptive statistics
************************************
tab2 logit_purge firstgen, m
tab2 ml_purge firstgen, m



*************************************
*** Figure 1: Determinants of purges
*************************************
qui logit logit_purge firstgen, cluster(cowcode)
est store l1
qui clogit logit_purge firstgen, group(cowcode) vce(cluster cowcode)
est store l1fe
qui logit logit_purge firstgen milsec, cluster(cowcode)
est store l2
qui clogit logit_purge firstgen milsec, group(cowcode) vce(cluster cowcode)
est store l2fe
qui logit logit_purge firstgen milsec exitage, cluster(cowcode)
est store l3
qui clogit logit_purge firstgen milsec exitage, group(cowcode) vce(cluster cowcode)
est store l3fe
qui logit logit_purge firstgen milsec exitage exittenure, cluster(cowcode)
est store l4
qui clogit logit_purge firstgen milsec exitage exittenure, group(cowcode) vce(cluster cowcode)
est store l4fe
qui logit logit_purge firstgen milsec exitage exittenure military monarchy personal, cluster(cowcode)
est store l5
qui clogit logit_purge firstgen milsec exitage exittenure military monarchy personal, group(cowcode) vce(cluster cowcode)
est store l5fe

coefplot (l1, offset(.43) mlabels(firstgen=12 "Base models") xscale(range(-.5 2.5)) mcolor(black) ciopts(lcolor(black))) (l1fe, offset(.41) mcolor(gray) ciopts(lcolor(gray))) (l2, offset(.23) mlabels(firstgen=12 "Military control") mcolor(black) ciopts(lcolor(black))) (l2fe, offset(.21) mcolor(gray) ciopts(lcolor(gray))) (l3, offset(.03) mlabels(firstgen=12 "Military and age controls") mcolor(black) ciopts(lcolor(black))) (l3fe, offset(.01) mcolor(gray) ciopts(lcolor(gray))) (l4, offset(-.21) mlabels(firstgen=12 "Military, age, and tenure controls") mcolor(black) ciopts(lcolor(black))) (l4fe, offset(-.23) mcolor(gray) ciopts(lcolor(gray))) (l5, offset(-.41) mlabels(firstgen=12 "Military, age, tenure, and regime controls") mcolor(black) ciopts(lcolor(black))) (l5fe, offset(-.43) mcolor(gray) ciopts(lcolor(gray))), ///
	keep(firstgen) ///
	levels(95) ///
	msym(o) ///
	title("", size(tiny)) ///
	xlabel(-.5(.5)2.5) xline(0, lpattern(dash)) ///
	yscale(off) ///
	grid(glcolor(gs15)) mfcolor(white) scheme(s1mono) ///
	legend(order(2 "Logit models" 4 "Country FE logit models")) legend(size(small) region(lstyle(none))) ///
	ysize(2) ///
	xsize(1.0) ///
	note("{it:First generation} coefficients with 95% confidence intervals" "{it:N} ranges from 495-572" "For the full results, see Table A1 in the Appendix", size(small) pos(6)) ///
	name(logit_plot, replace)



***************************************************************************
*** In text: Substantive effects of logit models: observed values approach
***************************************************************************
qui logit logit_purge firstgen, cluster(cowcode)
qui gen px1_1 = normal(_b[_cons] + _b[firstgen]*1) if e(sample)
qui gen px1_0 = normal(_b[_cons] + _b[firstgen]*0) if e(sample)
qui gen effectx1 = px1_1-px1_0
sum px1_1 px1_0 effectx1
qui drop px1_1 px1_0 effectx1

qui logit logit_purge firstgen milsec, cluster(cowcode)
qui gen px1_1 = normal(_b[_cons] + _b[firstgen]*1 + _b[milsec]*milsec) if e(sample)
qui gen px1_0 = normal(_b[_cons] + _b[firstgen]*0 + _b[milsec]*milsec) if e(sample)
qui gen effectx1 = px1_1-px1_0
sum px1_1 px1_0 effectx1
qui drop px1_1 px1_0 effectx1

qui logit logit_purge firstgen milsec exitage, cluster(cowcode)
qui gen px1_1 = normal(_b[_cons] + _b[firstgen]*1 + _b[milsec]*milsec + _b[exitage]*exitage) if e(sample)
qui gen px1_0 = normal(_b[_cons] + _b[firstgen]*0 + _b[milsec]*milsec + _b[exitage]*exitage) if e(sample)
qui gen effectx1 = px1_1-px1_0
sum px1_1 px1_0 effectx1
qui drop px1_1 px1_0 effectx1

qui logit logit_purge firstgen milsec exitage exittenure, cluster(cowcode)
qui gen px1_1 = normal(_b[_cons] + _b[firstgen]*1 + _b[milsec]*milsec + _b[exitage]*exitage + _b[exittenure]*exittenure) if e(sample)
qui gen px1_0 = normal(_b[_cons] + _b[firstgen]*0 + _b[milsec]*milsec + _b[exitage]*exitage + _b[exittenure]*exittenure) if e(sample)
qui gen effectx1 = px1_1-px1_0
sum px1_1 px1_0 effectx1
qui drop px1_1 px1_0 effectx1

qui logit logit_purge firstgen milsec exitage exittenure military monarchy personal, cluster(cowcode)
qui gen px1_1 = normal(_b[_cons] + _b[firstgen]*1 + _b[milsec]*milsec + _b[exitage]*exitage + _b[exittenure]*exittenure + _b[military]*military + _b[monarchy]*monarchy + _b[personal]*personal) if e(sample)
qui gen px1_0 = normal(_b[_cons] + _b[firstgen]*0 + _b[milsec]*milsec + _b[exitage]*exitage + _b[exittenure]*exittenure + _b[military]*military + _b[monarchy]*monarchy + _b[personal]*personal) if e(sample)
qui gen effectx1 = px1_1-px1_0
sum px1_1 px1_0 effectx1
qui drop px1_1 px1_0 effectx1



******************************************
*** Figure 2: ML models of purge outcomes
******************************************
qui mlogit ml_purge firstgen if ml_purge!=0, cluster(cowcode) base(4)
est store m1
qui mlogit ml_purge firstgen milsec if ml_purge!=0, cluster(cowcode) base(4)
est store m2
qui mlogit ml_purge firstgen milsec exitage if ml_purge!=0, cluster(cowcode) base(4)
est store m3
qui mlogit ml_purge firstgen milsec exitage exittenure if ml_purge!=0, cluster(cowcode) base(4)
est store m4
qui mlogit ml_purge firstgen milsec exitage exittenure military monarchy personal if ml_purge!=0, cluster(cowcode) base(4)
est store m5

coefplot (m1, mlabels(firstgen=12 "Base models") mcolor(black) ciopts(lcolor(black))) (m2, mlabels(firstgen=12 "Military control") mcolor(black) ciopts(lcolor(black))) (m3, mlabels(firstgen=12 "Military and age controls") mcolor(black) ciopts(lcolor(black))) (m4, mlabels(firstgen=12 "Military, age, and tenure controls") mcolor(black) ciopts(lcolor(black))) (m5, mlabels(firstgen=12 "Military, age, tenure, and regime controls") mcolor(black) ciopts(lcolor(black))), ///
	keep(*:firstgen) ///
	levels(95) ///
	msym(o) ///
	title("", size(tiny)) ///
	xlabel(-6(2)4) xline(0, lpattern(dash)) ///
	coeflabel(1.ml_purge = "No punishment" 2.ml_purge = "Exile" 3.ml_purge = "Execution") ///
	yscale(off) ///
	grid(glcolor(gs15)) mfcolor(white) scheme(s1mono) ///
	ysize(2) ///
	xsize(1.0) ///
	note("{it:First generation} coefficients with 95% confidence intervals" "{it:N} ranges from 90-91" "For the full results, see Table A2 in the Appendix" "Base category is {it:Incarceration}", size(small) pos(6)) ///
	legend(off) ///
	name(mlogit_imprison, replace)
	
	

******************************************
*** Figure 3: ML models of purge outcomes
******************************************
qui mlogit ml_purge firstgen if ml_purge!=0, cluster(cowcode) base(3)
est store m1
qui mlogit ml_purge firstgen milsec if ml_purge!=0, cluster(cowcode) base(3)
est store m2
qui mlogit ml_purge firstgen milsec exitage if ml_purge!=0, cluster(cowcode) base(3)
est store m3
qui mlogit ml_purge firstgen milsec exitage exittenure if ml_purge!=0, cluster(cowcode) base(3)
est store m4
qui mlogit ml_purge firstgen milsec exitage exittenure military monarchy personal if ml_purge!=0, cluster(cowcode) base(3)
est store m5

coefplot (m1, mlabels(firstgen=12 "Base models") mcolor(black) ciopts(lcolor(black))) (m2, mlabels(firstgen=12 "Military control") mcolor(black) ciopts(lcolor(black))) (m3, mlabels(firstgen=12 "Military and age controls") mcolor(black) ciopts(lcolor(black))) (m4, mlabels(firstgen=12 "Military, age, and tenure controls") mcolor(black) ciopts(lcolor(black))) (m5, mlabels(firstgen=12 "Military, age, tenure, and regime controls") mcolor(black) ciopts(lcolor(black))), ///
	keep(*:firstgen) ///
	levels(95) ///
	msym(o) ///
	title("", size(tiny)) ///
	xlabel(-2(2)8) xline(0, lpattern(dash)) ///
	coeflabels(1.ml_purge = "No punishment" 2.ml_purge = "Exile" 4.ml_purge = "Incarceration") ///
	yscale(off) ///
	grid(glcolor(gs15)) mfcolor(white) scheme(s1mono) ///
	ysize(2) ///
	xsize(1.0) ///
	note("{it:First generation} coefficients with 95% confidence intervals" "{it:N} ranges from 90-91" "For the full results, see Table A2 in the Appendix" "Base category is {it:Execution}", size(small) pos(6)) ///
	legend(off) ///
	name(mlogit_execute, replace)

	

**********************************************************************
*** In text: Substantive effects of multinomial logit models: Clarify
**********************************************************************
set seed 987654321
estsimp mlogit ml_purge firstgen if ml_purge!=0, cluster(cowcode) base(4)
setx firstgen 0
simqi, pr
setx firstgen 1
simqi, pr
drop b1-b6

estsimp mlogit ml_purge firstgen milsec if ml_purge!=0, cluster(cowcode) base(4)
setx firstgen 0 milsec 0
simqi, pr
setx firstgen 1 milsec 0
simqi, pr
drop b1-b9

estsimp mlogit ml_purge firstgen milsec exitage if ml_purge!=0, cluster(cowcode) base(4)
setx firstgen 0 milsec 0 exitage mean
simqi, pr
setx firstgen 1 milsec 0 exitage mean
simqi, pr
drop b1-b12



**********************************************************************************
*** Substantive effects of multinomial logit models: percentages from simulations
**********************************************************************************
estsimp mlogit ml_purge firstgen milsec exitage if ml_purge!=0, cluster(cowcode) base(4)
setx firstgen 0 milsec 0 exitage mean
simqi, genpv(Pred_Sim1)
tab Pred_Sim1, m
setx firstgen 1 milsec 0 exitage mean
simqi, genpv(Pred_Sim2)
tab Pred_Sim2, m
drop b1-b12


*************************************
*** Table A1: Determinants of purges
*************************************
logit logit_purge firstgen, cluster(cowcode)
clogit logit_purge firstgen, group(cowcode) vce(cluster cowcode)
logit logit_purge firstgen milsec, cluster(cowcode)
clogit logit_purge firstgen milsec, group(cowcode) vce(cluster cowcode)
logit logit_purge firstgen milsec exitage, cluster(cowcode)
clogit logit_purge firstgen milsec exitage, group(cowcode) vce(cluster cowcode)
logit logit_purge firstgen milsec exitage exittenure, cluster(cowcode)
clogit logit_purge firstgen milsec exitage exittenure, group(cowcode) vce(cluster cowcode)
logit logit_purge firstgen milsec exitage exittenure military monarchy personal, cluster(cowcode)
clogit logit_purge firstgen milsec exitage exittenure military monarchy personal, group(cowcode) vce(cluster cowcode)



******************************************
*** Table A2: ML models of purge outcomes
******************************************
mlogit ml_purge firstgen if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitage if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitage exittenure if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitage exittenure military monarchy personal if ml_purge!=0, cluster(cowcode) base(4)



**************************************
*** Table A3: Heckman selection model
**************************************
gen incarceration=.
replace incarceration=0 if ml_purge!=0
replace incarceration=1 if ml_purge==4
heckprob incarceration firstgen milsec exitage exittenure military monarchy personal, cluster(cowcode) sel(logit_purge=firstgen milsec exitage exittenure military monarchy personal)



*****************************************************************
*** Table A4: Controlling for elite participation in failed coup
*****************************************************************
clogit logit_purge firstgen exitpostcoup, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen milsec exitpostcoup, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen milsec exitage exitpostcoup, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen milsec exitage exittenure exitpostcoup, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen milsec exitage exittenure military monarchy personal exitpostcoup, group(cowcode) vce(cluster cowcode)



*****************************************************************
*** Table A5: Controlling for elite participation in failed coup
*****************************************************************
mlogit ml_purge firstgen exitpostcoup if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitpostcoup if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitage exitpostcoup if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitage exittenure exitpostcoup if ml_purge!=0, cluster(cowcode) base(4)
mlogit ml_purge firstgen milsec exitage exittenure military monarchy exitpostcoup personal if ml_purge!=0, cluster(cowcode) base(4)



***********************************
*** Table A6: leader fixed effects
***********************************
clogit logit_purge firstgen, group(exitldrno) vce(cluster exitldrno)
clogit logit_purge firstgen milsec, group(exitldrno) vce(cluster exitldrno)
clogit logit_purge firstgen milsec exitage, group(exitldrno) vce(cluster exitldrno)
clogit logit_purge firstgen milsec exitage exittenure, group(exitldrno) vce(cluster exitldrno)
clogit logit_purge firstgen milsec exitage exittenure military monarchy personal, group(exitldrno) vce(cluster exitldrno)



**************************************************
*** Table A7: first generation purges across time
**************************************************
gen interaction = firstgen*exittenure
clogit logit_purge firstgen exittenure interaction, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen exittenure interaction milsec, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen exittenure interaction milsec exitage, group(cowcode) vce(cluster cowcode)
clogit logit_purge firstgen exittenure interaction milsec exitage military monarchy personal, group(cowcode) vce(cluster cowcode)



****************************************************************
*** Table A8 and Figure A1: estimating H1 with a survival model
****************************************************************
stset exittenure, failure(logit_purge)
streg firstgen, dist(weib) cluster(cowcode)
streg firstgen milsec, dist(weib) cluster(cowcode)
streg firstgen milsec exitage, dist(weib) cluster(cowcode)
streg firstgen milsec exitage military monarchy personal, dist(weib) cluster(cowcode)

sts graph, by(firstgen) ci scheme(plotplain) legend(order(5 "Non-first generation elite" 6 "First generation elite") cols(2) pos(6)) xtitle("") xla(minmax, labcolor(white)) xla(, tlength(0)) title("") ytitle("Kaplan-Meier""survival estimates") name(survival, replace)
kdensity exittenure, scheme(plotplain) xtitle("Tenure in ruling coalition (years)") ytitle("Density""function") title("") fysize(25) note("") name(tenure_hist, replace)
graph combine survival tenure_hist, cols(1) xcommon graphregion(color(white)) name(survival_robust, replace) note("{it:Notes}: Shaded areas in top panel represent 95% confidence intervals.""Graph estimated based on Model 4 in Table A9.", size(vsmall))



***********************************************************************************
*** Table A9: H1 and accounting for revolutionary regimes (Lachapelle et al. 2020)
***********************************************************************************
gen revolutionary=0
replace revolutionary=1 if state=="Albania" | state=="China" | state=="Mozambique" | state=="Soviet Union"
logit logit_purge firstgen revolutionary, cluster(cowcode)
logit logit_purge firstgen revolutionary milsec, cluster(cowcode)
logit logit_purge firstgen revolutionary milsec exitage, cluster(cowcode)
logit logit_purge firstgen revolutionary milsec exitage exittenure, cluster(cowcode)
logit logit_purge firstgen revolutionary milsec exitage exittenure military monarchy personal, cluster(cowcode)



*******************************************
*** Robustness test: dropping each country
*******************************************
levelsof cowcode, local(levels)
foreach l of local levels {
	logit logit_purge firstgen milsec exitage exittenure military monarchy personal if cowcode!=`l', cluster(cowcode)
	clogit logit_purge firstgen milsec exitage exittenure military monarchy personal if cowcode!=`l', group(cowcode) vce(cluster cowcode)
	mlogit ml_purge firstgen milsec exitage if ml_purge!=0 & cowcode!=`l', cluster(cowcode) base(4)
}



***********************************************
*** Robustness test: omitting each regime type
***********************************************
local regime "military monarchy personal party"
foreach r of local regime {
	clogit logit_purge firstgen milsec exitage exittenure military monarchy personal if `r'==0, group(cowcode) vce(cluster cowcode)
	mlogit ml_purge firstgen milsec exitage exittenure military monarchy personal if ml_purge!=0 & `r'==0, cluster(cowcode) base(4)
}


