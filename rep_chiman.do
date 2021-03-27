clear
set more off
set mem 100m
set matsize 800

use data
keep if info==0
cd "C:\Users\chiman\Dropbox\dev_field\econ270b\replication_ps2\112386-V1"

*** III. Experimental Results

*TABLE 2: SUMMARY STATISTICS OF SESSIONS
estpost sum econ class earning  political math verbal number qvoteresult qinitialpay qmodpay if round ==1
esttab . using "table2.csv", cells("count mean sd min max")

*TABLE 3: DETERMINANTS OF VOTING
eststo clear
reg votemod coop_0 if round==1
eststo model1
reg votemod ocoop_0 if round==1
eststo model2
reg votemod class if round==1
eststo model3
reg votemod number if round==1
eststo model4
reg votemod verbal if round==1
eststo model5
reg votemod  math if round==1
eststo model6
reg votemod econ if round==1
eststo model7
reg votemod political if round==1
eststo model8
reg votemod coop_0 ocoop_0 class number verbal math econ political if round==1
eststo model9

// eststo raw_spec
esttab * using "table3.csv", title("Voting determinats") b(%9.3f) se(%9.3f) ///
	 keep(*) stats(N r2, fmt(%12.0f %12.3f)) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	nomtitles ///
	obslast number label replace nobaselevels ///
	nonotes

*TABLE 3: DETERMINANTS OF VOTING (ROBUST)
eststo clear
reg votemod coop_0 if round==1, r
eststo model1
reg votemod ocoop_0 if round==1, r
eststo model2
reg votemod class if round==1, r
eststo model3
reg votemod number if round==1, r
eststo model4
reg votemod verbal if round==1, r
eststo model5
reg votemod  math if round==1, r
eststo model6
reg votemod econ if round==1, r
eststo model7
reg votemod political if round==1, r
eststo model8
reg votemod coop_0 ocoop_0 class number verbal math econ political if round==1, r
eststo model9

// eststo raw_spec
esttab * using "table3_robust.csv", title("Voting determinats") b(%9.3f) se(%9.3f) ///
	 keep(*) stats(N r2, fmt(%12.0f %12.3f)) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	nomtitles ///
	obslast number label replace nobaselevels ///
	nonotes

*TABLE 4: THE EFFECT OF DEMOCRACY -INDIVIDUAL LEVEL DATA
use data, clear
keep if info==0
keep if inrange(round,10,11)
keep votemod votestage id coop round

*Panel A
table votemod votestage if round==11, c(count id)

// row total; 129, 147
table votemod if round==11, c(count id)

// column total; 72, 80, 64, 60
table votestage if round==11, c(count id)

*Panel B
table votemod votestage if round==10 , c(mean coop)
table votestage if round==10 , c(mean coop)
*Panel C
table votemod votestage if round==11, c(mean coop)
table votestage if round==11, c(mean coop)


/*
total 54.72 (difference in the last row between the first two columns in Panel C) = 
selection 4.27
endogenous treatment effect 50.45
	exogenous 36
	endogeneity premium 14.45
// vary both subject type composition and treatment
TE = ((17/72)41.18 + (55/72)81.82) - ((55/80)14.55 + (25/80)24) = 54.72.
	38 ppl
	g(N|Endo,Mod)C(N|Endo,Mod) + g(Y|Endo,Mod)C(Y|Endo,Mod) - g(N|Endo,Not)C(N|Endo,Not) - g(Y|Endo,Not)C(Y|Endo,Not)

// vary only subject type composition; fix treatment as before (Endo,Not)
SE = (17/72 - 55/80)14.55 + (55/72 - 25/80)24 = 4.27
	(g(N|Endo,Mod) - g(N|Endo,Not))*C(N|Endo,Not) + (g(Y|Endo,Mod) - g(Y|Endo,Not))*C(Y|Endo,Not)

// fix subject type composition; vary only treatment (endogenous)
EndoTrE = (17/72)(41.18 - 14.55) + (55/72)(81.82 - 24) = 50.45
	g(N|Endo,Mod)[C(N|Endo,Mod)-C(N|Endo,Not)] + g(Y|Endo,Mod)[C(Y|Endo,Mod)-C(Y|Endo,Not)]

// fix subject type composition; vary only treatment (exogenous)
ExoTrE = (17/72)(41.94 - 3.85) + (55/72)(57.58 - 23.53) = 36
	g(N|Endo,Mod)[C(N|Exo,Mod)-C(N|Exo,Not)] + g(Y|Endo,Mod)[C(Y|Exo,Mod)-C(Y|Exo,Not)]
*/

// Figure
*FIGURE 3: COOPERATION BY ROUND, VOTE STAGE AND INDIVIDUAL VOTE
use data, clear
gen groupmodc=coop if votestage==1
gen groupnotc=coop if votestage==2
gen compmodc=coop if votestage==3
gen compnotc=coop if votestage==4

collapse groupmodc groupnotc compmodc compnotc if info==0, by (round votemod)

*graph by treatment
twoway (line groupmodc round if votemod==0, title(Did not vote for modification) graphregion(color(white)) xline(10.5) xtitle("Round") xscale(range(1 20)) xlabel(#20) ytitle("Cooperation") yscale(range(0 1)) ylabel(#6) scale(.6) ) (line groupnotc round if votemod==0, lpattern(dot)) (line compmodc round if votemod==0, lpattern(dash)) (line compnotc round if votemod==0, lpattern(dash_dot) legend(label(1 "EndoMod") label(2 "EndoNot") label(3 "ExoMod") label(4 "ExoNot"))) , saving(graphvm0,replace)
twoway (line groupmodc round if votemod==1, title(Voted for modification) graphregion(color(white)) xline(10.5) xtitle("Round") xscale(range(1 20)) xlabel(#20) ytitle("Cooperation") yscale(range(0 1)) ylabel(#6) scale(.6) ) (line groupnotc round if votemod==1, lpattern(dot)) (line compmodc round if votemod==1, lpattern(dash)) (line compnotc round if votemod==1, lpattern(dash_dot) legend(label(1 "EndoMod") label(2 "EndoNot") label(3 "ExoMod") label(4 "ExoNot"))) , saving(graphvm1,replace)

graph combine "graphvm1" "graphvm0" , graphregion(color(white)) saving(graphvs,replace)
graph export "graph3.pdf", replace

