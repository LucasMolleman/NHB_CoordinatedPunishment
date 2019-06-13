*-------------------------------------------------------------------
* THIS ANALYSIS HAS BEEN CONDUCTED WITH STATA VERSION 15.1
*-------------------------------------------------------------------

cd "c:\Dropbox\ACADEMIA\RESEARCH\Peer effects in punishment\06 Writing\Data\"

* Load Dataset
use "data_NHB.dta", clear


*-------------------------------------------------------------------
* Descriptive Statistics Main Study
*-------------------------------------------------------------------
* Cooperation rate
tab cooperate if rerun==0

* Unconditional punishment rate
tab unconditional_punishment if rerun==0
table cooperate target_cooperate if rerun==0, c(mean unconditional_punishment) format(%9.3f)

* Conditional punishment rate
su conditional_other_nopun conditional_other_pun if rerun==0

* McNemar Test
tab conditional_other_nopun conditional_other_pun  if rerun==0
mcci 1054 134 75 73


*-------------------------------------------------------------------
* Table 1: Logistic regression
*-------------------------------------------------------------------
use "data_NHB.dta", clear

* Only keep punisher
drop if punisher==0

* Reshape data from wide to long
rename conditional_other_nopun conditional_other_pun0  
rename conditional_other_pun   conditional_other_pun1 

reshape long conditional_other_pun, i(id) j(otherpun)

* Model (1)
xi: logit conditional_other_pun i.otherpun*unconditional_punishment if rerun==0, cl(id) 
eststo m1
* Wald-test for joint effect
test _Iotherpun_1  + _IothXuncon_1  = 0

* Model (2)
xi: logit conditional_other_pun i.otherpun*target_cooperate if rerun==0, cl(id) 
eststo m2

* Show Regression Output
esttab m1 m2,  star(* 0.05 ** 0.01 *** 0.001) b(%9.3f) se(%9.3f) varwidth(25) modelwidth(10)




*-------------------------------------------------------------------
* Distribution of Punishment types Main Experiment
*-------------------------------------------------------------------
use "data_NHB.dta", clear

* Show distribution of punishment types
tab pun_type if rerun==0
* Only consider those who punishe at least once in the strategy method
tab pun_type if NP==0 & rerun==0

* Test whether distribution of punishment types differ across unconditional punishers and unconditional non-punishers
tab unconditional_punishment pun_type if NP==0 & rerun==0, row chi

tab unconditional_punishment IP if NP==0 & rerun==0, row chi
tab unconditional_punishment CP if NP==0 & rerun==0, row chi
tab unconditional_punishment ACP if NP==0 & rerun==0, row chi





*-------------------------------------------------------------------
* Punishment resuls Replication Study & Pooled Data
*-------------------------------------------------------------------
use "data_NHB.dta", clear

* Test whether distribution of punishment types differ across unconditional punishers and unconditional non-punishers
tab unconditional_punishment pun_type if NP==0 & rerun==1, row chi

* Test whether distribution of punishment types differ across main & replication study
bys unconditional_punishment: tab rerun  pun_type if NP==0, row chi

* Test whether distribution of punishment types differ across the two orders in replication study
bys unconditional_punishment: tab order  pun_type if rerun==1 & NP==0, row chi

* Overall Distribution of Punishment Types pooling across studies
tab pun_type if NP==0
tab unconditional_punishment pun_type if NP==0, row chi


*-------------------------------------------------------------------
* Punishment resuls Replication Study & Pooled Data
*-------------------------------------------------------------------
use "data_NHB.dta", clear



*-------------------------------------------------------------------
* Anger Analysis
*-------------------------------------------------------------------
use "data_NHB.dta", clear

* Levels of expressed anger by own and other's cooperation decision
table cooperate target_cooperate, c(mean anger) format(%9.1f)

* Levels of expressed anger by unconditional punishment decision
bys unconditional_punishment: su anger
ranksum anger, by(unconditional_punishment)

* Does the distribution of expressed anger differ by punishment type?
kwallis anger, by(pun_type)
table pun_type, c(mean anger) format(%9.1f)

* Pairwise comparisons of expressed anger between punishment types
ranksum anger if (pun_type==1 | pun_type==2), by(pun_type)
ranksum anger if (pun_type==1 | pun_type==3), by(pun_type)
ranksum anger if (pun_type==1 | pun_type==4), by(pun_type)
ranksum anger if (pun_type==2 | pun_type==3), by(pun_type)
ranksum anger if (pun_type==2 | pun_type==4), by(pun_type)
ranksum anger if (pun_type==3 | pun_type==4), by(pun_type)



*-------------------------------------------------------------------
* Analysis of Motivations underlying Preferences for Punishment
*-------------------------------------------------------------------
use "data_NHB.dta", clear

* Is there a differences in the distribution of responses by punishment type?
kwallis unsure, by(pun_type) 
kwallis unsure_appropriate, by(pun_type) 
kwallis earnless, by(pun_type)  
kwallis letdown, by(pun_type)  
kwallis reduce_not_much, by(pun_type)  
kwallis revenge, by(pun_type)  
kwallis own_earnings, by(pun_type) 

* Testing specific motivations of specific punishment types
ranksum revenge if pun_type!=1, by(IP)

ranksum reduce_not_much if pun_type!=1, by(ACP)

ranksum earnless if pun_type!=1, by(CP)
ranksum letdown if pun_type!=1, by(CP)
ranksum unsure if pun_type!=1, by(CP)
ranksum unsure_appropriate if pun_type!=1, by(CP)


*-------------------------------------------------------------------
* LINK BETWEEN PREFERENCES FOR PUNISHMENT & PREFERENCES FOR COOPERATION
*-------------------------------------------------------------------
use "data_NHB.dta", clear

* Distribution of Punishment Types in Subsample
tab pun_type if coop_type!=.

* Distribution of Cooperation Types
tab coop_type

* Correlation of Cooperation & Punishment Types
tab coop_type pun_type, row chi

* Are Conditional Cooperators more likely to Punish Conditionally than the other cooperation types
tab CP CC, row chi

* Differences in Distribution of Punishment Types between Free riders and Conditional Cooperators?
tab FR pun_type if coop_type!=3, row chi


*----------------------------------------------------------------------------------------------------------------
* CREATE FIGURE 2
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear
drop if pun_type==1
drop if rerun==1


* Generate X-Axis
gen xaxis = .
replace xaxis = 1.0 if pun_type==2 
replace xaxis = 2.0 if pun_type==3 
replace xaxis = 3.0 if pun_type==4 

* Needed for Panel A
egen mean_ip = mean(IP)
egen mean_cp = mean(CP)
egen mean_acp = mean(ACP)

gen mean_type = .
replace mean_type = mean_ip if pun_type==2
replace mean_type = mean_cp if pun_type==3
replace mean_type = mean_acp if pun_type==4

* Needed for Panel B and C
bys unconditional_punishment: egen mean_ip2 = mean(IP)
bys unconditional_punishment: egen mean_cp2 = mean(CP)
bys unconditional_punishment: egen mean_acp2 = mean(ACP)

gen mean_type2 = .
replace mean_type2 = mean_ip2 if pun_type==2
replace mean_type2 = mean_cp2 if pun_type==3
replace mean_type2 = mean_acp2 if pun_type==4


* Panel A: All Punishers	
tw (bar mean_type xaxis if pun_type==2, barwidth(0.8)  fcolor(maroon)  lcolor(black) lwidth(medthick) ) ///
(bar mean_type xaxis if pun_type==3, barwidth(0.8)  fcolor(green)  lcolor(black) lwidth(medthick) ) ///
(bar mean_type xaxis if pun_type==4, barwidth(0.8)  fcolor(gs6)  lcolor(black) lwidth(medthick) ), ///
	xlabel(1 `""Independent" "punisher" "{it:(n = 73)}" "' 2 `""Coord." "punisher" "{it:(n = 134)}""' 3.0 `""Anti-coord." "punisher" "({it:n = 75)}""',  labgap(1) labsize(small) nogrid)  ///
	ylabel(0 .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60"   , nogrid angle(0) gmin gmax labsize(medlarge)) yscale(range(0 0.65)) 	///
	ytitle("Percent", size(medlarge)) xtitle("") xscale(range(0.45 3.55))  legend(off) plotregion(margin(b = 0)  lcolor(black)) ///
	text(0.13 1.0 "25.9%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.24 2.0 "47.5%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.13 3.0 "26.6%", box fcolor(white) lcolor(white) size(medsmall)) ///
	 name(fig_2a, replace) subtitle("All punishers", box bexpand alignment(middle) fcolor(sand*0.6)) fxsize(55)	text(0.645 0.5 "{bf:a}", size(medlarge))
	 

* Panel B: Unconditional Non-Punishers
tw (bar mean_type2 xaxis if pun_type==2 & unconditional_punishment==0, barwidth(0.8)  fcolor(maroon)  lcolor(black) lwidth(medthick)  ) ///
(bar mean_type2 xaxis if pun_type==3 & unconditional_punishment==0, barwidth(0.8)  fcolor(green)  lcolor(black) lwidth(medthick) ) ///
(bar mean_type2 xaxis if pun_type==4 & unconditional_punishment==0, barwidth(0.8)  fcolor(gs6)  lcolor(black) lwidth(medthick) ), ///
	xlabel(1 `""Independent" "punisher" "{it:(n = 20)}""' 2 `""Coord." "punisher" "{it:(n = 106)}""' 3.0 `""Anti-coord." "punisher" "{it:(n = 44)}""',  labgap(1) labsize(small) nogrid)  ///
	ylabel("", nogrid) yscale(range(0 0.65))  plotregion(margin(b = 0)  lcolor(black))	///
	ytitle("") xtitle("") xscale(range(0.45 3.55))   ///
	name(fig_2b, replace) legend(off) subtitle("Unconditional non-punishers", box bexpand alignment(middle) fcolor(sand*0.6)) ///
	text(0.06 1.0 "11.8%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.31 2.0 "62.4%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.13 3.0 "25.9%", box fcolor(white) lcolor(white) size(medsmall)) text(0.645 0.5 "{bf:b}", size(medlarge))
	

* Panel C: Unconditional Punishers	
tw (bar mean_type2 xaxis if pun_type==2 & unconditional_punishment==1, barwidth(0.8)  fcolor(maroon)  lcolor(black) lwidth(medthick)  ) ///
(bar mean_type2 xaxis if pun_type==3 & unconditional_punishment==1, barwidth(0.8)  fcolor(green)  lcolor(black) lwidth(medthick) ) ///
(bar mean_type2 xaxis if pun_type==4 & unconditional_punishment==1, barwidth(0.8)  fcolor(gs6)  lcolor(black) lwidth(medthick)  ), ///
	xlabel(1 `""Independent" "punisher" "{it:(n = 53)}""' 2 `""Coord." "punisher" "{it:(n = 28)}""' 3.0 `""Anti-coord." "punisher" "{it:(n = 31)}""',  labgap(1) labsize(small) nogrid)  ///
	ylabel("", nogrid) yscale(range(0 0.65))  plotregion(margin(b = 0)  lcolor(black))	///
	ytitle("") xtitle("") xscale(range(0.45 3.55))   ///
	name(fig_2c, replace) legend(off) subtitle("Unconditional punishers", box bexpand alignment(middle) fcolor(sand*0.6)) ///
	text(0.24 1.0 "47.3%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.125 2.0 "25.0%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.14 3.0 "27.7%", box fcolor(white) lcolor(white) size(medsmall)) 	text(0.645 0.5 "{bf:c}", size(medlarge))
	
	
* Combine Panels & Save Figure
graph combine fig_2a fig_2b fig_2c, rows(1)  imargin(vsmall)
graph export "fig2.pdf", replace



*----------------------------------------------------------------------------------------------------------------
* CREATE FIGURE 3: DISTRIBUTION OF ANGER LEVELS
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear

* Panel A
tw (hist anger if pun_type==2, discrete percent fcolor(maroon) plotregion(lcolor(black)) lcolor(black)), ///
subtitle("Independent " "punishers (n = 188)", box bexpand alignment(middle) fcolor(sand*0.6))  ///
xlabel(1(1)7, nogrid labsize(medlarge)) xtitle("") ytitle("", size(medlarge)) ylabel(0(10)60, nogrid angle(0) labsize(medlarge)) ///
yscale(range(0 60)) fxsize(40) name(one, replace) ///
text(56 6.05 "Mean: 3.8""Median: 4""SD: 2.1", size(medsmall) box just(left) al(middle) color(none) fcolor(none) lcolor(none) linegap(1.5) margin(small) ) 

* Panel B
tw hist anger if pun_type==3, discrete percent color(green)  plotregion(lcolor(black)) lcolor(black) ///
subtitle("Coordinated" "punishers (n = 245)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
xlabel(1(1)7, nogrid labsize(medlarge)) xtitle("") ytitle("") ylabel("", nogrid) yscale(range(0 60)) name(two, replace) ///
text(56 6.05 "Mean: 3.1""Median: 3""SD: 1.8", size(medsmall) box just(left) al(middle) color(none) fcolor(none) lcolor(none) linegap(1.5) margin(small) ) 

* Panel C
tw hist anger if pun_type==4, discrete percent fcolor(gs6) plotregion(lcolor(black)) lcolor(black) ///
subtitle("Anti-coordinated" "punishers (n = 146)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
xlabel(1(1)7, nogrid labsize(medlarge)) xtitle("") ytitle("") ylabel("", nogrid ) yscale(range(0 60))  name(three, replace) ///
text(56 6.05 "Mean: 3.2""Median: 3""SD: 1.8", size(medsmall) box just(left) al(middle) color(none) fcolor(none) lcolor(none) linegap(1.5) margin(small) ) 

* Panel D
tw hist anger if pun_type==1, discrete percent fcolor(white)  plotregion(lcolor(black)) lcolor(black) ///
subtitle("Non-punishers" "(n = 2285)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
xlabel(1(1)7, nogrid labsize(medlarge)) xtitle("") ytitle("") ylabel("", nogrid) yscale(range(0 60))  name(four, replace) ///
text(56 6.05 "Mean: 2.1""Median: 1""SD: 1.6", size(medsmall) box just(left) al(middle) color(none) fcolor(none) lcolor(none) linegap(1.5) margin(small) ) 

* Combine Panels & Save Graph
graph combine one two three four, rows(1) b1title("Anger") b2title("{it:(1 = not angry at all, 7 = very angry)}", size(small)) imargin(vsmall)
graph export "fig3.pdf", replace





*----------------------------------------------------------------------------------------------------------------
* CREATE FIGURE 4: CORRELATION OF COOPERATION & PUNSIHMENT TYPES
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear
drop if coop_type==.

bys coop_type: egen mean_np = mean(NP)
bys coop_type: egen mean_ip = mean(IP)
bys coop_type: egen mean_cp = mean(CP)
bys coop_type: egen mean_acp = mean(ACP)

gen mean_ip_cp =  mean_ip + mean_cp
gen mean_ip_cp_acp =  mean_ip + mean_cp + mean_acp
gen mean_ip_cp_acp_np = mean_ip + mean_cp + mean_acp + mean_np


tw ///
(bar mean_ip_cp_acp_np coop_type, barwidth(0.8)  fcolor(white)  lcolor(black) lwidth(medthick) ) ///
(bar mean_ip_cp_acp coop_type, barwidth(0.8)  fcolor(gs6)  lcolor(black) lwidth(medthick) ) ///
(bar mean_ip_cp coop_type, barwidth(0.8)  fcolor(green)  lcolor(black) lwidth(medthick) ) ///
(bar mean_ip coop_type, barwidth(0.8)  fcolor(maroon)  lcolor(black) lwidth(medthick) ) , ///
	xlabel(1 `""Free" "riders" "{it:(n = 105)}""' 2 `""Conditional" "cooperators" "{it:(n = 214)}" "'  3 `""Others""({it:n = 62)}""',  labgap(1) labsize(medsmall) )  ///
	ylabel(0  .2 "20"  .4 "40"  .6 "60" .8 "80"  1 "100"  , angle(0) nogrid labsize(medsmall)) yscale(range(0 1)) 	///
	ytitle("Percent", size(medsmall)) xtitle("") xscale(range(0.45 3.55))   plotregion(margin(zero) ) ///
	text(0.105 1.0 "21.0%", box fcolor(white) lcolor(white)) text(0.35 1 "27.6%", box fcolor(white) lcolor(white)) text(0.55 1 "13.3%", box fcolor(white) lcolor(white)) text(0.81 1 "38.1%") ///
	text(0.12 2.0 "23.8%", box fcolor(white) lcolor(white) ) text(0.35 2 "21.5%", box fcolor(white) lcolor(white)) text(0.54 2 "17.8%", box fcolor(white) lcolor(white)) text(0.81 2 "36.9%") ///
	text(0.08 3.0 "16.1%", box fcolor(white) lcolor(white))  text(0.26 3 "21.0%", box fcolor(white) lcolor(white)) text(0.44 3 "14.5%", box fcolor(white) lcolor(white)) text(0.76 3 "48.4%") ///
	name(fig4, replace) legend(order( 4 "Independent punisher" 3 "Coordinated punisher" 2 "Anti-coordinated punisher" 1 "Non-punisher") pos(12) rows(2))

* Save Graph
graph export "fig4.pdf", replace



*----------------------------------------------------------------------------------------------------------------
* ANALYSIS SUPPLEMENTARY INFORMATION
*----------------------------------------------------------------------------------------------------------------



*----------------------------------------------------------------------------------------------------------------
* CREATE SUPPLEMENTARY FIGURE 1: DISTRIBUTION OF PUNSIHMENT TYPES BY OWN AND OTHER'S COOPERATION DECISION
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear

gen case = .
replace case = 1 if (cooperate == 1 & target_cooperate == 0) & rerun==0 & punisher==1 & pun_type!=1
replace case = 2 if (cooperate == 1 & target_cooperate == 1) & rerun==0 & punisher==1 & pun_type!=1
replace case = 3 if (cooperate == 0 & target_cooperate == 0) & rerun==0 & punisher==1 & pun_type!=1
replace case = 4 if (cooperate == 0 & target_cooperate == 1) & rerun==0 & punisher==1 & pun_type!=1

* Distribution of Punishment Types by Cooperation Scenario
bys case: tab pun_type if rerun==0 & punisher==1 & pun_type!=1

* Test fraction of independent punishers across Cooperation Scenarios
gen coop_defect = case == 1

tab coop_defect pun_type  if rerun==0 & punisher==1 & pun_type!=1, row chi
tab coop_defect IP  if rerun==0 & punisher==1 & pun_type!=1, row chi
tab coop_defect CP  if rerun==0 & punisher==1 & pun_type!=1, row chi

* Distribution of Punishment Types across the remaining three treatments
tab case pun_type  if rerun==0 & punisher==1 & pun_type!=1 & case!=1, row chi


* Needed For Graph
tab case
label define case 1 "CD (n=105)" 2 "CC (n=34)" 3 "DD (n=83)" 4 "DC (n=60)"
label values case case

bys case: egen mean_ip_case = mean(IP)
bys case: egen mean_cp_case = mean(CP)
bys case: egen mean_acp_case = mean(ACP)

gen mean_type_case = .
replace mean_type_case = mean_ip_case if pun_type==2
replace mean_type_case = mean_cp_case if pun_type==3
replace mean_type_case = mean_acp_case if pun_type==4

gen xaxis = pun_type - 1

* Panel A
tw (bar mean_type_case xaxis if pun_type==2 & case==1, barwidth(0.8)  fcolor(maroon)  lcolor(black)  ) ///
(bar mean_type_case xaxis if pun_type==3 & case==1, barwidth(0.8)  fcolor(green)  lcolor(black) ) ///
(bar mean_type_case xaxis if pun_type==4 & case==1, barwidth(0.8)  fcolor(gs6)  lcolor(black)  ), ///
	xlabel("", noticks labgap(1) labsize(small)) yscale(range(0 0.6)) fxsize(75) fysize(44)  ///
	ylabel(0 .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60"   , angle(0) nogrid labsize(medlarge)) 	///
	ytitle("", size(medlarge)) xtitle("") xscale(range(0.45 3.55)) subtitle("CD (n=105)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
	name(sup_fig1a, replace) legend(off) text(0.59 0.5 "{bf:a}", size(medlarge))  plotregion(margin(b = 0) lcolor(black)) ///
	text(0.19 1 "38.1%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.19 2 "38.1%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.12 3 "23.8%", box fcolor(white) lcolor(white) size(medsmall)) 

* Panel B
tw (bar mean_type_case xaxis if pun_type==2 & case==2, barwidth(0.8)  fcolor(maroon)  lcolor(black)  ) ///
(bar mean_type_case xaxis if pun_type==3 & case==2, barwidth(0.8)  fcolor(green)  lcolor(black) ) ///
(bar mean_type_case xaxis if pun_type==4 & case==2, barwidth(0.8)  fcolor(gs6)  lcolor(black)  ), ///
	xlabel("", noticks labgap(1) labsize(small)) yscale(range(0 0.6)) fysize(44)   ///
	ylabel("", angle(0) nogrid labsize(medlarge))  plotregion(margin(b = 0) lcolor(black)) 	///
	ytitle("", size(medlarge)) xtitle("") xscale(range(0.45 3.55)) subtitle("CC (n=34)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
	name(sup_fig1b, replace) legend(off) text(0.59 0.5 "{bf:b}", size(medlarge)) ///
	text(0.085 1.0 "17.3%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.265 2.0 "53.0%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.15 3.0 "29.4%", box fcolor(white) lcolor(white) size(medsmall)) 

* Panel C
tw (bar mean_type_case xaxis if pun_type==2 & case==3, barwidth(0.8)  fcolor(maroon)  lcolor(black)  ) ///
(bar mean_type_case xaxis if pun_type==3 & case==3, barwidth(0.8)  fcolor(green)  lcolor(black) ) ///
(bar mean_type_case xaxis if pun_type==4 & case==3, barwidth(0.8)  fcolor(gs6)  lcolor(black)  ), fxsize(75) ///
	xlabel(1 `""Independent" "punisher""' 2 `""Coordinated" "punisher""' 3.0 `""Anti-coordinated" "punisher""',  labgap(1) nogrid labsize(medsmall))  ///
	ylabel(0 .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60"   , angle(0) nogrid labsize(medlarge))  yscale(range(0 0.6))  	///
	ytitle("", size(medlarge)) xtitle("") xscale(range(0.45 3.55)) subtitle("DD (n=83)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
	name(sup_fig1c, replace) legend(off) text(0.59 0.5 "{bf:c}", size(medlarge))  plotregion(margin(b = 0) lcolor(black)) ///
	text(0.095 1.0 "19.3%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.27 2.0 "54.2%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.13 3.0 "26.5%", box fcolor(white) lcolor(white) size(medsmall)) 

* Panel D	
tw (bar mean_type_case xaxis if pun_type==2 & case==4, barwidth(0.8)  fcolor(maroon)  lcolor(black)  ) ///
(bar mean_type_case xaxis if pun_type==3 & case==4, barwidth(0.8)  fcolor(green)  lcolor(black) ) ///
(bar mean_type_case xaxis if pun_type==4 & case==4, barwidth(0.8)  fcolor(gs6)  lcolor(black)  ), ///
	xlabel(1 `""Independent" "punisher""' 2 `""Coordinated" "punisher""' 3.0 `""Anti-coordinated" "punisher""', nogrid  labgap(1) labsize(medsmall))  ///
	ylabel("", angle(0) nogrid labsize(medlarge))  yscale(range(0 0.6))  plotregion(margin(b = 0) lcolor(black))  	///
	ytitle("", size(medlarge)) xtitle("") xscale(range(0.45 3.55)) subtitle("DC (n=60)", box bexpand alignment(middle) fcolor(sand*0.6))   ///
	name(sup_fig1d, replace) legend(off) text(0.59 0.5 "{bf:d}", size(medlarge)) ///
	text(0.09 1.0 "18.3%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.26 2.0 "51.7%", box fcolor(white) lcolor(white) size(medsmall)) ///
	text(0.15 3.0 "30.0%", box fcolor(white) lcolor(white) size(medsmall))

* Combine Panels & Save Graph
graph combine sup_fig1a sup_fig1b sup_fig1c sup_fig1d, imargin(vsmall)
graph export "sup_fig1.pdf", replace






*----------------------------------------------------------------------------------------------------------------
* CREATE SUPPLEMENTARY FIGURE 2: DISTRIBUTION OF PUNSIHMENT TYPES IN REPLICATION STUDY
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear

* Needed for Graph
drop if rerun==0
drop if pun_type==1

tab unconditional_punishment pun_type, row

bys unconditional_punishment: egen mean_ip = mean(IP) if punisher==1
bys unconditional_punishment: egen mean_cp = mean(CP) if punisher==1
bys unconditional_punishment: egen mean_acp = mean(ACP) if punisher==1

gen mean_type = .
replace mean_type = mean_ip if pun_type==2
replace mean_type = mean_cp if pun_type==3
replace mean_type = mean_acp if pun_type==4


* Generate X-Axis
gen xaxis = pun_type - 1


* Panel A
tw (bar mean_type xaxis if pun_type==2 & unconditional_punishment==0, barwidth(0.8)  fcolor(maroon)  lcolor(black) lwidth(medthick)  ) ///
(bar mean_type xaxis if pun_type==3 & unconditional_punishment==0, barwidth(0.8)  fcolor(green)  lcolor(black) lwidth(medthick) ) ///
(bar mean_type xaxis if pun_type==4 & unconditional_punishment==0, barwidth(0.8)  fcolor(gs6)  lcolor(black) lwidth(medthick) ), ///
	xlabel(1 `""Independent" "punisher" "{it:(n = 23)}""' 2 `""Coord." "punisher" "{it:(n = 68)}""' 3.0 `""Anti-coord." "punisher" "{it:(n = 32)}""',  labgap(1) labsize(medsmall) nogrid)  ///
	ylabel(0 .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60"   , nogrid angle(0) gmin gmax labsize(medlarge)) yscale(range(0 0.65))  	text(0.645 0.5 "{bf:a}", size(medlarge)) ///
	ytitle("Percent", size(medlarge)) xtitle("") xscale(range(0.45 3.55)) plotregion(margin(b = 0) lcolor(black))   ///
	name(sup_fig1a, replace) legend(off) subtitle("Unconditional non-punishers", box bexpand alignment(middle) fcolor(sand*0.6)) fxsize(77) ///
	text(0.09 1.0 "18.7%", box fcolor(white) lcolor(white) size(medium)) ///
	text(0.28 2.0 "55.3%", box fcolor(white) lcolor(white) size(medium)) ///
	text(0.13 3.0 "26.0%", box fcolor(white) lcolor(white) size(medium)) 
	
* Panel B
tw (bar mean_type xaxis if pun_type==2 & unconditional_punishment==1, barwidth(0.8)  fcolor(maroon)  lcolor(black) lwidth(medthick)  ) ///
(bar mean_type xaxis if pun_type==3 & unconditional_punishment==1, barwidth(0.8)  fcolor(green)  lcolor(black) lwidth(medthick) ) ///
(bar mean_type xaxis if pun_type==4 & unconditional_punishment==1, barwidth(0.8)  fcolor(gs6)  lcolor(black) lwidth(medthick)  ), ///
	xlabel(1 `""Independent" "punisher" "{it:(n = 92)}""' 2 `""Coord." "punisher" "{it:(n = 47)}""' 3.0 `""Anti-coord." "punisher" "{it:(n = 40)}""',  labgap(1) labsize(medsmall) nogrid)  ///
	ylabel("", nogrid) yscale(range(0 0.65))  plotregion(margin(b = 0) lcolor(black))	///
	ytitle("") xtitle("") xscale(range(0.45 3.55)) text(0.645 0.5 "{bf:b}", size(medlarge))  ///
	name(sup_fig1b, replace) legend(off) subtitle("Unconditional punishers", box bexpand alignment(middle) fcolor(sand*0.6)) ///
	text(0.25 1.0 "51.4%", box fcolor(white) lcolor(white) size(medium)) ///
	text(0.13 2.0 "26.3%", box fcolor(white) lcolor(white) size(medium)) ///
	text(0.11 3.0 "22.3%", box fcolor(white) lcolor(white) size(medium)) 	
	
* Combine Panels & SaveGraph
graph combine sup_fig1a sup_fig1b , rows(1)  imargin(vsmall)
graph export "sup_fig2.pdf", replace




*----------------------------------------------------------------------------------------------------------------
* CREATE SUPPLEMENTARY TABLE 1: 
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear

* Only keep punisher
drop if punisher==0

* Reshape data from wide to long
rename conditional_other_nopun conditional_other_pun0  
rename conditional_other_pun   conditional_other_pun1 

reshape long conditional_other_pun, i(id) j(otherpun)

* Model (1)
xi: logit conditional_other_pun i.otherpun*unconditional_punishment if rerun==1, cl(id) 
eststo m1

* Model (2)
xi: logit conditional_other_pun i.otherpun*target_cooperate if rerun==1, cl(id) 
eststo m2

* Model (3)
xi: logit conditional_other_pun i.otherpun*unconditional_punishment, cl(id) 
eststo m3

* Model (4)
xi: logit conditional_other_pun i.otherpun*target_cooperate, cl(id) 
eststo m4

* Show Regression Output
esttab m1 m2 m3 m4,  star(* 0.05 ** 0.01 *** 0.001) b(%9.3f) se(%9.3f) varwidth(25) modelwidth(10)




*----------------------------------------------------------------------------------------------------------------
* CREATE SUPPLEMENTARY TABLE 2: 
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear

* Only keep punisher
drop if punisher==0

gen case = .
replace case = 1 if (cooperate == 1 & target_cooperate == 0) 
replace case = 2 if (cooperate == 1 & target_cooperate == 1) 
replace case = 3 if (cooperate == 0 & target_cooperate == 1) 
replace case = 4 if (cooperate == 0 & target_cooperate == 0) 

gen coop_defect = case==1
gen coop_coop = case==2
gen defect_coop = case==3


* Reshape data from wide to long
rename conditional_other_nopun conditional_other_pun0  
rename conditional_other_pun   conditional_other_pun1 

reshape long conditional_other_pun, i(id) j(otherpun)

gen iakt_coop_defect = coop_defect * otherpun
gen iakt_coop_coop = coop_coop * otherpun
gen iakt_defect_coop = defect_coop * otherpun

* Model (1)
logit conditional_other_pun otherpun coop_defect defect_coop coop_coop  iakt_coop_defect iakt_defect_coop iakt_coop_coop  if rerun==0, cl(id)
eststo m1

* Model (2)
logit conditional_other_pun otherpun coop_defect defect_coop coop_coop  iakt_coop_defect iakt_defect_coop iakt_coop_coop if rerun==1, cl(id)
eststo m2

* Model (3)
logit conditional_other_pun otherpun coop_defect defect_coop coop_coop  iakt_coop_defect iakt_defect_coop iakt_coop_coop, cl(id)
eststo m3

* Show Regression Output
esttab m1 m2 m3,  star(* 0.05 ** 0.01 *** 0.001) b(%9.3f) p(%9.3f) varwidth(25) modelwidth(10)





*----------------------------------------------------------------------------------------------------------------
* CREATE SUPPLEMENTARY TABLE 3: Motivations of Punisher Types
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear


tabstat unsure unsure_appropriate letdown revenge earnless own_earnings reduce_not_much, by(pun_type) statistics(mean median sd) longstub format(%9.1f) labelwidth(25) varwidth(20) nototal


* Test Non-punishers against the rest
ranksum unsure, by(NP)
ranksum unsure_appropriate, by(NP)
ranksum letdown, by(NP)
ranksum revenge, by(NP)
ranksum own_earnings, by(NP)
ranksum reduce_not_much, by(NP)


*----------------------------------------------------------------------------------------------------------------
* CREATE SUPPLEMENTARY TABLE 4: Reciprocity and Personality Traits
*----------------------------------------------------------------------------------------------------------------
use "data_NHB.dta", clear


tabstat pos_reciprocity neg_reciprocity risk extraversion agreeableness conscientiousness emo_stability openness, by(pun_type) statistics(mean median sd) longstub format(%9.1f) labelwidth(25) varwidth(20) nototal


* Test whether distribution of responses differs across punishment types
kwallis pos_reciprocity, by(pun_type)
kwallis neg_reciprocity, by(pun_type)

ranksum neg_reciprocity, by(NP)
kwallis neg_reciprocity if pun_type!=1, by(pun_type)

* Risk Attitudes
ranksum risk, by(NP)
kwallis risk if pun_type!=1, by(pun_type)

* Big 5
kwallis extraversion, by(pun_type)
gen IP_CP = (pun_type==2 | pun_type==3)
ranksum extraversion, by(IP_CP)
ranksum extraversion if (pun_type==1 | pun_type==4), by(pun_type)
ranksum extraversion if (pun_type==2 | pun_type==3), by(pun_type)

kwallis agreeableness, by(pun_type)
kwallis conscientiousness, by(pun_type)
kwallis emo_stability, by(pun_type)
kwallis openness, by(pun_type)


















