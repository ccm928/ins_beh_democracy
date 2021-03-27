*do file for "Institutions and Behavior: Experimental Evidence on the Effect of Democracy"

capture log close
log using table.log, replace

set more 1
clear
set mem 100m
set matsize 800

use data
keep if info==0

*** III. Experimental Results

*TABLE 2: SUMMARY STATISTICS OF SESSIONS
sum econ class earning  political math verbal number qvoteresult qinitialpay qmodpay if round ==1

*"The average level of cooperation was 18% in the first part of the experiment... with a maximum of 31.9% in round 1 and a minimum of 6.9% in round 10"
tab coop if postvote==0
table round if postvote==0, c(mean coop)

** A. Results from the voting stage

*"Of the 276 subjects, 147 (53.26%) voted to modify payoffs and 129 (46.74%) voted not to modify payoffs in the second part of the experiment."
tab votemod if round==1

*TABLE 3: DETERMINANTS OF VOTING
reg votemod coop_0 if round==1
outreg coop_0 using table3, replace title(Table 3: Voting determinats) bracket 3aster se bdec(3)
reg votemod ocoop_0 if round==1
outreg ocoop_0 using table3, append bracket 3aster se bdec(3)
reg votemod class if round==1
outreg class using table3, append bracket 3aster se bdec(3)
reg votemod number if round==1
outreg number using table3, append bracket 3aster se bdec(3)
reg votemod verbal if round==1
outreg verbal using table3, append bracket 3aster se bdec(3)
reg votemod  math if round==1
outreg math using table3, append bracket 3aster se bdec(3)
reg votemod econ if round==1
outreg econ using table3, append bracket 3aster se bdec(3)
reg votemod political if round==1
outreg political using table3, append bracket 3aster se bdec(3)
reg votemod coop_0 ocoop_0 class number verbal math econ political if round==1
outreg coop_0 ocoop_0 class number verbal math econ political using table3, append bracket 3aster se bdec(3)

*"There is evidence that voting decisions are independent within groups. ....As Figure 2 shows there is little difference between the two distributions.
*In fact the difference is not statistically significant (p-value=0.68)." See footnote.

tab voteshare if round==1

*S=69*(.3043-.26526)

capture postclose xxx
postfile xxx float s using pedro, replace
local i=0
quietly while `i'<10000 {
local i=`i'+1
drop _all
set obs 69

gen a0=.047726
gen a1=.26526
gen a2=.637079
gen a3=.9195935
gen a4=1
gen x=uniform()
gen y=.
replace y=0 if x<=a0
replace y=1 if x<=a1 & y==.
replace y=2 if x<=a2 & y==.
replace y=3 if x<=a3 & y==.
replace y=4 if x<=a4 & y==.
gen yd1=y==0 
gen yd2=y==1
gen yd3=y==2
gen yd4=y==3

collapse (mean) yd* a0-a3
gen cyd1=yd1
gen cyd2=yd1+yd2
gen cyd3=yd1+yd2+yd3
gen cyd4=yd1+yd2+yd3+yd4
gen s=abs(cyd1-a0)
replace s=abs(cyd2-a1) if abs(cyd2-a1)>abs(cyd1-a0)
replace s=abs(cyd3-a2) if abs(cyd3-a2)>abs(cyd2-a1)
replace s=abs(cyd4-a3) if abs(cyd4-a3)>abs(cyd3-a2)
sum s
local s1=s[1]
post xxx (`s1')
display "post `pedro' (`s1')"
}
postclose xxx

use pedro, clear
gen simulateds=s*69
tab simulateds

*"A random-effects analysis of voting does not reject that there are no random-effects at the group level suggesting that voting decisions are independent within groups (p-value=0.368)"
use data, clear
keep if info==0
xtreg votemod if round==1, i(group)
xttest0

*TABLE 4: THE EFFECT OF DEMOCRACY -INDIVIDUAL LEVEL DATA
*Panel A
table votemod votestage if round==11, c(count id)
*Panel B
table votemod votestage if round==10 , c(mean coop)
table votestage if round==10 , c(mean coop)
*Panel C
table votemod votestage if round==11, c(mean coop)
table votestage if round==11, c(mean coop)

**B. Exogenous versus endogenous treatment effect: individual level analysis

*"There is little difference in the cooperation rates in round 10... there are no statistical differences in cooperation (p-value 0.88)"
reg coop groupmodn groupnotn compmodn compnotn groupmody groupnoty compmody compnoty if round==10, noconstant
test groupmodn=groupnotn=compmodn=compnotn=groupmody=groupnoty=compmody=compnoty

*TABLE 5: THE EFFECT OF DEMOCRACY -INDIVIDUAL LEVEL DATA
reg coop groupmod groupnot compmod compnot if round==11, noconstant
outreg using table5, replace title(Table 5) bracket 3aster se bdec(3)
test groupnot=compnot
test groupmod=compmod
test groupmod=groupnot
test compmod=compnot

reg coop groupmodn groupnotn compmodn compnotn groupmody groupnoty compmody compnoty if round==11, noconstant
outreg using table5, append bracket 3aster se bdec(3)
test groupnotn=compnotn
test groupmodn=compmodn
test groupmodn=groupnotn
test compmodn=compnotn
test groupnoty=compnoty
test groupmody=compmody
test groupmody=groupnoty
test compmody=compnoty

reg coop groupmodn groupnotn compmodn compnotn groupmody groupnoty compmody compnoty coop_0 ocoop_0 if round==11, noconstant
outreg using table5, append bracket 3aster se bdec(3)
test groupnotn=compnotn
test groupmodn=compmodn
test groupmodn=groupnotn
test compmodn=compnotn
test groupnoty=compnoty
test groupmody=compmody
test groupmody=groupnoty
test compmody=compnoty

reg coop groupmodn groupnotn compmodn compnotn groupmody groupnoty compmody compnoty if round==11 & qvoteresult==100, noconstant
outreg using table5, append bracket 3aster se bdec(3)
test groupnotn=compnotn
test groupmodn=compmodn
test groupmodn=groupnotn
test compmodn=compnotn
test groupnoty=compnoty
test groupmody=compmody
test groupmody=groupnoty
test compmody=compnoty

reg coop groupmodn groupnotn compmodn compnotn groupmody groupnoty compmody compnoty coop_0 ocoop_0 if round==11 & qvoteresult==100, noconstant
outreg using table5, append bracket 3aster se bdec(3)
test groupnotn=compnotn
test groupmodn=compmodn
test groupmodn=groupnotn
test compmodn=compnotn
test groupnoty=compnoty
test groupmody=compmody
test groupmody=groupnoty
test compmody=compnoty

*TABLE 6: THE EFFECT OF DEMOCRACY -INDIVIDUAL LEVEL DATA - ALL ROUNDS
use data, clear
keep if modified==1 & info==0

gen groupy=(computer==0 & votemod==1)
gen groupn=(computer==0 & votemod==0)
gen compy=(computer==1 & votemod==1)
gen compn=(computer==1 & votemod==0)

capture prog drop estim
prog def estim , rclass

reg coop groupy groupn compy compn if round==11, noc
local b1gy=_b[groupy]
local b1gn=_b[groupn]
local b1cy=_b[compy]
local b1cn=_b[compn]

reg coop groupy groupn compy compn coop11 ocoop11 if round==12, noc
local b2gy=_b[groupy]
local b2gn=_b[groupn]
local b2cy=_b[compy]
local b2cn=_b[compn]
local r2_1=_b[coop11]
local f2_1=_b[ocoop11]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 if round==13, noc
local b3gy=_b[groupy]
local b3gn=_b[groupn]
local b3cy=_b[compy]
local b3cn=_b[compn]
local r3_1=_b[coop11]
local f3_1=_b[ocoop11]
local r3_2=_b[coop12]
local f3_2=_b[ocoop12]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 if round==14, noc
local b4gy=_b[groupy]
local b4gn=_b[groupn]
local b4cy=_b[compy]
local b4cn=_b[compn]
local r4_1=_b[coop11]
local f4_1=_b[ocoop11]
local r4_2=_b[coop12]
local f4_2=_b[ocoop12]
local r4_3=_b[coop13]
local f4_3=_b[ocoop13]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 if round==15, noc
local b5gy=_b[groupy]
local b5gn=_b[groupn]
local b5cy=_b[compy]
local b5cn=_b[compn]
local r5_1=_b[coop11]
local f5_1=_b[ocoop11]
local r5_2=_b[coop12]
local f5_2=_b[ocoop12]
local r5_3=_b[coop13]
local f5_3=_b[ocoop13]
local r5_4=_b[coop14]
local f5_4=_b[ocoop14]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 if round==16, noc
local b6gy=_b[groupy]
local b6gn=_b[groupn]
local b6cy=_b[compy]
local b6cn=_b[compn]
local r6_1=_b[coop11]
local f6_1=_b[ocoop11]
local r6_2=_b[coop12]
local f6_2=_b[ocoop12]
local r6_3=_b[coop13]
local f6_3=_b[ocoop13]
local r6_4=_b[coop14]
local f6_4=_b[ocoop14]
local r6_5=_b[coop15]
local f6_5=_b[ocoop15]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 if round==17, noc
local b7gy=_b[groupy]
local b7gn=_b[groupn]
local b7cy=_b[compy]
local b7cn=_b[compn]
local r7_1=_b[coop11]
local f7_1=_b[ocoop11]
local r7_2=_b[coop12]
local f7_2=_b[ocoop12]
local r7_3=_b[coop13]
local f7_3=_b[ocoop13]
local r7_4=_b[coop14]
local f7_4=_b[ocoop14]
local r7_5=_b[coop15]
local f7_5=_b[ocoop15]
local r7_6=_b[coop16]
local f7_6=_b[ocoop16]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 coop17 ocoop17 if round==18, noc
local b8gy=_b[groupy]
local b8gn=_b[groupn]
local b8cy=_b[compy]
local b8cn=_b[compn]
local r8_1=_b[coop11]
local f8_1=_b[ocoop11]
local r8_2=_b[coop12]
local f8_2=_b[ocoop12]
local r8_3=_b[coop13]
local f8_3=_b[ocoop13]
local r8_4=_b[coop14]
local f8_4=_b[ocoop14]
local r8_5=_b[coop15]
local f8_5=_b[ocoop15]
local r8_6=_b[coop16]
local f8_6=_b[ocoop16]
local r8_7=_b[coop17]
local f8_7=_b[ocoop17]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 coop17 ocoop17 coop18 ocoop18 if round==19, noc
local b9gy=_b[groupy]
local b9gn=_b[groupn]
local b9cy=_b[compy]
local b9cn=_b[compn]
local r9_1=_b[coop11]
local f9_1=_b[ocoop11]
local r9_2=_b[coop12]
local f9_2=_b[ocoop12]
local r9_3=_b[coop13]
local f9_3=_b[ocoop13]
local r9_4=_b[coop14]
local f9_4=_b[ocoop14]
local r9_5=_b[coop15]
local f9_5=_b[ocoop15]
local r9_6=_b[coop16]
local f9_6=_b[ocoop16]
local r9_7=_b[coop17]
local f9_7=_b[ocoop17]
local r9_8=_b[coop18]
local f9_8=_b[ocoop18]

reg coop groupy groupn compy compn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 coop17 ocoop17 coop18 ocoop18 coop19 ocoop19 if round==20, noc
local b0gy=_b[groupy]
local b0gn=_b[groupn]
local b0cy=_b[compy]
local b0cn=_b[compn]
local r0_1=_b[coop11]
local f0_1=_b[ocoop11]
local r0_2=_b[coop12]
local f0_2=_b[ocoop12]
local r0_3=_b[coop13]
local f0_3=_b[ocoop13]
local r0_4=_b[coop14]
local f0_4=_b[ocoop14]
local r0_5=_b[coop15]
local f0_5=_b[ocoop15]
local r0_6=_b[coop16]
local f0_6=_b[ocoop16]
local r0_7=_b[coop17]
local f0_7=_b[ocoop17]
local r0_8=_b[coop18]
local f0_8=_b[ocoop18]
local r0_9=_b[coop19]
local f0_9=_b[ocoop19]

*types of partners under democracy by own vote
egen poyya = mean(ovotemod) if votemod==1 & computer==0
egen poyy = mean(poyya)
local poyy=poyy[1]
drop poyya poyy

egen poyna = mean(ovotemod) if votemod==0 & computer==0
egen poyn = mean(poyna)
local poyn=poyn[1]
drop poyna poyn

*calculate bs: diff between groupy and compy and groupn and compn
local by1 =`b1gy'-`b1cy'
local by2 =`b2gy'-`b2cy'
local by3 =`b3gy'-`b3cy'
local by4 =`b4gy'-`b4cy'
local by5 =`b5gy'-`b5cy'
local by6 =`b6gy'-`b6cy'
local by7 =`b7gy'-`b7cy'
local by8 =`b8gy'-`b8cy'
local by9 =`b9gy'-`b9cy'
local by0 =`b0gy'-`b0cy'
local bn1 =`b1gn'-`b1cn'
local bn2 =`b2gn'-`b2cn'
local bn3 =`b3gn'-`b3cn'
local bn4 =`b4gn'-`b4cn'
local bn5 =`b5gn'-`b5cn'
local bn6 =`b6gn'-`b6cn'
local bn7 =`b7gn'-`b7cn'
local bn8 =`b8gn'-`b8cn'
local bn9 =`b9gn'-`b9cn'
local bn0 =`b0gn'-`b0cn'

*calculate rhos
local ro2_1=`r2_1'

local ro3_2=`r3_2'
local ro3_1=`r3_1'+`ro3_2'*`r2_1'

local ro4_3=`r4_3'
local ro4_2=`r4_2'+`ro4_3'*`r3_2'
local ro4_1=`r4_1'+`ro4_2'*`r2_1'+`ro4_3'*`r3_1'

local ro5_4=`r5_4'
local ro5_3=`r5_3'+`ro5_4'*`r4_3'
local ro5_2=`r5_2'+`ro5_3'*`r3_2'+`ro5_4'*`r4_2'
local ro5_1=`r5_1'+`ro5_2'*`r2_1'+`ro5_3'*`r3_1'+`ro5_4'*`r4_1'

local ro6_5=`r6_5'
local ro6_4=`r6_4'+`ro6_5'*`r5_4'
local ro6_3=`r6_3'+`ro6_4'*`r4_3'+`ro6_5'*`r5_3'
local ro6_2=`r6_2'+`ro6_3'*`r3_2'+`ro6_4'*`r4_2'+`ro6_5'*`r5_2'
local ro6_1=`r6_1'+`ro6_2'*`r2_1'+`ro6_3'*`r3_1'+`ro6_4'*`r4_1'+`ro6_5'*`r5_1'

local ro7_6=`r7_6'
local ro7_5=`r7_5'+`ro7_6'*`r6_5'
local ro7_4=`r7_4'+`ro7_5'*`r5_4'+`ro7_6'*`r6_4'
local ro7_3=`r7_3'+`ro7_4'*`r4_3'+`ro7_5'*`r5_3'+`ro7_6'*`r6_3'
local ro7_2=`r7_2'+`ro7_3'*`r3_2'+`ro7_4'*`r4_2'+`ro7_5'*`r5_2'+`ro7_6'*`r6_2'
local ro7_1=`r7_1'+`ro7_2'*`r2_1'+`ro7_3'*`r3_1'+`ro7_4'*`r4_1'+`ro7_5'*`r5_1'+`ro7_6'*`r6_1'

local ro8_7=`r8_7'
local ro8_6=`r8_6'+`ro8_7'*`r7_6'
local ro8_5=`r8_5'+`ro8_6'*`r6_5'+`ro8_7'*`r7_5'
local ro8_4=`r8_4'+`ro8_5'*`r5_4'+`ro8_6'*`r6_4'+`ro8_7'*`r7_4'
local ro8_3=`r8_3'+`ro8_4'*`r4_3'+`ro8_5'*`r5_3'+`ro8_6'*`r6_3'+`ro8_7'*`r7_3'
local ro8_2=`r8_2'+`ro8_3'*`r3_2'+`ro8_4'*`r4_2'+`ro8_5'*`r5_2'+`ro8_6'*`r6_2'+`ro8_7'*`r7_2'
local ro8_1=`r8_1'+`ro8_2'*`r2_1'+`ro8_3'*`r3_1'+`ro8_4'*`r4_1'+`ro8_5'*`r5_1'+`ro8_6'*`r6_1'+`ro8_7'*`r7_1'

local ro9_8=`r9_8'
local ro9_7=`r9_7'+`ro9_8'*`r8_7'
local ro9_6=`r9_6'+`ro9_7'*`r7_6'+`ro9_8'*`r8_6'
local ro9_5=`r9_5'+`ro9_6'*`r6_5'+`ro9_7'*`r7_5'+`ro9_8'*`r8_5'
local ro9_4=`r9_4'+`ro9_5'*`r5_4'+`ro9_6'*`r6_4'+`ro9_7'*`r7_4'+`ro9_8'*`r8_4'
local ro9_3=`r9_3'+`ro9_4'*`r4_3'+`ro9_5'*`r5_3'+`ro9_6'*`r6_3'+`ro9_7'*`r7_3'+`ro9_8'*`r8_3'
local ro9_2=`r9_2'+`ro9_3'*`r3_2'+`ro9_4'*`r4_2'+`ro9_5'*`r5_2'+`ro9_6'*`r6_2'+`ro9_7'*`r7_2'+`ro9_8'*`r8_2'
local ro9_1=`r9_1'+`ro9_2'*`r2_1'+`ro9_3'*`r3_1'+`ro9_4'*`r4_1'+`ro9_5'*`r5_1'+`ro9_6'*`r6_1'+`ro9_7'*`r7_1'+`ro9_8'*`r8_1'

local ro0_9=`r0_9'
local ro0_8=`r0_8'+`ro0_9'*`r9_8'
local ro0_7=`r0_7'+`ro0_8'*`r8_7'+`ro0_9'*`r9_7'
local ro0_6=`r0_6'+`ro0_7'*`r7_6'+`ro0_8'*`r8_6'+`ro0_9'*`r9_6'
local ro0_5=`r0_5'+`ro0_6'*`r6_5'+`ro0_7'*`r7_5'+`ro0_8'*`r8_5'+`ro0_9'*`r9_5'
local ro0_4=`r0_4'+`ro0_5'*`r5_4'+`ro0_6'*`r6_4'+`ro0_7'*`r7_4'+`ro0_8'*`r8_4'+`ro0_9'*`r9_4'
local ro0_3=`r0_3'+`ro0_4'*`r4_3'+`ro0_5'*`r5_3'+`ro0_6'*`r6_3'+`ro0_7'*`r7_3'+`ro0_8'*`r8_3'+`ro0_9'*`r9_3'
local ro0_2=`r0_2'+`ro0_3'*`r3_2'+`ro0_4'*`r4_2'+`ro0_5'*`r5_2'+`ro0_6'*`r6_2'+`ro0_7'*`r7_2'+`ro0_8'*`r8_2'+`ro0_9'*`r9_2'
local ro0_1=`r0_1'+`ro0_2'*`r2_1'+`ro0_3'*`r3_1'+`ro0_4'*`r4_1'+`ro0_5'*`r5_1'+`ro0_6'*`r6_1'+`ro0_7'*`r7_1'+`ro0_8'*`r8_1'+`ro0_9'*`r9_1'

*calculate phis
local fi2_1=`f2_1'

local fi3_2=`f3_2'
local fi3_1=`f3_1'+`ro3_2'*`f2_1'

local fi4_3=`f4_3'
local fi4_2=`f4_2'+`ro4_3'*`f3_2'
local fi4_1=`f4_1'+`ro4_2'*`f2_1'+`ro4_3'*`f3_1'

local fi5_4=`f5_4'
local fi5_3=`f5_3'+`ro5_4'*`f4_3'
local fi5_2=`f5_2'+`ro5_3'*`f3_2'+`ro5_4'*`f4_2'
local fi5_1=`f5_1'+`ro5_2'*`f2_1'+`ro5_3'*`f3_1'+`ro5_4'*`f4_1'

local fi6_5=`f6_5'
local fi6_4=`f6_4'+`ro6_5'*`f5_4'
local fi6_3=`f6_3'+`ro6_4'*`f4_3'+`ro6_5'*`f5_3'
local fi6_2=`f6_2'+`ro6_3'*`f3_2'+`ro6_4'*`f4_2'+`ro6_5'*`f5_2'
local fi6_1=`f6_1'+`ro6_2'*`f2_1'+`ro6_3'*`f3_1'+`ro6_4'*`f4_1'+`ro6_5'*`f5_1'

local fi7_6=`f7_6'
local fi7_5=`f7_5'+`ro7_6'*`f6_5'
local fi7_4=`f7_4'+`ro7_5'*`f5_4'+`ro7_6'*`f6_4'
local fi7_3=`f7_3'+`ro7_4'*`f4_3'+`ro7_5'*`f5_3'+`ro7_6'*`f6_3'
local fi7_2=`f7_2'+`ro7_3'*`f3_2'+`ro7_4'*`f4_2'+`ro7_5'*`f5_2'+`ro7_6'*`f6_2'
local fi7_1=`f7_1'+`ro7_2'*`f2_1'+`ro7_3'*`f3_1'+`ro7_4'*`f4_1'+`ro7_5'*`f5_1'+`ro7_6'*`f6_1'

local fi8_7=`f8_7'
local fi8_6=`f8_6'+`ro8_7'*`f7_6'
local fi8_5=`f8_5'+`ro8_6'*`f6_5'+`ro8_7'*`f7_5'
local fi8_4=`f8_4'+`ro8_5'*`f5_4'+`ro8_6'*`f6_4'+`ro8_7'*`f7_4'
local fi8_3=`f8_3'+`ro8_4'*`f4_3'+`ro8_5'*`f5_3'+`ro8_6'*`f6_3'+`ro8_7'*`f7_3'
local fi8_2=`f8_2'+`ro8_3'*`f3_2'+`ro8_4'*`f4_2'+`ro8_5'*`f5_2'+`ro8_6'*`f6_2'+`ro8_7'*`f7_2'
local fi8_1=`f8_1'+`ro8_2'*`f2_1'+`ro8_3'*`f3_1'+`ro8_4'*`f4_1'+`ro8_5'*`f5_1'+`ro8_6'*`f6_1'+`ro8_7'*`f7_1'

local fi9_8=`f9_8'
local fi9_7=`f9_7'+`ro9_8'*`f8_7'
local fi9_6=`f9_6'+`ro9_7'*`f7_6'+`ro9_8'*`f8_6'
local fi9_5=`f9_5'+`ro9_6'*`f6_5'+`ro9_7'*`f7_5'+`ro9_8'*`f8_5'
local fi9_4=`f9_4'+`ro9_5'*`f5_4'+`ro9_6'*`f6_4'+`ro9_7'*`f7_4'+`ro9_8'*`f8_4'
local fi9_3=`f9_3'+`ro9_4'*`f4_3'+`ro9_5'*`f5_3'+`ro9_6'*`f6_3'+`ro9_7'*`f7_3'+`ro9_8'*`f8_3'
local fi9_2=`f9_2'+`ro9_3'*`f3_2'+`ro9_4'*`f4_2'+`ro9_5'*`f5_2'+`ro9_6'*`f6_2'+`ro9_7'*`f7_2'+`ro9_8'*`f8_2'
local fi9_1=`f9_1'+`ro9_2'*`f2_1'+`ro9_3'*`f3_1'+`ro9_4'*`f4_1'+`ro9_5'*`f5_1'+`ro9_6'*`f6_1'+`ro9_7'*`f7_1'+`ro9_8'*`f8_1'

local fi0_9=`f0_9'
local fi0_8=`f0_8'+`ro0_9'*`f9_8'
local fi0_7=`f0_7'+`ro0_8'*`f8_7'+`ro0_9'*`f9_7'
local fi0_6=`f0_6'+`ro0_7'*`f7_6'+`ro0_8'*`f8_6'+`ro0_9'*`f9_6'
local fi0_5=`f0_5'+`ro0_6'*`f6_5'+`ro0_7'*`f7_5'+`ro0_8'*`f8_5'+`ro0_9'*`f9_5'
local fi0_4=`f0_4'+`ro0_5'*`f5_4'+`ro0_6'*`f6_4'+`ro0_7'*`f7_4'+`ro0_8'*`f8_4'+`ro0_9'*`f9_4'
local fi0_3=`f0_3'+`ro0_4'*`f4_3'+`ro0_5'*`f5_3'+`ro0_6'*`f6_3'+`ro0_7'*`f7_3'+`ro0_8'*`f8_3'+`ro0_9'*`f9_3'
local fi0_2=`f0_2'+`ro0_3'*`f3_2'+`ro0_4'*`f4_2'+`ro0_5'*`f5_2'+`ro0_6'*`f6_2'+`ro0_7'*`f7_2'+`ro0_8'*`f8_2'+`ro0_9'*`f9_2'
local fi0_1=`f0_1'+`ro0_2'*`f2_1'+`ro0_3'*`f3_1'+`ro0_4'*`f4_1'+`ro0_5'*`f5_1'+`ro0_6'*`f6_1'+`ro0_7'*`f7_1'+`ro0_8'*`f8_1'+`ro0_9'*`f9_1'


*calculate direct democracy effect for yes and no people
local dey1 =`by1'
local dey2 =`by2'+`ro2_1'*(`by1')
local dey3 =`by3'+`ro3_1'*(`by1') +`ro3_2'*(`by2')
local dey4 =`by4'+`ro4_1'*(`by1') +`ro4_2'*(`by2') +`ro4_3'*(`by3')
local dey5 =`by5'+`ro5_1'*(`by1') +`ro5_2'*(`by2') +`ro5_3'*(`by3') +`ro5_4'*(`by4')
local dey6 =`by6'+`ro6_1'*(`by1') +`ro6_2'*(`by2') +`ro6_3'*(`by3') +`ro6_4'*(`by4') +`ro6_5'*(`by5')
local dey7 =`by7'+`ro7_1'*(`by1') +`ro7_2'*(`by2') +`ro7_3'*(`by3') +`ro7_4'*(`by4') +`ro7_5'*(`by5') +`ro7_6'*(`by6')
local dey8 =`by8'+`ro8_1'*(`by1') +`ro8_2'*(`by2') +`ro8_3'*(`by3') +`ro8_4'*(`by4') +`ro8_5'*(`by5') +`ro8_6'*(`by6') +`ro8_7'*(`by7')
local dey9 =`by9'+`ro9_1'*(`by1') +`ro9_2'*(`by2') +`ro9_3'*(`by3') +`ro9_4'*(`by4') +`ro9_5'*(`by5') +`ro9_6'*(`by6') +`ro9_7'*(`by7') +`ro9_8'*(`by8')
local dey0 =`by0'+`ro0_1'*(`by1') +`ro0_2'*(`by2') +`ro0_3'*(`by3') +`ro0_4'*(`by4') +`ro0_5'*(`by5') +`ro0_6'*(`by6') +`ro0_7'*(`by7') +`ro0_8'*(`by8') +`ro0_9'*(`by9')

local den1 =`bn1'
local den2 =`bn2'+`ro2_1'*(`bn1')
local den3 =`bn3'+`ro3_1'*(`bn1') +`ro3_2'*(`bn2')
local den4 =`bn4'+`ro4_1'*(`bn1') +`ro4_2'*(`bn2') +`ro4_3'*(`bn3')
local den5 =`bn5'+`ro5_1'*(`bn1') +`ro5_2'*(`bn2') +`ro5_3'*(`bn3') +`ro5_4'*(`bn4')
local den6 =`bn6'+`ro6_1'*(`bn1') +`ro6_2'*(`bn2') +`ro6_3'*(`bn3') +`ro6_4'*(`bn4') +`ro6_5'*(`bn5')
local den7 =`bn7'+`ro7_1'*(`bn1') +`ro7_2'*(`bn2') +`ro7_3'*(`bn3') +`ro7_4'*(`bn4') +`ro7_5'*(`bn5') +`ro7_6'*(`bn6')
local den8 =`bn8'+`ro8_1'*(`bn1') +`ro8_2'*(`bn2') +`ro8_3'*(`bn3') +`ro8_4'*(`bn4') +`ro8_5'*(`bn5') +`ro8_6'*(`bn6') +`ro8_7'*(`bn7')
local den9 =`bn9'+`ro9_1'*(`bn1') +`ro9_2'*(`bn2') +`ro9_3'*(`bn3') +`ro9_4'*(`bn4') +`ro9_5'*(`bn5') +`ro9_6'*(`bn6') +`ro9_7'*(`bn7') +`ro9_8'*(`bn8')
local den0 =`bn0'+`ro0_1'*(`bn1') +`ro0_2'*(`bn2') +`ro0_3'*(`bn3') +`ro0_4'*(`bn4') +`ro0_5'*(`bn5') +`ro0_6'*(`bn6') +`ro0_7'*(`bn7') +`ro0_8'*(`bn8') +`ro0_9'*(`bn9')

*calculate total democracy effect for yes and no people
local tey1 =`dey1'
local ten1 =`den1'

local tey2 =`dey2'+`fi2_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1')
local ten2 =`den2'+`fi2_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1')

local tey3 =`dey3'+`fi3_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi3_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2')
local ten3 =`den3'+`fi3_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi3_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2')

local tey4 =`dey4'+`fi4_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi4_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi4_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3')
local ten4 =`den4'+`fi4_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi4_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi4_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3')

local tey5 =`dey5'+`fi5_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi5_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi5_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi5_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4')
local ten5 =`den5'+`fi5_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi5_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi5_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi5_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4')

local tey6 =`dey6'+`fi6_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi6_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi6_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi6_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi6_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5')
local ten6 =`den6'+`fi6_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi6_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi6_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi6_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi6_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5')

local tey7 =`dey7'+`fi7_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi7_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi7_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi7_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi7_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi7_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6')
local ten7 =`den7'+`fi7_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi7_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi7_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi7_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi7_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi7_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6')

local tey8 =`dey8'+`fi8_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi8_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi8_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi8_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi8_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi8_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6') +`fi8_7'*(`poyy'*`tey7'+(1-`poyy')*`ten7') 
local ten8 =`den8'+`fi8_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi8_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi8_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi8_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi8_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi8_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6') +`fi8_7'*(`poyn'*`tey7'+(1-`poyn')*`ten7')

local tey9 =`dey9'+`fi9_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi9_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi9_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi9_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi9_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi9_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6') +`fi9_7'*(`poyy'*`tey7'+(1-`poyy')*`ten7') +`fi9_8'*(`poyy'*`tey8'+(1-`poyy')*`ten8')
local ten9 =`den9'+`fi9_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi9_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi9_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi9_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi9_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi9_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6') +`fi9_7'*(`poyn'*`tey7'+(1-`poyn')*`ten7') +`fi9_8'*(`poyn'*`tey8'+(1-`poyn')*`ten8')

local tey0 =`dey0'+`fi0_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi0_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi0_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi0_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi0_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi0_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6') +`fi0_7'*(`poyy'*`tey7'+(1-`poyy')*`ten7') +`fi0_8'*(`poyy'*`tey8'+(1-`poyy')*`ten8') +`fi0_9'*(`poyy'*`tey9'+(1-`poyy')*`ten9')
local ten0 =`den0'+`fi0_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi0_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi0_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi0_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi0_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi0_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6') +`fi0_7'*(`poyn'*`tey7'+(1-`poyn')*`ten7') +`fi0_8'*(`poyn'*`tey8'+(1-`poyn')*`ten8') +`fi0_9'*(`poyn'*`tey9'+(1-`poyn')*`ten9')

mat input coefs=(`tey1', `ten1', `tey2', `ten2', `tey3', `ten3', `tey4', `ten4', `tey5' ,`ten5', `tey6', `ten6', `tey7', `ten7', `tey8', `ten8', `tey9', `ten9', `tey0', `ten0')
return matrix c=coefs
end

local i=0
capture postclose jack 
postfile jack ty1 tn1 ty2 tn2 ty3 tn3 ty4 tn4 ty5 tn5 ty6 tn6 ty7 tn7 ty8 tn8 ty9 tn9 ty0 tn0 using jack , replace
egen igroup=group(group)
sort igroup
quietly while `i'<igroup[_N] {
local i = `i'+1
noisily display `i'
preserve
drop if igroup==`i'
estim
mat x=r(c)
post jack (x[1,1]) (x[1,2]) (x[1,3]) (x[1,4]) (x[1,5]) (x[1,6]) (x[1,7]) (x[1,8]) (x[1,9]) (x[1,10]) (x[1,11]) (x[1,12]) (x[1,13]) (x[1,14]) (x[1,15]) (x[1,16]) (x[1,17]) (x[1,18]) (x[1,19]) (x[1,20]) 
restore
}
postclose jack 

use jack, clear
foreach var of varlist t* {
quietly sum `var'
*displays total effect and t
display "`var' " r(mean) " " r(mean)/(r(sd)*sqrt(r(N)))
}

outsheet using table6, replace
*For test of equality of effects across rounds use the following matrix of variance and covariance
corr, cov `ty*'

*"Subjects who voted for modification are significantly more likely to say that the voting stage modified their behavior under endogenous modification than under exogenous modification (p-value less than 0.01)."
*q1=1 if said that votestage affected behavior

use data, clear
keep if info==0
reg q1 groupmodn groupnotn compmodn compnotn groupmody groupnoty compmody compnoty if round==11 & info==0, noconstant
test groupmody=compmody

*q3=1 if menting voting being counted/considered or not.
table votemod computer modified if round==11, c(mean q3) col scol


** D. Exogenous versus endogenous treatment effect: group level analysis

*TABLE 7: THE EFFECT OF DEMOCRACY - GROUP LEVEL DATA
*Panel A
table voteshare votestage if round==1, c(count id)
*Panel B
table voteshare votestage if postvote==0 , c(mean coop)
*Panel C
table voteshare votestage if postvote==1 , c(mean coop)

collapse coop coop_0 votestage voteshare math verbal econ number political class qvoteresult qinitialpay qmodpay groupmod groupnot compmod compnot modified computer, by (group postvote)

*"There is little difference in the cooperation rates of groups with vote share 2 in part 1... there are no statistical differences in cooperation across all four voting stage results (p-value 0.47)"
reg coop groupmod groupnot compmod compnot if postvote==0 & voteshare==2, noconstant
test groupmod=groupnot=compmod=compnot

*"If anything, the groups with exogenous modification (ExoMod) cooperated more in the first part of the experiment than those with endogenous modification (EndoMod) but this difference is not statistically significant (p-value 0.24)."
test groupmod=compmod

*TABLE 8: THE EFFECT OF DEMOCRACY - GROUP LEVEL DATA - Voteshare=2
reg coop groupmod groupnot compmod compnot  if postvote==1 & voteshare==2, noconstant
outreg using table8, replace title(Table 8) bracket 3aster se bdec(3)
test groupnot=compnot
test groupmod=compmod
test groupmod=groupnot
test compmod=compnot

reg coop groupmod groupnot compmod compnot coop_0 if postvote==1 & voteshare==2, noconstant
outreg using table8, append bracket 3aster se bdec(3)
test groupnot=compnot
test groupmod=compmod
test groupmod=groupnot
test compmod=compnot

reg coop groupmod groupnot compmod compnot if postvote==1 & voteshare==2 & qvoteresult==100, noconstant
outreg using table8, append bracket 3aster se bdec(3)
test groupnot=compnot
test groupmod=compmod
test groupmod=groupnot
test compmod=compnot

reg coop groupmod groupnot compmod compnot coop_0 if postvote==1 & voteshare==2 & qvoteresult==100, noconstant
outreg using table8, append bracket 3aster se bdec(3)
test groupnot=compnot
test groupmod=compmod
test groupmod=groupnot
test compmod=compnot

*"[O]f groups with voteshare 3, those under endogenous modification reach higher cooperation rates than those with exogenous modification...  However, this difference is not statistically significant."
reg coop groupmod compmod compnot if postvote==1 & voteshare==3, noconstant
test groupmod=compmod


*** IV. Democracy and information

** A. Information and sophistication

*TABLE 9: COOPERATION % IN ROUND 11 BY "SOPHISTICATION" - YES VOTERS
use data, clear
keep if info==0
table votemod if round==1, c(median math median number)
*For YES medians are 740 and 33
gen mathhigh=(math>740)
gen bchigh=(number>33)
table mathhigh computer if round==11 & info==0 & modified==1 & votemod==1, c(mean coop)
table bchigh computer if round==11 & info==0 & modified==1 & votemod==1, c(mean coop)

*Significance of difference regarding sophistication
gen high=(math>740)
gen groupmodh=groupmody*high
gen compmodh=compmody*high
gen groupmodl=groupmody*(1-high)
gen compmodl=compmody*(1-high)

reg coop groupmodl compmodl groupmodh compmodh if round==11 & votemod==1 & info==0, noconstant
test groupmodl=compmodl
test groupmodh=compmodh
test groupmodl-compmodl=groupmodh-compmodh

use data, clear
keep if info==0
gen high=(number>33)
gen groupmodh=groupmody*high
gen compmodh=compmody*high
gen groupmodl=groupmody*(1-high)
gen compmodl=compmody*(1-high)

reg coop groupmodl compmodl groupmodh compmodh if round==11 & votemod==1 & info==0, noconstant
test groupmodl=compmodl
test groupmodh=compmodh
test groupmodl-compmodl=groupmodh-compmodh

** B. Controlling for information

use data, clear

*TABLE 10: SUMMARY STATISTICS OF ADDITIONAL SESSIONS
sum econ class earning  political math verbal number qvoteresult qinitialpay qmodpay if round ==1 & info==1

*"The average level of cooperation was 17.3% in the first part of the experiment. The level of cooperation was decreasing with experience, with a maximum of 30.5% in round 1 and a minimum of 10.1% in round 10 (the last round of part 1)."
table info if postvote==0, c(mean coop)
table round info if postvote==0, c(mean coop)

*"Of the 148 subjects in the additional sessions, 54.72% voted to modify payoffs in the second part of the experiment compared to 53.26% of the original sessions (p-value of difference is 0.773)."
table info if round==1, c(mean votemod)
reg votemod info if round==1

*TABLE 11: THE EFFECT OF DEMOCRACY CONTROLING FOR INFORMATION - MODIFIED PAYOFFS
*Panel A
table votemod computer if round==11 & info==0 & modified==1, c(count id)
table votemod groupvotemod if round==11 & info==1 & modified==1 & computer==1, c(count id)
*Panel B
table votemod computer if round==11 & info==0 & modified==1, c(mean coop)
table votemod groupvotemod if round==11 & info==1 & modified==1 & computer==1, c(mean coop)
table computer if round==11 & info==0 & modified==1, c(mean coop)
table groupvotemod if round==11 & info==1 & modified==1 & computer==1, c(mean coop)
*Panel C
table votemod computer if postvote==1 & info==0 & modified==1, c(mean coop)
table votemod groupvotemod if postvote==1 & info==1 & modified==1 & computer==1, c(mean coop)
table computer if postvote==1 & info==0 & modified==1, c(mean coop)
table groupvotemod if postvote==1 & info==1 & modified==1 & computer==1, c(mean coop)

*"The cooperation rate in round 11 under exogenous modification for voters who voted for modification and were informed that there were at least two votes for modification in their group is significantly smaller
*than the cooperation rate under endogenous modification (62.5% for ExoModH versus 81.82% for EndoMod, p-value of 0.027)."
reg coop groupmodn groupmody compmodn compmody if round==11 & groupvotemod==1 & ((votestage==1 & info==0)|(votestage==3 & info==1)), noconstant
test groupmody=compmody

*"the cooperation rates under exogenous modification in the additional sessions do not depend on the information regarding the votes in the group (62.5% for ExoModH versus 64.3% for ExoModL, p-value of 0.903)"
reg coop groupvotemod if round==11 & compmod==1 & info==1 & votemod==1

*"Moreover, these two cooperation levels are not significantly different from the cooperation level under exogenous modification in the original sessions (p-values of .65 and .676)."
reg coop info if round==11 & votestage==3 & votemod==1 &(info==0|(info==1 & groupvotemod==1))
reg coop info if round==11 & votestage==3 & votemod==1 &(info==0|(info==1 & groupvotemod==0))

*"[W]hen we control for information, democracy has no effect in round 11 for subjects that did not vote for modification (35% for ExoModH versus 41.18% for EndoMod, p-value of 0.68)."
reg coop groupmodn groupmody compmodn compmody if round==11 & groupvotemod==1 & ((votestage==1 & info==0)|(votestage==3 & info==1)), noconstant
test groupmodn=compmodn

*"And information about the group vote share does not affect cooperation by subjects that did not vote for modification under exogenous modification in the additional sessions (35% for ExoModH versus 23.68% for ExoModL, p-value of 0.368)."
reg coop groupvotemod if round==11 & compmod==1 & info==1 & votemod==0

*"[T]he cooperation rate in part 2 is 71.82% under endogenous modification and 50.36% for exogenous modification with at least two subjects that voted for modification (see Table 11, panel C; the difference is statistically significant with p-value of .056)."
collapse coop groupmodn groupmody compmodn compmody info groupvotemod votestage group, by(id postvote)
reg coop groupmodn groupmody compmodn compmody if postvote==1 & groupvotemod==1 & ((votestage==1 & info==0)|(votestage==3 & info==1)), noconstant cluster(group)
test groupmody=compmody

*"for subjects that did not vote for modification cooperation is greater under endogenous modification than under exogenous modification with at least two subjects that voted for modification (43.53% for EndoMod versus 22% for ExoModH, p-value of .041)."
test groupmodn=compmodn

*Footnote: "Once we control for this difference, as done is section III.B and explained in the appendix, we find that information about the vote share has no effect on behavior under exogenous modification in any of the rounds."
use data, replace
keep if votestage==3 & info==1

gen yy=(groupvote==1 & votemod==1)
gen yn=(groupvote==1 & votemod==0)
gen ny=(groupvote==0 & votemod==1)
gen nn=(groupvote==0 & votemod==0)

capture prog drop estim
prog def estim , rclass

reg coop yy yn ny nn if round==11, noc
local b1yy=_b[yy]
local b1yn=_b[yn]
local b1ny=_b[ny]
local b1nn=_b[nn]

reg coop yy yn ny nn coop11 ocoop11 if round==12, noc
local b2yy=_b[yy]
local b2yn=_b[yn]
local b2ny=_b[ny]
local b2nn=_b[nn]
local r2_1=_b[coop11]
local f2_1=_b[ocoop11]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 if round==13, noc
local b3yy=_b[yy]
local b3yn=_b[yn]
local b3ny=_b[ny]
local b3nn=_b[nn]
local r3_1=_b[coop11]
local f3_1=_b[ocoop11]
local r3_2=_b[coop12]
local f3_2=_b[ocoop12]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 if round==14, noc
local b4yy=_b[yy]
local b4yn=_b[yn]
local b4ny=_b[ny]
local b4nn=_b[nn]
local r4_1=_b[coop11]
local f4_1=_b[ocoop11]
local r4_2=_b[coop12]
local f4_2=_b[ocoop12]
local r4_3=_b[coop13]
local f4_3=_b[ocoop13]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 if round==15, noc
local b5yy=_b[yy]
local b5yn=_b[yn]
local b5ny=_b[ny]
local b5nn=_b[nn]
local r5_1=_b[coop11]
local f5_1=_b[ocoop11]
local r5_2=_b[coop12]
local f5_2=_b[ocoop12]
local r5_3=_b[coop13]
local f5_3=_b[ocoop13]
local r5_4=_b[coop14]
local f5_4=_b[ocoop14]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 if round==16, noc
local b6yy=_b[yy]
local b6yn=_b[yn]
local b6ny=_b[ny]
local b6nn=_b[nn]
local r6_1=_b[coop11]
local f6_1=_b[ocoop11]
local r6_2=_b[coop12]
local f6_2=_b[ocoop12]
local r6_3=_b[coop13]
local f6_3=_b[ocoop13]
local r6_4=_b[coop14]
local f6_4=_b[ocoop14]
local r6_5=_b[coop15]
local f6_5=_b[ocoop15]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 if round==17, noc
local b7yy=_b[yy]
local b7yn=_b[yn]
local b7ny=_b[ny]
local b7nn=_b[nn]
local r7_1=_b[coop11]
local f7_1=_b[ocoop11]
local r7_2=_b[coop12]
local f7_2=_b[ocoop12]
local r7_3=_b[coop13]
local f7_3=_b[ocoop13]
local r7_4=_b[coop14]
local f7_4=_b[ocoop14]
local r7_5=_b[coop15]
local f7_5=_b[ocoop15]
local r7_6=_b[coop16]
local f7_6=_b[ocoop16]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 coop17 ocoop17 if round==18, noc
local b8yy=_b[yy]
local b8yn=_b[yn]
local b8ny=_b[ny]
local b8nn=_b[nn]
local r8_1=_b[coop11]
local f8_1=_b[ocoop11]
local r8_2=_b[coop12]
local f8_2=_b[ocoop12]
local r8_3=_b[coop13]
local f8_3=_b[ocoop13]
local r8_4=_b[coop14]
local f8_4=_b[ocoop14]
local r8_5=_b[coop15]
local f8_5=_b[ocoop15]
local r8_6=_b[coop16]
local f8_6=_b[ocoop16]
local r8_7=_b[coop17]
local f8_7=_b[ocoop17]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 coop17 ocoop17 coop18 ocoop18 if round==19, noc
local b9yy=_b[yy]
local b9yn=_b[yn]
local b9ny=_b[ny]
local b9nn=_b[nn]
local r9_1=_b[coop11]
local f9_1=_b[ocoop11]
local r9_2=_b[coop12]
local f9_2=_b[ocoop12]
local r9_3=_b[coop13]
local f9_3=_b[ocoop13]
local r9_4=_b[coop14]
local f9_4=_b[ocoop14]
local r9_5=_b[coop15]
local f9_5=_b[ocoop15]
local r9_6=_b[coop16]
local f9_6=_b[ocoop16]
local r9_7=_b[coop17]
local f9_7=_b[ocoop17]
local r9_8=_b[coop18]
local f9_8=_b[ocoop18]

reg coop yy yn ny nn coop11 ocoop11 coop12 ocoop12 coop13 ocoop13 coop14 ocoop14 coop15 ocoop15 coop16 ocoop16 coop17 ocoop17 coop18 ocoop18 coop19 ocoop19 if round==20, noc
local b0yy=_b[yy]
local b0yn=_b[yn]
local b0ny=_b[ny]
local b0nn=_b[nn]
local r0_1=_b[coop11]
local f0_1=_b[ocoop11]
local r0_2=_b[coop12]
local f0_2=_b[ocoop12]
local r0_3=_b[coop13]
local f0_3=_b[ocoop13]
local r0_4=_b[coop14]
local f0_4=_b[ocoop14]
local r0_5=_b[coop15]
local f0_5=_b[ocoop15]
local r0_6=_b[coop16]
local f0_6=_b[ocoop16]
local r0_7=_b[coop17]
local f0_7=_b[ocoop17]
local r0_8=_b[coop18]
local f0_8=_b[ocoop18]
local r0_9=_b[coop19]
local f0_9=_b[ocoop19]

*types of partners under democracy by own vote
egen poyya = mean(ovotemod) if votemod==1 & groupvote==1
egen poyy = mean(poyya)
local poyy=poyy[1]
drop poyya poyy

egen poyna = mean(ovotemod) if votemod==0 & groupvote==1
egen poyn = mean(poyna)
local poyn=poyn[1]
drop poyna poyn

*calculate bs: diff between yy and ny and yn and nn
local by1 =`b1yy'-`b1ny'
local by2 =`b2yy'-`b2ny'
local by3 =`b3yy'-`b3ny'
local by4 =`b4yy'-`b4ny'
local by5 =`b5yy'-`b5ny'
local by6 =`b6yy'-`b6ny'
local by7 =`b7yy'-`b7ny'
local by8 =`b8yy'-`b8ny'
local by9 =`b9yy'-`b9ny'
local by0 =`b0yy'-`b0ny'
local bn1 =`b1yn'-`b1nn'
local bn2 =`b2yn'-`b2nn'
local bn3 =`b3yn'-`b3nn'
local bn4 =`b4yn'-`b4nn'
local bn5 =`b5yn'-`b5nn'
local bn6 =`b6yn'-`b6nn'
local bn7 =`b7yn'-`b7nn'
local bn8 =`b8yn'-`b8nn'
local bn9 =`b9yn'-`b9nn'
local bn0 =`b0yn'-`b0nn'

*calculate rhos
local ro2_1=`r2_1'

local ro3_2=`r3_2'
local ro3_1=`r3_1'+`ro3_2'*`r2_1'

local ro4_3=`r4_3'
local ro4_2=`r4_2'+`ro4_3'*`r3_2'
local ro4_1=`r4_1'+`ro4_2'*`r2_1'+`ro4_3'*`r3_1'

local ro5_4=`r5_4'
local ro5_3=`r5_3'+`ro5_4'*`r4_3'
local ro5_2=`r5_2'+`ro5_3'*`r3_2'+`ro5_4'*`r4_2'
local ro5_1=`r5_1'+`ro5_2'*`r2_1'+`ro5_3'*`r3_1'+`ro5_4'*`r4_1'

local ro6_5=`r6_5'
local ro6_4=`r6_4'+`ro6_5'*`r5_4'
local ro6_3=`r6_3'+`ro6_4'*`r4_3'+`ro6_5'*`r5_3'
local ro6_2=`r6_2'+`ro6_3'*`r3_2'+`ro6_4'*`r4_2'+`ro6_5'*`r5_2'
local ro6_1=`r6_1'+`ro6_2'*`r2_1'+`ro6_3'*`r3_1'+`ro6_4'*`r4_1'+`ro6_5'*`r5_1'

local ro7_6=`r7_6'
local ro7_5=`r7_5'+`ro7_6'*`r6_5'
local ro7_4=`r7_4'+`ro7_5'*`r5_4'+`ro7_6'*`r6_4'
local ro7_3=`r7_3'+`ro7_4'*`r4_3'+`ro7_5'*`r5_3'+`ro7_6'*`r6_3'
local ro7_2=`r7_2'+`ro7_3'*`r3_2'+`ro7_4'*`r4_2'+`ro7_5'*`r5_2'+`ro7_6'*`r6_2'
local ro7_1=`r7_1'+`ro7_2'*`r2_1'+`ro7_3'*`r3_1'+`ro7_4'*`r4_1'+`ro7_5'*`r5_1'+`ro7_6'*`r6_1'

local ro8_7=`r8_7'
local ro8_6=`r8_6'+`ro8_7'*`r7_6'
local ro8_5=`r8_5'+`ro8_6'*`r6_5'+`ro8_7'*`r7_5'
local ro8_4=`r8_4'+`ro8_5'*`r5_4'+`ro8_6'*`r6_4'+`ro8_7'*`r7_4'
local ro8_3=`r8_3'+`ro8_4'*`r4_3'+`ro8_5'*`r5_3'+`ro8_6'*`r6_3'+`ro8_7'*`r7_3'
local ro8_2=`r8_2'+`ro8_3'*`r3_2'+`ro8_4'*`r4_2'+`ro8_5'*`r5_2'+`ro8_6'*`r6_2'+`ro8_7'*`r7_2'
local ro8_1=`r8_1'+`ro8_2'*`r2_1'+`ro8_3'*`r3_1'+`ro8_4'*`r4_1'+`ro8_5'*`r5_1'+`ro8_6'*`r6_1'+`ro8_7'*`r7_1'

local ro9_8=`r9_8'
local ro9_7=`r9_7'+`ro9_8'*`r8_7'
local ro9_6=`r9_6'+`ro9_7'*`r7_6'+`ro9_8'*`r8_6'
local ro9_5=`r9_5'+`ro9_6'*`r6_5'+`ro9_7'*`r7_5'+`ro9_8'*`r8_5'
local ro9_4=`r9_4'+`ro9_5'*`r5_4'+`ro9_6'*`r6_4'+`ro9_7'*`r7_4'+`ro9_8'*`r8_4'
local ro9_3=`r9_3'+`ro9_4'*`r4_3'+`ro9_5'*`r5_3'+`ro9_6'*`r6_3'+`ro9_7'*`r7_3'+`ro9_8'*`r8_3'
local ro9_2=`r9_2'+`ro9_3'*`r3_2'+`ro9_4'*`r4_2'+`ro9_5'*`r5_2'+`ro9_6'*`r6_2'+`ro9_7'*`r7_2'+`ro9_8'*`r8_2'
local ro9_1=`r9_1'+`ro9_2'*`r2_1'+`ro9_3'*`r3_1'+`ro9_4'*`r4_1'+`ro9_5'*`r5_1'+`ro9_6'*`r6_1'+`ro9_7'*`r7_1'+`ro9_8'*`r8_1'

local ro0_9=`r0_9'
local ro0_8=`r0_8'+`ro0_9'*`r9_8'
local ro0_7=`r0_7'+`ro0_8'*`r8_7'+`ro0_9'*`r9_7'
local ro0_6=`r0_6'+`ro0_7'*`r7_6'+`ro0_8'*`r8_6'+`ro0_9'*`r9_6'
local ro0_5=`r0_5'+`ro0_6'*`r6_5'+`ro0_7'*`r7_5'+`ro0_8'*`r8_5'+`ro0_9'*`r9_5'
local ro0_4=`r0_4'+`ro0_5'*`r5_4'+`ro0_6'*`r6_4'+`ro0_7'*`r7_4'+`ro0_8'*`r8_4'+`ro0_9'*`r9_4'
local ro0_3=`r0_3'+`ro0_4'*`r4_3'+`ro0_5'*`r5_3'+`ro0_6'*`r6_3'+`ro0_7'*`r7_3'+`ro0_8'*`r8_3'+`ro0_9'*`r9_3'
local ro0_2=`r0_2'+`ro0_3'*`r3_2'+`ro0_4'*`r4_2'+`ro0_5'*`r5_2'+`ro0_6'*`r6_2'+`ro0_7'*`r7_2'+`ro0_8'*`r8_2'+`ro0_9'*`r9_2'
local ro0_1=`r0_1'+`ro0_2'*`r2_1'+`ro0_3'*`r3_1'+`ro0_4'*`r4_1'+`ro0_5'*`r5_1'+`ro0_6'*`r6_1'+`ro0_7'*`r7_1'+`ro0_8'*`r8_1'+`ro0_9'*`r9_1'

*calculate phis
local fi2_1=`f2_1'

local fi3_2=`f3_2'
local fi3_1=`f3_1'+`ro3_2'*`f2_1'

local fi4_3=`f4_3'
local fi4_2=`f4_2'+`ro4_3'*`f3_2'
local fi4_1=`f4_1'+`ro4_2'*`f2_1'+`ro4_3'*`f3_1'

local fi5_4=`f5_4'
local fi5_3=`f5_3'+`ro5_4'*`f4_3'
local fi5_2=`f5_2'+`ro5_3'*`f3_2'+`ro5_4'*`f4_2'
local fi5_1=`f5_1'+`ro5_2'*`f2_1'+`ro5_3'*`f3_1'+`ro5_4'*`f4_1'

local fi6_5=`f6_5'
local fi6_4=`f6_4'+`ro6_5'*`f5_4'
local fi6_3=`f6_3'+`ro6_4'*`f4_3'+`ro6_5'*`f5_3'
local fi6_2=`f6_2'+`ro6_3'*`f3_2'+`ro6_4'*`f4_2'+`ro6_5'*`f5_2'
local fi6_1=`f6_1'+`ro6_2'*`f2_1'+`ro6_3'*`f3_1'+`ro6_4'*`f4_1'+`ro6_5'*`f5_1'

local fi7_6=`f7_6'
local fi7_5=`f7_5'+`ro7_6'*`f6_5'
local fi7_4=`f7_4'+`ro7_5'*`f5_4'+`ro7_6'*`f6_4'
local fi7_3=`f7_3'+`ro7_4'*`f4_3'+`ro7_5'*`f5_3'+`ro7_6'*`f6_3'
local fi7_2=`f7_2'+`ro7_3'*`f3_2'+`ro7_4'*`f4_2'+`ro7_5'*`f5_2'+`ro7_6'*`f6_2'
local fi7_1=`f7_1'+`ro7_2'*`f2_1'+`ro7_3'*`f3_1'+`ro7_4'*`f4_1'+`ro7_5'*`f5_1'+`ro7_6'*`f6_1'

local fi8_7=`f8_7'
local fi8_6=`f8_6'+`ro8_7'*`f7_6'
local fi8_5=`f8_5'+`ro8_6'*`f6_5'+`ro8_7'*`f7_5'
local fi8_4=`f8_4'+`ro8_5'*`f5_4'+`ro8_6'*`f6_4'+`ro8_7'*`f7_4'
local fi8_3=`f8_3'+`ro8_4'*`f4_3'+`ro8_5'*`f5_3'+`ro8_6'*`f6_3'+`ro8_7'*`f7_3'
local fi8_2=`f8_2'+`ro8_3'*`f3_2'+`ro8_4'*`f4_2'+`ro8_5'*`f5_2'+`ro8_6'*`f6_2'+`ro8_7'*`f7_2'
local fi8_1=`f8_1'+`ro8_2'*`f2_1'+`ro8_3'*`f3_1'+`ro8_4'*`f4_1'+`ro8_5'*`f5_1'+`ro8_6'*`f6_1'+`ro8_7'*`f7_1'

local fi9_8=`f9_8'
local fi9_7=`f9_7'+`ro9_8'*`f8_7'
local fi9_6=`f9_6'+`ro9_7'*`f7_6'+`ro9_8'*`f8_6'
local fi9_5=`f9_5'+`ro9_6'*`f6_5'+`ro9_7'*`f7_5'+`ro9_8'*`f8_5'
local fi9_4=`f9_4'+`ro9_5'*`f5_4'+`ro9_6'*`f6_4'+`ro9_7'*`f7_4'+`ro9_8'*`f8_4'
local fi9_3=`f9_3'+`ro9_4'*`f4_3'+`ro9_5'*`f5_3'+`ro9_6'*`f6_3'+`ro9_7'*`f7_3'+`ro9_8'*`f8_3'
local fi9_2=`f9_2'+`ro9_3'*`f3_2'+`ro9_4'*`f4_2'+`ro9_5'*`f5_2'+`ro9_6'*`f6_2'+`ro9_7'*`f7_2'+`ro9_8'*`f8_2'
local fi9_1=`f9_1'+`ro9_2'*`f2_1'+`ro9_3'*`f3_1'+`ro9_4'*`f4_1'+`ro9_5'*`f5_1'+`ro9_6'*`f6_1'+`ro9_7'*`f7_1'+`ro9_8'*`f8_1'

local fi0_9=`f0_9'
local fi0_8=`f0_8'+`ro0_9'*`f9_8'
local fi0_7=`f0_7'+`ro0_8'*`f8_7'+`ro0_9'*`f9_7'
local fi0_6=`f0_6'+`ro0_7'*`f7_6'+`ro0_8'*`f8_6'+`ro0_9'*`f9_6'
local fi0_5=`f0_5'+`ro0_6'*`f6_5'+`ro0_7'*`f7_5'+`ro0_8'*`f8_5'+`ro0_9'*`f9_5'
local fi0_4=`f0_4'+`ro0_5'*`f5_4'+`ro0_6'*`f6_4'+`ro0_7'*`f7_4'+`ro0_8'*`f8_4'+`ro0_9'*`f9_4'
local fi0_3=`f0_3'+`ro0_4'*`f4_3'+`ro0_5'*`f5_3'+`ro0_6'*`f6_3'+`ro0_7'*`f7_3'+`ro0_8'*`f8_3'+`ro0_9'*`f9_3'
local fi0_2=`f0_2'+`ro0_3'*`f3_2'+`ro0_4'*`f4_2'+`ro0_5'*`f5_2'+`ro0_6'*`f6_2'+`ro0_7'*`f7_2'+`ro0_8'*`f8_2'+`ro0_9'*`f9_2'
local fi0_1=`f0_1'+`ro0_2'*`f2_1'+`ro0_3'*`f3_1'+`ro0_4'*`f4_1'+`ro0_5'*`f5_1'+`ro0_6'*`f6_1'+`ro0_7'*`f7_1'+`ro0_8'*`f8_1'+`ro0_9'*`f9_1'


*calculate direct information effect for yes and no people
local dey1 =`by1'
local dey2 =`by2'+`ro2_1'*(`by1')
local dey3 =`by3'+`ro3_1'*(`by1') +`ro3_2'*(`by2')
local dey4 =`by4'+`ro4_1'*(`by1') +`ro4_2'*(`by2') +`ro4_3'*(`by3')
local dey5 =`by5'+`ro5_1'*(`by1') +`ro5_2'*(`by2') +`ro5_3'*(`by3') +`ro5_4'*(`by4')
local dey6 =`by6'+`ro6_1'*(`by1') +`ro6_2'*(`by2') +`ro6_3'*(`by3') +`ro6_4'*(`by4') +`ro6_5'*(`by5')
local dey7 =`by7'+`ro7_1'*(`by1') +`ro7_2'*(`by2') +`ro7_3'*(`by3') +`ro7_4'*(`by4') +`ro7_5'*(`by5') +`ro7_6'*(`by6')
local dey8 =`by8'+`ro8_1'*(`by1') +`ro8_2'*(`by2') +`ro8_3'*(`by3') +`ro8_4'*(`by4') +`ro8_5'*(`by5') +`ro8_6'*(`by6') +`ro8_7'*(`by7')
local dey9 =`by9'+`ro9_1'*(`by1') +`ro9_2'*(`by2') +`ro9_3'*(`by3') +`ro9_4'*(`by4') +`ro9_5'*(`by5') +`ro9_6'*(`by6') +`ro9_7'*(`by7') +`ro9_8'*(`by8')
local dey0 =`by0'+`ro0_1'*(`by1') +`ro0_2'*(`by2') +`ro0_3'*(`by3') +`ro0_4'*(`by4') +`ro0_5'*(`by5') +`ro0_6'*(`by6') +`ro0_7'*(`by7') +`ro0_8'*(`by8') +`ro0_9'*(`by9')

local den1 =`bn1'
local den2 =`bn2'+`ro2_1'*(`bn1')
local den3 =`bn3'+`ro3_1'*(`bn1') +`ro3_2'*(`bn2')
local den4 =`bn4'+`ro4_1'*(`bn1') +`ro4_2'*(`bn2') +`ro4_3'*(`bn3')
local den5 =`bn5'+`ro5_1'*(`bn1') +`ro5_2'*(`bn2') +`ro5_3'*(`bn3') +`ro5_4'*(`bn4')
local den6 =`bn6'+`ro6_1'*(`bn1') +`ro6_2'*(`bn2') +`ro6_3'*(`bn3') +`ro6_4'*(`bn4') +`ro6_5'*(`bn5')
local den7 =`bn7'+`ro7_1'*(`bn1') +`ro7_2'*(`bn2') +`ro7_3'*(`bn3') +`ro7_4'*(`bn4') +`ro7_5'*(`bn5') +`ro7_6'*(`bn6')
local den8 =`bn8'+`ro8_1'*(`bn1') +`ro8_2'*(`bn2') +`ro8_3'*(`bn3') +`ro8_4'*(`bn4') +`ro8_5'*(`bn5') +`ro8_6'*(`bn6') +`ro8_7'*(`bn7')
local den9 =`bn9'+`ro9_1'*(`bn1') +`ro9_2'*(`bn2') +`ro9_3'*(`bn3') +`ro9_4'*(`bn4') +`ro9_5'*(`bn5') +`ro9_6'*(`bn6') +`ro9_7'*(`bn7') +`ro9_8'*(`bn8')
local den0 =`bn0'+`ro0_1'*(`bn1') +`ro0_2'*(`bn2') +`ro0_3'*(`bn3') +`ro0_4'*(`bn4') +`ro0_5'*(`bn5') +`ro0_6'*(`bn6') +`ro0_7'*(`bn7') +`ro0_8'*(`bn8') +`ro0_9'*(`bn9')

*calculate total information effect for yes and no people
local tey1 =`dey1'
local ten1 =`den1'

local tey2 =`dey2'+`fi2_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1')
local ten2 =`den2'+`fi2_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1')

local tey3 =`dey3'+`fi3_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi3_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2')
local ten3 =`den3'+`fi3_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi3_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2')

local tey4 =`dey4'+`fi4_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi4_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi4_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3')
local ten4 =`den4'+`fi4_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi4_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi4_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3')

local tey5 =`dey5'+`fi5_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi5_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi5_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi5_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4')
local ten5 =`den5'+`fi5_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi5_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi5_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi5_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4')

local tey6 =`dey6'+`fi6_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi6_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi6_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi6_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi6_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5')
local ten6 =`den6'+`fi6_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi6_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi6_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi6_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi6_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5')

local tey7 =`dey7'+`fi7_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi7_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi7_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi7_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi7_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi7_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6')
local ten7 =`den7'+`fi7_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi7_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi7_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi7_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi7_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi7_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6')

local tey8 =`dey8'+`fi8_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi8_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi8_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi8_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi8_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi8_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6') +`fi8_7'*(`poyy'*`tey7'+(1-`poyy')*`ten7') 
local ten8 =`den8'+`fi8_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi8_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi8_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi8_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi8_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi8_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6') +`fi8_7'*(`poyn'*`tey7'+(1-`poyn')*`ten7')

local tey9 =`dey9'+`fi9_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi9_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi9_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi9_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi9_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi9_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6') +`fi9_7'*(`poyy'*`tey7'+(1-`poyy')*`ten7') +`fi9_8'*(`poyy'*`tey8'+(1-`poyy')*`ten8')
local ten9 =`den9'+`fi9_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi9_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi9_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi9_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi9_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi9_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6') +`fi9_7'*(`poyn'*`tey7'+(1-`poyn')*`ten7') +`fi9_8'*(`poyn'*`tey8'+(1-`poyn')*`ten8')

local tey0 =`dey0'+`fi0_1'*(`poyy'*`tey1'+(1-`poyy')*`ten1') +`fi0_2'*(`poyy'*`tey2'+(1-`poyy')*`ten2') +`fi0_3'*(`poyy'*`tey3'+(1-`poyy')*`ten3') +`fi0_4'*(`poyy'*`tey4'+(1-`poyy')*`ten4') +`fi0_5'*(`poyy'*`tey5'+(1-`poyy')*`ten5') +`fi0_6'*(`poyy'*`tey6'+(1-`poyy')*`ten6') +`fi0_7'*(`poyy'*`tey7'+(1-`poyy')*`ten7') +`fi0_8'*(`poyy'*`tey8'+(1-`poyy')*`ten8') +`fi0_9'*(`poyy'*`tey9'+(1-`poyy')*`ten9')
local ten0 =`den0'+`fi0_1'*(`poyn'*`tey1'+(1-`poyn')*`ten1') +`fi0_2'*(`poyn'*`tey2'+(1-`poyn')*`ten2') +`fi0_3'*(`poyn'*`tey3'+(1-`poyn')*`ten3') +`fi0_4'*(`poyn'*`tey4'+(1-`poyn')*`ten4') +`fi0_5'*(`poyn'*`tey5'+(1-`poyn')*`ten5') +`fi0_6'*(`poyn'*`tey6'+(1-`poyn')*`ten6') +`fi0_7'*(`poyn'*`tey7'+(1-`poyn')*`ten7') +`fi0_8'*(`poyn'*`tey8'+(1-`poyn')*`ten8') +`fi0_9'*(`poyn'*`tey9'+(1-`poyn')*`ten9')

mat input coefs=(`tey1', `ten1', `tey2', `ten2', `tey3', `ten3', `tey4', `ten4', `tey5' ,`ten5', `tey6', `ten6', `tey7', `ten7', `tey8', `ten8', `tey9', `ten9', `tey0', `ten0')
return matrix c=coefs
end

local i=0
capture postclose jack 
postfile jack ty1 tn1 ty2 tn2 ty3 tn3 ty4 tn4 ty5 tn5 ty6 tn6 ty7 tn7 ty8 tn8 ty9 tn9 ty0 tn0 using jack , replace
egen igroup=group(group)
sort igroup
quietly while `i'<igroup[_N] {
local i = `i'+1
noisily display `i'
preserve
drop if igroup==`i'
estim
mat x=r(c)
post jack (x[1,1]) (x[1,2]) (x[1,3]) (x[1,4]) (x[1,5]) (x[1,6]) (x[1,7]) (x[1,8]) (x[1,9]) (x[1,10]) (x[1,11]) (x[1,12]) (x[1,13]) (x[1,14]) (x[1,15]) (x[1,16]) (x[1,17]) (x[1,18]) (x[1,19]) (x[1,20]) 
restore
}
postclose jack 


use jack, clear
foreach var of varlist t* {
quietly sum `var'
*displays total effect and t
display "`var' " r(mean) " " r(mean)/(r(sd)*sqrt(r(N)))
}

corr, cov `ty*'

*** Figures

*FIGURE 2: CUMULATIVE DISTRIBUTION OF VOTE SHARE
*(done in Excel, data follows)
use data, clear
collapse id voteshare if info==0, by(group)
tab voteshare

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
graph export graph3.wmf, replace

*FIGURE 4: COOPERATION BY ROUND AND INDIVIDUAL VOTE UNDER MODIFIED PAYOFFS - ORIGINAL AND ADDITIONAL SESSIONS COMPARISON
use data, clear
gen groupmodc=coop if votestage==1 & info==0
gen compmodc=coop if votestage==3 & info==0
gen compmodyc=coop if votestage==3 & info==1 & groupvote==1
gen compmodnc=coop if votestage==3 & info==1 & groupvote==0

collapse groupmodc compmodc compmodyc compmodnc, by (round votemod)

*graph by treatment
twoway (line groupmodc round if votemod==0, title(Did not vote for modification) graphregion(color(white)) xline(10.5) xtitle("Round") xscale(range(1 20)) xlabel(#20) ytitle("Cooperation") yscale(range(0 1)) ylabel(#6) scale(.6) ) (line compmodyc round if votemod==0, lpattern(dot)) (line compmodc round if votemod==0, lpattern(dash)) (line compmodnc round if votemod==0, lpattern(dash_dot) legend(label(1 "EndoMod") label(2 "ExoModH") label(3 "ExoMod") label(4 "ExoModL"))) , saving(graphvm0,replace)
twoway (line groupmodc round if votemod==1, title(Voted for modification) graphregion(color(white)) xline(10.5) xtitle("Round") xscale(range(1 20)) xlabel(#20) ytitle("Cooperation") yscale(range(0 1)) ylabel(#6) scale(.6) ) (line compmodyc round if votemod==1, lpattern(dot)) (line compmodc round if votemod==1, lpattern(dash)) (line compmodnc round if votemod==1, lpattern(dash_dot) legend(label(1 "EndoMod") label(2 "ExoModH") label(3 "ExoMod") label(4 "ExoModL"))) , saving(graphvm1,replace)

graph combine "graphvm1" "graphvm0" , graphregion(color(white)) saving(graphvs,replace)
graph export graph4.wmf, replace




