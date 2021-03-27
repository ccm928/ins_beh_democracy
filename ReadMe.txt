ReadMe.txt for Institutions and Behavior: Experimental Evidence on the Effects of Democracy

Thank you four your interest in our paper.

This zip file includes the data in STATA format (data.dta), in text format (data.txt) and the stata program (tables_AER_final.do) to obtain all the tables
and tests in the paper. Replication should be straightforward. If you have any questions please contact us.

Dictionary for the variables in the data file follows:

date: the date of the session in format ddmmyy 
clientid: subject id in a session
opponentid: subject id of partner in a round
id: unique id for a subject (not repeated across sessions)
group: unique id for a group of four subjects
round: round of interaction from 1 to 20
payoff: payoff from the interaction
qvoteresult: 0 if subject did not answer correctly question regarding vote stage result, 1 if subject did answer correctly
econ: =1 if subject in economics or business economics concentration
math: self-reported Math SAT score
verbal: self-reported Verbal SAT score
number: number from beauty contest game
session: id for experimental session
info: =0 for original sessions, =1 for sessions controlling for information about the group vote
coop: =1 if subject cooperated
ocoop: =1 if subject's partner in that round cooperated
postvote: =1 for rounds 11 to 20
votestage: Voting stage result with 1=EndoMod, 2=EndoNot, 3=ExoMod, 4=ExoNot
groupvotemod: 1=group voted to modified payoffs
groupmod: =1 if votestage=1
groupnot: =1 if votestage=2
compmod: =1 if votestage=3
compnot: =1 if votestage=4
class: 1 for freshmen to 4 for senior
political: 1 for very liberal to 5 for very conservative
votemod: =1 if subject voted in favor of modifying payoffs
ovotemod: =1 if partner voted in favor of modifying payoffs
groupmody: =groupmod*votemod
groupnoty: =groupnot*votemod
compmody: =compmod*votemod
compnoty: =compnot*votemod
groupmodn: =groupmod*(1-votemod)
groupnotn: =groupnot*(1-votemod)
compmodn: =compmod*(1-votemod)
compnotn: =compnot*(1-votemod)
computer: =1 if computer did not consider the votes (votestage=3 or 4)
modified: =1 if payoffs were modified (votestage=1 or 3)
qinitialpay: =1 if subject remembered correctly all 4 payoffs from the initial payoff matrix 
qmodpay: =1 if subject remembered correctly all 4 payoffs from the modified payoff matrix
voteshare: number of vote in favor of modifying payoff in the group
coop_0: own cooperation rate in the first part of the experiment (rounds 1 to 10)
ocoop_0: cooperation rate of partners in the first part of the experiment (rounds 1 to 10)
coop11: =1 if cooperated in round 11
coop12: =1 if cooperated in round 12
coop13: =1 if cooperated in round 13
coop14: =1 if cooperated in round 14
coop15: =1 if cooperated in round 15
coop16: =1 if cooperated in round 16
coop17: =1 if cooperated in round 17
coop18: =1 if cooperated in round 18
coop19: =1 if cooperated in round 19
ocoop11: =1 if partner from round 11 cooperated in round 11
ocoop12: =1 if partner from round 12 cooperated in round 12
ocoop13: =1 if partner from round 13 cooperated in round 13
ocoop14: =1 if partner from round 14 cooperated in round 14
ocoop15: =1 if partner from round 15 cooperated in round 15
ocoop16: =1 if partner from round 16 cooperated in round 16
ocoop17: =1 if partner from round 17 cooperated in round 17
ocoop18: =1 if partner from round 18 cooperated in round 18
ocoop19: =1 if partner from round 19 cooperated in round 19
earning: total earning in the experiment including show up fee
q1=1 if subject wrote that vote stage result affected behavior
q2=1 if subject wrote that the payoff matrix mattered for behavior
q3=1 if subject mentioned voting being counted/considered or not