use "C:\Users\justin\OneDrive\ECO_4010\Stata\Datasets\hmda_sw.dta"

gen deny = (s7==3)
gen pi_rat = s46/100
gen black = (s13==3)

label var deny "denied a mortgage"
label define denylbl 1 "denied" 0 "approved"
label values deny denylbl

sort black
summarize deny if (black == 1)
summarize deny if (black == 0)

**equation 11.1
reg deny pi_rat, r

**** Equation 11.3;
reg deny pi_rat black, r

**** Equation 11.7;
probit deny pi_rat

**** Equation 11.8;
probit deny pi_rat black

**** Equation 11.10;
logit deny pi_rat black

quietly regress deny pi_rat
eststo model1

estout

eststo est1: reg deny pi_rat, r
eststo est2: reg deny pi_rat black, r
eststo est3 : probit deny pi_rat, r
eststo est4 : probit deny pi_rat black, r
eststo est5 : logit deny pi_rat black, r
#delimit ;
esttab using assign2.rtf,  label title(Mortgage Denial regressed on p_i ratio and race)
mtitle("LPM" "LPM" "Probit" "Probit" "Logit")order(black pi_rat) se compress;


