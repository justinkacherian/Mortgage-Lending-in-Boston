# delimit ;
clear;
set more off;
cap log close;
*************************************************************;
* Replication program for Chapter 11 in SW3U;
*************************************************************;


use "C:\Users\justin\OneDrive\ECO_4010\Stata\Datasets\hmda_sw(1).dta"

********************************************;
# delimit ;
gen deny = (s7==3);
gen pi_rat = s46/100;
gen black = (s13==3);

* this is an example of you create labels for your variables;
label var deny "denied a mortgage";
label define denylbl 1 "denied" 0 "approved";
label values deny denylbl;
tab deny;
***********************************;
* Results on Page 1;
**********************************;
sort black;
summarize deny if (black==1);
summarize deny if (black==0);
************************************;
**** Equation 11.1;
reg deny pi_rat, r;
************************************;

************************************;
**** Equation 11.3;
reg deny pi_rat black, r;
************************************;

************************************;
**** Equation 11.7;
probit deny pi_rat;
************************************;

************************************;
**** Equation 11.8;
probit deny pi_rat black;

**** Marginal effects in probit
# delimit ;
margins, dydx (pi_rat);
margins, dydx (pi_rat) atmeans;
predict double zbx , xb;
gen double mar_pi_rat  = normalden(zbx)*_b[pi_rat];
sum mar_pi_rat;

* average treatment effect in text
quietly probit deny pi_rat black;
predict double zbx_black, xb;
gen double zbx_white = zbx_black - _b[black];
gen Pdeny_black = normprob(zbx_black) ;
gen Pdeny_white = normprob(zbx_white);
gen mar_black   = Pdeny_black - Pdeny_white;
sum mar_black Pdeny_black Pdeny_white;
probit deny pi_rat black;

*drop zbx_black zbx_white;
scalar zbx_black = _b[_cons] + _b[pi_rat]*.3 + _b[black];
scalar Pblack=normprob(zbx_black);
di zbx_black;
di Pblack;
scalar zbx_white = _b[_cons] + _b[pi_rat]*.3;
scalar Pwhite=normprob(zbx_white);
di zbx_white;
di Pwhite;
scalar Effect_black = normprob(zbx_black) - normprob(zbx_white);
di Effect_black;
sum pi_rat;
scalar mpi_rat=r(mean);


*Average effects at means;
scalar zbx_black = _b[_cons] + _b[pi_rat]*mpi_rat + _b[black];
scalar Pblack=normprob(zbx_black);
di zbx_black;
di Pblack;
scalar zbx_white = _b[_cons] + _b[pi_rat]*mpi_rat;
scalar Pwhite=normprob(zbx_white);
di zbx_white;
di Pwhite;
scalar Effect_black = normprob(zbx_black) - normprob(zbx_white);
di Effect_black;


probit deny pi_rat i.black;
margins  black;

********************************************;
********************************************;
******  Marginal Effects in Logit **********;
********************************************;
# delimit ;
logit deny pi_rat black;

**** Marginal effects in probit
# delimit ;
margins, dydx (pi_rat);
margins, dydx (pi_rat) atmeans;
margins, dydx (black);
margins, dydx (black) atmeans;
predict double ibx , xb;
gen plogit = 1/(1+exp(-ibx));
gen double marlogit_pi_rat  = plogit*(1-plogit)*_b[pi_rat];
sum marlogit_pi_rat;

gen plogit_black = 1/(1+exp(-ibx));
gen plogit_white = 1/(1+exp(-(ibx-_b[black])));
sum plogit_black;
scalar meanPblack=r(mean);
sum plogit_white;
scalar meanPwhite=r(mean);
scalar AvgEffectblack=meanPblack-meanPwhite;
di AvgEffectblack;



*Average effects at means;
scalar ibx_black = _b[_cons] + _b[pi_rat]*mpi_rat + _b[black];
scalar Plogit_black=1/(1+exp(-ibx_black));
di ibx_black;
di Plogit_black;
scalar ibx_white = _b[_cons] + _b[pi_rat]*mpi_rat;
scalar Plogit_white=1/(1+exp(-ibx_white));
di ibx_white;
di Plogit_white;
scalar Effect_black = Plogit_black - Plogit_white;
di Effect_black;

* odds ratios;
# delimit ;
logit deny pi_rat black;
scalar ORblack=exp(_b[black]);
di ORblack;
logistic deny pi_rat black;


*******************************;
****  Odds ratios  ;
*******************************;

logistic deny black pi_rat;


************************************;

************************************;
**** Equation 11.10;
logit deny pi_rat black;
************************************;

*************************************
**** Table 11.1 ;
************************************;
gen hse_inc = s45/100;
gen loan_val = s6/s50;
gen ccred = s43;
gen mcred = s42;
gen pubrec = (s44>0);
gen denpmi = (s53==1);
gen selfemp = (s27a==1);
gen married = (s23a=="M");
gen single = (married==0);
gen hischl = (school>=12);
gen probunmp = uria;
gen condo = (s51 == 1);
sum pi_rat hse_inc loan_val ccred mcred pubrec denpmi selfemp
 single hischl probunmp condo black deny;
*************************************
**** Table 11.2 ;
************************************;
gen ltv_med = (loan_val>=0.80)*(loan_val<=.95);
gen ltv_high = (loan_val>0.95); 
gen blk_pi = black*pi_rat;
gen blk_hse = black*hse_inc;
gen ccred3 = (ccred==3); 
gen ccred4 = (ccred==4);
gen ccred5 = (ccred==5);
gen ccred6 = (ccred==6);
gen mcred3 = (mcred==3);
gen mcred4 = (mcred==4);

 * create variable labels and codes for deny;
label var deny "denied a mortgage";
label define denylbl 1 "denied" 0 "approved", replace;
label values deny denylbl;
tab deny;

* create varible labels;

label var pi_rat "P/I ratio";
label var hse_inc "monthly housing expenses/income";
label var ltv_med "medium loan amt to value property";
label var ltv_high "high loan amt to value property";
label var mcred " mortgage credit score";
label var pubrec "public bad credit recort";
label var denpmi "denied mortgage insurance";
label var selfemp "self empoyed";
label var single "Single";
label var hischl " high school diploma";
label var probunmp " unemployment rate";
label var condo "condominium";
label var ccred "consumer credit score";
label define ccredlbl 1 "no slow payments/delinquencies" 2 "one or two slow payments/del" 3 " more than two slow payments"
                      4 "insufficient credit history"  5 "delinquent history payment 60 days late"  6 "delinquencies 90 days late";
label values ccred ccredlbl;
label var blk_pi "black x P/I ratio";
label var blk_hse "black x housing expense/income";
tab ccred;

** Preliminary Analysis ... compute means of all variables;
sum deny black pi_rat hse_inc ltv_med ltv_high ccred mcred pubrec denpmi
 selfemp single hischl probunmp mcred3 mcred4 ccred3 ccred4 ccred5 ccred6
 condo; 
 
 * this example shows you how to evaluate effect of a 
 * dummy variable at the mean of the other X variables
 * you have to extend this to the larger set of covariabes in Table 11.1;
 
 probit deny black pi_rat hse_inc , r;

 scalar z0 =  _b[black]*0
              + _b[ pi_rat]*  .3308136 
              + _b[ hse_inc]* .2553461 
              + _b[   _cons]*  1;
scalar z1 = z0 + _b[black]*1;
dis "Prob for white at means = " normprob(z0);
dis "Prob for black at means = " normprob(z1);
dis "Difference in probs     = " normprob(z1)-normprob(z0);

# delimit ;
probit deny black pi_rat hse_inc single;
test hse_inc single;
scalar lnlUR=e(ll);
estimates store A;

probit deny black pi_rat;
scalar lnlR=e(ll);
scalar LRtest=-2*(lnlR-lnlUR);
estimates store B;
di LRtest;
lrtest A B;


probit deny black pi_rat hse_inc ltv_med ltv_high ccred mcred pubrec denpmi selfemp  blk_pi blk_hse;
scalar lnlR=e(ll);
estimates store A;
probit deny black pi_rat hse_inc ltv_med ltv_high ccred mcred pubrec denpmi selfemp single hischl probunmp blk_pi blk_hse;
scalar lnlUR=e(ll);
scalar LRtest=-2*(lnlR-lnlUR);
di LRtest;
estimates store B;
lrtest A B;

test single hischl probunmp;
scalar chisq1=r(chi2);
scalar df1 =r(df);
scalar F1= chisq1/df1;
di chisq1;
di F1;

