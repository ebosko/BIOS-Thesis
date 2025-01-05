*demographics;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=demo dbms=xlsx replace; sheet=demographics; run;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=comp dbms=xlsx replace; sheet=comp_vars; run;

*tib;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=tib dbms=xlsx replace; sheet=tib; run;
proc transpose data=tib out=tib_long; by id; run;
data tib_long; set tib_long; label _name_="lobe"; rename col1=tib _name_=lobe; drop _label_; run;

*LN;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=ln dbms=xlsx replace; sheet=ln; run;
proc transpose data=ln out=ln_long; by id; run;
data ln_long; set ln_long; label _name_="lobe"; rename col1=ln _name_=lobe; drop _label_; run;

*GGO;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=ggo dbms=xlsx replace; sheet=ggo; run;
proc transpose data=ggo out=ggo_long; by id; run;
data ggo_long; set ggo_long; label _name_="lobe"; rename col1=ggo _name_=lobe; drop _label_; run;

*Consol;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=consol dbms=xlsx replace; sheet=consol; run;
proc transpose data=consol out=consol_long; by id; run;
data consol_long; set consol_long; label _name_="lobe"; rename col1=consol _name_=lobe; drop _label_; run;

*Bronch;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=bronch dbms=xlsx replace; sheet=bronch; run;
proc transpose data=bronch out=bronch_long; by id; run;
data bronch_long; set bronch_long; label _name_="lobe"; rename col1=bronch _name_=lobe; drop _label_; run;

*Atel;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=atel dbms=xlsx replace; sheet=atel; run;
proc transpose data=atel out=atel_long; by id; run;
data atel_long; set atel_long; label _name_="lobe"; rename col1=atel _name_=lobe; drop _label_; run;

*Thin;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=thin dbms=xlsx replace; sheet=thin; run;
proc transpose data=thin out=thin_long; by id; run;
data thin_long; set thin_long; label _name_="lobe"; rename col1=thin _name_=lobe; drop _label_; run;

*Thick;
proc import datafile="h:/consulting/chan/ntm and lobe analysis/ntm_ld_radiology mjs.xlsx" out=thick dbms=xlsx replace; sheet=thick; run;
proc transpose data=thick out=thick_long; by id; run;
data thick_long; set thick_long; label _name_="lobe"; rename col1=thick _name_=lobe; drop _label_; run;

data toget; merge tib_long ln_long ggo_long consol_long bronch_long atel_long thin_long thick_long; 
 if tib>0 then tib_binary=1; else tib_binary=0; 
 if ln>0 then ln_binary=1; else ln_binary=0;
 if ggo>0 then ggo_binary=1; else ggo_binary=0;
 if consol>0 then consol_binary=1; else consol_binary=0;
 if bronch>0 then bronch_binary=1; else bronch_binary=0;
 if atel>0 then atel_binary=1; else atel_binary=0;
 if thin>0 then thin_binary=1; else thin_binary=0;
 if thick>0 then thick_binary=1; else thick_binary=0;run;

data togetb; merge demo comp; by id; run;


*Comparing RUL vs. LUS;
*Comparing RML, LLS vs. Others;

*LLL LLS LUS RLL RML RUL
*1   2   3   4   5   6;

%MACRO MONNY(Y);
proc glimmix data=toget method=quad; 
 class lobe; 
 model &y = lobe / link=cumlogit dist=multinomial solution; 
 random intercept / subject=id; 
 estimate 'RUL vs. LUS' lobe 0 0 1 0 0 -1 / cl;

 estimate 'RML vs. LLL' lobe 1 0 0 0 -1 0 / cl;
 estimate 'RML vs. LLS' lobe 0 1 0 0 -1 0 / cl;
 estimate 'RML vs. LUS' lobe 0 0 1 0 -1 0 / cl;
 estimate 'RML vs. RLL' lobe 0 0 0 1 -1 0 / cl;
 estimate 'RML vs. RUL' lobe 0 0 0 0 -1 1 / cl;

 estimate 'LLS vs. LLL' lobe 1 -1 0 0 0 0 / cl;
 estimate 'LLS vs. LUS' lobe 0 -1 1 0 0 0 / cl;
 estimate 'LLS vs. RLL' lobe 0 -1 0 1 0 0 / cl;
 estimate 'LLS vs. RML' lobe 0 -1 0 0 1 0 / cl;
 estimate 'LLS vs. RUL' lobe 0 -1 0 0 0 1 / cl; run; 
%MEND;
%monny(tib);
%monny(ln);
%monny(ggo);
%monny(consol);
%monny(bronch);
%monny(atel);



*Table 1 and 2 p-values;
proc freq data=togetb; table sex*group cavity*group / fisher; run;
proc glm data=togetb; class group; model age=group; run;

*Table 3;
data tib2; 	set tib; 		sum_tib=rul+rml+rll+lus+lls+lll;
data ln2; 	set ln; 		sum_ln=rul+rml+rll+lus+lls+lll;
data ggo2; 	set ggo; 		sum_ggo=rul+rml+rll+lus+lls+lll;
data consol2; set consol; 	sum_consol=rul+rml+rll+lus+lls+lll;
data bronch2; set bronch; 	sum_bronch=rul+rml+rll+lus+lls+lll;
data atel2; set atel; 		sum_atel=rul+rml+rll+lus+lls+lll;
data sums; merge togetb tib2 ln2 ggo2 consol2 bronch2 atel2; keep id cavity group sum_tib sum_ln sum_ggo sum_consol sum_bronch sum_atel; run;
proc glm data=sums; class group; model sum_tib=group; run;
proc glm data=sums; class group; model sum_ln=group; run;
proc glm data=sums; class group; model sum_ggo=group; run;
proc glm data=sums; class group; model sum_consol=group; run;
proc glm data=sums; class group; model sum_bronch=group; run;
proc glm data=sums; class group; model sum_atel=group; run;


*extra descriptive stats;
proc freq data=toget; table tib*lobe ln*lobe ggo*lobe consol*lobe bronch*lobe atel*lobe; run;

proc sgplot data=toget; scatter y=tib x=lobe / group=id; run;
proc freq data=toget; table tib*lobe ln*lobe ggo*lobe consol*lobe bronch*lobe atel*lobe thin*lobe thick*lobe; run;

proc sort data=sums; by group;
proc freq data=sums; by group; table sum_tib sum_ln sum_ggo sum_consol sum_bronch sum_atel; run;
proc means data=tib sum; by group; run;


*Example;
PROC NLMIXED DATA=toget; PARMS _Intrcpt1 -1 _Intrcpt2 drg .1 ; 

*linear predictors; 
eta1 = _Intrcpt1 + drg*(drug=1); 

* drug=2(bottom row) as reference; 
eta2 = _Intrcpt2 + drg*(drug=1); 
eta3 = _Intrcpt3 + drg*(drug=1); 

* ordered logit model estimates cumulative probabilities; 
cp1 = 1 / (1 + exp(-eta1)); cp2 = 1 / (1 + exp(-eta2)); cp3 = 1 / (1 + exp(-eta3)); 

* compute individual response probabilities by subtraction; 
p1 = cp1;   
p2 = cp2-cp1;   
p3 = cp3-cp2;  
p4 = 1-cp3; 

* Maximize likelihood equation; 
lk = (p1**(y=4)) * (p2**(y=3)) * (p3**(y=2)) * (p4**(y=1)); * descending; 
IF (lk > 1e-8) then lglk = log(lk); else lglk=-1e100; 

MODEL y ~ general(lglk); 
REPLICATE count; 
ESTIMATE ‘Odds Ratio: C15 & C60 vs Z100 & EC4’ EXP(drg); RUN; 
