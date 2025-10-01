/* read in data */

%let filepath = /home/u63545637/NTM/Cultures.xlsx;

proc import datafile="&filepath"
    out=tib
    dbms=xlsx
    replace;
    sheet="tib.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=ggo
    dbms=xlsx
    replace;
    sheet="ggo.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=cons
    dbms=xlsx
    replace;
    sheet="cons.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=bronch
    dbms=xlsx
    replace;
    sheet="bronch.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=atel
    dbms=xlsx
    replace;
    sheet="atel.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=ln
    dbms=xlsx
    replace;
    sheet="ln.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=thin
    dbms=xlsx
    replace;
    sheet="thin.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=thick
    dbms=xlsx
    replace;
    sheet="thick.long";
    getnames=yes;
run;

proc import datafile="&filepath"
    out=demo
    dbms=xlsx
    replace;
    sheet="demo";
    getnames=yes;
run;

/*******************************************/

/* TREE-IN-BUD */

proc sort data=tib;
    by id;
run;

proc glimmix data=tib method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

/*******************************************/

/* pathcode levels: 1=MAC, 2=MAB, 3=MAC+MAB, 4=Other */
%let alpha = %sysevalf(0.05/6);   /* Bonferroni within-lobe */

proc sort data=tib; by id; run;

/* --- RUL --- */
proc glimmix data=tib(where=(lobe='RUL')) method=quad order=internal asycov;
  class id rater pathcode;   /* GLM coding */
  model score = pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'RUL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RUL: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/* --- RLL --- */
proc glimmix data=tib(where=(lobe='RLL')) method=quad order=internal asycov;
  class id rater pathcode;
  model score = pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'RLL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RLL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RLL: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'RLL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'RLL: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'RLL: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/*******************************************/

proc glimmix data=tib method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* The full model remains the same */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    /* Add options to the SLICE statement for pairwise comparisons */
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* LARGE NODULES */

proc sort data=ln;
    by id;
run;

proc glimmix data=ln method=laplace order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* binary outcome using a logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=binomial
          link=logit
          solution;
    
    random intercept / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

/**************************/

/* pathcode levels: 1=MAC, 2=MAB, 3=MAC+MAB, 4=Other */
%let alpha = %sysevalf(0.05/3);   /* Bonferroni within-lobe */

proc sort data=ln; by id; run;

/* --- RUL --- */
proc glimmix data=ln(where=(lobe='RUL')) method=quad order=internal asycov;
  class id rater pathcode;   /* GLM coding */
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'RUL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
run;

/* --- RML --- */
proc glimmix data=ln(where=(lobe='RML')) method=quad order=internal asycov;
  class id rater pathcode;
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'RML: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RML: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RML: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
run;

/* --- LUS --- */
proc glimmix data=ln(where=(lobe='LUS')) method=quad order=internal asycov;
  class id rater pathcode;   
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept / subject=id;

  estimate 'LUS: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
run;

/* --- LLL --- */
proc glimmix data=ln(where=(lobe='LLL')) method=quad order=internal asycov;
  class id rater pathcode;
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'LLL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'LLL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'LLL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
run;

/*******************************************/

proc glimmix data=ln(where=(pathcode < 4)) method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    model score(event='1') = lobe pathcode lobe*pathcode rater
        / dist=binary
          link=logit
          solution;
    
    random intercept / subject=id;
    
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* GROUND-GLASS OPACITY */

proc sort data=ggo;
    by id;
run;

proc glimmix data=ggo method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

%let alpha = %sysevalf(0.05/6);   /* Bonferroni within-lobe */
proc glimmix data=ggo method=quad order=internal asycov;
  class id rater lobe pathcode;   /* GLM coding */
  model score = lobe pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/*******************************************/

proc glimmix data=ggo method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* The full model remains the same */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    /* Add options to the SLICE statement for pairwise comparisons */
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* CONSOLIDATION */

proc sort data=cons;
    by id;
run;

proc glimmix data=cons method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

%let alpha = %sysevalf(0.05/6);   /* Bonferroni within-lobe */
proc glimmix data=cons method=quad order=internal asycov;
  class id rater lobe pathcode;   /* GLM coding */
  model score = lobe pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/*******************************************/

proc glimmix data=cons method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* The full model remains the same */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    /* Add options to the SLICE statement for pairwise comparisons */
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* BRONCHIECTASIS */

proc sort data=bronch;
    by id;
run;

proc glimmix data=bronch method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

/*******************************************/

/* pathcode levels: 1=MAC, 2=MAB, 3=MAC+MAB, 4=Other */
%let alpha = %sysevalf(0.05/6);   /* Bonferroni within-lobe */

proc sort data=bronch; by id; run;

/* --- RUL --- */
proc glimmix data=bronch(where=(lobe='RUL')) method=quad order=internal asycov;
  class id rater pathcode;   /* GLM coding */
  model score = pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'RUL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RUL: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/* --- LLS --- */
proc glimmix data=bronch(where=(lobe='LLS')) method=quad order=internal asycov;
  class id rater pathcode;
  model score = pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'LLS: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'LLS: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'LLS: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'LLS: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'LLS: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'LLS: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/*******************************************/

proc glimmix data=bronch method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* The full model remains the same */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    /* Add options to the SLICE statement for pairwise comparisons */
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* ATELECTASIS */

proc sort data=atel;
    by id;
run;

proc glimmix data=atel method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

data atel_bin;
    set atel;
    if score in (1, 2, 3) then score = 1;
    else if score in (0) then score = 0;
run;

proc sort data=atel_bin;
    by id;
run;

proc glimmix data=atel_bin method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=binomial
          link=logit
          solution;
    
    random intercept rater / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

/*******************************************/

/* pathcode levels: 1=MAC, 2=MAB, 3=MAC+MAB, 4=Other */
%let alpha = %sysevalf(0.05/3);   /* Bonferroni within-lobe */

proc sort data=atel; by id; run;

/* --- RLL --- */
proc glimmix data=atel(where=(lobe='RLL')) method=quad order=internal asycov;
  class id rater pathcode;   /* GLM coding */
  model score = pathcode rater
        / dist=multinomial link=cumlogit solution ddfm=none;
  random intercept rater / subject=id;

  estimate 'RLL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RLL: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'RLL: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
run;

/*******************************************/

proc glimmix data=atel method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* The full model remains the same */
    model score = lobe pathcode lobe*pathcode rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept / subject=id;
    
    /* Add options to the SLICE statement for pairwise comparisons */
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* THICK WALL CAVITY */

proc sort data=thick;
    by id;
run;

proc glimmix data=thick method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* binary outcome using a logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=binomial
          link=logit
          solution;
    
    random intercept / subject=id;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

/*******************************************/

proc glimmix data=thick(where=(pathcode < 3)) method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    model score(event='1') = lobe pathcode lobe*pathcode rater
        / dist=binary
          link=logit
          solution;
    
    random intercept / subject=id;
    
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* THIN WALL CAVITY */

proc sort data=thin;
    by id;
run;

proc glimmix data=thin method=laplace order=internal ASYCOV;
    class id lobe rater pathcode;
    
    /* binary outcome using a logit link */
    model score = lobe pathcode lobe*pathcode rater
        / dist=binomial
          link=logit
          solution;
    
    slice lobe*pathcode / sliceby=lobe;
    
run;

/*******************************************/

proc glimmix data=thin(where=(pathcode < 3)) method=quad order=internal ASYCOV;
    class id lobe rater pathcode;
    
    model score(event='1') = lobe pathcode lobe*pathcode rater
        / dist=binary
          link=logit
          solution;
    
    random intercept / subject=id;
    
    slice lobe*pathcode / sliceby=lobe diff=all exp cl adjust=tukey;
    
run;

/*******************************************/

/* Sort the data by lobe to use a BY statement */
proc sort data=thin;
    by lobe;
run;

proc freq data=thin;
    /* Run a separate test for each lobe */
    by lobe;

    /* Create a 2x4 table of score by pathogen */
    tables score*pathcode / chisq;

    /* Request Fisher's Exact Test */
    exact fisher;

    title "Fisher's Exact Test of Score vs. Pathogen Group by Lobe";
run;

/*******************************************/

/* pathcode levels: 1=MAC, 2=MAB, 3=MAC+MAB, 4=Other */
%let alpha = %sysevalf(0.05/6);   /* Bonferroni within-lobe */

proc sort data=thin; by id; run;

/* --- RUL --- */
proc glimmix data=thin(where=(lobe='RUL')) method=laplace order=internal asycov;
  class id rater pathcode;   /* GLM coding */
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept / subject=id;

  estimate 'RUL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RUL: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'RUL: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'RUL: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/* --- RML --- */
proc glimmix data=thin(where=(lobe='RML')) method=laplace order=internal asycov;
  class id rater pathcode;
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept / subject=id;

  estimate 'RML: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'RML: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'RML: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'RML: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'RML: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'RML: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/* --- LLS --- */
proc glimmix data=thin(where=(lobe='LLS')) method=laplace order=internal asycov;
  class id rater pathcode;   /* GLM coding */
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept / subject=id;

  estimate 'LLS: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'LLS: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'LLS: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'LLS: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'LLS: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'LLS: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;

/* --- LLL --- */
proc glimmix data=thin(where=(lobe='LLL')) method=laplace order=internal asycov;
  class id rater pathcode;
  model score = pathcode rater
        / dist=binomial link=logit solution ddfm=none;
  random intercept  / subject=id;

  estimate 'LLL: MAB vs MAC'         pathcode -1  1  0  0 / exp cl alpha=&alpha;
  estimate 'LLL: MAC+MAB vs MAC'     pathcode -1  0  1  0 / exp cl alpha=&alpha;
  estimate 'LLL: Other vs MAC'       pathcode -1  0  0  1 / exp cl alpha=&alpha;
  estimate 'LLL: MAB vs MAC+MAB'     pathcode  0  1 -1  0 / exp cl alpha=&alpha;
  estimate 'LLL: MAB vs Other'       pathcode  0  1  0 -1 / exp cl alpha=&alpha;
  estimate 'LLL: MAC+MAB vs Other'   pathcode  0  0  1 -1 / exp cl alpha=&alpha;
run;