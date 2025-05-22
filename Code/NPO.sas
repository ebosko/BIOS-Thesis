%let filepath = /home/u63545637/NTM/2025.4.2.xlsx;

proc import datafile="&filepath"
    out=tib
    dbms=xlsx
    replace;
    sheet="tib.long";
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

/*Fit the NPO models*/

/*************************************************************************/
/* TREE-IN-BUD                                                           */
/*************************************************************************/
proc sort data=tib;
    by id;
run;

proc glimmix data=tib method=quad order=data;
    class lobe rater;
    
    /* ordinal outcome using a generalized logit link */
    model score = lobe rater
        / dist=multinomial
          link=glogit
          solution;
    
    random intercept rater / subject=id group=score solution;
 
run;

/*************************************************************************/
/* CONSOLIDATIONS                                                        */
/*************************************************************************/
proc sort data=cons;
    by id;
run;

proc glimmix data=cons method=quad order=data;
    class lobe rater;
    
    /* ordinal outcome using a generalized logit link */
    model score = lobe rater
        / dist=multinomial
          link=glogit
          solution;
    
    random intercept rater / subject=id group=score solution;
 
run;

/*************************************************************************/
/* BRONCHIECTASIS                                                        */
/*************************************************************************/
proc sort data=bronch;
    by id;
run;

proc glimmix data=bronch method=quad order=data;
    class lobe rater;
    
    /* ordinal outcome using a generalized logit link */
    model score = lobe rater
        / dist=multinomial
          link=glogit
          solution;
    
    random intercept rater / subject=id group=score solution;
 
run;

/*************************************************************************/
/* ATELECTASIS                                                           */
/*************************************************************************/
proc sort data=atel;
    by id;
run;

proc glimmix data=atel method=quad order=data;
    class lobe rater;
    
    /* ordinal outcome using a generalized logit link */
    model score = lobe rater
        / dist=multinomial
          link=glogit
          solution;
    
    random intercept rater / subject=id group=score solution;
 
run;