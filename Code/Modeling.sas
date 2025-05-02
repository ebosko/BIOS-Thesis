/*************************************************************************/
/* Import the combined rater sheets from the updated Excel file          */
/*************************************************************************/

%let filepath = /home/u63545637/NTM/2025.4.2.xlsx;

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

/*need to dichotomize ggo since model no longer fits */

data ggo_bin;
    set ggo;
    if score in (1, 2, 3) then score = 1;
    else if score in (0) then score = 0;
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


/*************************************************************************/
/*         Fit the ordinal models with rater as a RANDOM effect          */
/*        (plus random intercept for subject)
/*************************************************************************/

/*************************************************************************/
/* TREE-IN-BUD                                                           */
/*************************************************************************/
proc sort data=tib;
    by id;
run;

proc glimmix data=tib method=quad;
    class lobe rater;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id solution;
 
run;

/*************************************************************************/
/* GROUND GLASS OPACITIES                                                */
/*************************************************************************/
proc sort data=ggo_bin;
    by id;
run;

proc glimmix data=ggo_bin method=quad;
    class lobe rater;

    model score = lobe rater
        / dist=binomial
          link=logit
          solution;

    random intercept rater / subject=id solution;
    
    
run;

/*************************************************************************/
/* CONSOLIDATIONS                                                        */
/*************************************************************************/
proc sort data=cons;
    by id;
run;

proc glimmix data=cons method=quad;
    class lobe rater;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id solution;
 
run;

/*************************************************************************/
/* BRONCHIECTASIS                                                        */
/*************************************************************************/
proc sort data=bronch;
    by id;
run;

proc glimmix data=bronch method=quad;
    class lobe rater;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=id solution;
 
run;

/*************************************************************************/
/* ATELECTASIS                                                           */
/*************************************************************************/
proc sort data = atel;
	by id;
run;

proc glimmix data=atel method=quad;
    class lobe rater;
    
    /* ordinal outcome using a cumulative logit link */
    model score = lobe rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept / subject=id solution;
 
run;

/*************************************************************************/
/* Step 4: Fit the logistic models with rater as a RANDOM effect         */
/*        (plus random intercept for subject)
/*************************************************************************/

/*************************************************************************/
/* LARGE NODULES                                                         */
/*************************************************************************/
proc sort data=ln;
    by id;
run;

proc glimmix data=ln method=quad;
    class lobe rater;

    model score = lobe rater
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=id solution;
    
run;

proc freq data=ln;
    tables score;
run;

/*************************************************************************/
/* THIN WALL CAVITY                                                      */
/*************************************************************************/
proc sort data=thin;
    by id;
run;

proc glimmix data=thin method=quad(qpoints=30);
    class lobe rater;

    model score = lobe rater
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=id solution;
run;

/*************************************************************************/
/* THICK WALL CAVITY                                                     */
/*************************************************************************/
proc sort data=thick;
    by id;
run;

proc glimmix data=thick method=quad;
    class lobe rater;

    model score = lobe rater
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=id solution;
run;

/*************************************************************************/
/* pairwise comparisons with output for full ordinal                     */
/*************************************************************************/

%MACRO PAIRWISE(DATASET);
	/* sort by subject */
	proc sort data=&DATASET out = SortedData;
		by id;
	run;

    /* capture the results from PROC GLIMMIX */
    ods output Estimates=glimmix_results;

    proc glimmix data=SortedData method=quad order = data;
        class lobe rater;

        /* ordinal outcome using a cumulative logit link */
        model score = lobe rater
            / dist=multinomial
              link=cumlogit
              solution;

        random intercept rater / subject=id type=vc solution;

        
        /* Columns: RUL   RML   RLL   LUS   LLS   LLL */

		/* RUL compared to others */
		estimate 'RUL vs RML' lobe -1  1  0  0  0  0 / cl;
		estimate 'RUL vs RLL' lobe -1  0  1  0  0  0 / cl;
		estimate 'RUL vs LUS' lobe -1  0  0  1  0  0 / cl;
		estimate 'RUL vs LLS' lobe -1  0  0  0  1  0 / cl;
		estimate 'RUL vs LLL' lobe -1  0  0  0  0  1 / cl;
		
		/* RML compared to RLL, LUS, LLS, LLL */
		estimate 'RML vs RLL' lobe  0  -1  1  0  0  0 / cl;
		estimate 'RML vs LUS' lobe  0  -1  0  1  0  0 / cl;
		estimate 'RML vs LLS' lobe  0  -1  0  0  1  0 / cl;
		estimate 'RML vs LLL' lobe  0  -1  0  0  0  1 / cl;
		
		/* RLL compared to LUS, LLS, LLL */
		estimate 'RLL vs LUS' lobe  0  0  -1  1  0  0 / cl;
		estimate 'RLL vs LLS' lobe  0  0  -1  0  1  0 / cl;
		estimate 'RLL vs LLL' lobe  0  0  -1  0  0  1 / cl;
		
		/* LUS compared to LLS, LLL */
		estimate 'LUS vs LLS' lobe  0  0  0  -1  1  0 / cl;
		estimate 'LUS vs LLL' lobe  0  0  0  -1  0  1 / cl;
		
		/* LLS compared to LLL */
		estimate 'LLS vs LLL' lobe  0  0  0   0 -1  1 / cl;


    run;

    ods output close;

    /* Process the results: Extract only significant comparisons */
    data significant_results;
        set glimmix_results;
        if Probt < 0.05; 
        Odds_Ratio = exp(Estimate); 
        keep Label Odds_Ratio Probt; 
        rename Probt=P_score Label=Comparison;
    run;

    /* Print the formatted table */
    proc print data=significant_results label noobs;
        title "Significant Pairwise Comparisons for &DATASET";
        label Comparison="Comparison"
              Odds_Ratio="Exponentiated Estimate (Odds Ratio)"
              P_score="P-score";
    run;

%MEND;

%PAIRWISE(tib)
%PAIRWISE(cons)
%PAIRWISE(bronch)
%PAIRWISE(atel)

/* Pairwise Comparisons for Binary Outcome Models */

/*************************************************************************/
/* LARGE NODULES with random intercept and rater as random and fixed     */
/*************************************************************************/
proc sort data=ln;
    by id;
run;

proc glimmix data=ln method=quad order=data; 
    class lobe rater;                   

    model score = lobe 
        / dist=binomial link=logit solution;
    random intercept rater / subject=id solution type=vc;

    lsmeans lobe / ilink diff oddsratio cl;
    ods output diffs=LobeDiffs_ln;
run;

data LobeDiffs_ln_sig;
    set LobeDiffs_ln;
    if AdjP < 0.05;
run;

proc print data=LobeDiffs_ln_sig;
    title "Significant Pairwise Comparisons of Lobes for Large Nodules";
    var lobe _lobe Estimate OddsRatio AdjP;
run;

/*************************************************************************/
/* THIN WALL CAVITY - Random Intercept, Rater as Fixed and random        */
/*************************************************************************/
proc sort data=thin;
    by id;
run;

proc glimmix data=thin method=quad;
    class lobe rater;
    model score = lobe rater
        / dist=binomial
          link=logit
          solution;
    random intercept rater/ subject=id solution type=vc;

    lsmeans lobe / ilink diff oddsratio cl;
    ods output diffs=LobeDiffs_thin;
run;

data SigDiffs_thin;
    set LobeDiffs_thin;
    where probt < 0.05;
run;

/*************************************************************************/
/* THICK WALL CAVITY - Random Intercept, Rater as Fixed                  */
/*************************************************************************/
proc sort data=thick;
    by id;
run;

proc glimmix data=thick method=quad;
    class lobe rater;
    model score = lobe rater
        / dist=binomial
          link=logit
          solution;
    random intercept / subject=id solution type=vc;

    lsmeans lobe / ilink diff oddsratio cl;
    ods output diffs=LobeDiffs_thick;
run;

data SigDiffs_thick;
    set LobeDiffs_thick;
    where probt < 0.05;
run;