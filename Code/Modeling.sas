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
    by newID;
run;

proc glimmix data=tib method=quad;
    class Attribute rater;
    
    /* ordinal outcome using a cumulative logit link */
    model Value = Attribute rater
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=newID solution;
 
run;

/*************************************************************************/
/* GROUND GLASS OPACITIES                                                */
/*************************************************************************/
proc glimmix data=ggo method=quad;
    class Attribute rater;
    
    /* ordinal outcome using a cumulative logit link */
    model Value = Attribute
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=newID type=vc;
 
run;

/*************************************************************************/
/* CONSOLIDATIONS                                                        */
/*************************************************************************/
proc glimmix data=cons method=quad;
    class Attribute rater;
    
    /* ordinal outcome using a cumulative logit link */
    model Value = Attribute
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=newID type=vc;
 
run;

/*************************************************************************/
/* BRONCHIECTASIS                                                        */
/*************************************************************************/
proc glimmix data=bronch method=quad;
    class Attribute rater;
    
    /* ordinal outcome using a cumulative logit link */
    model Value = Attribute
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=newID type=vc;
 
run;

/*************************************************************************/
/* ATELECTASIS                                                           */
/*************************************************************************/
proc glimmix data=atel method=quad;
    class Attribute rater;
    
    /* ordinal outcome using a cumulative logit link */
    model Value = Attribute
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=newID type=vc;
 
run;

/*************************************************************************/
/* Step 4: Fit the logistic models with rater as a RANDOM effect         */
/*        (plus random intercept for subject)
/*************************************************************************/

/*************************************************************************/
/* LARGE NODULES                                                         */
/*************************************************************************/

proc glimmix data=ln method=quad;
    class Attribute rater;

    model Value = Attribute
        / dist=binomial
          link=logit
          solution;

    random intercept rater / subject=newID type=vc;
    
    
run;

/*************************************************************************/
/* THIN WALL CAVITY - Random Intercept Only                              */
/*************************************************************************/

proc glimmix data=thin method=laplace;
    class Attribute rater;

    model Value = Attribute
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=newID type=vc;
run;

/*************************************************************************/
/* THIN WALL CAVITY - Random Intercept and Rater Fixed Effect            */
/*************************************************************************/

proc glimmix data=thin method=quad(qpoints=5);
    class Attribute rater;

    model Value = Attribute rater
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=newID type=vc;
run;

/*************************************************************************/
/* THICK WALL CAVITY - Random Intercept Only                             */
/*************************************************************************/

proc glimmix data=thick method=quad;
    class Attribute rater;

    model Value = Attribute
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=newID type=vc;
run;

/*************************************************************************/
/* THICK WALL CAVITY - Random Intercept and Rater Fixed Effect           */
/*************************************************************************/

proc glimmix data=thick method=quad;
    class Attribute rater;

    model Value = Attribute rater
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=newID type=vc;
run;

/*************************************************************************/
/* pairwise comparisons with output                                      */
/*************************************************************************/

%MACRO PAIRWISE(DATASET);
	/* sort by subject */
	proc sort data=&DATASET out = SortedData;
		by newID;
	run;

    /* capture the results from PROC GLIMMIX */
    ods output Estimates=glimmix_results;

    proc glimmix data=SortedData method=quad order = data;
        class Attribute rater;

        /* ordinal outcome using a cumulative logit link */
        model Value = Attribute rater
            / dist=multinomial
              link=cumlogit
              solution;

        random intercept rater / subject=newID type=vc solution;

        
        /* Columns: RUL   RML   RLL   LUS   LLS   LLL */

		/* RUL compared to others */
		estimate 'RUL vs RML' Attribute -1  1  0  0  0  0 / cl;
		estimate 'RUL vs RLL' Attribute -1  0  1  0  0  0 / cl;
		estimate 'RUL vs LUS' Attribute -1  0  0  1  0  0 / cl;
		estimate 'RUL vs LLS' Attribute -1  0  0  0  1  0 / cl;
		estimate 'RUL vs LLL' Attribute -1  0  0  0  0  1 / cl;
		
		/* RML compared to RLL, LUS, LLS, LLL */
		estimate 'RML vs RLL' Attribute  0  -1 1  0  0  0 / cl;
		estimate 'RML vs LUS' Attribute  0  -1  0 1  0  0 / cl;
		estimate 'RML vs LLS' Attribute  0  -1  0  0 1  0 / cl;
		estimate 'RML vs LLL' Attribute  0  -1  0  0  0 1 / cl;
		
		/* RLL compared to LUS, LLS, LLL */
		estimate 'RLL vs LUS' Attribute  0  0  -1 1  0  0 / cl;
		estimate 'RLL vs LLS' Attribute  0  0  -1  0 1  0 / cl;
		estimate 'RLL vs LLL' Attribute  0  0  -1  0  0 1 / cl;
		
		/* LUS compared to LLS, LLL */
		estimate 'LUS vs LLS' Attribute  0  0  0  -1 1  0 / cl;
		estimate 'LUS vs LLL' Attribute  0  0  0  -1  0 1 / cl;
		
		/* LLS compared to LLL */
		estimate 'LLS vs LLL' Attribute  0  0  0  0  -1 1 / cl;


    run;

    ods output close;

    /* Process the results: Extract only significant comparisons */
    data significant_results;
        set glimmix_results;
        if Probt < 0.05; 
        Odds_Ratio = exp(Estimate); 
        keep Label Odds_Ratio Probt; 
        rename Probt=P_Value Label=Comparison;
    run;

    /* Print the formatted table */
    proc print data=significant_results label noobs;
        title "Significant Pairwise Comparisons for &DATASET";
        label Comparison="Comparison"
              Odds_Ratio="Exponentiated Estimate (Odds Ratio)"
              P_Value="P-Value";
    run;

%MEND;

%PAIRWISE(tib)
%PAIRWISE(ggo)
%PAIRWISE(cons)
%PAIRWISE(bronch)
%PAIRWISE(atel)

/* Pairwise Comparisons for Binary Outcome Models */

/*************************************************************************/
/* LARGE NODULES with random intercept and rater as random and fixed     */
/*************************************************************************/
proc sort data=ln;
    by newID;
run;

proc glimmix data=ln method=quad order=data; 
    class Attribute rater;                   

    model Value = Attribute 
        / dist=binomial link=logit solution;
    random intercept rater / subject=newID solution type=vc;

    lsmeans Attribute / ilink diff oddsratio cl;
    ods output diffs=LobeDiffs_ln;
run;

data LobeDiffs_ln_sig;
    set LobeDiffs_ln;
    if AdjP < 0.05;
run;

proc print data=LobeDiffs_ln_sig;
    title "Significant Pairwise Comparisons of Lobes for Large Nodules";
    var Attribute _Attribute Estimate OddsRatio AdjP;
run;

/*************************************************************************/
/* THIN WALL CAVITY - Random Intercept, Rater as Fixed and random        */
/*************************************************************************/
proc sort data=thin;
    by newID;
run;

proc glimmix data=thin method=quad;
    class Attribute rater;
    model Value = Attribute rater
        / dist=binomial
          link=logit
          solution;
    random intercept rater/ subject=newID solution type=vc;

    lsmeans Attribute / ilink diff oddsratio cl;
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
    by newID;
run;

proc glimmix data=thick method=quad;
    class Attribute rater;
    model Value = Attribute rater
        / dist=binomial
          link=logit
          solution;
    random intercept / subject=newID solution type=vc;

    lsmeans Attribute / ilink diff oddsratio cl;
    ods output diffs=LobeDiffs_thick;
run;

data SigDiffs_thick;
    set LobeDiffs_thick;
    where probt < 0.05;
run;