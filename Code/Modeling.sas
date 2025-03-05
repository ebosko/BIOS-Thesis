/*************************************************************************/
/* Step 1: Import the JW and VH sheets from the Excel file               */
/*************************************************************************/
proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_TIB
    dbms=xlsx
    replace;
    sheet="JW.TIB.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_TIB
    dbms=xlsx
    replace;
    sheet="VH.TIB.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_LN
    dbms=xlsx
    replace;
    sheet="JW.LN.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_LN
    dbms=xlsx
    replace;
    sheet="VH.LN.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_GGO
    dbms=xlsx
    replace;
    sheet="JW.GGO.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_GGO
    dbms=xlsx
    replace;
    sheet="VH.GGO.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_CONS
    dbms=xlsx
    replace;
    sheet="JW.CONS.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_CONS
    dbms=xlsx
    replace;
    sheet="VH.CONS.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_BRONCH
    dbms=xlsx
    replace;
    sheet="JW.BRONCH.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_BRONCH
    dbms=xlsx
    replace;
    sheet="VH.BRONCH.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_ATEL
    dbms=xlsx
    replace;
    sheet="JW.ATEL.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_ATEL
    dbms=xlsx
    replace;
    sheet="VH.ATEL.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_Thin
    dbms=xlsx
    replace;
    sheet="JW.Thin.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_Thin
    dbms=xlsx
    replace;
    sheet="VH.Thin.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=JW_Thick
    dbms=xlsx
    replace;
    sheet="JW.Thick.Long";
    getnames=yes;
run;

proc import datafile="/home/u63545637/NTM/2025.2.4.xlsx"
    out=VH_Thick
    dbms=xlsx
    replace;
    sheet="VH.Thick.Long";
    getnames=yes;
run;

/*************************************************************************/
/* Step 2: Combine the JW and VH data sets into one, adding a 'rater' var*/
/*************************************************************************/

data tib;
    set JW_TIB(in=a) VH_TIB(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data ln;
    set JW_LN(in=a) VH_LN(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data ggo;
    set JW_GGO(in=a) VH_GGO(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data cons;
    set JW_CONS(in=a) VH_CONS(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data bronch;
    set JW_BRONCH(in=a) VH_BRONCH(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data atel;
    set JW_ATEL(in=a) VH_ATEL(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data thin;
    set JW_Thin(in=a) VH_Thin(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

data thick;
    set JW_Thick(in=a) VH_Thick(in=b);
    if a then rater = "JW";
    if b then rater = "VH";
run;

/*************************************************************************/
/* Step 3: Fit the ordinal models with rater as a RANDOM effect          */
/*        (plus random intercept for subject)
/*************************************************************************/

/*************************************************************************/
/* TREE-IN-BUD                                                           */
/*************************************************************************/
proc glimmix data=tib method=quad;
    class Attribute rater;
    
    /* ordinal outcome using a cumulative logit link */
    model Value = Attribute
        / dist=multinomial
          link=cumlogit
          solution;
    
    random intercept rater / subject=newID type=vc;
 
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
/* Pairwise Comparisons                                                  */
/*************************************************************************/

%MACRO PAIRWISE(DATASET);
proc glimmix data=&DATASET method=quad;
    class Attribute rater;

    /* Ordinal outcome using a cumulative logit link */
    model Value = Attribute 
        / dist=multinomial
          link=cumlogit
          solution;

    random intercept rater / subject=newID type=vc;

    /* Pairwise comparisons using ESTIMATE */
	estimate 'RUL vs. RML' Attribute  0  0  0  0  1 -1 / cl;
	estimate 'RUL vs. RLL' Attribute  0  0  0  1  0 -1 / cl;
	estimate 'RUL vs. LUS' Attribute  0  0  1  0  0 -1 / cl;
	estimate 'RUL vs. LLS' Attribute  0  1  0  0  0 -1 / cl;
	estimate 'RUL vs. LLL' Attribute  1  0  0  0  0 -1 / cl;
	
	estimate 'RML vs. RLL' Attribute  0  0  0  1 -1  0 / cl;
	estimate 'RML vs. LUS' Attribute  0  0  1  0 -1  0 / cl;
	estimate 'RML vs. LLS' Attribute  0  1  0  0 -1  0 / cl;
	estimate 'RML vs. LLL' Attribute  1  0  0  0 -1  0 / cl;
	estimate 'RML vs. RUL' Attribute  0  0  0  0 -1  1 / cl;
	
	estimate 'RLL vs. LUS' Attribute  0  0  1 -1  0  0 / cl;
	estimate 'RLL vs. LLS' Attribute  0  1  0 -1  0  0 / cl;
	estimate 'RLL vs. LLL' Attribute  1  0  0 -1  0  0 / cl;
	
	estimate 'LUS vs. LLS' Attribute  0  1 -1  0  0  0 / cl;
	estimate 'LUS vs. LLL' Attribute  1  0 -1  0  0  0 / cl;
	
	estimate 'LLS vs. LLL' Attribute  1 -1  0  0  0  0 / cl;



run;
%MEND;

%PAIRWISE(tib)

/* pairwise with output */

%MACRO PAIRWISE(DATASET);
    /* Capture the results from PROC GLIMMIX */
    ods output Estimates=glimmix_results;

    proc glimmix data=&DATASET method=quad;
        class Attribute rater;

        /* Ordinal outcome using a cumulative logit link */
        model Value = Attribute 
            / dist=multinomial
              link=cumlogit
              solution;

        random intercept rater / subject=newID type=vc;

        
        /* Pairwise comparisons using ESTIMATE */
		estimate 'RUL vs. RML' Attribute  0  0  0  0  1 -1 / cl;
		estimate 'RUL vs. RLL' Attribute  0  0  0  1  0 -1 / cl;
		estimate 'RUL vs. LUS' Attribute  0  0  1  0  0 -1 / cl;
		estimate 'RUL vs. LLS' Attribute  0  1  0  0  0 -1 / cl;
		estimate 'RUL vs. LLL' Attribute  1  0  0  0  0 -1 / cl;
		
		estimate 'RML vs. RLL' Attribute  0  0  0  1 -1  0 / cl;
		estimate 'RML vs. LUS' Attribute  0  0  1  0 -1  0 / cl;
		estimate 'RML vs. LLS' Attribute  0  1  0  0 -1  0 / cl;
		estimate 'RML vs. LLL' Attribute  1  0  0  0 -1  0 / cl;
		
		estimate 'RLL vs. LUS' Attribute  0  0  1 -1  0  0 / cl;
		estimate 'RLL vs. LLS' Attribute  0  1  0 -1  0  0 / cl;
		estimate 'RLL vs. LLL' Attribute  1  0  0 -1  0  0 / cl;
		
		estimate 'LUS vs. LLS' Attribute  0  1 -1  0  0  0 / cl;
		estimate 'LUS vs. LLL' Attribute  1  0 -1  0  0  0 / cl;
		
		estimate 'LLS vs. LLL' Attribute  1 -1  0  0  0  0 / cl;


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
/* LARGE NODULES with random intercept and rater as random               */
/*************************************************************************/
proc glimmix data=ln method=quad order=data; 
    class Attribute rater;                   

    model Value = Attribute
        / dist=binomial link=logit solution;
    random intercept rater / subject=newID type=vc;

    lsmeans Attribute / ilink diff oddsratio cl adjust=tukey;
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
/* THIN WALL CAVITY - Random Intercept, Rater as Fixed Effect            */
/*************************************************************************/
proc glimmix data=thin method=quad(qpoints=5);
    class Attribute rater;
    model Value = Attribute rater
        / dist=binomial
          link=logit
          solution;
    random intercept / subject=newID type=vc;

    lsmeans Attribute / ilink diff oddsratio cl adjust=tukey;
    ods output diffs=LobeDiffs_thin;
run;

data SigDiffs_thin;
    set LobeDiffs_thin;
    where probt < 0.05;
run;

/*************************************************************************/
/* THICK WALL CAVITY - Random Intercept, Rater as Fixed                  */
/*************************************************************************/
proc glimmix data=thick method=quad;
    class Attribute rater;
    model Value = Attribute rater
        / dist=binomial
          link=logit
          solution;
    random intercept / subject=newID type=vc;

    lsmeans Attribute / ilink diff oddsratio cl adjust=tukey;
    ods output diffs=LobeDiffs_thick;
run;

data SigDiffs_thick;
    set LobeDiffs_thick;
    where probt < 0.05;
run;