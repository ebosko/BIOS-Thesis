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

proc glimmix data=tib method=quad order=data;
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

proc glimmix data=ggo_bin method=quad order=data;
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

proc glimmix data=cons method=quad order=data;
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

proc glimmix data=bronch method=quad order=data;
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

proc glimmix data=atel method=quad order=data;
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

proc glimmix data=ln method=quad order=data;
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

proc glimmix data=thin method=quad(qpoints=30) order=data;
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

proc glimmix data=thick method=quad order=data;
    class lobe rater;

    model score = lobe rater
        / dist=binomial
          link=logit
          solution;

    random intercept / subject=id solution;
run;

/*************************************************************************/
/* pairwise comparisons with output for full random effects              */
/*************************************************************************/

/**************************************for tib, cons, bronch********************************/

%MACRO PAIRWISE(DATASET, alpha = 0.05);
	/* sort by subject */
	proc sort data=&DATASET out = SortedData;
		by id;
	run;

    /* capture the results from PROC GLIMMIX */
    ods select none;
   	ods output Estimates = _rawEst_; 

    proc glimmix data=SortedData method=quad order=data;
        class lobe rater;

        /* ordinal outcome using a cumulative logit link */
        model score = lobe rater
            / dist=multinomial
              link=cumlogit
              solution;

        random intercept rater / subject=id solution;

        
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
	ods select all;
    ods output close;

    /* Keep label, estimate, and raw p‑value */
   data _pvals_;
      set _rawEst_(keep=Label Estimate Lower Upper Probt);
      where Label ne '';                 /* keep only ESTIMATE rows */
      Comparison = Label;
      raw_p      = Probt;
      
      Odds_Ratio=exp(Estimate);
      OR_LowerCI=exp(Lower);
      OR_UpperCI=exp(Upper);
   run;

   /*Benjamini–Hochberg adjustment */
   proc sort data=_pvals_;  by raw_p; run;

   data _bh1_;
      set _pvals_ nobs=m;
      by raw_p;
      rank + 1;
      initial_bh = raw_p * m / rank;     /* p_(i) * m / i           */
   run;

   /* enforce monotone non‑decreasing rule */
   proc sort data=_bh1_;  by descending raw_p; run;

   data _bh2_;
      set _bh1_;
      retain bh_adj 1;                   /* running minimum         */
      bh_adj = min(bh_adj, initial_bh);
   run;

   proc sort data=_bh2_;  by Comparison; run;

   /*final table and significance flag*/
   data final_FDR;
      set _bh2_;
      Odds_Ratio = exp(Estimate);
      OR_LowerCI=exp(Lower);
      OR_UpperCI=exp(Upper);
      FDR_sig    = (bh_adj < &alpha);
      label raw_p   = 'Unadjusted p'
            bh_adj  = 'BH‑adjusted p'
            Odds_Ratio = 'Exp(Estimate)'
            OR_LowerCI='Lower 95% CI (OR)'
      		OR_UpperCI='Upper 95% CI (OR)'
      		FDR_sig = 'Significant?';
   run;
   
   proc sort data=final_FDR; by bh_adj; run;

   title "Benjamini–Hochberg FDR‑adjusted pairwise comparisons (&dataset)";
   proc print data=final_FDR noobs label;
      var Comparison Odds_Ratio OR_LowerCI OR_UpperCI raw_p bh_adj FDR_sig;
   run;

%mend PAIRWISE;

%PAIRWISE(tib)
%PAIRWISE(cons)
%PAIRWISE(bronch)

/******************** for ggo ************************************/

%MACRO PAIRWISE_ggo(DATASET, alpha = 0.05);
	/* sort by subject */
	proc sort data=&DATASET out = SortedData;
		by id;
	run;
	
	ods exclude all;
		ods output Estimates = _rawEst_;
	ods select ClassLevels;


    proc glimmix data=SortedData method=quad order=data;
        class lobe rater;

        /* binary outcome using a logit link */
        model score = lobe rater
            / dist=binomial
              link=logit
              solution;

        random intercept rater / subject=id solution;

        
        /* Columns: RUL   RML   RLL   LUS   LLS   LLL */

		/* RUL compared to others */
		estimate 'RUL vs RML' lobe 1  -1  0  0  0  0 / cl;
		estimate 'RUL vs RLL' lobe 1  0  -1  0  0  0 / cl;
		estimate 'RUL vs LUS' lobe 1  0  0  -1  0  0 / cl;
		estimate 'RUL vs LLS' lobe 1  0  0  0  -1  0 / cl;
		estimate 'RUL vs LLL' lobe 1  0  0  0  0  -1 / cl;
		
		/* RML compared to RLL, LUS, LLS, LLL */
		estimate 'RML vs RLL' lobe  0  1  -1  0  0  0 / cl;
		estimate 'RML vs LUS' lobe  0  1  0  -1  0  0 / cl;
		estimate 'RML vs LLS' lobe  0  1  0  0  -1  0 / cl;
		estimate 'RML vs LLL' lobe  0  1  0  0  0  -1 / cl;
		
		/* RLL compared to LUS, LLS, LLL */
		estimate 'RLL vs LUS' lobe  0  0  1  -1  0  0 / cl;
		estimate 'RLL vs LLS' lobe  0  0  1  0  -1  0 / cl;
		estimate 'RLL vs LLL' lobe  0  0  1  0  0  -1 / cl;
		
		/* LUS compared to LLS, LLL */
		estimate 'LUS vs LLS' lobe  0  0  0  1  -1  0 / cl;
		estimate 'LUS vs LLL' lobe  0  0  0  1  0  -1 / cl;
		
		/* LLS compared to LLL */
		estimate 'LLS vs LLL' lobe  0  0  0   0 1  -1 / cl;


    run;

    /* Keep label, estimate, and raw p‑value */
   data _pvals_;
      set _rawEst_(keep=Label Estimate Lower Upper Probt);
      where Label ne '';                 /* keep only ESTIMATE rows */
      Comparison = Label;
      raw_p      = Probt;
      
      Odds_Ratio=exp(Estimate);
      OR_LowerCI=exp(Lower);
      OR_UpperCI=exp(Upper);
   run;

   /*Benjamini–Hochberg adjustment */
   proc sort data=_pvals_;  by raw_p; run;

   data _bh1_;
      set _pvals_ nobs=m;
      by raw_p;
      rank + 1;
      initial_bh = raw_p * m / rank;     /* p_(i) * m / i           */
   run;

   /* enforce monotone non‑decreasing rule */
   proc sort data=_bh1_;  by descending raw_p; run;

   data _bh2_;
      set _bh1_;
      retain bh_adj 1;                   /* running minimum         */
      bh_adj = min(bh_adj, initial_bh);
   run;

   proc sort data=_bh2_;  by Comparison; run;

   /*final table and significance flag*/
   data final_FDR;
      set _bh2_;
  
      FDR_sig    = (bh_adj < &alpha);
      label raw_p   = 'Unadjusted p'
            bh_adj  = 'BH‑adjusted p'
            Odds_Ratio = 'Exp(Estimate)'
            OR_LowerCI='Lower 95% CI (OR)'
      		OR_UpperCI='Upper 95% CI (OR)'
      		FDR_sig = 'Significant?';
   run;
   
   proc sort data=final_FDR; by bh_adj; run;

   title "Benjamini–Hochberg FDR‑adjusted pairwise comparisons (&dataset)";
   proc print data=final_FDR noobs label;
      var Comparison Odds_Ratio OR_LowerCI OR_UpperCI raw_p bh_adj FDR_sig;
   run;

%mend PAIRWISE_ggo;

%PAIRWISE_ggo(ggo_bin)

/*************************************************************************/
/* pairwise comparisons with output for random subject only/bin. variable*/
/*************************************************************************/

/**************************************for atel, ln, thick********************************/

%MACRO PAIRWISE_reduced(DATASET, alpha = 0.05);
	/* sort by subject */
	proc sort data=&DATASET out = SortedData;
		by id;
	run;

    /* capture the results from PROC GLIMMIX */
    ods exclude all;
    	ods output Estimates = _rawEst_;
   	ods select ClassLevels; 

    proc glimmix data=SortedData method=quad(qpoints=30) order=data;
        class lobe rater;

        /* binary outcome using a logit link */
        model score = lobe rater
            / dist=binomial
              link=logit
              solution;

        random intercept / subject=id solution;

        
        /* Columns: RUL   RML   RLL   LUS   LLS   LLL */

		/* RUL compared to others */
		estimate 'RUL vs RML' lobe 1  -1  0  0  0  0 / cl;
		estimate 'RUL vs RLL' lobe 1  0  -1  0  0  0 / cl;
		estimate 'RUL vs LUS' lobe 1  0  0  -1  0  0 / cl;
		estimate 'RUL vs LLS' lobe 1  0  0  0  -1  0 / cl;
		estimate 'RUL vs LLL' lobe 1  0  0  0  0  -1 / cl;
		
		/* RML compared to RLL, LUS, LLS, LLL */
		estimate 'RML vs RLL' lobe  0  1  -1  0  0  0 / cl;
		estimate 'RML vs LUS' lobe  0  1  0  -1  0  0 / cl;
		estimate 'RML vs LLS' lobe  0  1  0  0  -1  0 / cl;
		estimate 'RML vs LLL' lobe  0  1  0  0  0  -1 / cl;
		
		/* RLL compared to LUS, LLS, LLL */
		estimate 'RLL vs LUS' lobe  0  0  1  -1  0  0 / cl;
		estimate 'RLL vs LLS' lobe  0  0  1  0  -1  0 / cl;
		estimate 'RLL vs LLL' lobe  0  0  1  0  0  -1 / cl;
		
		/* LUS compared to LLS, LLL */
		estimate 'LUS vs LLS' lobe  0  0  0  1  -1  0 / cl;
		estimate 'LUS vs LLL' lobe  0  0  0  1  0  -1 / cl;
		
		/* LLS compared to LLL */
		estimate 'LLS vs LLL' lobe  0  0  0   0 1  -1 / cl;


    run;
	ods output close;
    	ods select all;

    /* Keep label, estimate, and raw p‑value */
   data _pvals_;
      set _rawEst_(keep=Label Estimate Lower Upper Probt);
      where Label ne '';                 /* keep only ESTIMATE rows */
      Comparison = Label;
      raw_p      = Probt;
      
      Odds_Ratio=exp(Estimate);
      OR_LowerCI=exp(Lower);
      OR_UpperCI=exp(Upper);
   run;

   /*Benjamini–Hochberg adjustment */
   proc sort data=_pvals_;  by raw_p; run;

   data _bh1_;
      set _pvals_ nobs=m;
      by raw_p;
      rank + 1;
      initial_bh = raw_p * m / rank;     /* p_(i) * m / i           */
   run;

   /* enforce monotone non‑decreasing rule */
   proc sort data=_bh1_;  by descending raw_p; run;

   data _bh2_;
      set _bh1_;
      retain bh_adj 1;                   /* running minimum         */
      bh_adj = min(bh_adj, initial_bh);
   run;

   proc sort data=_bh2_;  by Comparison; run;

   /*final table and significance flag*/
   data final_FDR;
      set _bh2_;
      FDR_sig    = (bh_adj < &alpha);
      label raw_p   = 'Unadjusted p'
            bh_adj  = 'BH‑adjusted p'
            Odds_Ratio = 'Exp(Estimate)'
            OR_LowerCI='Lower 95% CI (OR)'
      		OR_UpperCI='Upper 95% CI (OR)'
      		FDR_sig = 'Significant?';
   run;
   
   proc sort data=final_FDR; by bh_adj; run;

   title "Benjamini–Hochberg FDR‑adjusted pairwise comparisons (&dataset)";
   proc print data=final_FDR noobs label;
      var Comparison Odds_Ratio OR_LowerCI OR_UpperCI raw_p bh_adj FDR_sig;
   run;

%mend PAIRWISE_reduced;

%PAIRWISE_reduced(atel)
%PAIRWISE_reduced(ln)
%PAIRWISE_reduced(thick)
%PAIRWISE_reduced(thin) /* change qpoints to 30 to fit this*/





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

/**************************************************************************************************/
proc sort data=ggo_bin;
		by id;
	run;


proc glimmix data=ggo_bin method=quad order=data;
    class lobe rater;

    /* ordinal outcome using a cumulative logit link */
    model score = lobe rater
        / dist=binomial
          link=logit
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

proc glimmix data=ggo_bin method=quad order=data;
    class lobe rater;

    model score = lobe rater
        / dist=binomial
          link=logit
          solution;

    random intercept rater / subject=id type=vc solution;

    /* All pairwise comparisons among lobe levels */
    lsmeans lobe / diff oddsratio cl;
    ods output diffs=LobeDiffs_ggo;   /* capture the pairwise table */

run;

/* Keep one direction only (e.g., A vs. B, not B vs. A) */
data LobeDiffs_ggo_unique;
    set LobeDiffs_ggo;
    if lobe < _lobe; /* keep only one of each symmetric pair */
run;

proc print data=LobeDiffs_ggo_unique label noobs;
    var lobe _lobe Estimate StdErr Probt OddsRatio LowerOR UpperOR;
    label lobe = "Lobe 1"
          _lobe = "Lobe 2"
          Estimate = "Log Odds Ratio"
          Probt = "P-value"
          OddsRatio = "Odds Ratio"
          LowerOR = "Lower 95% CI"
          UpperOR = "Upper 95% CI";
run;




/***************** non-random rater models for those that fit random rater *****************/

%MACRO PAIRWISE_reduced2(DATASET, alpha = 0.05);
	/* sort by subject */
	proc sort data=&DATASET out = SortedData;
		by id;
	run;

    /* capture the results from PROC GLIMMIX */
    ods exclude all;
    	ods output Estimates = _rawEst_;
   	ods select ClassLevels; 

    proc glimmix data=SortedData method=quad order=data;
        class lobe rater;

        /* ordinal outcome using a cumulative logit link */
        model score = lobe rater
            / dist=multinomial
              link=cumlogit
              solution;

        random intercept / subject=id solution;

        
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
    	ods select all;

    /* Keep label, estimate, and raw p‑value */
   data _pvals_;
      set _rawEst_(keep=Label Estimate Lower Upper Probt);
      where Label ne '';                 /* keep only ESTIMATE rows */
      Comparison = Label;
      raw_p      = Probt;
      
      Odds_Ratio=exp(Estimate);
      OR_LowerCI=exp(Lower);
      OR_UpperCI=exp(Upper);
   run;

   /*Benjamini–Hochberg adjustment */
   proc sort data=_pvals_;  by raw_p; run;

   data _bh1_;
      set _pvals_ nobs=m;
      by raw_p;
      rank + 1;
      initial_bh = raw_p * m / rank;     /* p_(i) * m / i           */
   run;

   /* enforce monotone non‑decreasing rule */
   proc sort data=_bh1_;  by descending raw_p; run;

   data _bh2_;
      set _bh1_;
      retain bh_adj 1;                   /* running minimum         */
      bh_adj = min(bh_adj, initial_bh);
   run;

   proc sort data=_bh2_;  by Comparison; run;

   /*final table and significance flag*/
   data final_FDR;
      set _bh2_;
      Odds_Ratio = exp(Estimate);
      OR_LowerCI=exp(Lower);
      OR_UpperCI=exp(Upper);
      FDR_sig    = (bh_adj < &alpha);
      label raw_p   = 'Unadjusted p'
            bh_adj  = 'BH‑adjusted p'
            Odds_Ratio = 'Exp(Estimate)'
            OR_LowerCI='Lower 95% CI (OR)'
      		OR_UpperCI='Upper 95% CI (OR)'
      		FDR_sig = 'Significant?';
   run;
   
   proc sort data=final_FDR; by bh_adj; run;

   title "Benjamini–Hochberg FDR‑adjusted pairwise comparisons (&dataset)";
   proc print data=final_FDR noobs label;
      var Comparison Odds_Ratio OR_LowerCI OR_UpperCI raw_p bh_adj FDR_sig;
   run;

%mend PAIRWISE_reduced2;

%PAIRWISE_reduced2(tib)
%PAIRWISE_reduced2(cons)
%PAIRWISE_reduced2(bronch)