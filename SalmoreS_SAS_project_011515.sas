/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 15, 2015     TIME: 11:16:06 AM
PROJECT: SalmoreS_SAS_project_011515
PROJECT PATH: P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\ssalmore' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\ssalmore' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Query Builder1   */
LIBNAME EC100010 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* In_ER */
            (1) AS In_ER
      FROM EC100010.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA BASE "P:\QAC\qac200\students\ssalmore" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
LIBNAME EC100012 "P:\QAC\qac200\students\ssalmore";


%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MANAGEDDATA_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MANAGEDDATA_SAS7BDAT AS 
   SELECT t1.MARRY12X_categorical, 
          t1.Sum_SF12_variables_categorical, 
          t1.'SAQ: Health in General SF-12V21'n, 
          t1.'SAQ Hlth Limits Mod Activities1'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs1'n, 
          t1.'SAQ Work Limits Bc Mnt Probs1'n, 
          t1.'Got med appointment when wanted1'n, 
          t1.'Doc explained so understood1'n, 
          t1.'SAQ: Health in General SF-12V211'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHDAGED, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CASHP12X, 
          t1.CHLDP12X, 
          t1.CHOLAGED, 
          t1.COLOAGED, 
          t1.DIABAGED, 
          t1.DIABDX, 
          t1.DNTINS12, 
          t1.DOBMM, 
          t1.DOBYY, 
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EICRDT12, 
          t1.ELGRND12, 
          t1.EMPHAGED, 
          t1.EVRWRK, 
          t1.FAMID12, 
          t1.FAMINC12, 
          t1.FAMSZE12, 
          t1.FAMWT12C, 
          t1.HIBPAGED, 
          t1.LEUKAGED, 
          t1.LUNGAGED, 
          t1.LYMPAGED, 
          t1.MARRY12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.MCRPB12, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPHO12, 
          t1.MELAAGED, 
          t1.MIDX, 
          t1.MSA12, 
          t1.OHRTAGED, 
          t1.OTHRAGED, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.PMDINS12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PROXY12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.RACETHX, 
          t1.REFPRS12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.RFREL12X, 
          t1.RULETR12, 
          t1.RUSIZE12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SKDKAGED, 
          t1.SPOUIN12, 
          t1.STAPR12, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THYRAGED, 
          t1.TRIAT12X, 
          t1.TRILI12X, 
          t1.TRIPR12X, 
          t1.TRIST12X, 
          t1.TTLP12X, 
          t1.'Marital Status1'n, 
          t1.'Yeasr of education1'n, 
          t1.'Marital status 1'n, 
          t1.'Ever Wrkd for pay in life'n, 
          t1.'Family''s total income'n, 
          t1.'Person''s Total Income'n, 
          t1.'Number visits to med'n, 
          t1.'Date completed day'n, 
          t1.'Date completed month'n, 
          t1.'Date completed year'n, 
          t1.'Felt down and depressed'n, 
          t1.'Dr checked blood pressure'n, 
          t1.'Easy getting needed med care1'n, 
          t1.'Doc explained so understood11'n, 
          t1.'Doctor given instructions1'n, 
          t1.'SAQ: Health in General SF-12V212'n, 
          t1.'SAQ Hlth Limits Mod Activities11'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs2'n, 
          t1.'SAQ Work Limits Bc Mnt Probs11'n, 
          t1.'SAQ Pain Limits Normal Work1'n, 
          t1.'SAQ Felt Calm and Peaceful1'n, 
          t1.'SAQ Had a lot of Energy1'n, 
          t1.'SAQ Felt Downhearted and depr1'n, 
          t1.'SAQ Hlt Stopped SOC Activ1'n, 
          t1.'Got med appointment when wanted2'n, 
          t1.'Illness needing immediate care'n, 
          t1.'Need any care test treatment'n, 
          t1.'Rating of Health Care1'n, 
          t1.'Doctor Listened to You1'n, 
          t1.'Needed to see specialist'n, 
          t1.'SAQ Hlth Limits Mod Activities2'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs3'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob3'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob3'n, 
          t1.'Doc explained so understood2'n, 
          t1.'Doctor given instructions2'n, 
          t1.'Easy getting needed med care2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs3'n, 
          t1.'SAQ Work Limits Bc Mnt Probs2'n, 
          t1.'Got med appointment when wanted3'n, 
          t1.'Rating of Health Care2'n, 
          t1.'SAQ Pain Limits Normal Work2'n, 
          t1.'Yeasr of education2'n, 
          t1.'Doctor Listened to You2'n, 
          t1.'Marital Status2'n, 
          t1.'SAQ Felt Calm and Peaceful2'n, 
          t1.'SAQ Had a lot of Energy2'n, 
          t1.'SAQ Felt Downhearted and depr2'n, 
          t1.'SAQ Hlt Stopped SOC Activ2'n, 
          t1.ADGENH42_R1, 
          t1.ADPAIN42_R1, 
          t1.ADCAPE42_R1, 
          t1.ADNRGY42_R1, 
          t1.MARRY12XRECODED, 
          t1.EDUYRDEGRECODED, 
          t1.'SAQ Pain Limits Normal Work3'n, 
          t1.'Easy getting needed med care3'n, 
          t1.'SAQ Felt Calm and Peaceful3'n, 
          t1.'Yeasr of education3'n, 
          t1.'SAQ Had a lot of Energy3'n, 
          t1.'Doctor given instructions3'n, 
          t1.'Marital Status3'n, 
          t1.'SAQ Felt Downhearted and depr3'n, 
          t1.'SAQ Hlt Stopped SOC Activ3'n, 
          t1.'Rating of Health Care3'n, 
          t1.'Doctor Listened to You3'n, 
          t1.ADGENH42_R2, 
          t1.ADPAIN42_R2, 
          t1.ADCAPE42_R2, 
          t1.ADNRGY42_R2, 
          t1.Sum_SF12_variables1, 
          t1.Sum_SF12_variables_categorical1, 
          t1.'SAQ: Health in General SF-12V22'n, 
          t1.'SAQ Hlth Limits Mod Activities3'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs4'n, 
          t1.'SAQ Work Limits Bc Mnt Probs3'n, 
          t1.'SAQ Pain Limits Normal Work4'n, 
          t1.'SAQ Felt Calm and Peaceful4'n, 
          t1.'Got med appointment when wanted4'n, 
          t1.'Doc explained so understood3'n, 
          t1.'Easy getting needed med care4'n, 
          t1.'Rating of Health Care4'n, 
          t1.'Doctor Listened to You4'n, 
          t1.'SAQ Had a lot of Energy4'n, 
          t1.'SAQ Felt Downhearted and depr4'n, 
          t1.'SAQ Hlt Stopped SOC Activ4'n, 
          t1.ADGENH42_R3, 
          t1.'Doctor given instructions4'n, 
          t1.'Yeasr of education4'n, 
          t1.ADPAIN42_R3, 
          t1.'Marital Status4'n, 
          t1.ADCAPE42_R3, 
          t1.ADNRGY42_R3, 
          t1.Sum_SF12_variables2, 
          t1.SUM_Access_Care1, 
          t1.'SAQ: Health in General SF-12V2'n, 
          t1.'SAQ Hlth Limits Mod Activities'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          t1.'Yeasr of education'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          t1.'Marital Status'n, 
          t1.'SAQ Work Limits Bc Mnt Probs'n, 
          t1.'SAQ Pain Limits Normal Work'n, 
          t1.'SAQ Felt Calm and Peaceful'n, 
          t1.'Got med appointment when wanted'n, 
          t1.'Doc explained so understood'n, 
          t1.'Easy getting needed med care'n, 
          t1.'Rating of Health Care'n, 
          t1.'Doctor Listened to You'n, 
          t1.'SAQ Had a lot of Energy'n, 
          t1.'SAQ Felt Downhearted and depr'n, 
          t1.'SAQ Hlt Stopped SOC Activ'n, 
          t1.ADGENH42_R, 
          t1.'Doctor given instructions'n, 
          t1.ADPAIN42_R, 
          t1.ADCAPE42_R, 
          t1.ADNRGY42_R, 
          t1.Sum_SF12_variables, 
          t1.SUM_Access_Care, 
          t1.ACCESS_CARE_categorical, 
          t1.EDUYRDEG_categorical, 
          /* In_Full_Year */
            (1) LABEL="In Full Year" AS In_Full_Year
      FROM EC100012.manageddata t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Full Outer Join ER and FullYR   */
%LET _CLIENTTASKLABEL='Full Outer Join ER and FullYR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.ER_AND_FULLR_0000);

PROC SQL;
   CREATE TABLE MYDATA.ER_AND_FULLR_0000(label="ER_AND_FULLR") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t2.MARRY12X_categorical, 
          t2.Sum_SF12_variables_categorical, 
          t2.'SAQ: Health in General SF-12V21'n, 
          t2.'SAQ Hlth Limits Mod Activities1'n, 
          t2.'SAQ: Hlt Limits Climbing Stairs1'n, 
          t2.'SAQ 4WKS Accmp Less Bc Phy Prob1'n, 
          t2.'SAQ 4WKS Work Limit Bc Phy Prob1'n, 
          t2.'SAQ 4WKS Accmp Less Bc Mnt Prbs1'n, 
          t2.'SAQ Work Limits Bc Mnt Probs1'n, 
          t2.'Got med appointment when wanted1'n, 
          t2.'Doc explained so understood1'n, 
          t2.'SAQ: Health in General SF-12V211'n, 
          t2.ADAPPT42, 
          t2.ADCAPE42, 
          t2.ADCLIM42, 
          t2.ADCMPD42, 
          t2.ADCMPM42, 
          t2.ADCMPY42, 
          t2.ADDAYA42, 
          t2.ADDOWN42, 
          t2.ADDPRS42, 
          t2.ADDRBP42, 
          t2.ADEFRT42, 
          t2.ADEGMC42, 
          t2.ADEXPL42, 
          t2.ADEZUN42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADGENH42, 
          t2.ADHDAGED, 
          t2.ADHECR42, 
          t2.ADHOPE42, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADINST42, 
          t2.ADINTR42, 
          t2.ADLANG42, 
          t2.ADLIST42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADNDCR42, 
          t2.ADNERV42, 
          t2.ADNRGY42, 
          t2.ADNSMK42, 
          t2.ADOVER42, 
          t2.ADPAIN42, 
          t2.ADPALS42, 
          t2.ADPRTM42, 
          t2.ADPRX42, 
          t2.ADPWLM42, 
          t2.ADRESP42, 
          t2.ADREST42, 
          t2.ADRISK42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADSAD42, 
          t2.ADSMOK42, 
          t2.ADSOCA42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADTLHW42, 
          t2.ADWRTH42, 
          t2.AGE12X, 
          t2.ALIMP12X, 
          t2.BUSNP12X, 
          t2.CASHP12X, 
          t2.CHLDP12X, 
          t2.CHOLAGED, 
          t2.COLOAGED, 
          t2.DIABAGED, 
          t2.DIABDX, 
          t2.DNTINS12, 
          t2.DOBMM, 
          t2.DOBYY, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EDUYRDEG, 
          t2.EICRDT12, 
          t2.ELGRND12, 
          t2.EMPHAGED, 
          t2.EVRWRK, 
          t2.FAMID12, 
          t2.FAMINC12, 
          t2.FAMSZE12, 
          t2.FAMWT12C, 
          t2.HIBPAGED, 
          t2.LEUKAGED, 
          t2.LUNGAGED, 
          t2.LYMPAGED, 
          t2.MARRY12X, 
          t2.MCAID12, 
          t2.MCAID12X, 
          t2.MCARE12, 
          t2.MCARE12X, 
          t2.MCDAT12X, 
          t2.MCDHMO12, 
          t2.MCDMC12, 
          t2.MCRPB12, 
          t2.MCRPD12, 
          t2.MCRPD12X, 
          t2.MCRPHO12, 
          t2.MELAAGED, 
          t2.MIDX, 
          t2.MSA12, 
          t2.OHRTAGED, 
          t2.OTHRAGED, 
          t2.OTPAAT12, 
          t2.OTPBAT12, 
          t2.OTPUBA12, 
          t2.OTPUBB12, 
          t2.PCS42, 
          t2.PHQ242, 
          t2.PMDINS12, 
          t2.PMNCNP12, 
          t2.PRDRNP12, 
          t2.PRIDK12, 
          t2.PRIEU12, 
          t2.PRING12, 
          t2.PRIOG12, 
          t2.PRIS12, 
          t2.PRIV12, 
          t2.PRIVAT12, 
          t2.PROUT12, 
          t2.PROXY12, 
          t2.PRVHMO12, 
          t2.PRVMNC12, 
          t2.PUB12X, 
          t2.PUBAT12X, 
          t2.RACETHX, 
          t2.REFPRS12, 
          t2.REGION12, 
          t2.RESP12, 
          t2.RFREL12X, 
          t2.RULETR12, 
          t2.RUSIZE12, 
          t2.SAQELIG, 
          t2.SEX, 
          t2.SFFLAG42, 
          t2.SKDKAGED, 
          t2.SPOUIN12, 
          t2.STAPR12, 
          t2.STRKAGED, 
          t2.STRKDX, 
          t2.THRTAGED, 
          t2.THYRAGED, 
          t2.TRIAT12X, 
          t2.TRILI12X, 
          t2.TRIPR12X, 
          t2.TRIST12X, 
          t2.TTLP12X, 
          t2.'Marital Status1'n, 
          t2.'Yeasr of education1'n, 
          t2.'Marital status 1'n, 
          t2.'Ever Wrkd for pay in life'n, 
          t2.'Family''s total income'n, 
          t2.'Person''s Total Income'n, 
          t2.'Number visits to med'n, 
          t2.'Date completed day'n, 
          t2.'Date completed month'n, 
          t2.'Date completed year'n, 
          t2.'Felt down and depressed'n, 
          t2.'Dr checked blood pressure'n, 
          t2.'Easy getting needed med care1'n, 
          t2.'Doc explained so understood11'n, 
          t2.'Doctor given instructions1'n, 
          t2.'SAQ: Health in General SF-12V212'n, 
          t2.'SAQ Hlth Limits Mod Activities11'n, 
          t2.'SAQ: Hlt Limits Climbing Stairs2'n, 
          t2.'SAQ 4WKS Accmp Less Bc Phy Prob2'n, 
          t2.'SAQ 4WKS Work Limit Bc Phy Prob2'n, 
          t2.'SAQ 4WKS Accmp Less Bc Mnt Prbs2'n, 
          t2.'SAQ Work Limits Bc Mnt Probs11'n, 
          t2.'SAQ Pain Limits Normal Work1'n, 
          t2.'SAQ Felt Calm and Peaceful1'n, 
          t2.'SAQ Had a lot of Energy1'n, 
          t2.'SAQ Felt Downhearted and depr1'n, 
          t2.'SAQ Hlt Stopped SOC Activ1'n, 
          t2.'Got med appointment when wanted2'n, 
          t2.'Illness needing immediate care'n, 
          t2.'Need any care test treatment'n, 
          t2.'Rating of Health Care1'n, 
          t2.'Doctor Listened to You1'n, 
          t2.'Needed to see specialist'n, 
          t2.'SAQ Hlth Limits Mod Activities2'n, 
          t2.'SAQ: Hlt Limits Climbing Stairs3'n, 
          t2.'SAQ 4WKS Accmp Less Bc Phy Prob3'n, 
          t2.'SAQ 4WKS Work Limit Bc Phy Prob3'n, 
          t2.'Doc explained so understood2'n, 
          t2.'Doctor given instructions2'n, 
          t2.'Easy getting needed med care2'n, 
          t2.'SAQ 4WKS Accmp Less Bc Mnt Prbs3'n, 
          t2.'SAQ Work Limits Bc Mnt Probs2'n, 
          t2.'Got med appointment when wanted3'n, 
          t2.'Rating of Health Care2'n, 
          t2.'SAQ Pain Limits Normal Work2'n, 
          t2.'Yeasr of education2'n, 
          t2.'Doctor Listened to You2'n, 
          t2.'Marital Status2'n, 
          t2.'SAQ Felt Calm and Peaceful2'n, 
          t2.'SAQ Had a lot of Energy2'n, 
          t2.'SAQ Felt Downhearted and depr2'n, 
          t2.'SAQ Hlt Stopped SOC Activ2'n, 
          t2.ADGENH42_R1, 
          t2.ADPAIN42_R1, 
          t2.ADCAPE42_R1, 
          t2.ADNRGY42_R1, 
          t2.MARRY12XRECODED, 
          t2.EDUYRDEGRECODED, 
          t2.'SAQ Pain Limits Normal Work3'n, 
          t2.'Easy getting needed med care3'n, 
          t2.'SAQ Felt Calm and Peaceful3'n, 
          t2.'Yeasr of education3'n, 
          t2.'SAQ Had a lot of Energy3'n, 
          t2.'Doctor given instructions3'n, 
          t2.'Marital Status3'n, 
          t2.'SAQ Felt Downhearted and depr3'n, 
          t2.'SAQ Hlt Stopped SOC Activ3'n, 
          t2.'Rating of Health Care3'n, 
          t2.'Doctor Listened to You3'n, 
          t2.ADGENH42_R2, 
          t2.ADPAIN42_R2, 
          t2.ADCAPE42_R2, 
          t2.ADNRGY42_R2, 
          t2.Sum_SF12_variables1, 
          t2.Sum_SF12_variables_categorical1, 
          t2.'SAQ: Health in General SF-12V22'n, 
          t2.'SAQ Hlth Limits Mod Activities3'n, 
          t2.'SAQ: Hlt Limits Climbing Stairs4'n, 
          t2.'SAQ 4WKS Accmp Less Bc Phy Prob4'n, 
          t2.'SAQ 4WKS Work Limit Bc Phy Prob4'n, 
          t2.'SAQ 4WKS Accmp Less Bc Mnt Prbs4'n, 
          t2.'SAQ Work Limits Bc Mnt Probs3'n, 
          t2.'SAQ Pain Limits Normal Work4'n, 
          t2.'SAQ Felt Calm and Peaceful4'n, 
          t2.'Got med appointment when wanted4'n, 
          t2.'Doc explained so understood3'n, 
          t2.'Easy getting needed med care4'n, 
          t2.'Rating of Health Care4'n, 
          t2.'Doctor Listened to You4'n, 
          t2.'SAQ Had a lot of Energy4'n, 
          t2.'SAQ Felt Downhearted and depr4'n, 
          t2.'SAQ Hlt Stopped SOC Activ4'n, 
          t2.ADGENH42_R3, 
          t2.'Doctor given instructions4'n, 
          t2.'Yeasr of education4'n, 
          t2.ADPAIN42_R3, 
          t2.'Marital Status4'n, 
          t2.ADCAPE42_R3, 
          t2.ADNRGY42_R3, 
          t2.Sum_SF12_variables2, 
          t2.SUM_Access_Care1, 
          t2.'SAQ: Health in General SF-12V2'n, 
          t2.'SAQ Hlth Limits Mod Activities'n, 
          t2.'SAQ: Hlt Limits Climbing Stairs'n, 
          t2.'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          t2.'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          t2.'Yeasr of education'n, 
          t2.'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          t2.'Marital Status'n, 
          t2.'SAQ Work Limits Bc Mnt Probs'n, 
          t2.'SAQ Pain Limits Normal Work'n, 
          t2.'SAQ Felt Calm and Peaceful'n, 
          t2.'Got med appointment when wanted'n, 
          t2.'Doc explained so understood'n, 
          t2.'Easy getting needed med care'n, 
          t2.'Rating of Health Care'n, 
          t2.'Doctor Listened to You'n, 
          t2.'SAQ Had a lot of Energy'n, 
          t2.'SAQ Felt Downhearted and depr'n, 
          t2.'SAQ Hlt Stopped SOC Activ'n, 
          t2.ADGENH42_R, 
          t2.'Doctor given instructions'n, 
          t2.ADPAIN42_R, 
          t2.ADCAPE42_R, 
          t2.ADNRGY42_R, 
          t2.Sum_SF12_variables, 
          t2.SUM_Access_Care, 
          t2.ACCESS_CARE_categorical, 
          t2.EDUYRDEG_categorical, 
          t2.In_Full_Year, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.In_ER
      FROM WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t1
           FULL JOIN WORK.QUERY_FOR_MANAGEDDATA_SAS7BDAT t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t1.In_ER = 1 AND t2.In_Full_Year = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:46 AM
   By task: List Data

   Input Data: Local:MYDATA.ER_AND_FULLR_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.SUBSTempTableSubsetInputTable);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.ER_AND_FULLR_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.In_ER, T.DUPERSID, T.DUPERSID1
	FROM MYDATA.ER_AND_FULLR_0000 as T
;
QUIT;

DATA WORK.SUBSTempTableSubsetInputTable / VIEW=WORK.SUBSTempTableSubsetInputTable;
	SET WORK.SORTTempTableSorted;
	IF MOD(_N_, 100) = 0 THEN
		OUTPUT;
RUN;

TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SUBSTempTableSubsetInputTable
	OBS="Row number"
	LABEL
	;
	VAR In_ER DUPERSID DUPERSID1;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.SUBSTempTableSubsetInputTable);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:47 AM
   By task: Data Set Attributes

   Input Data: Local:MYDATA.ER_AND_FULLR_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForER_AND_FULLR_0000);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.ER_AND_FULLR_0000 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForER_AND_FULLR_0000(LABEL="Contents Details for ER_AND_FULLR_0000");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForER_AND_FULLR_0000
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='ER_AND_FULLR_0000';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForER_AND_FULLR_0000 OUT=WORK.CONTContentsForER_AND_FULLR_0000;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForER_AND_FULLR_0000
		WHERE memname='ER_AND_FULLR_0000';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MRI and  XRAY Recode   */
%LET _CLIENTTASKLABEL='MRI and  XRAY Recode';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_ER_AND_FULLR_MANAGED);

PROC SQL;
   CREATE TABLE WORK."QUERY_ER_AND_FULLR_MANAGED"n AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.MARRY12X_categorical, 
          t1.Sum_SF12_variables_categorical, 
          t1.'SAQ: Health in General SF-12V21'n, 
          t1.'SAQ Hlth Limits Mod Activities1'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs1'n, 
          t1.'SAQ Work Limits Bc Mnt Probs1'n, 
          t1.'Got med appointment when wanted1'n, 
          t1.'Doc explained so understood1'n, 
          t1.'SAQ: Health in General SF-12V211'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHDAGED, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CASHP12X, 
          t1.CHLDP12X, 
          t1.CHOLAGED, 
          t1.COLOAGED, 
          t1.DIABAGED, 
          t1.DIABDX, 
          t1.DNTINS12, 
          t1.DOBMM, 
          t1.DOBYY, 
          t1.DUPERSID1, 
          t1.EDUYRDEG, 
          t1.EICRDT12, 
          t1.ELGRND12, 
          t1.EMPHAGED, 
          t1.EVRWRK, 
          t1.FAMID12, 
          t1.FAMINC12, 
          t1.FAMSZE12, 
          t1.FAMWT12C, 
          t1.HIBPAGED, 
          t1.LEUKAGED, 
          t1.LUNGAGED, 
          t1.LYMPAGED, 
          t1.MARRY12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.MCRPB12, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPHO12, 
          t1.MELAAGED, 
          t1.MIDX, 
          t1.MSA12, 
          t1.OHRTAGED, 
          t1.OTHRAGED, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.PMDINS12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PROXY12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.RACETHX, 
          t1.REFPRS12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.RFREL12X, 
          t1.RULETR12, 
          t1.RUSIZE12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SKDKAGED, 
          t1.SPOUIN12, 
          t1.STAPR12, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THYRAGED, 
          t1.TRIAT12X, 
          t1.TRILI12X, 
          t1.TRIPR12X, 
          t1.TRIST12X, 
          t1.TTLP12X, 
          t1.'Marital Status1'n, 
          t1.'Yeasr of education1'n, 
          t1.'Marital status 1'n, 
          t1.'Ever Wrkd for pay in life'n, 
          t1.'Family''s total income'n, 
          t1.'Person''s Total Income'n, 
          t1.'Number visits to med'n, 
          t1.'Date completed day'n, 
          t1.'Date completed month'n, 
          t1.'Date completed year'n, 
          t1.'Felt down and depressed'n, 
          t1.'Dr checked blood pressure'n, 
          t1.'Easy getting needed med care1'n, 
          t1.'Doc explained so understood11'n, 
          t1.'Doctor given instructions1'n, 
          t1.'SAQ: Health in General SF-12V212'n, 
          t1.'SAQ Hlth Limits Mod Activities11'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs2'n, 
          t1.'SAQ Work Limits Bc Mnt Probs11'n, 
          t1.'SAQ Pain Limits Normal Work1'n, 
          t1.'SAQ Felt Calm and Peaceful1'n, 
          t1.'SAQ Had a lot of Energy1'n, 
          t1.'SAQ Felt Downhearted and depr1'n, 
          t1.'SAQ Hlt Stopped SOC Activ1'n, 
          t1.'Got med appointment when wanted2'n, 
          t1.'Illness needing immediate care'n, 
          t1.'Need any care test treatment'n, 
          t1.'Rating of Health Care1'n, 
          t1.'Doctor Listened to You1'n, 
          t1.'Needed to see specialist'n, 
          t1.'SAQ Hlth Limits Mod Activities2'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs3'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob3'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob3'n, 
          t1.'Doc explained so understood2'n, 
          t1.'Doctor given instructions2'n, 
          t1.'Easy getting needed med care2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs3'n, 
          t1.'SAQ Work Limits Bc Mnt Probs2'n, 
          t1.'Got med appointment when wanted3'n, 
          t1.'Rating of Health Care2'n, 
          t1.'SAQ Pain Limits Normal Work2'n, 
          t1.'Yeasr of education2'n, 
          t1.'Doctor Listened to You2'n, 
          t1.'Marital Status2'n, 
          t1.'SAQ Felt Calm and Peaceful2'n, 
          t1.'SAQ Had a lot of Energy2'n, 
          t1.'SAQ Felt Downhearted and depr2'n, 
          t1.'SAQ Hlt Stopped SOC Activ2'n, 
          t1.ADGENH42_R1, 
          t1.ADPAIN42_R1, 
          t1.ADCAPE42_R1, 
          t1.ADNRGY42_R1, 
          t1.MARRY12XRECODED, 
          t1.EDUYRDEGRECODED, 
          t1.'SAQ Pain Limits Normal Work3'n, 
          t1.'Easy getting needed med care3'n, 
          t1.'SAQ Felt Calm and Peaceful3'n, 
          t1.'Yeasr of education3'n, 
          t1.'SAQ Had a lot of Energy3'n, 
          t1.'Doctor given instructions3'n, 
          t1.'Marital Status3'n, 
          t1.'SAQ Felt Downhearted and depr3'n, 
          t1.'SAQ Hlt Stopped SOC Activ3'n, 
          t1.'Rating of Health Care3'n, 
          t1.'Doctor Listened to You3'n, 
          t1.ADGENH42_R2, 
          t1.ADPAIN42_R2, 
          t1.ADCAPE42_R2, 
          t1.ADNRGY42_R2, 
          t1.Sum_SF12_variables1, 
          t1.Sum_SF12_variables_categorical1, 
          t1.'SAQ: Health in General SF-12V22'n, 
          t1.'SAQ Hlth Limits Mod Activities3'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs4'n, 
          t1.'SAQ Work Limits Bc Mnt Probs3'n, 
          t1.'SAQ Pain Limits Normal Work4'n, 
          t1.'SAQ Felt Calm and Peaceful4'n, 
          t1.'Got med appointment when wanted4'n, 
          t1.'Doc explained so understood3'n, 
          t1.'Easy getting needed med care4'n, 
          t1.'Rating of Health Care4'n, 
          t1.'Doctor Listened to You4'n, 
          t1.'SAQ Had a lot of Energy4'n, 
          t1.'SAQ Felt Downhearted and depr4'n, 
          t1.'SAQ Hlt Stopped SOC Activ4'n, 
          t1.ADGENH42_R3, 
          t1.'Doctor given instructions4'n, 
          t1.'Yeasr of education4'n, 
          t1.ADPAIN42_R3, 
          t1.'Marital Status4'n, 
          t1.ADCAPE42_R3, 
          t1.ADNRGY42_R3, 
          t1.Sum_SF12_variables2, 
          t1.SUM_Access_Care1, 
          t1.'SAQ: Health in General SF-12V2'n, 
          t1.'SAQ Hlth Limits Mod Activities'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          t1.'Yeasr of education'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          t1.'Marital Status'n, 
          t1.'SAQ Work Limits Bc Mnt Probs'n, 
          t1.'SAQ Pain Limits Normal Work'n, 
          t1.'SAQ Felt Calm and Peaceful'n, 
          t1.'Got med appointment when wanted'n, 
          t1.'Doc explained so understood'n, 
          t1.'Easy getting needed med care'n, 
          t1.'Rating of Health Care'n, 
          t1.'Doctor Listened to You'n, 
          t1.'SAQ Had a lot of Energy'n, 
          t1.'SAQ Felt Downhearted and depr'n, 
          t1.'SAQ Hlt Stopped SOC Activ'n, 
          t1.ADGENH42_R, 
          t1.'Doctor given instructions'n, 
          t1.ADPAIN42_R, 
          t1.ADCAPE42_R, 
          t1.ADNRGY42_R, 
          t1.Sum_SF12_variables, 
          t1.SUM_Access_Care, 
          t1.ACCESS_CARE_categorical, 
          t1.EDUYRDEG_categorical, 
          t1.In_Full_Year, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.In_ER, 
          /* MRI_recode */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               ELSE t1.MRI
            END) LABEL="MRI_Recode (recoded missing)" AS MRI_recode, 
          /* XRAYS_Recode */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               ELSE t1.XRAYS
            END) LABEL="XRAYS_Recode (recoded missing)" AS XRAYS_Recode
      FROM MYDATA.ER_AND_FULLR_0000 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for MRI and XRAYS   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for MRI and XRAYS';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:47 AM
   By task: One-Way Frequencies for MRI and XRAYS

   Input Data: Local:WORK.QUERY_ER_AND_FULLR_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_ER_AND_FULLR_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_Recode, T.MRI_recode
	FROM WORK.QUERY_ER_AND_FULLR_MANAGED(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MRI and XRAYS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By Sophie Salmore";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS_Recode / MISSPRINT  SCORES=TABLE;
	TABLES MRI_recode / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_COUNT_VISITS);

PROC SQL;
   CREATE TABLE WORK."QUERY_COUNT_VISITS"n AS 
   SELECT t1.DUPERSID1, 
          /* COUNT_of_DUPERSID1 */
            (COUNT(t1.DUPERSID1)) AS COUNT_of_DUPERSID1
      FROM WORK.QUERY_ER_AND_FULLR_MANAGED t1
      GROUP BY t1.DUPERSID1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_JOIN);

PROC SQL;
   CREATE TABLE WORK."QUERY_JOIN"n AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.MARRY12X_categorical, 
          t1.Sum_SF12_variables_categorical, 
          t1.'SAQ: Health in General SF-12V21'n, 
          t1.'SAQ Hlth Limits Mod Activities1'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs1'n, 
          t1.'SAQ Work Limits Bc Mnt Probs1'n, 
          t1.'Got med appointment when wanted1'n, 
          t1.'Doc explained so understood1'n, 
          t1.'SAQ: Health in General SF-12V211'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHDAGED, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t2.DUPERSID1 AS DUPERSID11, 
          t2.COUNT_of_DUPERSID1, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CASHP12X, 
          t1.CHLDP12X, 
          t1.CHOLAGED, 
          t1.COLOAGED, 
          t1.DIABAGED, 
          t1.DIABDX, 
          t1.DNTINS12, 
          t1.DOBMM, 
          t1.DOBYY, 
          t1.DUPERSID1, 
          t1.EDUYRDEG, 
          t1.EICRDT12, 
          t1.ELGRND12, 
          t1.EMPHAGED, 
          t1.EVRWRK, 
          t1.FAMID12, 
          t1.FAMINC12, 
          t1.FAMSZE12, 
          t1.FAMWT12C, 
          t1.HIBPAGED, 
          t1.LEUKAGED, 
          t1.LUNGAGED, 
          t1.LYMPAGED, 
          t1.MARRY12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.MCRPB12, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPHO12, 
          t1.MELAAGED, 
          t1.MIDX, 
          t1.MSA12, 
          t1.OHRTAGED, 
          t1.OTHRAGED, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.PMDINS12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PROXY12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.RACETHX, 
          t1.REFPRS12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.RFREL12X, 
          t1.RULETR12, 
          t1.RUSIZE12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SKDKAGED, 
          t1.SPOUIN12, 
          t1.STAPR12, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THYRAGED, 
          t1.TRIAT12X, 
          t1.TRILI12X, 
          t1.TRIPR12X, 
          t1.TRIST12X, 
          t1.TTLP12X, 
          t1.'Marital Status1'n, 
          t1.'Yeasr of education1'n, 
          t1.'Marital status 1'n, 
          t1.'Ever Wrkd for pay in life'n, 
          t1.'Family''s total income'n, 
          t1.'Person''s Total Income'n, 
          t1.'Number visits to med'n, 
          t1.'Date completed day'n, 
          t1.'Date completed month'n, 
          t1.'Date completed year'n, 
          t1.'Felt down and depressed'n, 
          t1.'Dr checked blood pressure'n, 
          t1.'Easy getting needed med care1'n, 
          t1.'Doc explained so understood11'n, 
          t1.'Doctor given instructions1'n, 
          t1.'SAQ: Health in General SF-12V212'n, 
          t1.'SAQ Hlth Limits Mod Activities11'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs2'n, 
          t1.'SAQ Work Limits Bc Mnt Probs11'n, 
          t1.'SAQ Pain Limits Normal Work1'n, 
          t1.'SAQ Felt Calm and Peaceful1'n, 
          t1.'SAQ Had a lot of Energy1'n, 
          t1.'SAQ Felt Downhearted and depr1'n, 
          t1.'SAQ Hlt Stopped SOC Activ1'n, 
          t1.'Got med appointment when wanted2'n, 
          t1.'Illness needing immediate care'n, 
          t1.'Need any care test treatment'n, 
          t1.'Rating of Health Care1'n, 
          t1.'Doctor Listened to You1'n, 
          t1.'Needed to see specialist'n, 
          t1.'SAQ Hlth Limits Mod Activities2'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs3'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob3'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob3'n, 
          t1.'Doc explained so understood2'n, 
          t1.'Doctor given instructions2'n, 
          t1.'Easy getting needed med care2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs3'n, 
          t1.'SAQ Work Limits Bc Mnt Probs2'n, 
          t1.'Got med appointment when wanted3'n, 
          t1.'Rating of Health Care2'n, 
          t1.'SAQ Pain Limits Normal Work2'n, 
          t1.'Yeasr of education2'n, 
          t1.'Doctor Listened to You2'n, 
          t1.'Marital Status2'n, 
          t1.'SAQ Felt Calm and Peaceful2'n, 
          t1.'SAQ Had a lot of Energy2'n, 
          t1.'SAQ Felt Downhearted and depr2'n, 
          t1.'SAQ Hlt Stopped SOC Activ2'n, 
          t1.ADGENH42_R1, 
          t1.ADPAIN42_R1, 
          t1.ADCAPE42_R1, 
          t1.ADNRGY42_R1, 
          t1.MARRY12XRECODED, 
          t1.EDUYRDEGRECODED, 
          t1.'SAQ Pain Limits Normal Work3'n, 
          t1.'Easy getting needed med care3'n, 
          t1.'SAQ Felt Calm and Peaceful3'n, 
          t1.'Yeasr of education3'n, 
          t1.'SAQ Had a lot of Energy3'n, 
          t1.'Doctor given instructions3'n, 
          t1.'Marital Status3'n, 
          t1.'SAQ Felt Downhearted and depr3'n, 
          t1.'SAQ Hlt Stopped SOC Activ3'n, 
          t1.'Rating of Health Care3'n, 
          t1.'Doctor Listened to You3'n, 
          t1.ADGENH42_R2, 
          t1.ADPAIN42_R2, 
          t1.ADCAPE42_R2, 
          t1.ADNRGY42_R2, 
          t1.Sum_SF12_variables1, 
          t1.Sum_SF12_variables_categorical1, 
          t1.'SAQ: Health in General SF-12V22'n, 
          t1.'SAQ Hlth Limits Mod Activities3'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs4'n, 
          t1.'SAQ Work Limits Bc Mnt Probs3'n, 
          t1.'SAQ Pain Limits Normal Work4'n, 
          t1.'SAQ Felt Calm and Peaceful4'n, 
          t1.'Got med appointment when wanted4'n, 
          t1.'Doc explained so understood3'n, 
          t1.'Easy getting needed med care4'n, 
          t1.'Rating of Health Care4'n, 
          t1.'Doctor Listened to You4'n, 
          t1.'SAQ Had a lot of Energy4'n, 
          t1.'SAQ Felt Downhearted and depr4'n, 
          t1.'SAQ Hlt Stopped SOC Activ4'n, 
          t1.ADGENH42_R3, 
          t1.'Doctor given instructions4'n, 
          t1.'Yeasr of education4'n, 
          t1.ADPAIN42_R3, 
          t1.'Marital Status4'n, 
          t1.ADCAPE42_R3, 
          t1.ADNRGY42_R3, 
          t1.Sum_SF12_variables2, 
          t1.SUM_Access_Care1, 
          t1.'SAQ: Health in General SF-12V2'n, 
          t1.'SAQ Hlth Limits Mod Activities'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          t1.'Yeasr of education'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          t1.'Marital Status'n, 
          t1.'SAQ Work Limits Bc Mnt Probs'n, 
          t1.'SAQ Pain Limits Normal Work'n, 
          t1.'SAQ Felt Calm and Peaceful'n, 
          t1.'Got med appointment when wanted'n, 
          t1.'Doc explained so understood'n, 
          t1.'Easy getting needed med care'n, 
          t1.'Rating of Health Care'n, 
          t1.'Doctor Listened to You'n, 
          t1.'SAQ Had a lot of Energy'n, 
          t1.'SAQ Felt Downhearted and depr'n, 
          t1.'SAQ Hlt Stopped SOC Activ'n, 
          t1.ADGENH42_R, 
          t1.'Doctor given instructions'n, 
          t1.ADPAIN42_R, 
          t1.ADCAPE42_R, 
          t1.ADNRGY42_R, 
          t1.Sum_SF12_variables, 
          t1.SUM_Access_Care, 
          t1.ACCESS_CARE_categorical, 
          t1.EDUYRDEG_categorical, 
          t1.In_Full_Year, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.In_ER, 
          t1.MRI_recode, 
          t1.XRAYS_Recode
      FROM WORK.QUERY_ER_AND_FULLR_MANAGED t1
           INNER JOIN WORK.QUERY_COUNT_VISITS t2 ON (t1.DUPERSID1 = t2.DUPERSID1);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for ER Visits (Count DUPERSID)   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for ER Visits (Count DUPERSID)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:47 AM
   By task: One-Way Frequencies for ER Visits (Count DUPERSID)

   Input Data: Local:WORK.QUERY_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID1
	FROM WORK.QUERY_JOIN as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for ER Visits Variable (count DUPERSID)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By Sophie Salmore";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of ER Visits (Count DUPERSID)   */
%LET _CLIENTTASKLABEL='Distribution Analysis of ER Visits (Count DUPERSID)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:48 AM
   By task: Distribution Analysis of ER Visits (Count DUPERSID)

   Input Data: Local:WORK.QUERY_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID1
	FROM WORK.QUERY_JOIN as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: ER visits (count DUPERSID) variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By Sophie Salmore";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID1;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder4   */
%LET _CLIENTTASKLABEL='Query Builder4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_JOIN);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_JOIN AS 
   SELECT /* COUNT_DUPERSID_categorical */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID1 >0 and t1.COUNT_of_DUPERSID1 <1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID1 =1 
               THEN 2
            WHEN t1.COUNT_of_DUPERSID1 >=1 and t1.COUNT_of_DUPERSID1 <=2
               THEN 3
            WHEN t1.COUNT_of_DUPERSID1 >2
               THEN 4
            END) LABEL="ER_VISITS_COUNT_DUPERSID_categorical " AS COUNT_DUPERSID_categorical, 
          t1.DUID, 
          t1.PID, 
          t1.MARRY12X_categorical, 
          t1.Sum_SF12_variables_categorical, 
          t1.'SAQ: Health in General SF-12V21'n, 
          t1.'SAQ Hlth Limits Mod Activities1'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob1'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs1'n, 
          t1.'SAQ Work Limits Bc Mnt Probs1'n, 
          t1.'Got med appointment when wanted1'n, 
          t1.'Doc explained so understood1'n, 
          t1.'SAQ: Health in General SF-12V211'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHDAGED, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.DUPERSID11, 
          t1.COUNT_of_DUPERSID1, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CASHP12X, 
          t1.CHLDP12X, 
          t1.CHOLAGED, 
          t1.COLOAGED, 
          t1.DIABAGED, 
          t1.DIABDX, 
          t1.DNTINS12, 
          t1.DOBMM, 
          t1.DOBYY, 
          t1.DUPERSID1, 
          t1.EDUYRDEG, 
          t1.EICRDT12, 
          t1.ELGRND12, 
          t1.EMPHAGED, 
          t1.EVRWRK, 
          t1.FAMID12, 
          t1.FAMINC12, 
          t1.FAMSZE12, 
          t1.FAMWT12C, 
          t1.HIBPAGED, 
          t1.LEUKAGED, 
          t1.LUNGAGED, 
          t1.LYMPAGED, 
          t1.MARRY12X, 
          t1.MCAID12, 
          t1.MCAID12X, 
          t1.MCARE12, 
          t1.MCARE12X, 
          t1.MCDAT12X, 
          t1.MCDHMO12, 
          t1.MCDMC12, 
          t1.MCRPB12, 
          t1.MCRPD12, 
          t1.MCRPD12X, 
          t1.MCRPHO12, 
          t1.MELAAGED, 
          t1.MIDX, 
          t1.MSA12, 
          t1.OHRTAGED, 
          t1.OTHRAGED, 
          t1.OTPAAT12, 
          t1.OTPBAT12, 
          t1.OTPUBA12, 
          t1.OTPUBB12, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.PMDINS12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.PRIDK12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PRIVAT12, 
          t1.PROUT12, 
          t1.PROXY12, 
          t1.PRVHMO12, 
          t1.PRVMNC12, 
          t1.PUB12X, 
          t1.PUBAT12X, 
          t1.RACETHX, 
          t1.REFPRS12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.RFREL12X, 
          t1.RULETR12, 
          t1.RUSIZE12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SKDKAGED, 
          t1.SPOUIN12, 
          t1.STAPR12, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THYRAGED, 
          t1.TRIAT12X, 
          t1.TRILI12X, 
          t1.TRIPR12X, 
          t1.TRIST12X, 
          t1.TTLP12X, 
          t1.'Marital Status1'n, 
          t1.'Yeasr of education1'n, 
          t1.'Marital status 1'n, 
          t1.'Ever Wrkd for pay in life'n, 
          t1.'Family''s total income'n, 
          t1.'Person''s Total Income'n, 
          t1.'Number visits to med'n, 
          t1.'Date completed day'n, 
          t1.'Date completed month'n, 
          t1.'Date completed year'n, 
          t1.'Felt down and depressed'n, 
          t1.'Dr checked blood pressure'n, 
          t1.'Easy getting needed med care1'n, 
          t1.'Doc explained so understood11'n, 
          t1.'Doctor given instructions1'n, 
          t1.'SAQ: Health in General SF-12V212'n, 
          t1.'SAQ Hlth Limits Mod Activities11'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs2'n, 
          t1.'SAQ Work Limits Bc Mnt Probs11'n, 
          t1.'SAQ Pain Limits Normal Work1'n, 
          t1.'SAQ Felt Calm and Peaceful1'n, 
          t1.'SAQ Had a lot of Energy1'n, 
          t1.'SAQ Felt Downhearted and depr1'n, 
          t1.'SAQ Hlt Stopped SOC Activ1'n, 
          t1.'Got med appointment when wanted2'n, 
          t1.'Illness needing immediate care'n, 
          t1.'Need any care test treatment'n, 
          t1.'Rating of Health Care1'n, 
          t1.'Doctor Listened to You1'n, 
          t1.'Needed to see specialist'n, 
          t1.'SAQ Hlth Limits Mod Activities2'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs3'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob3'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob3'n, 
          t1.'Doc explained so understood2'n, 
          t1.'Doctor given instructions2'n, 
          t1.'Easy getting needed med care2'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs3'n, 
          t1.'SAQ Work Limits Bc Mnt Probs2'n, 
          t1.'Got med appointment when wanted3'n, 
          t1.'Rating of Health Care2'n, 
          t1.'SAQ Pain Limits Normal Work2'n, 
          t1.'Yeasr of education2'n, 
          t1.'Doctor Listened to You2'n, 
          t1.'Marital Status2'n, 
          t1.'SAQ Felt Calm and Peaceful2'n, 
          t1.'SAQ Had a lot of Energy2'n, 
          t1.'SAQ Felt Downhearted and depr2'n, 
          t1.'SAQ Hlt Stopped SOC Activ2'n, 
          t1.ADGENH42_R1, 
          t1.ADPAIN42_R1, 
          t1.ADCAPE42_R1, 
          t1.ADNRGY42_R1, 
          t1.MARRY12XRECODED, 
          t1.EDUYRDEGRECODED, 
          t1.'SAQ Pain Limits Normal Work3'n, 
          t1.'Easy getting needed med care3'n, 
          t1.'SAQ Felt Calm and Peaceful3'n, 
          t1.'Yeasr of education3'n, 
          t1.'SAQ Had a lot of Energy3'n, 
          t1.'Doctor given instructions3'n, 
          t1.'Marital Status3'n, 
          t1.'SAQ Felt Downhearted and depr3'n, 
          t1.'SAQ Hlt Stopped SOC Activ3'n, 
          t1.'Rating of Health Care3'n, 
          t1.'Doctor Listened to You3'n, 
          t1.ADGENH42_R2, 
          t1.ADPAIN42_R2, 
          t1.ADCAPE42_R2, 
          t1.ADNRGY42_R2, 
          t1.Sum_SF12_variables1, 
          t1.Sum_SF12_variables_categorical1, 
          t1.'SAQ: Health in General SF-12V22'n, 
          t1.'SAQ Hlth Limits Mod Activities3'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Prob4'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs4'n, 
          t1.'SAQ Work Limits Bc Mnt Probs3'n, 
          t1.'SAQ Pain Limits Normal Work4'n, 
          t1.'SAQ Felt Calm and Peaceful4'n, 
          t1.'Got med appointment when wanted4'n, 
          t1.'Doc explained so understood3'n, 
          t1.'Easy getting needed med care4'n, 
          t1.'Rating of Health Care4'n, 
          t1.'Doctor Listened to You4'n, 
          t1.'SAQ Had a lot of Energy4'n, 
          t1.'SAQ Felt Downhearted and depr4'n, 
          t1.'SAQ Hlt Stopped SOC Activ4'n, 
          t1.ADGENH42_R3, 
          t1.'Doctor given instructions4'n, 
          t1.'Yeasr of education4'n, 
          t1.ADPAIN42_R3, 
          t1.'Marital Status4'n, 
          t1.ADCAPE42_R3, 
          t1.ADNRGY42_R3, 
          t1.Sum_SF12_variables2, 
          t1.SUM_Access_Care1, 
          t1.'SAQ: Health in General SF-12V2'n, 
          t1.'SAQ Hlth Limits Mod Activities'n, 
          t1.'SAQ: Hlt Limits Climbing Stairs'n, 
          t1.'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          t1.'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          t1.'Yeasr of education'n, 
          t1.'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          t1.'Marital Status'n, 
          t1.'SAQ Work Limits Bc Mnt Probs'n, 
          t1.'SAQ Pain Limits Normal Work'n, 
          t1.'SAQ Felt Calm and Peaceful'n, 
          t1.'Got med appointment when wanted'n, 
          t1.'Doc explained so understood'n, 
          t1.'Easy getting needed med care'n, 
          t1.'Rating of Health Care'n, 
          t1.'Doctor Listened to You'n, 
          t1.'SAQ Had a lot of Energy'n, 
          t1.'SAQ Felt Downhearted and depr'n, 
          t1.'SAQ Hlt Stopped SOC Activ'n, 
          t1.ADGENH42_R, 
          t1.'Doctor given instructions'n, 
          t1.ADPAIN42_R, 
          t1.ADCAPE42_R, 
          t1.ADNRGY42_R, 
          t1.Sum_SF12_variables, 
          t1.SUM_Access_Care, 
          t1.ACCESS_CARE_categorical, 
          t1.EDUYRDEG_categorical, 
          t1.In_Full_Year, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.In_ER, 
          t1.MRI_recode, 
          t1.XRAYS_Recode
      FROM WORK.QUERY_JOIN t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis for ER Visits (Count DUPERSID) and Count DUPERSID Categorical   */
%LET _CLIENTTASKLABEL='Table Analysis for ER Visits (Count DUPERSID) and Count DUPERSID Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:48 AM
   By task: Table Analysis for ER Visits (Count DUPERSID) and Count DUPERSID Categorical

   Input Data: Local:WORK.QUERY_FOR_QUERY_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID1, T.COUNT_DUPERSID_categorical
	FROM WORK.QUERY_FOR_QUERY_JOIN as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for COUNT DUPERSID and COUNT DUPERSID CATEGORICAL";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By Sophie Salmore";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 * COUNT_DUPERSID_categorical /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for COUNT DUPERSID and COUNT DUPERSID CATEGORICAL   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for COUNT DUPERSID and COUNT DUPERSID CATEGORICAL';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\SalmoreS_SAS_project_011515.egp';
%LET _CLIENTPROJECTNAME='SalmoreS_SAS_project_011515.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 15, 2015 at 11:15:48 AM
   By task: One-Way Frequencies for COUNT DUPERSID and COUNT DUPERSID CATEGORICAL

   Input Data: Local:WORK.QUERY_FOR_QUERY_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID1, T.COUNT_DUPERSID_categorical
	FROM WORK.QUERY_FOR_QUERY_JOIN as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for COUNT DUPERSID and COUNT DUPERSID CATEGORICAL";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By Sophie Salmore";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 /  SCORES=TABLE;
	TABLES COUNT_DUPERSID_categorical /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
