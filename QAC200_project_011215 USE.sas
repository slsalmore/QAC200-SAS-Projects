/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 2:47:10 PM
PROJECT: QAC200_project_011215 USE
PROJECT PATH: P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp
---------------------------------------- */

/* Library assignment for Local.SALDATA */
Libname SALDATA BASE 'P:\QAC\qac200\students\ssalmore' ;
/* Library assignment for Local.SALDATA */
Libname SALDATA BASE 'P:\QAC\qac200\students\ssalmore' ;


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

/*   START OF NODE: Assign Project Library (SALDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (SALDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp';
%LET _CLIENTPROJECTNAME='QAC200_project_011215 USE.egp';

GOPTIONS ACCESSIBLE;
LIBNAME SALDATA BASE "P:\QAC\qac200\students\ssalmore" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for the 2012 Full Year Consolidated Data Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for the 2012 Full Year Consolidated Data Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp';
%LET _CLIENTPROJECTNAME='QAC200_project_011215 USE.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 2:46:50 PM
   By task: One-Way Frequencies for the 2012 Full Year Consolidated Data Subset

   Input Data: Local:SALDATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:SALDATA.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHDAGED, T.ADHECR42
		     , T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42
		     , T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.AGE12X, T.ALIMP12X, T.BUSNP12X, T.CASHP12X
		     , T.CHLDP12X, T.CHOLAGED, T.COLOAGED, T.DIABAGED, T.DIABDX, T.DNTINS12, T.DOBMM, T.DOBYY, T.EDUYRDEG, T.EICRDT12, T.ELGRND12, T.EMPHAGED, T.EVRWRK, T.FAMID12, T.FAMINC12, T.FAMSZE12, T.FAMWT12C, T.HIBPAGED, T.LEUKAGED, T.LUNGAGED
		     , T.LYMPAGED, T.MARRY12X, T.MCAID12, T.MCAID12X, T.MCARE12, T.MCARE12X, T.MCDAT12X, T.MCDHMO12, T.MCDMC12, T.MCRPB12, T.MCRPD12, T.MCRPD12X, T.MCRPHO12, T.MELAAGED, T.MIDX, T.MSA12, T.OHRTAGED, T.OTHRAGED, T.OTPAAT12, T.OTPBAT12
		     , T.OTPUBA12, T.OTPUBB12, T.PCS42, T.PHQ242, T.PMDINS12, T.PMNCNP12, T.PRDRNP12, T.PRIDK12, T.PRIEU12, T.PRING12, T.PRIOG12, T.PRIS12, T.PRIV12, T.PRIVAT12, T.PROUT12, T.PROXY12, T.PRVHMO12, T.PRVMNC12, T.PUB12X, T.PUBAT12X
		     , T.RACETHX, T.REFPRS12, T.REGION12, T.RESP12, T.RFREL12X, T.RULETR12, T.RUSIZE12, T.SAQELIG, T.SEX, T.SFFLAG42, T.SKDKAGED, T.SPOUIN12, T.STAPR12, T.STRKAGED, T.STRKDX, T.THRTAGED, T.THYRAGED, T.TRIAT12X, T.TRILI12X
		     , T.TRIPR12X, T.TRIST12X, T.TTLP12X
	FROM SALDATA.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 Full Year Consolidated Data Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Sophia Salmore";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHDAGED / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ALIMP12X / MISSPRINT  SCORES=TABLE;
	TABLES BUSNP12X / MISSPRINT  SCORES=TABLE;
	TABLES CASHP12X / MISSPRINT  SCORES=TABLE;
	TABLES CHLDP12X / MISSPRINT  SCORES=TABLE;
	TABLES CHOLAGED / MISSPRINT  SCORES=TABLE;
	TABLES COLOAGED / MISSPRINT  SCORES=TABLE;
	TABLES DIABAGED / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES DNTINS12 / MISSPRINT  SCORES=TABLE;
	TABLES DOBMM / MISSPRINT  SCORES=TABLE;
	TABLES DOBYY / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES EICRDT12 / MISSPRINT  SCORES=TABLE;
	TABLES ELGRND12 / MISSPRINT  SCORES=TABLE;
	TABLES EMPHAGED / MISSPRINT  SCORES=TABLE;
	TABLES EVRWRK / MISSPRINT  SCORES=TABLE;
	TABLES FAMID12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMSZE12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMWT12C / MISSPRINT  SCORES=TABLE;
	TABLES HIBPAGED / MISSPRINT  SCORES=TABLE;
	TABLES LEUKAGED / MISSPRINT  SCORES=TABLE;
	TABLES LUNGAGED / MISSPRINT  SCORES=TABLE;
	TABLES LYMPAGED / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12 / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12X / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12 / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12X / MISSPRINT  SCORES=TABLE;
	TABLES MCDAT12X / MISSPRINT  SCORES=TABLE;
	TABLES MCDHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES MCDMC12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPB12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12X / MISSPRINT  SCORES=TABLE;
	TABLES MCRPHO12 / MISSPRINT  SCORES=TABLE;
	TABLES MELAAGED / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES MSA12 / MISSPRINT  SCORES=TABLE;
	TABLES OHRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES OTHRAGED / MISSPRINT  SCORES=TABLE;
	TABLES OTPAAT12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPBAT12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPUBA12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPUBB12 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES PMDINS12 / MISSPRINT  SCORES=TABLE;
	TABLES PMNCNP12 / MISSPRINT  SCORES=TABLE;
	TABLES PRDRNP12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIDK12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIEU12 / MISSPRINT  SCORES=TABLE;
	TABLES PRING12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIOG12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIS12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIV12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIVAT12 / MISSPRINT  SCORES=TABLE;
	TABLES PROUT12 / MISSPRINT  SCORES=TABLE;
	TABLES PROXY12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVMNC12 / MISSPRINT  SCORES=TABLE;
	TABLES PUB12X / MISSPRINT  SCORES=TABLE;
	TABLES PUBAT12X / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES REFPRS12 / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES RESP12 / MISSPRINT  SCORES=TABLE;
	TABLES RFREL12X / MISSPRINT  SCORES=TABLE;
	TABLES RULETR12 / MISSPRINT  SCORES=TABLE;
	TABLES RUSIZE12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES SKDKAGED / MISSPRINT  SCORES=TABLE;
	TABLES SPOUIN12 / MISSPRINT  SCORES=TABLE;
	TABLES STAPR12 / MISSPRINT  SCORES=TABLE;
	TABLES STRKAGED / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES THRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES THYRAGED / MISSPRINT  SCORES=TABLE;
	TABLES TRIAT12X / MISSPRINT  SCORES=TABLE;
	TABLES TRILI12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIPR12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIST12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp';
%LET _CLIENTPROJECTNAME='QAC200_project_011215 USE.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(SALDATA.RECODED_1);

PROC SQL;
   CREATE TABLE SALDATA.RECODED_1(label="RECODED_1") AS 
   SELECT t1.ADAPPT42, 
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
          /* SAQ: Health in General SF-12V2 */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in General (recoded missing)" AS 'SAQ: Health in General SF-12V2'n, 
          /* SAQ Hlth Limits Mod Activities  */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Hlth Limits Mod Activities (recoded missing)" AS 'SAQ Hlth Limits Mod Activities 'n, 
          /* SAQ: Hlt Limits Climbing Stairs */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Hlt Limits Climbing Stairs (recoded missing)" AS 'SAQ: Hlt Limits Climbing Stairs'n, 
          /* SAQ 4WKS Accmp Less Bc Phy Probs */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accmp Less Bc Phy Probs (recoded missing)" AS 'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          /* SAQ 4WKS Work Limit Bc Phy Probs */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work Limit Bc Phy Probs (recoded missing)" AS 'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          /* SAQ 4WKS Accmp Less Bc Mnt Prbs */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accmp Less Bc Mnt Prbs (recoded missing)" AS 'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          /* SAQ Work Limits Bc Mnt Probs */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL=" Four Weeks Work Limits Bc Mnt Probs (recoded missing)" AS 'SAQ Work Limits Bc Mnt Probs'n, 
          /* SAQ Pain Limits Normal Work  */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Four Weeks Pain Limits Normal Work (recoded missing)" AS 'SAQ Pain Limits Normal Work 'n, 
          /* SAQ Felt Calm and Peaceful */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Four Weeks Felt Calm and Peaceful (recoded missing)" AS 'SAQ Felt Calm and Peaceful'n, 
          /* SAQ Had a lot of Energy */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Four Weeks Had a lot of energy (recoded missing)" AS 'SAQ Had a lot of Energy'n, 
          /* SAQ Felt Downhearted and depr */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Four weeks felt downhearted and depr (recoded missing)" AS 'SAQ Felt Downhearted and depr'n, 
          /* SAQ Hlt Stopped SOC Activ */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Four Weeks Health Stopped SOC Activ (recoded missing)" AS 'SAQ Hlt Stopped SOC Activ'n
      FROM SALDATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Query Builder   */
%LET SYSLAST=SALDATA.RECODED_1;
%LET _CLIENTTASKLABEL='Code For Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp';
%LET _CLIENTPROJECTNAME='QAC200_project_011215 USE.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

%_eg_conditional_dropds(WORK.RECODED);

PROC SQL;
   CREATE TABLE saldata.RECODED_ALL (label="RECODED_all") AS 
   SELECT ADAPPT42, 
          ADCAPE42, 
          ADCLIM42, 
          ADCMPD42, 
          ADCMPM42, 
          ADCMPY42, 
          ADDAYA42, 
          ADDOWN42, 
          ADDPRS42, 
          ADDRBP42, 
          ADEFRT42, 
          ADEGMC42, 
          ADEXPL42, 
          ADEZUN42, 
          ADFFRM42, 
          ADFHLP42, 
          ADGENH42, 
          ADHDAGED, 
          ADHECR42, 
          ADHOPE42, 
          ADILCR42, 
          ADILWW42, 
          ADINSA42, 
          ADINSB42, 
          ADINST42, 
          ADINTR42, 
          ADLANG42, 
          ADLIST42, 
          ADMALS42, 
          ADMWLM42, 
          ADNDCR42, 
          ADNERV42, 
          ADNRGY42, 
          ADNSMK42, 
          ADOVER42, 
          ADPAIN42, 
          ADPALS42, 
          ADPRTM42, 
          ADPRX42, 
          ADPWLM42, 
          ADRESP42, 
          ADREST42, 
          ADRISK42, 
          ADRTCR42, 
          ADRTWW42, 
          ADSAD42, 
          ADSMOK42, 
          ADSOCA42, 
          ADSPEC42, 
          ADSPRF42, 
          ADTLHW42, 
          ADWRTH42, 
          AGE12X, 
          ALIMP12X, 
          BUSNP12X, 
          CASHP12X, 
          CHLDP12X, 
          CHOLAGED, 
          COLOAGED, 
          DIABAGED, 
          DIABDX, 
          DNTINS12, 
          DOBMM, 
          DOBYY, 
          DUPERSID, 
          EDUYRDEG, 
          EICRDT12, 
          ELGRND12, 
          EMPHAGED, 
          EVRWRK, 
          FAMID12, 
          FAMINC12, 
          FAMSZE12, 
          FAMWT12C, 
          HIBPAGED, 
          LEUKAGED, 
          LUNGAGED, 
          LYMPAGED, 
          MARRY12X, 
          MCAID12, 
          MCAID12X, 
          MCARE12, 
          MCARE12X, 
          MCDAT12X, 
          MCDHMO12, 
          MCDMC12, 
          MCRPB12, 
          MCRPD12, 
          MCRPD12X, 
          MCRPHO12, 
          MELAAGED, 
          MIDX, 
          MSA12, 
          OHRTAGED, 
          OTHRAGED, 
          OTPAAT12, 
          OTPBAT12, 
          OTPUBA12, 
          OTPUBB12, 
          PCS42, 
          PHQ242, 
          PMDINS12, 
          PMNCNP12, 
          PRDRNP12, 
          PRIDK12, 
          PRIEU12, 
          PRING12, 
          PRIOG12, 
          PRIS12, 
          PRIV12, 
          PRIVAT12, 
          PROUT12, 
          PROXY12, 
          PRVHMO12, 
          PRVMNC12, 
          PUB12X, 
          PUBAT12X, 
          RACETHX, 
          REFPRS12, 
          REGION12, 
          RESP12, 
          RFREL12X, 
          RULETR12, 
          RUSIZE12, 
          SAQELIG, 
          SEX, 
          SFFLAG42, 
          SKDKAGED, 
          SPOUIN12, 
          STAPR12, 
          STRKAGED, 
          STRKDX, 
          THRTAGED, 
          THYRAGED, 
          TRIAT12X, 
          TRILI12X, 
          TRIPR12X, 
          TRIST12X, 
          TTLP12X, 
          /* Marital Status  */
            (CASE 
               WHEN -7 = MARRY12X THEN .
               WHEN -9 = MARRY12X THEN .
            END) LABEL="marital status (recoded missing)" AS 'Marital Status 'n, 
          /* Yeasr of education  */
            (CASE 
               WHEN -1 = EDUYRDEG THEN .
               WHEN -7 = EDUYRDEG THEN .
               WHEN -8 = EDUYRDEG THEN .
               WHEN -9 = EDUYRDEG THEN .
               ELSE EDUYRDEG
            END) LABEL="Year of education (recoded missing)" AS 'Yeasr of education 'n, 
          /* Marital status 1 */
            (CASE 
               WHEN -9 = SPOUIN12 THEN .
               ELSE SPOUIN12
            END) LABEL="Marital Status (recoded missing)" AS 'Marital status 1'n, 
          /* Ever Wrkd for pay in life */
            (CASE 
               WHEN -1 = EVRWRK THEN .
               WHEN -7 = EVRWRK THEN .
               WHEN -8 = EVRWRK THEN .
               WHEN -9 = EVRWRK THEN .
            END) LABEL="Ever Wrkd for pay in life (recoded missing)" AS 'Ever Wrkd for pay in life'n, 
          /* Family's total income */
            (CASE 
               WHEN -10794 = FAMINC12 THEN .
               WHEN -15612 = FAMINC12 THEN .
               WHEN -19924 = FAMINC12 THEN .
               WHEN -3772 = FAMINC12 THEN .
               WHEN -4246 = FAMINC12 THEN .
               WHEN -9063 = FAMINC12 THEN .
               ELSE FAMINC12
            END) LABEL="Family's total income (recoded missing)" AS 'Family''s total income'n, 
          /* Person's Total Income */
            (CASE 
               WHEN -1000 = TTLP12X THEN .
               WHEN -11459 = TTLP12X THEN .
               WHEN -1200 = TTLP12X THEN .
               WHEN -12002 = TTLP12X THEN .
               WHEN -1289 = TTLP12X THEN .
               WHEN -1500 = TTLP12X THEN .
               WHEN -15982 = TTLP12X THEN .
               WHEN -18132 = TTLP12X THEN .
               WHEN -18938 = TTLP12X THEN .
               WHEN -22919 = TTLP12X THEN .
               WHEN -26861 = TTLP12X THEN .
               WHEN -2890 = TTLP12X THEN .
               WHEN -3772 = TTLP12X THEN .
               WHEN -38074 = TTLP12X THEN .
               WHEN -4246 = TTLP12X THEN .
               WHEN -600 = TTLP12X THEN .
               WHEN -6981 = TTLP12X THEN .
               WHEN -7283 = TTLP12X THEN .
               WHEN -8051 = TTLP12X THEN .
               WHEN -9063 = TTLP12X THEN .
               WHEN -9903 = TTLP12X THEN .
               ELSE TTLP12X
            END) LABEL="Person's Total Income (recoded missing)" AS 'Person''s Total Income'n, 
          /* Number visits to med  */
            (CASE 
               WHEN -1 = ADAPPT42 THEN .
               WHEN -8 = ADAPPT42 THEN .
               WHEN -9 = ADAPPT42 THEN .
               ELSE ADAPPT42
            END) LABEL="Visits to med (recoded missing)" AS 'Number visits to med 'n, 
          /* Date completed day */
            (CASE 
               WHEN -1 = ADCMPD42 THEN .
               WHEN -9 = ADCMPD42 THEN .
               ELSE ADCMPD42
            END) LABEL="date completed day (recoded missing)" AS 'Date completed day'n, 
          /* Date completed month */
            (CASE 
               WHEN -1 = ADCMPM42 THEN .
               WHEN -9 = ADCMPM42 THEN .
               ELSE ADCMPM42
            END) LABEL="date completed month (recoded missing)" AS 'Date completed month'n, 
          /* Date completed year */
            (CASE 
               WHEN -1 = ADCMPY42 THEN .
               WHEN -9 = ADCMPY42 THEN .
               ELSE ADCMPY42
            END) LABEL="date completed year (recoded missing)" AS 'Date completed year'n, 
          /* Felt down and depressed  */
            (CASE 
               WHEN -1 = ADDPRS42 THEN .
               WHEN -7 = ADDPRS42 THEN .
               WHEN -8 = ADDPRS42 THEN .
               WHEN -9 = ADDPRS42 THEN .
               ELSE ADDPRS42
            END) LABEL="Depressed and hopeless (recoded missing)" AS 'Felt down and depressed 'n, 
          /* Dr checked blood pressure */
            (CASE 
               WHEN -1 = ADDRBP42 THEN .
               WHEN -8 = ADDRBP42 THEN .
               WHEN -9 = ADDRBP42 THEN .
               ELSE ADDRBP42
            END) LABEL="Blood pressure checked (recoded missing)" AS 'Dr checked blood pressure'n, 
          /* Easy getting needed med care */
            (CASE 
               WHEN -1 = ADEGMC42 THEN .
               WHEN -9 = ADEGMC42 THEN .
               ELSE ADEGMC42
            END) LABEL="Easy getting med care (recoded missing)" AS 'Easy getting needed med care'n, 
          /* Doc explained so understood */
            (CASE 
               WHEN -1 = ADEXPL42 THEN .
               WHEN -9 = ADEXPL42 THEN .
               ELSE ADEXPL42
            END) LABEL="Doc explained so understood (recoded missing)" AS 'Doc explained so understood'n, 
          /* Doctor given instructions  */
            (CASE 
               WHEN -1 = ADEZUN42 THEN .
               WHEN -9 = ADEZUN42 THEN .
               ELSE ADEZUN42
            END) LABEL="Instructions understood (recoded missing)" AS 'Doctor given instructions 'n, 
          'SAQ: Health in General SF-12V2'n, 
          'SAQ Hlth Limits Mod Activities'n, 
          'SAQ: Hlt Limits Climbing Stairs'n, 
          'SAQ 4WKS Accmp Less Bc Phy Probs'n, 
          'SAQ 4WKS Work Limit Bc Phy Probs'n, 
          'SAQ 4WKS Accmp Less Bc Mnt Prbs'n, 
          'SAQ Work Limits Bc Mnt Probs'n, 
          'SAQ Pain Limits Normal Work'n, 
          'SAQ Felt Calm and Peaceful'n, 
          'SAQ Had a lot of Energy'n, 
          'SAQ Felt Downhearted and depr'n, 
          'SAQ Hlt Stopped SOC Activ'n, 
          /* Got med appointment when wanted */
            (CASE 
               WHEN -1 = ADRTWW42 THEN .
               WHEN -9 = ADRTWW42 THEN .
               ELSE ADRTWW42
            END) LABEL="Got appointment (recoded missing)" AS 'Got med appointment when wanted'n, 
          /* Illness needing immediate care */
            (CASE 
               WHEN -1 = ADILCR42 THEN .
               WHEN -9 = ADILCR42 THEN .
               ELSE ADILCR42
            END) LABEL="In need of meediate care (recoded missing)" AS 'Illness needing immediate care'n, 
          /* Need any care test treatment */
            (CASE 
               WHEN -1 = ADNDCR42 THEN .
               WHEN -8 = ADNDCR42 THEN .
               WHEN -9 = ADNDCR42 THEN .
               ELSE ADNDCR42
            END) LABEL="Need any care (recoded missing)" AS 'Need any care test treatment'n, 
          /* Rating of Health Care */
            (CASE 
               WHEN 0 = ADHECR42 THEN .
               WHEN -1 = ADHECR42 THEN .
               WHEN -9 = ADHECR42 THEN .
               ELSE ADHECR42
            END) LABEL="Rating of Health Care (recoded missing)" AS 'Rating of Health Care'n, 
          /* Doctor Listened to You */
            (CASE 
               WHEN -1 = ADLIST42 THEN .
               WHEN -7 = ADLIST42 THEN .
               WHEN -9 = ADLIST42 THEN .
               ELSE ADLIST42
            END) LABEL="Doctor Listened to You (recoded missing)" AS 'Doctor Listened to You'n, 
          /* Needed to see specialist */
            (CASE 
               WHEN -1 = ADSPEC42 THEN .
               WHEN -9 = ADSPEC42 THEN .
            END) LABEL="Needed to see specialist (recoded missing)" AS 'Needed to see specialist'n
      FROM SALDATA.RECODED_1;
           
QUIT;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp';
%LET _CLIENTPROJECTNAME='QAC200_project_011215 USE.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 2:46:50 PM
   By task: Data Set Attributes

   Input Data: Local:SALDATA.RECODED_ALL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForRECODED_ALL);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=SALDATA.RECODED_ALL OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForRECODED_ALL(LABEL="Contents Details for RECODED_ALL");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForRECODED_ALL
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='RECODED_ALL';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForRECODED_ALL OUT=WORK.CONTContentsForRECODED_ALL;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForRECODED_ALL
		WHERE memname='RECODED_ALL';
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


/*   START OF NODE: One-Way Frequencies for 2012 Full Year Consolidated Data Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 Full Year Consolidated Data Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\ssalmore\UPDATED SAS program code USE\QAC200_project_011215 USE.egp';
%LET _CLIENTPROJECTNAME='QAC200_project_011215 USE.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 2:46:51 PM
   By task: One-Way Frequencies for 2012 Full Year Consolidated Data Subset

   Input Data: Local:SALDATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:SALDATA.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHDAGED, T.ADHECR42
		     , T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42
		     , T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.AGE12X, T.ALIMP12X, T.BUSNP12X, T.CASHP12X
		     , T.CHLDP12X, T.CHOLAGED, T.COLOAGED, T.DIABAGED, T.DIABDX, T.DNTINS12, T.DOBMM, T.DOBYY, T.EDUYRDEG, T.EICRDT12, T.ELGRND12, T.EMPHAGED, T.EVRWRK, T.FAMID12, T.FAMINC12, T.FAMSZE12, T.FAMWT12C, T.HIBPAGED, T.LEUKAGED, T.LUNGAGED
		     , T.LYMPAGED, T.MARRY12X, T.MCAID12, T.MCAID12X, T.MCARE12, T.MCARE12X, T.MCDAT12X, T.MCDHMO12, T.MCDMC12, T.MCRPB12, T.MCRPD12, T.MCRPD12X, T.MCRPHO12, T.MELAAGED, T.MIDX, T.MSA12, T.OHRTAGED, T.OTHRAGED, T.OTPAAT12, T.OTPBAT12
		     , T.OTPUBA12, T.OTPUBB12, T.PCS42, T.PHQ242, T.PMDINS12, T.PMNCNP12, T.PRDRNP12, T.PRIDK12, T.PRIEU12, T.PRING12, T.PRIOG12, T.PRIS12, T.PRIV12, T.PRIVAT12, T.PROUT12, T.PROXY12, T.PRVHMO12, T.PRVMNC12, T.PUB12X, T.PUBAT12X
		     , T.RACETHX, T.REFPRS12, T.REGION12, T.RESP12, T.RFREL12X, T.RULETR12, T.RUSIZE12, T.SAQELIG, T.SEX, T.SFFLAG42, T.SKDKAGED, T.SPOUIN12, T.STAPR12, T.STRKAGED, T.STRKDX, T.THRTAGED, T.THYRAGED, T.TRIAT12X, T.TRILI12X
		     , T.TRIPR12X, T.TRIST12X, T.TTLP12X
	FROM SALDATA.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for the 2012 Full Year Consolidated Data Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Sophia Salmore";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHDAGED / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ALIMP12X / MISSPRINT  SCORES=TABLE;
	TABLES BUSNP12X / MISSPRINT  SCORES=TABLE;
	TABLES CASHP12X / MISSPRINT  SCORES=TABLE;
	TABLES CHLDP12X / MISSPRINT  SCORES=TABLE;
	TABLES CHOLAGED / MISSPRINT  SCORES=TABLE;
	TABLES COLOAGED / MISSPRINT  SCORES=TABLE;
	TABLES DIABAGED / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES DNTINS12 / MISSPRINT  SCORES=TABLE;
	TABLES DOBMM / MISSPRINT  SCORES=TABLE;
	TABLES DOBYY / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES EICRDT12 / MISSPRINT  SCORES=TABLE;
	TABLES ELGRND12 / MISSPRINT  SCORES=TABLE;
	TABLES EMPHAGED / MISSPRINT  SCORES=TABLE;
	TABLES EVRWRK / MISSPRINT  SCORES=TABLE;
	TABLES FAMID12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMSZE12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMWT12C / MISSPRINT  SCORES=TABLE;
	TABLES HIBPAGED / MISSPRINT  SCORES=TABLE;
	TABLES LEUKAGED / MISSPRINT  SCORES=TABLE;
	TABLES LUNGAGED / MISSPRINT  SCORES=TABLE;
	TABLES LYMPAGED / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12 / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12X / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12 / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12X / MISSPRINT  SCORES=TABLE;
	TABLES MCDAT12X / MISSPRINT  SCORES=TABLE;
	TABLES MCDHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES MCDMC12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPB12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12 / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12X / MISSPRINT  SCORES=TABLE;
	TABLES MCRPHO12 / MISSPRINT  SCORES=TABLE;
	TABLES MELAAGED / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES MSA12 / MISSPRINT  SCORES=TABLE;
	TABLES OHRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES OTHRAGED / MISSPRINT  SCORES=TABLE;
	TABLES OTPAAT12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPBAT12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPUBA12 / MISSPRINT  SCORES=TABLE;
	TABLES OTPUBB12 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES PMDINS12 / MISSPRINT  SCORES=TABLE;
	TABLES PMNCNP12 / MISSPRINT  SCORES=TABLE;
	TABLES PRDRNP12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIDK12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIEU12 / MISSPRINT  SCORES=TABLE;
	TABLES PRING12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIOG12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIS12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIV12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIVAT12 / MISSPRINT  SCORES=TABLE;
	TABLES PROUT12 / MISSPRINT  SCORES=TABLE;
	TABLES PROXY12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVMNC12 / MISSPRINT  SCORES=TABLE;
	TABLES PUB12X / MISSPRINT  SCORES=TABLE;
	TABLES PUBAT12X / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES REFPRS12 / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES RESP12 / MISSPRINT  SCORES=TABLE;
	TABLES RFREL12X / MISSPRINT  SCORES=TABLE;
	TABLES RULETR12 / MISSPRINT  SCORES=TABLE;
	TABLES RUSIZE12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES SKDKAGED / MISSPRINT  SCORES=TABLE;
	TABLES SPOUIN12 / MISSPRINT  SCORES=TABLE;
	TABLES STAPR12 / MISSPRINT  SCORES=TABLE;
	TABLES STRKAGED / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES THRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES THYRAGED / MISSPRINT  SCORES=TABLE;
	TABLES TRIAT12X / MISSPRINT  SCORES=TABLE;
	TABLES TRILI12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIPR12X / MISSPRINT  SCORES=TABLE;
	TABLES TRIST12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
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
