/* ----------------------------------------
Kod wyeksportowany z SAS Enterprise Guide
---------------------------------------- */

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
%mend _sas_pushchartsize;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend _sas_popchartsize;


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


%*--------------------------------------------------------------*
 * Tests the current version against a required version. A      *
 * negative result means that the SAS server version is less    *
 * than the version required.  A positive result means that     *
 * the SAS server version is greater than the version required. *
 * A result of zero indicates that the SAS server is exactly    *
 * the version required.                                        *
 *                                                              *
 * NOTE: The parameter maint is optional.                       *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP(major, minor, maint);
    %_SAS_VERCOMP_FV(&major, &minor, &maint, &major, &minor, &maint)
%mend _SAS_VERCOMP;

%*--------------------------------------------------------------*
 * Tests the current version against either the required        *
 * foundation or Viya required version depending on whether the *
 * SYSVLONG version is a foundation or Viya one. A negative     *
 * result means that the SAS server version is less than the    *
 * version required.  A positive result means that the SAS      *
 * server version is greater than the version required. A       *
 * result of zero indicates that the SAS server is exactly the  *
 * version required.                                            *
 *                                                              *
 * NOTE: The *maint parameters are optional.                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP_FV(fmajor, fminor, fmaint, vmajor, vminor, vmaint);
    %local major;
    %local minor;
    %local maint;
    %local CurMaj;
    %local CurMin;
    %local CurMnt;

    %* Pull the current version string apart.;
    %let CurMaj = %scan(&sysvlong, 1, %str(.));

    %* The Viya version number has a V on the front which means
       we need to adjust the Maint SCAN funtion index and also
       get the appropriate parameters for the major, minor, and
       maint values we need to check against (foundation or Viya);
    %if %eval(&CurMaj EQ V) %then
        %do;
		   %*   MM mm t           MM = Major version , mm = Minor version , t = Maint version ;
		   %* V.03.04M2P07112018 ;

            %let major = &vmajor;
            %let minor = &vminor;
            %let maint = &vmaint;
			%let CurMaj = %scan(&sysvlong, 2, %str(.));
			%* Index is purposely 2 because V is now one of the scan delimiters ;
			%let CurMin = %scan(&sysvlong, 2, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
			%let CurMnt = %scan(&sysvlong, 3, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;
    %else
        %do;
		    %* M mm    t           M = Major version , mm = Minor version , t = Maint version ;  
		    %* 9.01.02M0P11212005 ;

            %let major = &fmajor;
            %let minor = &fminor;
            %let maint = &fmaint;
			%let CurMin = %scan(&sysvlong, 2, %str(.));
			%let CurMnt = %scan(&sysvlong, 4, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;

    %* Now perform the version comparison.;
    %if %eval(&major NE &CurMaj) %then
        %eval(&CurMaj - &major);
    %else
        %if %eval(&minor NE &CurMin) %then
            %eval(&CurMin - &minor);
        %else
            %if "&maint" = "" %then
                %str(0);
            %else
                %eval(&CurMnt - &maint);
%mend _SAS_VERCOMP_FV;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCONDCODE_FV() with the passed       *
 * version. If the current server version matches or is newer,  *
 * then the true code (tcode) is executed, else the false code  *
 * (fcode) is executed.                                         *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE(9,2,0,                                 *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE( major, minor, maint, tcode=, fcode= );
    %_SAS_VERCONDCODE_FV( &major, &minor, &maint, &major, &minor, &maint, &tcode, fcode )
%mend _SAS_VERCONDCODE;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCOMP_FV() with the passed versions. *
 * If the current server version matches or is newer, then the  *
 * true code (tcode) is executed, else the false code (fcode)   *
 * is executed.                                                 *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE_FV(9,2,0, 3,5,0                        *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE_FV( fmajor, fminor, fmaint, vmajor, vminor, vmaint, tcode=, fcode= );
    %if %_SAS_VERCOMP_FV(&fmajor, &fminor, &fmaint, &vmajor, &vminor, &vmaint) >= 0 %then
        %do;
        &tcode
        %end;
    %else
        %do;
        &fcode
        %end;
%mend _SAS_VERCONDCODE_FV;

%*--------------------------------------------------------------*
 * Tests the current version to see if it is a Viya version     *
 * number.                                                      *
 * A result of 1 indicates that the SAS server is a Viya        *
 * server.                                                      *
 * A zero result indicates that the server version is not       *
 * that of a Viya server.                                       *
 *--------------------------------------------------------------*;
%macro _SAS_ISVIYA;
    %local Major;

    %* Get the major component of the current version string.;
    %let Major = %scan(&sysvlong, 1, %str(.));

    %* Check if it it V for Viya.;
    %if %eval(&Major EQ V) %then
        %str(1);
    %else
        %str(0);
%mend _SAS_ISVIYA;


ODS PROCTITLE;
OPTIONS DEV=SVG;
GOPTIONS XPIXELS=0 YPIXELS=0;
%macro HTML5AccessibleGraphSupported;
    %if %_SAS_VERCOMP_FV(9,4,4, 0,0,0) >= 0 %then ACCESSIBLE_GRAPH;
%mend;
FILENAME EGHTMLX TEMP;
ODS HTML5(ID=EGHTMLX) FILE=EGHTMLX
    OPTIONS(BITMAP_MODE='INLINE')
    %HTML5AccessibleGraphSupported
    ENCODING='utf-8'
    STYLE=HtmlBlue
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
;

/*   POCZ�TEK W�Z�A: Import danych (CH_final.csv)   */
%LET _CLIENTTASKLABEL='Import danych (CH_final.csv)';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* --------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-owe
   
   Wygenerowano czwartek, 1 czerwca 2023 na 20:22:43
   Zadanie:     Kreator importu danych
   
   Plik �r�d�owy: C:\Users\wikto\Desktop\Nowy folder (2)\CH_final.csv
   Serwer:      Lokalny system plik�w
   
   Dane wynikowe: WORK.CH_final
   Serwer:      Local
   -------------------------------------------------------------------- */

DATA WORK.CH_final;
    LENGTH
        name             $ 8
        essround           8
        edition            8
        proddate           8
        idno               8
        cntry            $ 2
        dweight            8
        pspwght            8
        pweight            8
        anweight           8
        prob               8
        stratum            8
        psu                8
        netusoft           8
        trstplc            8
        stfeco             8
        stfhlth            8
        happy              8
        sclmeet            8
        inprdsc            8
        sclact             8
        aesfdrk            8
        health             8
        rlgblg             8
        dscrrce            8
        gndr               8
        agea               8
        eisced             8
        eduyrs             8
        hincfel            8
        livpnt             8
        ipgdtim            8 ;
    FORMAT
        name             $CHAR8.
        essround         BEST2.
        edition          BEST3.
        proddate         DDMMYY10.
        idno             BEST5.
        cntry            $CHAR2.
        dweight          BEST1.
        pspwght          BEST10.
        pweight          BEST10.
        anweight         BEST10.
        prob             BEST13.
        stratum          BEST3.
        psu              BEST4.
        netusoft         BEST1.
        trstplc          BEST2.
        stfeco           BEST2.
        stfhlth          BEST2.
        happy            BEST2.
        sclmeet          BEST2.
        inprdsc          BEST2.
        sclact           BEST1.
        aesfdrk          BEST1.
        health           BEST1.
        rlgblg           BEST1.
        dscrrce          BEST1.
        gndr             BEST1.
        agea             BEST3.
        eisced           BEST2.
        eduyrs           BEST2.
        hincfel          BEST1.
        livpnt           BEST1.
        ipgdtim          BEST1. ;
    INFORMAT
        name             $CHAR8.
        essround         BEST2.
        edition          BEST3.
        proddate         DDMMYY10.
        idno             BEST5.
        cntry            $CHAR2.
        dweight          BEST1.
        pspwght          BEST10.
        pweight          BEST10.
        anweight         BEST10.
        prob             BEST13.
        stratum          BEST3.
        psu              BEST4.
        netusoft         BEST1.
        trstplc          BEST2.
        stfeco           BEST2.
        stfhlth          BEST2.
        happy            BEST2.
        sclmeet          BEST2.
        inprdsc          BEST2.
        sclact           BEST1.
        aesfdrk          BEST1.
        health           BEST1.
        rlgblg           BEST1.
        dscrrce          BEST1.
        gndr             BEST1.
        agea             BEST3.
        eisced           BEST2.
        eduyrs           BEST2.
        hincfel          BEST1.
        livpnt           BEST1.
        ipgdtim          BEST1. ;
    INFILE 'C:\Users\wikto\AppData\Roaming\SAS\EnterpriseGuide\EGTEMP\SEG-18680-333b7e2d\contents\CH_final-dafc80341a464938b77ed42b68dc2eb8.txt'
        LRECL=137
        ENCODING="WLATIN2"
        TERMSTR=CRLF
        DLM='7F'x
        MISSOVER
        DSD ;
    INPUT
        name             : $CHAR8.
        essround         : ?? BEST2.
        edition          : ?? COMMA3.
        proddate         : ?? DDMMYY10.
        idno             : ?? BEST5.
        cntry            : $CHAR2.
        dweight          : ?? BEST1.
        pspwght          : ?? COMMA10.
        pweight          : ?? COMMA10.
        anweight         : ?? COMMA10.
        prob             : ?? COMMA13.
        stratum          : ?? BEST3.
        psu              : ?? BEST4.
        netusoft         : ?? BEST1.
        trstplc          : ?? BEST2.
        stfeco           : ?? BEST2.
        stfhlth          : ?? BEST2.
        happy            : ?? BEST2.
        sclmeet          : ?? BEST2.
        inprdsc          : ?? BEST2.
        sclact           : ?? BEST1.
        aesfdrk          : ?? BEST1.
        health           : ?? BEST1.
        rlgblg           : ?? BEST1.
        dscrrce          : ?? BEST1.
        gndr             : ?? BEST1.
        agea             : ?? BEST3.
        eisced           : ?? BEST2.
        eduyrs           : ?? BEST2.
        hincfel          : ?? BEST1.
        livpnt           : ?? BEST1.
        ipgdtim          : ?? BEST1. ;
RUN;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Rekodowanie zmiennych SQL   */
%LET _CLIENTTASKLABEL='Rekodowanie zmiennych SQL';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

PROC SQL;
CREATE TABLE szwajcaria AS
SELECT happy, 
  CASE 
    WHEN happy <= 8 THEN 'Nieszcz�liwy/-a'
    WHEN happy >= 9 THEN 'Szcz�liwy/-a'
  END AS szczescie,

  CASE
    WHEN netusoft = 1 THEN 'Nigdy'
    WHEN netusoft IN (2,3) THEN 'Czasami'
    WHEN netusoft IN (4,5) THEN 'Cz�sto'
  END AS internet_czestosc,

  CASE
    WHEN trstplc BETWEEN 0 AND 3 THEN 'Brak zaufania'
    WHEN trstplc BETWEEN 4 AND 7 THEN 'Przeci�tne zaufanie'
    WHEN trstplc BETWEEN 8 AND 10 THEN 'Wysokie zaufanie'
  END AS policja_zaufanie,

  CASE
    WHEN stfeco BETWEEN 0 AND 3 THEN 'Niska satysfakcja'
    WHEN stfeco BETWEEN 4 AND 7 THEN 'Przeci�tna satysfakcja'
    WHEN stfeco BETWEEN 8 AND 10 THEN 'Wysoka satysfakcja'
  END AS ekonomia_satysfakcja,

  CASE
    WHEN stfhlth BETWEEN 0 AND 3 THEN 'Z�y stan'
    WHEN stfhlth BETWEEN 4 AND 7 THEN 'Przeci�tny stan'
    WHEN stfhlth BETWEEN 8 AND 10 THEN 'Bardzo dobry stan'
  END AS uslugi_zdrowotne_stan,

  CASE
    WHEN sclmeet IN (1,2) THEN 'Rzadko'
    WHEN sclmeet IN (3,4) THEN 'Czasami'
    WHEN sclmeet IN (5,6,7) THEN 'Cz�sto'
  END AS spotkania,

  CASE
    WHEN inprdsc = 0 THEN 'Zero os�b'
    WHEN inprdsc BETWEEN 1 AND 3 THEN 'Od 1 do 3 os�b'
    WHEN inprdsc IN (4,5) THEN 'Od 4 do 9 os�b'
    WHEN inprdsc = 6 THEN '10 lub wi�cej os�b'
  END AS osoby_rozmowy,

  CASE
    WHEN sclact IN (1,2) THEN 'Mniejszy (ni� innych os�b)'
    WHEN sclact = 3 THEN 'Taki sam (jak innych os�b)'
    WHEN sclact IN (4,5) THEN 'Wi�kszy (ni� innych os�b)'
  END AS akt_spol,

  CASE
    WHEN aesfdrk IN (1,2) THEN 'Wysokie (poczucie bezpiecze�stwa)'
    WHEN aesfdrk IN (3,4) THEN 'Niskie (poczucie bezpiecze�stwa)'
  END AS spacer_zmrok,

  CASE
    WHEN health IN (1,2) THEN 'Dobry stan zdrowia'
    WHEN health = 3 THEN 'Przeci�tny stan zdrowia'
    WHEN health IN (4,5) THEN 'Z�y stan zdrowia'
  END AS zdrowie,

  CASE
    WHEN rlgblg = 1 THEN 'Tak, przynale��'
    WHEN rlgblg = 2 THEN 'Nie przynale��'
  END AS religia,

  CASE
    WHEN dscrrce = 1 THEN 'Tak, jestem/by�em/-am dyskryminowany/-a'
    WHEN dscrrce = 0 THEN 'Nie jestem/by�em/-am dyskryminowany/-a'
  END AS dyskryminacja,

  CASE
    WHEN gndr = 1 THEN 'M�czyzna'
    WHEN gndr = 2 THEN 'Kobieta'
  END AS plec,

/*  case*/
/*    when agea <= 38 then 'm�ody wiek'*/
/*    when agea between 39 and 60 then '�redni wiek'*/
/*    when agea >= 60 then 'stary wiek'*/
/*  end as */
  agea as wiek,

  CASE
    WHEN eisced BETWEEN 1 AND 4 THEN 'Niskie wykszta�cenie (szko�a �rednia i poni�ej)'
    WHEN eisced = 5 THEN '�rednie wykszta�cenie (zaawansowane zawodowe, podyplomowe)'
    WHEN eisced IN (6,7) THEN 'Wy�sze wykszta�cenie (licencjat, magister)'
  END AS edukacja,

  CASE
    WHEN eduyrs <= 12 THEN 'Wykszta�cenie podstawowe'
    WHEN eduyrs > 12 THEN 'Wykszta�cenie wy�sze'
  END AS edukacja_lata,

  CASE
    WHEN hincfel = 1 THEN 'Wygodne �ycie (przy obecnych dochodach)'
    WHEN hincfel = 2 THEN 'Radz� sobie (przy obecnych dochodach)'
    WHEN hincfel IN (3,4) THEN 'Ci�kie �ycie (przy obecnych dochodach)'
  END AS dochody_odczucie,

  CASE
    WHEN livpnt = 1 THEN 'Tak, oboje'
    WHEN livpnt IN (2,3) THEN 'Jedno �yje'
    WHEN livpnt = 4 THEN 'Nie �yj�'
  END AS rodzice,

  CASE
    WHEN ipgdtim IN (1,2) THEN 'Istotne'
    WHEN ipgdtim IN (3,4) THEN '�rednio istotne'
    WHEN ipgdtim IN (5,6) THEN 'Nieistotne'
  END AS dobra_zabawa
FROM CH_final
WHERE 
  (happy BETWEEN 0 AND 10) AND
  (netusoft BETWEEN 1 AND 5) AND
  (trstplc BETWEEN 0 AND 10) AND
  (stfeco BETWEEN 0 AND 10) AND
  (stfhlth BETWEEN 0 AND 10) AND
  (sclmeet BETWEEN 1 AND 7) AND
  (inprdsc BETWEEN 0 AND 6) AND
  (sclact BETWEEN 1 AND 5) AND
  (aesfdrk BETWEEN 1 AND 4) AND
  (health BETWEEN 1 AND 5) AND
  (rlgblg BETWEEN 1 AND 2) AND
  (dscrrce BETWEEN 0 AND 1) AND
  (gndr BETWEEN 1 AND 2) AND
  (agea > 0) AND
  (agea <> 999) AND
  (eisced BETWEEN 1 AND 7) AND
  (eduyrs <> 88) AND
  (hincfel BETWEEN 1 AND 4) AND
  (livpnt BETWEEN 1 AND 4) AND
  (ipgdtim BETWEEN 1 AND 6);

QUIT;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;


/*   POCZ�TEK W�Z�A: Pie Chart   */
%LET _CLIENTTASKLABEL='Pie Chart';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:24
   Przez zadanie: Pie Chart

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rozk�ad zmiennej Szcz�cie";
FOOTNOTE;
FOOTNOTE2 "Opracowanie w�asne za pomoc� SAS";
PROC GCHART DATA =WORK.SORTTempTableSorted
;
	PIE	 szczescie /
 	NOLEGEND
	SLICE=OUTSIDE
	PERCENT=OUTSIDE
	VALUE=OUTSIDE
	OTHER=4
	OTHERLABEL="Inne"
	COUTLINE=BLACK
NOHEADING
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression    */
%LET _CLIENTTASKLABEL='Logistic Regression ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:24
   Przez zadanie: Logistic Regression 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.spotkania, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.rodzice, T.dobra_zabawa, T.osoby_rozmowy, T.akt_spol
		     , T.spacer_zmrok, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) spotkania 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF)
	  rodzice 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) osoby_rozmowy 	(PARAM=REF) akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan spotkania edukacja edukacja_lata dochody_odczucie rodzice dobra_zabawa osoby_rozmowy akt_spol spacer_zmrok zdrowie religia dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  - -usuni�cie rodzice   */
%LET _CLIENTTASKLABEL='Logistic Regression  - -usuni�cie rodzice';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:24
   Przez zadanie: Logistic Regression  - -usuni�cie rodzice

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.spotkania, T.osoby_rozmowy, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol
		     , T.spacer_zmrok, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) spotkania 	(PARAM=REF) osoby_rozmowy 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF)
	  dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan spotkania edukacja edukacja_lata dochody_odczucie dobra_zabawa akt_spol spacer_zmrok zdrowie religia dyskryminacja plec osoby_rozmowy		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  - usuni�cie osoby rozm   */
%LET _CLIENTTASKLABEL='Logistic Regression  - usuni�cie osoby rozm';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:25
   Przez zadanie: Logistic Regression  - usuni�cie osoby rozm

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.spotkania, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol, T.spacer_zmrok, T.zdrowie
		     , T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) spotkania 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF)
	  dobra_zabawa 	(PARAM=REF) akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan spotkania edukacja edukacja_lata dochody_odczucie dobra_zabawa akt_spol spacer_zmrok zdrowie religia dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  -usuni�cie spacer_zmrok   */
%LET _CLIENTTASKLABEL='Logistic Regression  -usuni�cie spacer_zmrok';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:25
   Przez zadanie: Logistic Regression  -usuni�cie spacer_zmrok

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.spotkania, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol, T.spacer_zmrok, T.zdrowie, T.religia
		     , T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) spotkania 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF)
	  akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan spotkania edukacja_lata dochody_odczucie dobra_zabawa akt_spol spacer_zmrok zdrowie religia dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  - usuni�cie edukacja   */
%LET _CLIENTTASKLABEL='Logistic Regression  - usuni�cie edukacja';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:25
   Przez zadanie: Logistic Regression  - usuni�cie edukacja

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.spotkania, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol, T.zdrowie, T.religia, T.dyskryminacja
		     , T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) spotkania 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF)
	  akt_spol 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan spotkania edukacja_lata dochody_odczucie dobra_zabawa akt_spol zdrowie religia dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  -usuni�cie spotkaniaCopy   */
%LET _CLIENTTASKLABEL='Logistic Regression  -usuni�cie spotkaniaCopy';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:26
   Przez zadanie: Logistic Regression  -usuni�cie spotkaniaCopy

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) akt_spol 	(PARAM=REF)
	  zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja_lata dochody_odczucie dobra_zabawa akt_spol zdrowie religia dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  - usuni�cie aktywno�� spo�   */
%LET _CLIENTTASKLABEL='Logistic Regression  - usuni�cie aktywno�� spo�';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:26
   Przez zadanie: Logistic Regression  - usuni�cie aktywno�� spo�

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF)
	  religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja_lata dochody_odczucie dobra_zabawa zdrowie religia dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  - usuni�cie religia- Copy   */
%LET _CLIENTTASKLABEL='Logistic Regression  - usuni�cie religia- Copy';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:26
   Przez zadanie: Logistic Regression  - usuni�cie religia- Copy

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF)
	  dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja_lata dochody_odczucie dobra_zabawa zdrowie dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression-usuni�cie edukacja_lata   */
%LET _CLIENTTASKLABEL='Logistic Regression-usuni�cie edukacja_lata';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:27
   Przez zadanie: Logistic Regression-usuni�cie edukacja_lata

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF) dyskryminacja 	(PARAM=REF)
	  plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan dochody_odczucie dobra_zabawa zdrowie dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  -usuni�cie internet cz�sto��   */
%LET _CLIENTTASKLABEL='Logistic Regression  -usuni�cie internet cz�sto��';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:27
   Przez zadanie: Logistic Regression  -usuni�cie internet cz�sto��

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan dochody_odczucie dobra_zabawa zdrowie dyskryminacja plec		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  - usuni�cie p�e�   */
%LET _CLIENTTASKLABEL='Logistic Regression  - usuni�cie p�e�';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:27
   Przez zadanie: Logistic Regression  - usuni�cie p�e�

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie, T.dyskryminacja
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF) dyskryminacja 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan dochody_odczucie dobra_zabawa zdrowie dyskryminacja		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression - u�ugi_zdrowotne_stan   */
%LET _CLIENTTASKLABEL='Logistic Regression - u�ugi_zdrowotne_stan';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:28
   Przez zadanie: Logistic Regression - u�ugi_zdrowotne_stan

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.policja_zaufanie, T.ekonomia_satysfakcja, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie, T.dyskryminacja
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF) dyskryminacja 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek policja_zaufanie ekonomia_satysfakcja dochody_odczucie dobra_zabawa zdrowie dyskryminacja		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Logistic Regression  -usuni�cie dyskryminacja   */
%LET _CLIENTTASKLABEL='Logistic Regression  -usuni�cie dyskryminacja';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:28
   Przez zadanie: Logistic Regression  -usuni�cie dyskryminacja

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.policja_zaufanie, T.ekonomia_satysfakcja, T.dochody_odczucie, T.dobra_zabawa, T.zdrowie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ODDSRATIO
		PLOTS(ONLY)=ROC
	;
	CLASS policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) zdrowie 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek policja_zaufanie ekonomia_satysfakcja dochody_odczucie dobra_zabawa zdrowie		/
		SELECTION=NONE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - policja_zaufanie   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - policja_zaufanie';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:28
   Przez zadanie: Regresja - Interakcje - policja_zaufanie

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.ekonomia_satysfakcja, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.dyskryminacja, T.plec, T.zdrowie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF) zdrowie 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc ekonomia_satysfakcja edukacja_lata dochody_odczucie dobra_zabawa zdrowie dyskryminacja plec wiek*internet_czestosc wiek*ekonomia_satysfakcja wiek*edukacja_lata wiek*dochody_odczucie wiek*dobra_zabawa wiek*zdrowie wiek*dyskryminacja wiek*plec internet_czestosc*ekonomia_satysfakcja internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*dobra_zabawa internet_czestosc*zdrowie internet_czestosc*dyskryminacja internet_czestosc*plec ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec edukacja_lata*dochody_odczucie edukacja_lata*dobra_zabawa edukacja_lata*zdrowie edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*dobra_zabawa dochody_odczucie*zdrowie dochody_odczucie*dyskryminacja dochody_odczucie*plec dobra_zabawa*zdrowie dobra_zabawa*dyskryminacja dobra_zabawa*plec zdrowie*dyskryminacja zdrowie*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=9
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�cie internet_cz�sto��   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�cie internet_cz�sto��';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:29
   Przez zadanie: Regresja - Interakcje - usuni�cie internet_cz�sto��

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.ekonomia_satysfakcja, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.dyskryminacja, T.plec, T.zdrowie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS ekonomia_satysfakcja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF) zdrowie 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek ekonomia_satysfakcja edukacja_lata dochody_odczucie dobra_zabawa zdrowie dyskryminacja plec wiek*ekonomia_satysfakcja wiek*edukacja_lata wiek*dochody_odczucie wiek*dobra_zabawa wiek*zdrowie wiek*dyskryminacja wiek*plec ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec edukacja_lata*dochody_odczucie edukacja_lata*dobra_zabawa edukacja_lata*zdrowie edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*dobra_zabawa dochody_odczucie*zdrowie dochody_odczucie*dyskryminacja dochody_odczucie*plec dobra_zabawa*zdrowie dobra_zabawa*dyskryminacja dobra_zabawa*plec zdrowie*dyskryminacja zdrowie*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=8
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - zmienna celu   */
%LET _CLIENTTASKLABEL='Liczebno�� - zmienna celu';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:29
   Przez zadanie: Liczebno�� - zmienna celu

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Liczebno�� - zmienna celu";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 szczescie
 /
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
PATTERNID=MIDPOINT
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Cz�sto�� korzystania z internetu    */
%LET _CLIENTTASKLABEL='Liczebno�� - Cz�sto�� korzystania z internetu ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:29
   Przez zadanie: Liczebno�� - Cz�sto�� korzystania z internetu 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.internet_czestosc, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Cz�sto�� korzystania z internetu";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 internet_czestosc
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Poziom ufno�ci policji    */
%LET _CLIENTTASKLABEL='Liczebno�� - Poziom ufno�ci policji ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:29
   Przez zadanie: Liczebno�� - Poziom ufno�ci policji 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.policja_zaufanie, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Poziom ufno�ci policji";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 policja_zaufanie
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Satysfakcja z sytuacji ekonomicznej w kraju    */
%LET _CLIENTTASKLABEL='Liczebno�� - Satysfakcja z sytuacji ekonomicznej w kraju ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:29
   Przez zadanie: Liczebno�� - Satysfakcja z sytuacji ekonomicznej w kraju 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ekonomia_satysfakcja, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Satysfakcja z sytuacji ekonomicznej  w kraju";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 ekonomia_satysfakcja
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Obecny stan us�ug zdrowotnych w kraju    */
%LET _CLIENTTASKLABEL='Liczebno�� - Obecny stan us�ug zdrowotnych w kraju ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:30
   Przez zadanie: Liczebno�� - Obecny stan us�ug zdrowotnych w kraju 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.uslugi_zdrowotne_stan, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Obecny stan us�ug zdrowotnych  w kraju";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 uslugi_zdrowotne_stan
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Intensywno�� spotka� ze znajomymi/krewnymi    */
%LET _CLIENTTASKLABEL='Liczebno�� - Intensywno�� spotka� ze znajomymi/krewnymi ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:30
   Przez zadanie: Liczebno�� - Intensywno�� spotka� ze znajomymi/krewnymi 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.spotkania, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Intensywno�� spotka� ze znajomymi/krewnymi";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 spotkania
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Liczba os�b, z kt�rymi respondent mo�e rozmawia� o sprawach intymnych i osobistych    */
%LET _CLIENTTASKLABEL='Liczebno�� - Liczba os�b, z kt�rymi respondent mo�e rozmawia� o sprawach intymnych i osobistych ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:30
   Przez zadanie: Liczebno�� - Liczba os�b, z kt�rymi respondent mo�e rozmawia� o sprawach intymnych i osobistych 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.osoby_rozmowy, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Liczba os�b, z kt�rymi respondent mo�e rozmawia� o sprawach intymnych i osobistych";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 osoby_rozmowy
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Udzia� w aktywno�ciach spo�ecznych w por�wnaniu z innymi osobami w tym samym wieku    */
%LET _CLIENTTASKLABEL='Liczebno�� - Udzia� w aktywno�ciach spo�ecznych w por�wnaniu z innymi osobami w tym samym wieku ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:30
   Przez zadanie: Liczebno�� - Udzia� w aktywno�ciach spo�ecznych w por�wnaniu z innymi osobami w tym samym wieku 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.akt_spol, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Udzia� w aktywno�ciach spo�ecznych w por�wnaniu z innymi osobami w tym samym wieku";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 akt_spol
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Poczucie bezpiecze�stwa podczas samotnych spacer�w w okolicy po zmroku    */
%LET _CLIENTTASKLABEL='Liczebno�� - Poczucie bezpiecze�stwa podczas samotnych spacer�w w okolicy po zmroku ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:30
   Przez zadanie: Liczebno�� - Poczucie bezpiecze�stwa podczas samotnych spacer�w w okolicy po zmroku 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.spacer_zmrok, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Poczucie bezpiecze�stwa podczas samotnych spacer�w w okolicy po zmroku";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 spacer_zmrok
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Stan zdrowia   */
%LET _CLIENTTASKLABEL='Liczebno�� - Stan zdrowia';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:31
   Przez zadanie: Liczebno�� - Stan zdrowia

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.zdrowie, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Stan zdrowia";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 zdrowie
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Przynale�no�� do okre�lonej religii lub wyznania    */
%LET _CLIENTTASKLABEL='Liczebno�� - Przynale�no�� do okre�lonej religii lub wyznania ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:31
   Przez zadanie: Liczebno�� - Przynale�no�� do okre�lonej religii lub wyznania 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.religia, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Przynale�no�� do okre�lonej religii lub wyznania";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 religia
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Dyskryminacja grupy respondenta: kolor sk�ry lub rasa    */
%LET _CLIENTTASKLABEL='Liczebno�� - Dyskryminacja grupy respondenta: kolor sk�ry lub rasa ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:31
   Przez zadanie: Liczebno�� - Dyskryminacja grupy respondenta: kolor sk�ry lub rasa 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.dyskryminacja, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Dyskryminacja grupy respondenta: kolor sk�ry lub rasa";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 dyskryminacja
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - P�e�    */
%LET _CLIENTTASKLABEL='Liczebno�� - P�e� ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:31
   Przez zadanie: Liczebno�� - P�e� 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.plec, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "P�e�";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 plec
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Wiek    */
%LET _CLIENTTASKLABEL='Liczebno�� - Wiek ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:32
   Przez zadanie: Liczebno�� - Wiek 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.wiek, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Wiek";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 wiek
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Uko�czone lata edukacji w pe�nym wymiarze godzin    */
%LET _CLIENTTASKLABEL='Liczebno�� - Uko�czone lata edukacji w pe�nym wymiarze godzin ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:32
   Przez zadanie: Liczebno�� - Uko�czone lata edukacji w pe�nym wymiarze godzin 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.edukacja_lata, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)
	LABEL=( "Liczebno��" )


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Uko�czone lata edukacji w pe�nym wymiarze godzin";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 edukacja_lata
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Obecne odczucia dotycz�ce dochod�w gospodarstwa domowego    */
%LET _CLIENTTASKLABEL='Liczebno�� - Obecne odczucia dotycz�ce dochod�w gospodarstwa domowego ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:32
   Przez zadanie: Liczebno�� - Obecne odczucia dotycz�ce dochod�w gospodarstwa domowego 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.dochody_odczucie, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1


;
Axis2
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)


;
TITLE;
TITLE1 "Obecne odczucia dotycz�ce dochod�w gospodarstwa domowego";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	HBAR3D 
	 dochody_odczucie
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	MAXIS=AXIS1
	RAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Czy rodzice wci�� �yj�    */
%LET _CLIENTTASKLABEL='Liczebno�� - Czy rodzice wci�� �yj� ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:32
   Przez zadanie: Liczebno�� - Czy rodzice wci�� �yj� 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.rodzice, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Czy rodzice wci�� �yj�";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 rodzice
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Liczebno�� - Wa�ne, by dobrze si� bawi�    */
%LET _CLIENTTASKLABEL='Liczebno�� - Wa�ne, by dobrze si� bawi� ';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:32
   Przez zadanie: Liczebno�� - Wa�ne, by dobrze si� bawi� 

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.dobra_zabawa, T.szczescie
	FROM WORK.SZWAJCARIA as T
;
QUIT;
Legend1
	FRAME
	POSITION = (BOTTOM CENTER OUTSIDE)
	;
Axis1
	STYLE=1
	WIDTH=1
	MINOR= 
	(NUMBER=1
	)


;
Axis2
	STYLE=1
	WIDTH=1


;
TITLE;
TITLE1 "Wa�ne, by dobrze si� bawi�";
FOOTNOTE;
PROC GCHART DATA=WORK.SORTTempTableSorted
;
	VBAR3D 
	 dobra_zabawa
 /
	SUBGROUP=szczescie
	SHAPE=BLOCK
FRAME	TYPE=FREQ
PCT
	LEGEND=LEGEND1
	COUTLINE=BLACK
	RAXIS=AXIS1
	MAXIS=AXIS2
	LREF=1
	CREF=BLACK
	AUTOREF
;
/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:33
   Przez zadanie: Regresja - Interakcje

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.spotkania, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.rodzice, T.dobra_zabawa, T.osoby_rozmowy, T.akt_spol
		     , T.spacer_zmrok, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) spotkania 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF)
	  rodzice 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) osoby_rozmowy 	(PARAM=REF) akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan spotkania edukacja edukacja_lata dochody_odczucie rodzice dobra_zabawa osoby_rozmowy akt_spol spacer_zmrok zdrowie religia dyskryminacja plec wiek*internet_czestosc wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*spotkania wiek*edukacja wiek*edukacja_lata wiek*dochody_odczucie wiek*rodzice wiek*dobra_zabawa wiek*osoby_rozmowy wiek*akt_spol wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec internet_czestosc*policja_zaufanie internet_czestosc*ekonomia_satysfakcja internet_czestosc*uslugi_zdrowotne_stan internet_czestosc*spotkania internet_czestosc*edukacja internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*rodzice internet_czestosc*dobra_zabawa internet_czestosc*osoby_rozmowy internet_czestosc*akt_spol internet_czestosc*spacer_zmrok internet_czestosc*zdrowie internet_czestosc*religia internet_czestosc*dyskryminacja internet_czestosc*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*spotkania policja_zaufanie*edukacja policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*rodzice policja_zaufanie*dobra_zabawa policja_zaufanie*osoby_rozmowy policja_zaufanie*akt_spol policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*spotkania ekonomia_satysfakcja*edukacja ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*rodzice ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*osoby_rozmowy ekonomia_satysfakcja*akt_spol ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*spotkania uslugi_zdrowotne_stan*edukacja uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*rodzice uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*osoby_rozmowy uslugi_zdrowotne_stan*akt_spol uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec spotkania*edukacja spotkania*edukacja_lata spotkania*dochody_odczucie spotkania*rodzice spotkania*dobra_zabawa spotkania*osoby_rozmowy spotkania*akt_spol spotkania*spacer_zmrok spotkania*zdrowie spotkania*religia spotkania*dyskryminacja spotkania*plec edukacja*edukacja_lata edukacja*dochody_odczucie edukacja*rodzice edukacja*dobra_zabawa edukacja*osoby_rozmowy edukacja*akt_spol edukacja*spacer_zmrok edukacja*zdrowie edukacja*religia edukacja*dyskryminacja edukacja*plec edukacja_lata*dochody_odczucie edukacja_lata*rodzice edukacja_lata*dobra_zabawa edukacja_lata*osoby_rozmowy edukacja_lata*akt_spol edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*rodzice dochody_odczucie*dobra_zabawa dochody_odczucie*osoby_rozmowy dochody_odczucie*akt_spol dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec rodzice*dobra_zabawa rodzice*osoby_rozmowy rodzice*akt_spol rodzice*spacer_zmrok rodzice*zdrowie rodzice*religia rodzice*dyskryminacja rodzice*plec dobra_zabawa*osoby_rozmowy dobra_zabawa*akt_spol dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec osoby_rozmowy*akt_spol osoby_rozmowy*spacer_zmrok osoby_rozmowy*zdrowie osoby_rozmowy*religia osoby_rozmowy*dyskryminacja osoby_rozmowy*plec akt_spol*spacer_zmrok akt_spol*zdrowie akt_spol*religia akt_spol*dyskryminacja akt_spol*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=18
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�te spotkania   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�te spotkania';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:33
   Przez zadanie: Regresja - Interakcje - usuni�te spotkania

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.rodzice, T.dobra_zabawa, T.osoby_rozmowy, T.akt_spol, T.spacer_zmrok
		     , T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) rodzice 	(PARAM=REF)
	  dobra_zabawa 	(PARAM=REF) osoby_rozmowy 	(PARAM=REF) akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja edukacja_lata dochody_odczucie rodzice dobra_zabawa osoby_rozmowy akt_spol spacer_zmrok zdrowie religia dyskryminacja plec wiek*internet_czestosc wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*edukacja wiek*edukacja_lata wiek*dochody_odczucie wiek*rodzice wiek*dobra_zabawa wiek*osoby_rozmowy wiek*akt_spol wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec internet_czestosc*policja_zaufanie internet_czestosc*ekonomia_satysfakcja internet_czestosc*uslugi_zdrowotne_stan internet_czestosc*edukacja internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*rodzice internet_czestosc*dobra_zabawa internet_czestosc*osoby_rozmowy internet_czestosc*akt_spol internet_czestosc*spacer_zmrok internet_czestosc*zdrowie internet_czestosc*religia internet_czestosc*dyskryminacja internet_czestosc*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*edukacja policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*rodzice policja_zaufanie*dobra_zabawa policja_zaufanie*osoby_rozmowy policja_zaufanie*akt_spol policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*edukacja ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*rodzice ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*osoby_rozmowy ekonomia_satysfakcja*akt_spol ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*edukacja uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*rodzice uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*osoby_rozmowy uslugi_zdrowotne_stan*akt_spol uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec edukacja*edukacja_lata edukacja*dochody_odczucie edukacja*rodzice edukacja*dobra_zabawa edukacja*osoby_rozmowy edukacja*akt_spol edukacja*spacer_zmrok edukacja*zdrowie edukacja*religia edukacja*dyskryminacja edukacja*plec edukacja_lata*dochody_odczucie edukacja_lata*rodzice edukacja_lata*dobra_zabawa edukacja_lata*osoby_rozmowy edukacja_lata*akt_spol edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*rodzice dochody_odczucie*dobra_zabawa dochody_odczucie*osoby_rozmowy dochody_odczucie*akt_spol dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec rodzice*dobra_zabawa rodzice*osoby_rozmowy rodzice*akt_spol rodzice*spacer_zmrok rodzice*zdrowie rodzice*religia rodzice*dyskryminacja rodzice*plec dobra_zabawa*osoby_rozmowy dobra_zabawa*akt_spol dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec osoby_rozmowy*akt_spol osoby_rozmowy*spacer_zmrok osoby_rozmowy*zdrowie osoby_rozmowy*religia osoby_rozmowy*dyskryminacja osoby_rozmowy*plec akt_spol*spacer_zmrok akt_spol*zdrowie akt_spol*religia akt_spol*dyskryminacja akt_spol*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=17
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�te osoby_romowy   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�te osoby_romowy';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:34
   Przez zadanie: Regresja - Interakcje - usuni�te osoby_romowy

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.rodzice, T.dobra_zabawa, T.akt_spol, T.spacer_zmrok, T.zdrowie
		     , T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) rodzice 	(PARAM=REF)
	  dobra_zabawa 	(PARAM=REF) akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja edukacja_lata dochody_odczucie rodzice dobra_zabawa akt_spol spacer_zmrok zdrowie religia dyskryminacja plec wiek*internet_czestosc wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*edukacja wiek*edukacja_lata wiek*dochody_odczucie wiek*rodzice wiek*dobra_zabawa wiek*akt_spol wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec internet_czestosc*policja_zaufanie internet_czestosc*ekonomia_satysfakcja internet_czestosc*uslugi_zdrowotne_stan internet_czestosc*edukacja internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*rodzice internet_czestosc*dobra_zabawa internet_czestosc*akt_spol internet_czestosc*spacer_zmrok internet_czestosc*zdrowie internet_czestosc*religia internet_czestosc*dyskryminacja internet_czestosc*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*edukacja policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*rodzice policja_zaufanie*dobra_zabawa policja_zaufanie*akt_spol policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*edukacja ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*rodzice ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*akt_spol ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*edukacja uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*rodzice uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*akt_spol uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec edukacja*edukacja_lata edukacja*dochody_odczucie edukacja*rodzice edukacja*dobra_zabawa edukacja*akt_spol edukacja*spacer_zmrok edukacja*zdrowie edukacja*religia edukacja*dyskryminacja edukacja*plec edukacja_lata*dochody_odczucie edukacja_lata*rodzice edukacja_lata*dobra_zabawa edukacja_lata*akt_spol edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*rodzice dochody_odczucie*dobra_zabawa dochody_odczucie*akt_spol dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec rodzice*dobra_zabawa rodzice*akt_spol rodzice*spacer_zmrok rodzice*zdrowie rodzice*religia rodzice*dyskryminacja rodzice*plec dobra_zabawa*akt_spol dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec akt_spol*spacer_zmrok akt_spol*zdrowie akt_spol*religia akt_spol*dyskryminacja akt_spol*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=16
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�te rodzice   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�te rodzice';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:34
   Przez zadanie: Regresja - Interakcje - usuni�te rodzice

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol, T.spacer_zmrok, T.zdrowie, T.religia
		     , T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF)
	  akt_spol 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja edukacja_lata dochody_odczucie dobra_zabawa akt_spol spacer_zmrok zdrowie religia dyskryminacja plec wiek*internet_czestosc wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*edukacja wiek*edukacja_lata wiek*dochody_odczucie wiek*dobra_zabawa wiek*akt_spol wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec internet_czestosc*policja_zaufanie internet_czestosc*ekonomia_satysfakcja internet_czestosc*uslugi_zdrowotne_stan internet_czestosc*edukacja internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*dobra_zabawa internet_czestosc*akt_spol internet_czestosc*spacer_zmrok internet_czestosc*zdrowie internet_czestosc*religia internet_czestosc*dyskryminacja internet_czestosc*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*edukacja policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*dobra_zabawa policja_zaufanie*akt_spol policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*edukacja ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*akt_spol ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*edukacja uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*akt_spol uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec edukacja*edukacja_lata edukacja*dochody_odczucie edukacja*dobra_zabawa edukacja*akt_spol edukacja*spacer_zmrok edukacja*zdrowie edukacja*religia edukacja*dyskryminacja edukacja*plec edukacja_lata*dochody_odczucie edukacja_lata*dobra_zabawa edukacja_lata*akt_spol edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*dobra_zabawa dochody_odczucie*akt_spol dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec dobra_zabawa*akt_spol dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec akt_spol*spacer_zmrok akt_spol*zdrowie akt_spol*religia akt_spol*dyskryminacja akt_spol*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=15
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�te edukacja   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�te edukacja';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:35
   Przez zadanie: Regresja - Interakcje - usuni�te edukacja

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.akt_spol, T.spacer_zmrok, T.zdrowie, T.religia, T.dyskryminacja
		     , T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) akt_spol 	(PARAM=REF)
	  spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja_lata dochody_odczucie dobra_zabawa akt_spol spacer_zmrok zdrowie religia dyskryminacja plec wiek*internet_czestosc wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*edukacja_lata wiek*dochody_odczucie wiek*dobra_zabawa wiek*akt_spol wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec internet_czestosc*policja_zaufanie internet_czestosc*ekonomia_satysfakcja internet_czestosc*uslugi_zdrowotne_stan internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*dobra_zabawa internet_czestosc*akt_spol internet_czestosc*spacer_zmrok internet_czestosc*zdrowie internet_czestosc*religia internet_czestosc*dyskryminacja internet_czestosc*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*dobra_zabawa policja_zaufanie*akt_spol policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*akt_spol ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*akt_spol uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec edukacja_lata*dochody_odczucie edukacja_lata*dobra_zabawa edukacja_lata*akt_spol edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*dobra_zabawa dochody_odczucie*akt_spol dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec dobra_zabawa*akt_spol dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec akt_spol*spacer_zmrok akt_spol*zdrowie akt_spol*religia akt_spol*dyskryminacja akt_spol*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=14
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�te akt_spol   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�te akt_spol';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:35
   Przez zadanie: Regresja - Interakcje - usuni�te akt_spol

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.internet_czestosc, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.spacer_zmrok, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS internet_czestosc 	(PARAM=REF) policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) spacer_zmrok 	(PARAM=REF)
	  zdrowie 	(PARAM=REF) religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek internet_czestosc policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja_lata dochody_odczucie dobra_zabawa spacer_zmrok zdrowie religia dyskryminacja plec wiek*internet_czestosc wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*edukacja_lata wiek*dochody_odczucie wiek*dobra_zabawa wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec internet_czestosc*policja_zaufanie internet_czestosc*ekonomia_satysfakcja internet_czestosc*uslugi_zdrowotne_stan internet_czestosc*edukacja_lata internet_czestosc*dochody_odczucie internet_czestosc*dobra_zabawa internet_czestosc*spacer_zmrok internet_czestosc*zdrowie internet_czestosc*religia internet_czestosc*dyskryminacja internet_czestosc*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*dobra_zabawa policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec edukacja_lata*dochody_odczucie edukacja_lata*dobra_zabawa edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*dobra_zabawa dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=13
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;


/*   POCZ�TEK W�Z�A: Regresja - Interakcje - usuni�te internet_czest   */
%LET _CLIENTTASKLABEL='Regresja - Interakcje - usuni�te internet_czest';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='C:\Users\wikto\Desktop\Nowy folder (2)\Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';
%LET _CLIENTPROJECTPATHHOST='LAPTOP-4KVP0NHN';
%LET _CLIENTPROJECTNAME='Szwajcaria_wiek_numeryczna_29.05_najnowszy.egp';

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: czwartek, 1 czerwca 2023 o godz. 21:32:36
   Przez zadanie: Regresja - Interakcje - usuni�te internet_czest

   Dane wej�ciowe: Local:WORK.SZWAJCARIA
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:WORK.SZWAJCARIA
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.szczescie, T.wiek, T.policja_zaufanie, T.ekonomia_satysfakcja, T.uslugi_zdrowotne_stan, T.edukacja_lata, T.dochody_odczucie, T.dobra_zabawa, T.spacer_zmrok, T.zdrowie, T.religia, T.dyskryminacja, T.plec
	FROM WORK.SZWAJCARIA as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS policja_zaufanie 	(PARAM=REF) ekonomia_satysfakcja 	(PARAM=REF) uslugi_zdrowotne_stan 	(PARAM=REF) edukacja_lata 	(PARAM=REF) dochody_odczucie 	(PARAM=REF) dobra_zabawa 	(PARAM=REF) spacer_zmrok 	(PARAM=REF) zdrowie 	(PARAM=REF)
	  religia 	(PARAM=REF) dyskryminacja 	(PARAM=REF) plec 	(PARAM=REF);
	MODEL szczescie (Event = 'Szcz�liwy/-a   ')=wiek policja_zaufanie ekonomia_satysfakcja uslugi_zdrowotne_stan edukacja_lata dochody_odczucie dobra_zabawa spacer_zmrok zdrowie religia dyskryminacja plec wiek*policja_zaufanie wiek*ekonomia_satysfakcja wiek*uslugi_zdrowotne_stan wiek*edukacja_lata wiek*dochody_odczucie wiek*dobra_zabawa wiek*spacer_zmrok wiek*zdrowie wiek*religia wiek*dyskryminacja wiek*plec policja_zaufanie*ekonomia_satysfakcja policja_zaufanie*uslugi_zdrowotne_stan policja_zaufanie*edukacja_lata policja_zaufanie*dochody_odczucie policja_zaufanie*dobra_zabawa policja_zaufanie*spacer_zmrok policja_zaufanie*zdrowie policja_zaufanie*religia policja_zaufanie*dyskryminacja policja_zaufanie*plec ekonomia_satysfakcja*uslugi_zdrowotne_stan ekonomia_satysfakcja*edukacja_lata ekonomia_satysfakcja*dochody_odczucie ekonomia_satysfakcja*dobra_zabawa ekonomia_satysfakcja*spacer_zmrok ekonomia_satysfakcja*zdrowie ekonomia_satysfakcja*religia ekonomia_satysfakcja*dyskryminacja ekonomia_satysfakcja*plec uslugi_zdrowotne_stan*edukacja_lata uslugi_zdrowotne_stan*dochody_odczucie uslugi_zdrowotne_stan*dobra_zabawa uslugi_zdrowotne_stan*spacer_zmrok uslugi_zdrowotne_stan*zdrowie uslugi_zdrowotne_stan*religia uslugi_zdrowotne_stan*dyskryminacja uslugi_zdrowotne_stan*plec edukacja_lata*dochody_odczucie edukacja_lata*dobra_zabawa edukacja_lata*spacer_zmrok edukacja_lata*zdrowie edukacja_lata*religia edukacja_lata*dyskryminacja edukacja_lata*plec dochody_odczucie*dobra_zabawa dochody_odczucie*spacer_zmrok dochody_odczucie*zdrowie dochody_odczucie*religia dochody_odczucie*dyskryminacja dochody_odczucie*plec dobra_zabawa*spacer_zmrok dobra_zabawa*zdrowie dobra_zabawa*religia dobra_zabawa*dyskryminacja dobra_zabawa*plec spacer_zmrok*zdrowie spacer_zmrok*religia spacer_zmrok*dyskryminacja spacer_zmrok*plec zdrowie*religia zdrowie*dyskryminacja zdrowie*plec religia*dyskryminacja religia*plec dyskryminacja*plec		/
		SELECTION=STEPWISE
		SLE=0.05
		SLS=0.05
		INCLUDE=12
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		CTABLE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
