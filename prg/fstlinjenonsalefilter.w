&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tmpLevBas NO-UNDO LIKE LevBas.
DEFINE TEMP-TABLE TT_StLinje NO-UNDO LIKE StLinje
       FIELD Solgt% AS DECI
       FIELD Vg LIKE VarGr.Vg
       FIELD VgBeskr LIKE VarGr.VgBeskr
       FIELD HgBeskr LIKE HuvGr.HgBeskr
       FIELD AvdelingNr LIKE Avdeling.AvdelingNr
       FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
       FIELD LevNr LIKE LevBas.LevNr
       FIELD LevNamn LIKE LevBas.levnamn
       FIElD ForsNr LIKE Forsalj.ForsNr
       FIELD FoNamn LIKE Forsalj.FoNamn
       FIELD SelgerNr LIKE Selger.SelgerNr
       FIELD SelgerNavn LIKE Selger.Navn
       FIELD Rab% AS DECI
       FIELD VgLopNr as CHAR
       FIELD LevKod LIKE ArtBas.LevKod
       FIELD Sasong LIKE SaSong.Sasong
       FIELD SasBeskr LIKE SaSong.SasBeskr
       FIELD Farg LIKE Farg.Farg
       FIELD FarBeskr LIKE Farg.FarBeskr
       FIELD MatKod LIKE Material.MatKod
       FIELD MatBeskr LIKE Material.MatBeskr
       FIELD VMId LIKE Varemerke.VMId
       FIELD VMBeskr LIKE Varemerke.Beskrivelse
       FIELD BruttoSolgt LIKE StLinje.VerdiSolgt
       FIELD Butnamn LIKE Butiker.Butnamn
       FIELD LevFargKod AS CHAR.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStTypeId  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
def var wTittel    as char no-undo.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_fstperiode AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE     NO-UNDO.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cQryString   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dTotSum AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cFieldDefsLevel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iFrozenCols AS INTEGER  INIT 4  NO-UNDO.  /* EksportLesInn. */

DEFINE VARIABLE tmp_qh         AS HANDLE     NO-UNDO.
DEFINE VARIABLE tmp_lLocal     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE tmp_cButiker   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterTxt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterNr AS CHARACTER  NO-UNDO.


DEFINE TEMP-TABLE TT_StLinjeTmp NO-UNDO LIKE TT_StLinje.
                
/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi,Vg,VgBeskr,LevNr,Levnamn,Sasong,SasBeskr,Farg,FarBeskr,MatKod,MatBeskr,VMId,VMBeskr" */
/*        cLabels = "ArtikkelNr,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr,Vg,VgBeskr,LevNr,Levnamn,Sesong,SasBeskr,Farve,FarBeskr,MatKod,MatBeskr,VareMerke,VMBeskr"               */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2,,,,,,,,,,,,"                                                                                                                                                                                                                                                                                   */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,,1,,1,,1,,1,,1,". /* Fält som skall högerjust i XPrint */                                                                                                                                                                                                                           */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter =
"AntSolgt,BruttoSolgt,VerdiSolgt,MvaVerdi,DbKr,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,KjopAnt,KjopVerdi,ReklVerdi,ReklLAnt,ReklLVerdi," +
"OvVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi".
ASSIGN cFieldDefs = 
        /*  1 */ "DataObjekt;Artikkelnr;;1," +
        /*  2 */ "Beskrivelse;Beskrivelse;;," +
        /* 34 */ "LevKod;LevKod;;," +
        /*  4 */ "AntSolgt;Solgt;3;1," +
        /* 4b */ "BruttoSolgt;Solgt brutto;2;1," +
        /*  5 */ "VerdiSolgt;Solgt netto;2;1," +
        /*  6 */ "Solgt%;Solgt%;2;1," +
        /*  7 */ "MvaVerdi;Mva verdi;2;1," +
        /*  8 */ "DbKr;DbKr;2;1," +
        /*  9 */ "Db%;Db%;2;1," +
        /* 10 */ "AntRabatt;Rabatter;;1," +
        /* 11 */ "VerdiRabatt;Rabatt kr;2;1," +
        /* 12 */ "Rab%;Rab%;2;1," +
        /* 13 */ "VVarekost;VVarekost;2;1," +
        /* 14 */ "KjopAnt;Kjøpt;;1," +
        /* 15 */ "KjopVerdi;Kjøpt kr;;1," +
        /* 16 */ "ReklAnt;Kunderekl;3;1," +
        /* 17 */ "ReklVerdi;Kunderekl kr;;1," +
        /* 18 */ "ReklLAnt;Levrekl;3;1," +
        /* 19 */ "ReklLVerdi;Levrekl kr;;1," +
        /* 20 */ "OvAnt;Overført;;1," +
        /* 21 */ "OvVerdi;Overført kr;;1," +
        /* 22 */ "SvinnAnt;Svinn;;1," +
        /* 23 */ "SvinnVerdi;Svinn kr;;1," +
        /* 24 */ "GjenkjopAnt;Gjenkjøp;3;1," +
        /* 25 */ "GjenkjopVerdi;Gjenkjøp kr;2;1," +
        /* 26 */ "AntTilbSolgt;Tilbud;;1," +
        /* 27 */ "VerdiTilbSolgt;Tilbud kr;;1," +
        /* 28 */ "BrekkAnt;Brekkasje;;1," +
        /* 29 */ "BrekkVerdi;Brekkasje kr;;1," +
        /* 30 */ "VgLopNr;Vg/Løpenr;;1," +
        /* 31 */ "Vg;Vg;;," +
        /* 32 */ "VgBeskr;VgBeskr;;," +
        /* 33 */ "Hg;Hg;;," +
        /* 34 */ "HgBeskr;HgBeskr;;," +
        /* 35 */ "LevNr;LevNr;;1," +
        /* 36 */ "Levnamn;Levnavn;;," +
        /* 37 */ "Sasong;Sesong;;1," +
        /* 38 */ "SasBeskr;SesBeskr;;," +
        /* 39 */ "Farg;Farve;;1," +
        /* 40 */ "FarBeskr;FarBeskr;;," +
        /* 41 */ "Levfargkod;Levfarge;;," +
        /* 42 */ "MatKod;MatKod;;1," +
        /* 43 */ "MatBeskr;MatBeskr;;," +
        /* 44 */ "VMId;VareMerke;;1," +
        /* 45 */ "VMBeskr;VMBeskr;;".

ASSIGN cFieldDefsLevel = 
        /*  4 */ "AntSolgt;Solgt;;1," +
        /* 4b */ "BruttoSolgt;Solgt brutto;2;1," +
        /*  5 */ "VerdiSolgt;Solgt netto;2;1," +
        /*  6 */ "Solgt%;Solgt%;2;1," +
        /*  7 */ "MvaVerdi;Mva verdi;2;1," +
        /*  8 */ "DbKr;DbKr;2;1," +
        /*  9 */ "Db%;Db%;2;1," +
        /* 10 */ "AntRabatt;Rabatter;;1," +
        /* 11 */ "VerdiRabatt;Rabatt kr;2;1," +
        /* 12 */ "Rab%;Rab%;2;1," +
        /* 13 */ "VVarekost;VVarekost;2;1," +
        /* 14 */ "KjopAnt;Kjøpt;;1," +
        /* 15 */ "KjopVerdi;Kjøpt kr;2;1," +
        /* 16 */ "ReklAnt;Kunderekl;;1," +
        /* 17 */ "ReklVerdi;Kunderekl kr;;1," +
        /* 18 */ "ReklLAnt;Levrekl;;1," +
        /* 19 */ "ReklLVerdi;Levrekl kr;;1," +
        /* 20 */ "OvAnt;Overført;;1," +
        /* 21 */ "OvVerdi;Overført kr;;1," +
        /* 22 */ "SvinnAnt;Svinn;;1," +
        /* 23 */ "SvinnVerdi;Svinn kr;;1," +
        /* 24 */ "GjenkjopAnt;Returer;;1," +
        /* 25 */ "GjenkjopVerdi;Returer kr;2;1," +
        /* 26 */ "AntTilbSolgt;Tilbud;;1," +
        /* 27 */ "VerdiTilbSolgt;Tilbud kr;;1," +
        /* 28 */ "BrekkAnt;Brekkasje;;1," +
        /* 29 */ "BrekkVerdi;Brekkasje kr;;1"
            .

/* ASSIGN cFelter = "Aar,AntSolgt,VerdiSolgt,MvaVerdi,DbKr,Db%,AntRabatt,AntTilbSolgt,Beskrivelse,BrekkAnt,BrekkVerdi,BrukerID,Butik,DataObjekt,Diverse,DiverseAnt,Diverseverdi,EDato,ETid,GjenkjopAnt,GjenkjopVerdi,Hg,IntAnt,IntVerdi,JustAnt,JustVerdi,KjopAnt,KjopVerdi,LagerAnt,LagerVerdi,NedAnt,NedVerdi,OmlHast,OvAnt,OvVerdi,PerId,PerLinNr,PerLinTxt,PrimoAnt,Primoverdi,RegistrertAv,RegistrertDato,RegistrertTid,ReklAnt,ReklLAnt,ReklLVerdi,ReklVerdi,StTypeId,SvinnAnt,SvinnVerdi,TilbMvaVerdi,TilbVVarekost,TotalPost,Utsolgt%,VerdiRabatt,VerdiTilbSolgt,VisBut,VVarekost"                                                                                                                                                                  */
/*        cLabels = "År,Antall solgt,Verdi solgt,Mva verdi,DbKr,Db%,Antall rabatt,Antall solgt på tilbud,Beskrivelse,Brekkasje,Verdi av brekasje,Bruker,Butikknummer,Dataobjekt,Diverse,,,Endret,Endret tid,Gjenkjøp fra kunde,Verdi av gjenkjøpte varer,,Internt forbruk,Verdi av internt forbruk,Justert antall,Justert verdi,Innkjopt antall,Verdi kjøpt,,,Nedskrevet antall,Verdi nedskrevet,,Overført antall,Verdi av overførte varer,PeriodeId,PeriodeLinje,,,,Registrert av,Registrert dato,Registreringstidspunkt,Kundereklamasjoner,Rekl.lev.antall,Verdi av leveerandørreklamasjoner,Verdi kundereklamasjoner,Statistikktype,Antall svinn,Svinn verdi,Tilb Mva verdi,Varekost tilbudssalg,,,Verdi rabatt,Verdi solgt på tilbud,,Vektet varekost". */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StLinje

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH StLinje NO-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH StLinje NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain StLinje
&Scoped-define FIRST-TABLE-IN-QUERY-fMain StLinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Tg-VisPeriode B-AvdelingBlank B-VMId ~
B-SesongBlank B-Aktiver B-HgBlank B-FargBlank RS-Level B-VgBlank ~
B-VMIdBlank B-Artikkelkort B-LevNrBlank B-MaterialBlank Tg-VisPerBut ~
CB-OPris TG-AvFilter B-Refresh B-Avdeling B-LevNr B-HuvGr B-VarGr ~
B-Material B-Farg B-Sesong FILL-IN-1 
&Scoped-Define DISPLAYED-OBJECTS Tg-VisPeriode FI-Avdeling FI-Sesong ~
FI-HuvGr FI-Farg RS-Level FI-VarGr FI-VMId FI-LevNr FI-Material ~
Tg-VisPerBut CB-OPris TG-AvFilter FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockVindu fFrameWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldDefsLevel fFrameWin 
FUNCTION getFieldDefsLevel RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD newDataObjekt fFrameWin 
FUNCTION newDataObjekt RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Artikkelkort 
     LABEL "Arti&kkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Avdeling  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AvdelingBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Farg  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-FargBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Material  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-MaterialBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Refresh 
     IMAGE-UP FILE "icon/oppdater.bmp":U NO-FOCUS
     LABEL "..." 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-Sesong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SesongBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VarGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VMId  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VMIdBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE VARIABLE CB-OPris AS CHARACTER FORMAT "X(256)":U 
     LABEL "Åpen pris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Farg AS CHARACTER FORMAT "X(10)":U 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Material AS CHARACTER FORMAT "X(10)":U 
     LABEL "Material" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VMId AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Rapportnivå" 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE RS-Level AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Artikkel", 1,
"Hovedgruppe", 2,
"Hovedruppe/sesong", 3,
"Varegruppe", 4,
"Varegruppe/sesong", 5
     SIZE 27.2 BY 4.43 NO-UNDO.

DEFINE VARIABLE TG-AvFilter AS LOGICAL INITIAL no 
     LABEL "Avansert filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE Tg-VisPerBut AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Tg-VisPeriode AS LOGICAL INITIAL no 
     LABEL "Vis periodelinjer" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      StLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Tg-VisPeriode AT ROW 1.19 COL 1
     FI-Avdeling AT ROW 1.19 COL 34.8 COLON-ALIGNED
     B-AvdelingBlank AT ROW 1.19 COL 56.8
     FI-Sesong AT ROW 1.19 COL 75.6 COLON-ALIGNED
     B-VMId AT ROW 3.19 COL 92.6 NO-TAB-STOP 
     B-SesongBlank AT ROW 1.19 COL 97.6
     B-Aktiver AT ROW 2.19 COL 1
     FI-HuvGr AT ROW 2.19 COL 34.8 COLON-ALIGNED
     B-HgBlank AT ROW 2.19 COL 56.8
     FI-Farg AT ROW 2.19 COL 75.6 COLON-ALIGNED
     B-FargBlank AT ROW 2.19 COL 97.6
     RS-Level AT ROW 2.24 COL 116.2 NO-LABEL
     FI-VarGr AT ROW 3.19 COL 34.8 COLON-ALIGNED
     B-VgBlank AT ROW 3.19 COL 56.8
     FI-VMId AT ROW 3.19 COL 75.6 COLON-ALIGNED
     B-VMIdBlank AT ROW 3.19 COL 97.6
     B-Artikkelkort AT ROW 3.52 COL 1
     FI-LevNr AT ROW 4.19 COL 34.8 COLON-ALIGNED
     B-LevNrBlank AT ROW 4.19 COL 56.8
     FI-Material AT ROW 4.19 COL 75.6 COLON-ALIGNED
     B-MaterialBlank AT ROW 4.19 COL 97.6
     Tg-VisPerBut AT ROW 4.91 COL 1
     CB-OPris AT ROW 5.19 COL 34.8 COLON-ALIGNED
     TG-AvFilter AT ROW 5.86 COL 1
     B-Refresh AT ROW 2.19 COL 16.4 NO-TAB-STOP 
     B-Avdeling AT ROW 1.19 COL 51.8 NO-TAB-STOP 
     B-LevNr AT ROW 4.19 COL 51.8 NO-TAB-STOP 
     B-HuvGr AT ROW 2.19 COL 51.8 NO-TAB-STOP 
     B-VarGr AT ROW 3.19 COL 51.8 NO-TAB-STOP 
     B-Material AT ROW 4.19 COL 92.6 NO-TAB-STOP 
     B-Farg AT ROW 2.19 COL 92.6 NO-TAB-STOP 
     B-Sesong AT ROW 1.19 COL 92.6 NO-TAB-STOP 
     FILL-IN-1 AT ROW 1.19 COL 114.2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.6 BY 5.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpLevBas T "NEW SHARED" NO-UNDO skotex LevBas
      TABLE: TT_StLinje T "?" NO-UNDO SkoTex StLinje
      ADDITIONAL-FIELDS:
          FIELD Solgt% AS DECI
          FIELD Vg LIKE VarGr.Vg
          FIELD VgBeskr LIKE VarGr.VgBeskr
          FIELD HgBeskr LIKE HuvGr.HgBeskr
          FIELD AvdelingNr LIKE Avdeling.AvdelingNr
          FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
          FIELD LevNr LIKE LevBas.LevNr
          FIELD LevNamn LIKE LevBas.levnamn
          FIElD ForsNr LIKE Forsalj.ForsNr
          FIELD FoNamn LIKE Forsalj.FoNamn
          FIELD SelgerNr LIKE Selger.SelgerNr
          FIELD SelgerNavn LIKE Selger.Navn
          FIELD Rab% AS DECI
          FIELD VgLopNr as CHAR
          FIELD LevKod LIKE ArtBas.LevKod
          FIELD Sasong LIKE SaSong.Sasong
          FIELD SasBeskr LIKE SaSong.SasBeskr
          FIELD Farg LIKE Farg.Farg
          FIELD FarBeskr LIKE Farg.FarBeskr
          FIELD MatKod LIKE Material.MatKod
          FIELD MatBeskr LIKE Material.MatBeskr
          FIELD VMId LIKE Varemerke.VMId
          FIELD VMBeskr LIKE Varemerke.Beskrivelse
          FIELD BruttoSolgt LIKE StLinje.VerdiSolgt
          FIELD Butnamn LIKE Butiker.Butnamn
          FIELD LevFargKod AS CHAR
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 5.67
         WIDTH              = 148.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Farg IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Material IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VMId IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "skotex.StLinje"
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
  DEFINE VARIABLE cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE qhParam     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh          AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
  IF NOT DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
     OUTPUT cKriterier /* CHARACTER */) THEN
      RETURN.
  ASSIGN pcFeltListe = "LevNr,Vg,Sasong,Farg,VMId,MatKod,OPris"
         pcVerdier   = FILL(CHR(1),NUM-ENTRIES(pcFeltListe) - 1).

  IF TG-AvFilter:CHECKED THEN DO:
      RUN Avancerat.
      RETURN.
  END.
  PUBLISH "VisTxtBox" ("Søker data......").
  RUN StartSok (cKriterier).
  RUN EksportLesInn.
  EMPTY TEMP-TABLE TT_StLinje.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort fFrameWin
ON CHOOSE OF B-Artikkelkort IN FRAME fMain /* Artikkelkort */
DO:
  IF RS-Level:SCREEN-VALUE = "1" THEN
      RUN ArtikkelKort.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Avdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avdeling fFrameWin
ON CHOOSE OF B-Avdeling IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Avdeling;AvdelingNr;AvdelingNavn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "AvdelingNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Avdeling:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Avdeling     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Avdeling:TOOLTIP = IF FI-Avdeling = "*" THEN "" ELSE FI-Avdeling.
        IF FI-Avdeling <> "*" THEN DO:
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Avdeling:BGCOLOR  = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Avdeling:BGCOLOR   = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AvdelingBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AvdelingBlank fFrameWin
ON CHOOSE OF B-AvdelingBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Avdeling:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Avdeling:SCREEN-VALUE = cAlle
               FI-Avdeling              = "*"
               FI-Avdeling:TOOLTIP      = ""
               FI-Avdeling:BGCOLOR      = ?
               B-Avdeling:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Farg fFrameWin
ON CHOOSE OF B-Farg IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Farg;Farg;FarBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Farg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Farg:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Farg     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Farg:TOOLTIP = IF FI-Farg = "*" THEN "" ELSE FI-Farg.
        IF FI-Farg <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Farg:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-Farg:BGCOLOR   = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FargBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FargBlank fFrameWin
ON CHOOSE OF B-FargBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Farg:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Farg:SCREEN-VALUE = cAlle
               FI-Farg              = "*"
               FI-Farg:TOOLTIP      = ""
               FI-Farg:BGCOLOR      = ?
               B-Farg:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HgBlank fFrameWin
ON CHOOSE OF B-HgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-HuvGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-HuvGr:SCREEN-VALUE = cAlle
               FI-HuvGr              = "*"
               FI-HuvGr:TOOLTIP      = ""
               FI-HuvGr:BGCOLOR      = ?
               B-HuvGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HuvGr fFrameWin
ON CHOOSE OF B-HuvGr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "HuvGr;Hg;HgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Hg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-HuvGr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-HuvGr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-HuvGr:TOOLTIP = IF FI-Huvgr = "*" THEN "" ELSE FI-HuvGr.
        IF FI-HuvGr <> "*" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-HuvGr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-HuvGr:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNr fFrameWin
ON CHOOSE OF B-LevNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "LevBas;LevNr;Levnamn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "LevNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Levnr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Levnr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Levnr:TOOLTIP = IF FI-Levnr = "*" THEN "" ELSE FI-Levnr.
        IF FI-Levnr <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-LevNr:BGCOLOR  = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-LevNr:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank fFrameWin
ON CHOOSE OF B-LevNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-LevNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-LevNr:SCREEN-VALUE = cAlle
               FI-LevNr              = "*"
               FI-LevNr:TOOLTIP      = ""
               FI-LevNr:BGCOLOR      = ?
               B-LevNr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Material
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Material fFrameWin
ON CHOOSE OF B-Material IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Material;MatKod;MatBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "MatKod",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Material:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Material     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Material:TOOLTIP = IF FI-Material = "*" THEN "" ELSE FI-Material.
        IF FI-Material <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Material:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Material:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-MaterialBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MaterialBlank fFrameWin
ON CHOOSE OF B-MaterialBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Material:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Material:SCREEN-VALUE = cAlle
               FI-Material              = "*"
               FI-Material:TOOLTIP      = ""
               FI-Material:BGCOLOR      = ?
               B-Material:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Refresh fFrameWin
ON CHOOSE OF B-Refresh IN FRAME fMain /* ... */
DO:
    IF TG-AvFilter:CHECKED AND VALID-HANDLE(tmp_qh) THEN
        RUN StartSokArtDyn IN THIS-PROCEDURE (tmp_qh,tmp_lLocal,tmp_cButiker).
    ELSE
        APPLY "CHOOSE" TO B-Aktiver.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sesong fFrameWin
ON CHOOSE OF B-Sesong IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "SaSong;Sasong;SasBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Sasong",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Sesong:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Sesong     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Sesong:TOOLTIP = IF FI-Sesong = "*" THEN "" ELSE FI-Sesong.
        IF FI-Sesong <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Sesong:BGCOLOR = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Sesong:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongBlank fFrameWin
ON CHOOSE OF B-SesongBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Sesong:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Sesong:SCREEN-VALUE = cAlle
               FI-Sesong              = "*"
               FI-Sesong:TOOLTIP      = ""
               FI-Sesong:BGCOLOR      = ?
               B-Sesong:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VarGr fFrameWin
ON CHOOSE OF B-VarGr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "VarGr;Vg;VgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Vg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-VarGr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-VarGr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-VarGr:TOOLTIP = IF FI-VarGr = "*" THEN "" ELSE FI-VarGr.
        IF FI-VarGr <> "*" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-VarGr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-VarGr:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgBlank fFrameWin
ON CHOOSE OF B-VgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-VarGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-VarGr:SCREEN-VALUE = cAlle
               FI-VarGr              = "*"
               FI-VarGr:TOOLTIP      = ""
               FI-VarGr:BGCOLOR      = ?
               B-VarGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VMId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VMId fFrameWin
ON CHOOSE OF B-VMId IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                         "Varemerke;VMId;Beskrivelse",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "VMId",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-VMId:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-VMId     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-VMId:TOOLTIP = IF FI-VMId = "*" THEN "" ELSE FI-VMId.
        IF FI-VMId <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-VMId:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-VMId:BGCOLOR   = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VMIdBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VMIdBlank fFrameWin
ON CHOOSE OF B-VMIdBlank IN FRAME fMain /* Blank */
DO:
    IF FI-VMId:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-VMId:SCREEN-VALUE = cAlle
               FI-VMId              = "*"
               FI-VMId:TOOLTIP      = ""
               FI-VMId:BGCOLOR      = ?
               B-VMId:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-OPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-OPris fFrameWin
ON VALUE-CHANGED OF CB-OPris IN FRAME fMain /* Åpen pris */
DO:
  ASSIGN CB-OPris.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AvFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AvFilter fFrameWin
ON VALUE-CHANGED OF TG-AvFilter IN FRAME fMain /* Avansert filter */
DO:
  ASSIGN tmp_qh = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF
{lng.i &SDO = "SDO"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort fFrameWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("DataObjekt",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,"SAME").                         
  IF cArtikkelNr = "" THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN 
      RETURN.
  fLockvindu(TRUE).
  run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoAktiver fFrameWin 
PROCEDURE AutoAktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cVerdi AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cRowId AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      APPLY "CHOOSE" TO B-AvdelingBlank.
      APPLY "CHOOSE" TO B-HgBlank.
      APPLY "CHOOSE" TO B-VgBlank.
      APPLY "CHOOSE" TO B-LevNrBlank.
      APPLY "CHOOSE" TO B-SesongBlank.
      APPLY "CHOOSE" TO B-FargBlank. 
      APPLY "CHOOSE" TO B-MaterialBlank.
      ASSIGN CB-OPris:SCREEN-VALUE = ""
             TG-AvFilter:CHECKED = FALSE
             Tg-VisPeriode:CHECKED = FALSE.
      APPLY "VALUE-CHANGED" TO CB-OPris.
      ASSIGN FI-VarGr:SCREEN-VALUE = "( 1 )"
             FI-VarGr     = cVerdi
             FI-VarGr:TOOLTIP = cVerdi
             B-VarGr:PRIVATE-DATA = cRowId + CHR(1) + cVerdi.
      APPLY "CHOOSE" TO B-Aktiver.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avancerat fFrameWin 
PROCEDURE Avancerat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     RUN tmpUtvalg IN h_Window (THIS-PROCEDURE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportLesInn fFrameWin 
PROCEDURE EksportLesInn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iPerBut AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iL      AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iTime AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iEntry AS INTEGER    NO-UNDO.
   RUN Kalk%.
   
   ASSIGN iL = INT(RS-Level:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   IF Tg-VisPerBut:CHECKED THEN DO:
       ASSIGN iFrozenCols   = 5
              cTmpFieldDefs = cFieldDefs.
       IF iL <> 1 THEN
            ASSIGN cFieldDefs = getFieldDefsLevel() + cFieldDefsLevel.
       ASSIGN iEntry = IF iL = 1 THEN 3 ELSE IF CAN-DO("2,4",STRING(iL)) THEN 2 ELSE 4.
       ASSIGN ENTRY(iEntry,cFieldDefs) = ENTRY(iEntry,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;" + IF Tg-VisPeriode:CHECKED THEN ",PerLinTxt;Periode;;" ELSE "".
       RUN FixStrings.
       ASSIGN cFieldDefs = cTmpFieldDefs.
   END.
   ELSE DO:
       ASSIGN cTmpFieldDefs       = cFieldDefs.
       ASSIGN iEntry = IF iL = 1 THEN 3 ELSE IF CAN-DO("2,4",STRING(iL)) THEN 2 ELSE 4.
       IF iL <> 1 THEN
           ASSIGN cFieldDefs = getFieldDefsLevel() + cFieldDefsLevel.
       ASSIGN ENTRY(iEntry,cFieldDefs) = ENTRY(iEntry,cFieldDefs) + IF Tg-VisPeriode:CHECKED THEN ",PerLinTxt;Periode;;" ELSE "".
       ASSIGN iFrozenCols = IF iL = 1 THEN 4 ELSE IF CAN-DO("2,4",STRING(iL)) THEN 3 ELSE 5
              iFrozenCols = iFrozenCols + IF Tg-VisPeriode:CHECKED THEN 1 ELSE 0.
       RUN FixStrings.
       ASSIGN cFieldDefs = cTmpFieldDefs.
   END.

   ASSIGN iFrozenCols = LOOKUP("AntSolgt",cFelter) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
        ASSIGN iFrozenCols = 5. /* Detta skall väl aldrig ske */

/*    ASSIGN iFrozenCols = IF iL = 1 THEN 4 ELSE IF CAN-DO("2,4",STRING(iL)) THEN 3 ELSE 5. */
/*    ASSIGN iFrozenCols = IF iL = 1 THEN 4 ELSE IF CAN-DO("2,4",STRING(iL)) THEN 2 ELSE 5. */
/*   IF Tg-VisPerBut:CHECKED IN FRAME {&FRAME-NAME} THEN DO:                                  */
/*       ASSIGN cTmpFieldDefs       = cFieldDefs                                              */
/*              ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;". */
/*       RUN FixStrings.                                                                      */
/*       ASSIGN cFieldDefs = cTmpFieldDefs.                                                   */
/*   END.                                                                                     */
/*   ELSE                                                                                     */
/*       RUN FixStrings.                                                                      */

   PUBLISH "VisTxtBox" ("Leser ut data......").
   CREATE QUERY qh.
   qh:SET-BUFFERS(BUFFER TT_StLinje:HANDLE).
   qh:QUERY-PREPARE("FOR EACH TT_StLinje").
   qh:QUERY-OPEN().
   PUBLISH "VisTxtBox" ("Leser ut data......").
   RUN rappgenqry.p ("","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
   RUN LesInnIGrid.
   PUBLISH "VisTxtBox" ("").
   
  qh:QUERY-CLOSE() NO-ERROR.
  DELETE OBJECT qh NO-ERROR.
  ASSIGN qh  = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY Tg-VisPeriode FI-Avdeling FI-Sesong FI-HuvGr FI-Farg RS-Level FI-VarGr 
          FI-VMId FI-LevNr FI-Material Tg-VisPerBut CB-OPris TG-AvFilter 
          FILL-IN-1 
      WITH FRAME fMain.
  ENABLE Tg-VisPeriode B-AvdelingBlank B-VMId B-SesongBlank B-Aktiver B-HgBlank 
         B-FargBlank RS-Level B-VgBlank B-VMIdBlank B-Artikkelkort B-LevNrBlank 
         B-MaterialBlank Tg-VisPerBut CB-OPris TG-AvFilter B-Refresh B-Avdeling 
         B-LevNr B-HuvGr B-VarGr B-Material B-Farg B-Sesong FILL-IN-1 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStrings fFrameWin 
PROCEDURE FixStrings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cFelter = FILL(",",NUM-ENTRIES(cFieldDefs) - 1)
       cLabels = cFelter
       cDecimaler = cFelter
       cRightCols = cFelter.
DO iCount = 1 TO NUM-ENTRIES(cFieldDefs):
    ASSIGN ENTRY(iCount,cFelter) = ENTRY(1,ENTRY(iCount,cFieldDefs),";")
           ENTRY(iCount,cLabels) = ENTRY(2,ENTRY(iCount,cFieldDefs),";")
           ENTRY(iCount,cDecimaler) = ENTRY(3,ENTRY(iCount,cFieldDefs),";")
           ENTRY(iCount,cRightCols) = ENTRY(4,ENTRY(iCount,cFieldDefs),";").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN FixStrings.
  RUN InitLeverandor.
  {syspara.i 1 100 1 cAlle}
  {syspara.i 220 1 1 cVisFelterTxt}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  ASSIGN CB-OPris:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cAlle + ",,Ja,TRUE,Nei,FALSE"
         CB-OPris = "".
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN DO:
      ASSIGN h_fstperiode = DYNAMIC-FUNCTION('geth_fstperiode':U IN h_Window).
      ASSIGN h_frapportgrid = DYNAMIC-FUNCTION('geth_frapportgrid':U IN h_Window).
  END.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cStTypeId = "NONSALE"
             cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt".
/*              FI-LevNr  = "*"                   */
/*              FI-LevNr:SCREEN-VALUE    = cAlle  */
/*              FI-Avdeling  = "*"                */
/*              FI-Avdeling:SCREEN-VALUE = cAlle  */
/*              FI-HuvGr  = "*"                   */
/*              FI-HuvGr:SCREEN-VALUE    = cAlle  */
/*              FI-VarGr  = "*"                   */
/*              FI-VarGr:SCREEN-VALUE    = cAlle  */
/*              FI-Sesong  = "*"                  */
/*              FI-Sesong:SCREEN-VALUE   = cAlle  */
/*              FI-Farg  = "*"                    */
/*              FI-Farg:SCREEN-VALUE     = cAlle  */
/*              FI-Material  = "*"                */
/*              FI-Material:SCREEN-VALUE = cAlle. */
      APPLY "CHOOSE" TO B-AvdelingBlank.
      APPLY "CHOOSE" TO B-HgBlank.
      APPLY "CHOOSE" TO B-VgBlank.
      APPLY "CHOOSE" TO B-LevNrBlank.
      APPLY "CHOOSE" TO B-FargBlank.
      APPLY "CHOOSE" TO B-SesongBlank.
      APPLY "CHOOSE" TO B-VMIdBlank.
      APPLY "CHOOSE" TO B-MaterialBlank.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLeverandor fFrameWin 
PROCEDURE InitLeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wLeverandorer AS CHARACTER  NO-UNDO.
  wLeverandorer = "".

  /* Leverandører - Leses fra Bestillingene */
/*   for each tmpLevBas: delete tmpLevBas. end. */
  for each LevBas no-lock where LevBas.LevNr > 0:
    assign
      wLeverandorer = wLeverandorer + 
                      (if wLeverandorer = ""
                         then ""
                         else ",") + string(LevBas.LevNr).
    create tmpLevBas.
    assign
      tmpLevBas.LevNr   = LevBas.LevNr
      tmpLevBas.LevNamn = LevBas.LeVNamn.          
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalk% fFrameWin 
PROCEDURE Kalk% :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_StLinje:
    ASSIGN TT_StLinje.Db%    = IF TT_StLinje.VerdiSolgt <= 0 THEN 0 ELSE (TT_StLinje.DbKr * 100) / TT_StLinje.VerdiSolgt
           TT_StLinje.Solgt% = IF dTotSum > 0 THEN ROUND(TT_StLinje.VerdiSolgt / dTotsum * 100,1) ELSE 0
           TT_StLinje.Rab%   = IF TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt = 0 THEN 0 ELSE 
                         ((TT_StLinje.VerdiRabatt) * 100) / (TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnIGrid fFrameWin 
PROCEDURE LesInnIGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cExtraFelt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilleggsFelter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      cTilleggsFelter = "Butik,Butnamn,PerLinTxt" + IF RS-Level:SCREEN-VALUE = "1" THEN "" ELSE ",Hg,HgBeskr,Vg,VgBeskr,Sasong,SasBeskr".

      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      DO:
          PUBLISH "LoadGrid" (cFileName,iFrozenCols).  /* 3 = antall frozen cols  */
          /* getSumFelter ger colnr för resp fält */
          ASSIGN cSumCols = getSumFelter(cSummerFelter)
                 /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
                 cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"
                           + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt")
                 /* Col för SummaRadTxt, SUM = txt  */
/*                  cSumString = getSumFelter("PerLinTxt") + ",SUM" . */
              cSumString = STRING(iFrozenCols - 1) + ",SUM" .
          /* nästa rad måste stå före 'Summer' */
          IF cVisFelterTxt <> "" THEN DO:
              cExtrafelt = "".
              DO ii = 1 TO NUM-ENTRIES(cTilleggsFelter):
                  IF CAN-DO(cFelter,ENTRY(ii,cTilleggsFelter)) THEN
                      cExtraFelt = cExtraFelt + "," + ENTRY(ii,cTilleggsFelter).
              END.
              cVisFelterNr = getSumFelter(cVisFelterTxt + cExtraFelt).
          END.
          PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")).
          PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).
          IF cVisFelterNr <> "" THEN
              PUBLISH "VisKun" (cVisFelterNr,"SKJUL").
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext fFrameWin 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.

    ASSIGN cGetVerdier = STRING(LOOKUP("DataObjekt",cFelter)).

    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,cRettning).      
        IF cArtikkelNr = "" THEN
          RETURN.
        PUBLISH "ByttArtikkel" (DECI(cArtikkelNr)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFeltInfo fFrameWin 
PROCEDURE SendFeltInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cField#List AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cColAlign   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iCount      AS INTEGER    NO-UNDO.
  ASSIGN cField#List = getSumFelter(cFeltListe).
         cColAlign   = FILL(",",NUM-ENTRIES(cField#List) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cField#List):
      IF ENTRY(INT(ENTRY(iCount,cField#List)),cRightCols) = "1" THEN 
          ASSIGN ENTRY(iCount,cColAlign) = "1".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFilterValues fFrameWin 
PROCEDURE SendFilterValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cFilterVerdier AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cColAlign      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFstPeriode    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriodeTmp    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cFraAar         AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cTilAar         AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cFraPerLinNr    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cTilPerLinNr    AS CHARACTER  NO-UNDO.

  DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
       OUTPUT cFstPeriode /* CHARACTER */).
  ASSIGN cButikker   = "Butikker: " + ENTRY(1,cFstPeriode,CHR(1))
         cPeriodeTmp = ENTRY(2,cFstPeriode,CHR(1)).
  CASE ENTRY(1,cPeriodeTmp):
      WHEN "AAR" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp).
      END.
      WHEN "MANED" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + ":" + ENTRY(4,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp) + ":" + ENTRY(5,cPeriodeTmp).
      END.
      WHEN "UKE" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + ":" + ENTRY(4,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp) + ":" + ENTRY(5,cPeriodeTmp).
      END.
      WHEN "DAG" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                             ENTRY(2,cPeriodeTmp) + "-" +
                             ENTRY(3,cPeriodeTmp).
      END.
      OTHERWISE DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                               ENTRY(2,cPeriodeTmp) + "-" +
                               ENTRY(3,cPeriodeTmp).
      END.
  END CASE.
  ASSIGN cColAlign = cRightCols.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokBku fFrameWin 
PROCEDURE SokBku :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER qh         AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER lLocal     AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry2     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerId    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFraAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iButik       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTilAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE pcFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPeriode     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cDataobjekt  AS CHARACTER  NO-UNDO.
  EMPTY TEMP-TABLE TT_StLinje.
  IF lLocal THEN DO:
      ASSIGN cPerId = ENTRY(1,cKriterier).
      CASE cPerId:
          WHEN "AAR" THEN DO:
              ASSIGN cFraAar      = ENTRY(2,cKriterier)
                     cTilAar      = ENTRY(3,cKriterier)
                     cFraPerLinNr = ENTRY(4,cKriterier)
                     cTilPerLinNr = ENTRY(5,cKriterier).
          END.
          WHEN "MANED" THEN DO:
              ASSIGN cFraAar      = ENTRY(2,cKriterier)
                     cTilAar      = ENTRY(3,cKriterier)
                     cFraPerLinNr = ENTRY(4,cKriterier)
                     cTilPerLinNr = ENTRY(5,cKriterier).
          END.
          WHEN "UKE" THEN DO:
              ASSIGN cFraAar      = ENTRY(2,cKriterier)
                     cTilAar      = ENTRY(3,cKriterier)
                     cFraPerLinNr = ENTRY(4,cKriterier)
                     cTilPerLinNr = ENTRY(5,cKriterier).
          END.
          WHEN "DAG" THEN DO:
              ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,cKriterier)))).
                     cTilAar      = STRING(YEAR(DATE(ENTRY(3,cKriterier)))).
                     cFraPerLinNr = STRING(DATE(ENTRY(2,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,cKriterier))) - 1)).
                     cTilPerLinNr = STRING(DATE(ENTRY(3,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,cKriterier))) - 1)).
          END.
          OTHERWISE DO:
              ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,cKriterier))))
                     cTilAar      = STRING(YEAR(DATE(ENTRY(3,cKriterier))))
                     cFraPerLinNr = STRING(DATE(ENTRY(2,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,cKriterier))) - 1))
                     cTilPerLinNr = STRING(DATE(ENTRY(3,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,cKriterier))) - 1)).
          END.
      END CASE.
      ASSIGN iFraAarPer = INT(cFraAar) * 1000 + INT(cFraPerLinNr)
             iTilAarPer = INT(cTilAar) * 1000 + INT(cTilPerLinNr)
             dTotSum    = 0.
      IF (FI-LevNr <> "*" OR FI-Farg  <> "*" OR FI-Material  <> "*" OR FI-Sesong  <> "*" OR FI-VarGr  <> "*" OR CB-OPris <> "") THEN DO:
          DO iCount = 1 TO 6:
              CASE iCount:
                  WHEN 1 THEN
                      IF CB-OPris:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN
                          ASSIGN cQry2 = " WHERE ArtBas.OPris = " + CB-OPris:SCREEN-VALUE.
                  WHEN 2 THEN
                      IF FI-LevNr <> "*" THEN
                          ASSIGN cQry2 = cQry2 + (IF cQry2 = "" THEN " WHERE " ELSE " AND ") +
                              IF NUM-ENTRIES(FI-LevNr) = 1 THEN "ArtBas.LevNr = " + FI-LevNr ELSE "CAN-DO('" + FI-LevNr + "',STRING(ArtBas.LevNr))".
                  WHEN 3 THEN
                      IF FI-VarGr <> "*" THEN
                          ASSIGN cQry2 = cQry2 + (IF cQry2 = "" THEN " WHERE " ELSE " AND ") +
                              IF NUM-ENTRIES(FI-VarGr) = 1 THEN "ArtBas.Vg = " + FI-VarGr ELSE "CAN-DO('" + FI-VarGr + "',STRING(ArtBas.Vg))".
                  WHEN 4 THEN
                      IF FI-Sesong <> "*" THEN
                          ASSIGN cQry2 = cQry2 + (IF cQry2 = "" THEN " WHERE " ELSE " AND ") +
                              IF NUM-ENTRIES(FI-Sesong) = 1 THEN "ArtBas.Sesong = " + FI-Sesong ELSE "CAN-DO('" + FI-Sesong + "',STRING(ArtBas.Sesong))".
                  WHEN 5 THEN
                      IF FI-Farg <> "*" THEN
                          ASSIGN cQry2 = cQry2 + (IF cQry2 = "" THEN " WHERE " ELSE " AND ") +
                              IF NUM-ENTRIES(FI-Farg) = 1 THEN "ArtBas.Farg = " + FI-Farg ELSE "CAN-DO('" + FI-Farg + "',STRING(ArtBas.Farg))".
                  WHEN 6 THEN
                      IF FI-Material <> "*" THEN
                          ASSIGN cQry2 = cQry2 + (IF cQry2 = "" THEN " WHERE " ELSE " AND ") +
                              IF NUM-ENTRIES(FI-Material) = 1 THEN "ArtBas.Material = " + FI-Material ELSE "CAN-DO('" + FI-Material + "',STRING(ArtBas.Material))".
              END CASE.
          END.
          cQry = "FOR EACH ArtBas NO-LOCK" + cQry2.
      END.
      CREATE QUERY qh.
      qh:SET-BUFFERS(BUFFER ArtBas:HANDLE).
      qh:QUERY-PREPARE(cQry).
      qh:QUERY-OPEN().
  END.
  DO WITH FRAME {&FRAME-NAME}:
    REPEAT:
        qh:GET-NEXT().
        IF qh:QUERY-OFF-END THEN
            LEAVE.
        cDataobjekt = STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE(),"9999999999999").
        FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cDataobjekt) NO-LOCK NO-ERROR.
        FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
        FIND Material OF ArtBas NO-LOCK NO-ERROR.
        FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        DO iCount = 1 TO NUM-ENTRIES(cButiker):
          iButik = INT(ENTRY(iCount,cButiker)).
          DO iPeriode = iFraAarPer TO iTilAarPer:
              FOR EACH StLinje WHERE Stlinje.Butik       = iButik    AND 
                                     StLinje.StTypeId    = cStTypeId AND
                                     StLinje.PerId       = cPerId    AND
                                     StLinje.AarPerLinNr = iPeriode  AND
                                     StLinje.Dataobjekt  = cDataobjekt USE-INDEX AarPerLinNr NO-LOCK.
                  IF AVAIL TT_StLinjeTMP THEN
                      DELETE TT_StLinjeTMP.
                  BUFFER-COPY StLinje TO TT_StLinjeTMP.
                  ASSIGN TT_StLinjeTMP.DbKr  = TT_StLinjeTMP.VerdiSolgt - TT_StLinjeTMP.VVareKost
                         TT_StLinjeTMP.Butik = 0
                         TT_StLinjeTMP.AarPerLinNr = IF INPUT Tg-VisPeriode = TRUE THEN TT_StLinjeTMP.AarPerLinNr ELSE 0.
                  ASSIGN dTotSum = dTotSum + TT_StLinjeTMP.VerdiSolgt.
                  DO:
                      FIND TT_StLinje WHERE TT_StLinje.Butik      = TT_StLinjeTMP.Butik      AND /* allt på en butik = 0 */
                                            TT_StLinje.StTypeId   = TT_StLinjeTMP.StTypeId   AND
                                            TT_StLinje.PerId      = TT_StLinjeTMP.PerId      AND
                                            TT_StLinje.AarPerLinNr = TT_StLinjeTMP.AarPerLinNr AND
                                            TT_StLinje.DataObjekt = TT_StLinjeTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
                      IF NOT AVAIL TT_Stlinje THEN DO:
                          IF INPUT Tg-VisPeriode = TRUE THEN
                              ASSIGN TT_StLinjeTMP.PerLinTxt = IF TT_StLinjeTMP.PerId = "DAG" THEN
                                        STRING(DATE(1,1,TT_StLinjeTMP.Aar) + TT_StLinjeTMP.PerLinNr - 1) ELSE 
                                        IF TT_StLinjeTMP.PerId = "AAR" THEN
                                            STRING(TT_StLinjeTMP.Aar) ELSE
                                        STRING(TT_StLinjeTMP.Aar) + "-" + STRING(TT_StLinjeTMP.PerLinNr,"99").
                          CREATE TT_StLinje.
                          BUFFER-COPY TT_StLinjeTMP TO TT_StLinje.
                          IF NOT AVAIL ArtBas THEN
                              ASSIGN TT_StLinje.VgLopNr = "Ukjent"
                                     TT_StLinje.LevKod = ""
                                     TT_StLinje.Beskrivelse = "Ukjent"
                                     TT_StLinje.LevNr = 0
                                     TT_StLinje.Levnamn = "Ukjent"
                                     TT_StLinje.Vg = 0
                                     TT_StLinje.VgBeskr = "Ukjent"
                                     TT_StLinje.Sasong = 0
                                     TT_StLinje.SasBeskr = "Ukjent"
                                     TT_StLinje.Farg = 0
                                     TT_StLinje.FarBeskr = "Ukjent"
                                     TT_StLinje.Matkod = 0
                                     TT_StLinje.MatBeskr = "Ukjent"
                                     TT_StLinje.VMId = 0
                                     TT_StLinje.VMBeskr = "Ukjent".
                          ELSE DO:
                              ASSIGN TT_StLinje.VgLopNr = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ")
                                     TT_StLinje.LevKod = ArtBas.LevKod
                                     TT_StLinje.Beskrivelse = ArtBas.Beskr
                                     TT_StLinje.LevNr = ArtBas.LevNr 
                                     TT_StLinje.Levnamn = IF AVAIL LevBas THEN LevBas.LevNamn ELSE "Ukjent"
                                     TT_StLinje.Vg = ArtBas.Vg
                                     TT_StLinje.VgBeskr = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent"
                                     TT_StLinje.Sasong = ArtBas.Sasong
                                     TT_StLinje.SasBeskr = IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent"
                                     TT_StLinje.Farg = ArtBas.Farg
                                     TT_StLinje.FarBeskr = IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent"
                                     TT_StLinje.Matkod = ArtBas.MatKod
                                     TT_StLinje.MatBeskr = IF AVAIL Material THEN Material.MatBeskr ELSE  "Ukjent"
                                     TT_StLinje.VMId = ArtBas.VMId
                                     TT_StLinje.VMBeskr = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE  "Ukjent".
                          END.
                      END.
                      ELSE
                          ASSIGN TT_StLinje.AntSolgt    = TT_StLinje.AntSolgt    + TT_StLinjeTMP.AntSolgt
                                 TT_StLinje.VerdiSolgt  = TT_StLinje.VerdiSolgt  + TT_StLinjeTMP.VerdiSolgt
                                 TT_StLinje.MvaVerdi    = TT_StLinje.MvaVerdi    + TT_StLinjeTMP.MvaVerdi
                                 TT_StLinje.DbKr        = TT_StLinje.DbKr        + TT_StLinjeTMP.DbKr
                                 TT_StLinje.AntRab      = TT_StLinje.AntRab      + TT_StLinjeTMP.AntRab
                                 TT_StLinje.VerdiRabatt = TT_StLinje.VerdiRabatt + TT_StLinjeTMP.VerdiRabatt
                                 TT_StLinje.AntTilbSolgt   = TT_StLinje.AntTilbSolgt  + TT_StLinjeTMP.AntTilbSolgt  
                                 TT_StLinje.BrekkAnt       = TT_StLinje.BrekkAnt      + TT_StLinjeTMP.BrekkAnt      
                                 TT_StLinje.BrekkVerdi     = TT_StLinje.BrekkVerdi    + TT_StLinjeTMP.BrekkVerdi    
                                 TT_StLinje.GjenkjopAnt    = TT_StLinje.GjenkjopAnt   + TT_StLinjeTMP.GjenkjopAnt   
                                 TT_StLinje.GjenkjopVerdi  = TT_StLinje.GjenkjopVerdi + TT_StLinjeTMP.GjenkjopVerdi 
                                 TT_StLinje.IntAnt         = TT_StLinje.IntAnt        + TT_StLinjeTMP.IntAnt        
                                 TT_StLinje.IntVerdi       = TT_StLinje.IntVerdi      + TT_StLinjeTMP.IntVerdi      
                                 TT_StLinje.JustAnt        = TT_StLinje.JustAnt       + TT_StLinjeTMP.JustAnt       
                                 TT_StLinje.JustVerdi      = TT_StLinje.JustVerdi     + TT_StLinjeTMP.JustVerdi     
                                 TT_StLinje.KjopAnt        = TT_StLinje.KjopAnt       + TT_StLinjeTMP.KjopAnt       
                                 TT_StLinje.KjopVerdi      = TT_StLinje.KjopVerdi     + TT_StLinjeTMP.KjopVerdi     
                                 TT_StLinje.LagerAnt       = TT_StLinje.LagerAnt      + TT_StLinjeTMP.LagerAnt      
                                 TT_StLinje.LagerVerdi     = TT_StLinje.LagerVerdi    + TT_StLinjeTMP.LagerVerdi    
                                 TT_StLinje.ReklAnt        = TT_StLinje.ReklAnt        + TT_StLinjeTMP.ReklAnt       
                                 TT_StLinje.ReklLAnt       = TT_StLinje.ReklLAnt       + TT_StLinjeTMP.ReklLAnt      
                                 TT_StLinje.ReklLVerdi     = TT_StLinje.ReklLVerdi     + TT_StLinjeTMP.ReklLVerdi    
                                 TT_StLinje.ReklVerdi      = TT_StLinje.ReklVerdi      + TT_StLinjeTMP.ReklVerdi     
                                 TT_StLinje.SvinnAnt       = TT_StLinje.SvinnAnt       + TT_StLinjeTMP.SvinnAnt       
                                 TT_StLinje.SvinnVerdi     = TT_StLinje.SvinnVerdi     + TT_StLinjeTMP.SvinnVerdi     
                                 TT_StLinje.VerdiTilbSolgt = TT_StLinje.VerdiTilbSolgt + TT_StLinjeTMP.VerdiTilbSolgt
                                 TT_StLinje.VVarekost      = TT_StLinje.VVarekost      + TT_StLinjeTMP.VVarekost.     

                  END.
              END.
          END.
        END. /* butikkloop */
    END.
    IF lLocal = TRUE THEN DO:
        qh:QUERY-CLOSE().
        DELETE OBJECT qh.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipKriterierTot   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cButiker  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerId AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount2      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFraAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTilAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdier    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPeriode    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDato AS DATE       NO-UNDO.
  DEFINE VARIABLE lOk AS LOGICAL    NO-UNDO.
  EMPTY TEMP-TABLE TT_StLinje.
  ASSIGN cButiker   = (ENTRY(1,ipKriterierTot,CHR(1)))
         cKriterier = (ENTRY(2,ipKriterierTot,CHR(1)))
         cPerId = ENTRY(1,cKriterier).
  CASE ENTRY(1,cKriterier):
    WHEN "AAR" THEN DO:
      ASSIGN cFraAar      = ENTRY(2,cKriterier)
             cTilAar      = ENTRY(3,cKriterier)
             cFraPerLinNr = ENTRY(4,cKriterier)
             cTilPerLinNr = ENTRY(5,cKriterier).
    END.
    WHEN "MANED" THEN DO:
      ASSIGN cFraAar      = ENTRY(2,cKriterier)
             cTilAar      = ENTRY(3,cKriterier)
             cFraPerLinNr = ENTRY(4,cKriterier)
             cTilPerLinNr = ENTRY(5,cKriterier).
    END.
    WHEN "UKE" THEN DO:
      ASSIGN cFraAar      = ENTRY(2,cKriterier)
             cTilAar      = ENTRY(3,cKriterier)
             cFraPerLinNr = ENTRY(4,cKriterier)
             cTilPerLinNr = ENTRY(5,cKriterier).
    END.
    WHEN "DAG" THEN DO:
      ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,cKriterier)))).
             cTilAar      = STRING(YEAR(DATE(ENTRY(3,cKriterier)))).
             cFraPerLinNr = STRING(DATE(ENTRY(2,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,cKriterier))) - 1)).
             cTilPerLinNr = STRING(DATE(ENTRY(3,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,cKriterier))) - 1)).
    END.
    OTHERWISE DO:
      ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,cKriterier))))
             cTilAar      = STRING(YEAR(DATE(ENTRY(3,cKriterier))))
             cFraPerLinNr = STRING(DATE(ENTRY(2,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,cKriterier))) - 1))
             cTilPerLinNr = STRING(DATE(ENTRY(3,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,cKriterier))) - 1)).
    END.
  END CASE.
  ASSIGN iFraAarPer = INT(cFraAar) * 1000 + INT(cFraPerLinNr)
         iTilAarPer = INT(cTilAar) * 1000 + INT(cTilPerLinNr)
         dTotSum    = 0.
  DO WITH FRAME {&FRAME-NAME}:
    DO iCount = 1 TO NUM-ENTRIES("AvdelingNr,HuvGr,VarGr,LevNr,Sesong,Farg,VMId,Material"):
      CASE ENTRY(iCount,"AvdelingNr,HuvGr,VarGr,LevNr,Sesong,Farg,VMId,Material"):
        WHEN "AvdelingNr" THEN
          IF FI-Avdeling <> "*" THEN
              ASSIGN cFeltListe = "AvdelingNr"
                     cVerdier   = FI-Avdeling.
        WHEN "HuvGr"    THEN
          IF FI-HuvGr <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "HuvGr"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-HuvGr.
        WHEN "VarGr"    THEN
          IF FI-VarGr <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "VarGr"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-VarGr.
        WHEN "LevNr"    THEN
          IF FI-LevNr <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "LevNr"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-LevNr.
        WHEN "Sesong"   THEN
          IF FI-Sesong <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "Sesong"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-Sesong.
        WHEN "Farg"     THEN
          IF FI-Farg  <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "Farg"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-Farg .
        WHEN "VMId"     THEN
          IF FI-VMId  <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "VMId"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-VMId .
        WHEN "Material" THEN
          IF FI-Material  <> "*" THEN
              ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "Material"
                     cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-Material .
      END CASE.
    END.
    DO iCount = 1 TO NUM-ENTRIES(cButiker):
      FOR EACH StLinje WHERE Stlinje.Butik    = INT(ENTRY(iCount,cButiker)) AND
                         StLinje.StTypeId = cStTypeId AND
                         StLinje.PerId    = cPerId AND
                         StLinje.AarPerLinNr >= iFraAarPer   AND
                         StLinje.AarPerLinNr <= iTilAarPer USE-INDEX AarPerLinNr NO-LOCK.
          IF TRIM(CB-Opris:SCREEN-VALUE) = "" THEN DO:
              FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(StLinje.DataObjekt) NO-LOCK NO-ERROR.
          END.
          ELSE DO:
            FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(StLinje.DataObjekt) AND ArtBas.Opris = CAN-DO("TRUE",CB-Opris:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF NOT AVAIL ArtBas THEN
                NEXT. /* om vi skall filtrera kan vi inte ha med okända artiklar */
          END.
          IF cFeltListe <> "" THEN DO:
/*             IF NOT AVAIL ArtBas THEN */
/*                 NEXT.                */
            ASSIGN lOK = TRUE.
            DOCASE: 
            DO iCount2 = 1 TO NUM-ENTRIES(cFeltListe,CHR(1)):
              CASE ENTRY(iCount2,cFeltListe,CHR(1)):
                WHEN "AvdelingNr" THEN DO:
                  RELEASE HuvGr.
                  IF AVAIL artbas THEN
                      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
                  ELSE
                      FIND Vargr WHERE vargr.vg = StLinje.artVg NO-LOCK NO-ERROR.
                  IF AVAIL VarGr THEN
                      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                  IF NOT (AVAIL HuvGr AND CAN-FIND(Avdeling OF HuvGr) AND CAN-DO(FI-Avdeling,STRING(HuvGr.AvdelingNr))) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "HuvGr"      THEN DO:
                  IF AVAIL artbas THEN
                      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
                  ELSE
                      FIND Vargr WHERE vargr.vg = StLinje.artVg NO-LOCK NO-ERROR.
                  IF NOT (AVAIL VarGr AND CAN-FIND(HuvGr OF VarGr) AND CAN-DO(FI-HuvGr,STRING(VarGr.Hg))) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "VarGr"      THEN DO:
                  IF AVAIL artbas AND NOT CAN-DO(FI-VarGr,STRING(ArtBas.Vg)) THEN
                      ASSIGN lOK = FALSE.
                  ELSE IF NOT AVAIL artbas AND NOT CAN-DO(FI-VarGr,STRING(Stlinje.ArtVG)) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "LevNr"      THEN DO:
                  IF AVAIL artbas AND NOT CAN-DO(FI-LevNr,STRING(ArtBas.LevNr)) THEN
                      ASSIGN lOK = FALSE.
                  ELSE IF NOT AVAIL artbas AND NOT CAN-DO(FI-LevNr,STRING(StLinje.artLevNr)) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "Sesong"     THEN DO:
                  IF AVAIL artbas AND NOT CAN-DO(FI-Sesong,STRING(ArtBas.Sasong)) THEN
                      ASSIGN lOK = FALSE.
                  ELSE IF NOT AVAIL artbas AND NOT CAN-DO(FI-Sesong,STRING(StLinje.artSasong)) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "Farg"       THEN DO:
                  IF AVAIL artbas AND NOT CAN-DO(FI-Farg,STRING(ArtBas.Farg)) THEN
                      ASSIGN lOK = FALSE.
                  ELSE IF NOT AVAIL artbas AND NOT CAN-DO(FI-Farg,STRING(StLinje.artFarg)) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "VMId"       THEN DO:
                  IF AVAIL artbas AND NOT CAN-DO(FI-VMId,STRING(ArtBas.VMId)) THEN
                      ASSIGN lOK = FALSE.
                  ELSE IF NOT AVAIL artbas AND NOT CAN-DO(FI-VMId,STRING(StLinje.artVMId)) THEN
                      ASSIGN lOK = FALSE.
                END.             
                WHEN "Material"   THEN DO:
                  IF AVAIL artbas AND NOT CAN-DO(FI-Material,STRING(ArtBas.MatKod)) THEN
                      ASSIGN lOK = FALSE.
                  ELSE IF NOT AVAIL artbas AND NOT CAN-DO(FI-Material,STRING(StLinje.artMatKod)) THEN
                      ASSIGN lOK = FALSE.
                END.
              END CASE.
              IF NOT lOK THEN
                LEAVE DOCASE.
            END.
            IF lOK = FALSE THEN
                NEXT.
          END.
        IF AVAIL TT_StLinjeTMP THEN
            DELETE TT_StLinjeTMP.
        BUFFER-COPY StLinje TO TT_StLinjeTMP.
        ASSIGN TT_StLinjeTMP.DbKr  = TT_StLinjeTMP.VerdiSolgt - TT_StLinjeTMP.VVareKost
               TT_StLinjeTMP.Butik = IF TG-VisPerBut:CHECKED THEN TT_StLinjeTMP.Butik ELSE 0
               TT_StLinjeTMP.AarPerLinNr = IF INPUT Tg-VisPeriode = TRUE THEN TT_StLinjeTMP.AarPerLinNr ELSE 0
               TT_StLinjeTMP.BruttoSolgt = TT_StLinjeTMP.VerdiSolgt + TT_StLinjeTMP.MvaVerdi.
        ASSIGN dTotSum = dTotSum + TT_StLinjeTMP.VerdiSolgt.
        DO:
          IF RS-Level:SCREEN-VALUE <> "1" THEN DO:
              TT_StLinjeTMP.DataObjekt = newDataObjekt().
          END.
          FIND TT_StLinje WHERE TT_StLinje.Butik      = TT_StLinjeTMP.Butik      AND /* allt på en butik = 0 */
                                TT_StLinje.StTypeId   = TT_StLinjeTMP.StTypeId   AND
                                TT_StLinje.PerId      = TT_StLinjeTMP.PerId      AND
                                TT_StLinje.AarPerLinNr = TT_StLinjeTMP.AarPerLinNr AND
                                TT_StLinje.DataObjekt = TT_StLinjeTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
          IF NOT AVAIL TT_Stlinje THEN DO:
            IF INPUT Tg-VisPeriode = TRUE THEN
              ASSIGN dDato = IF TT_StLinjeTMP.PerId = "DAG" THEN
                      DATE(1,1,TT_StLinjeTMP.Aar) + TT_StLinjeTMP.PerLinNr - 1 ELSE ?
                     TT_StLinjeTMP.PerLinTxt = IF TT_StLinjeTMP.PerId = "DAG" THEN
                         STRING(YEAR(dDato)) + "/" + STRING(MONTH(dDato),"99") + "/" + STRING(DAY(dDato),"99") ELSE 
                        IF TT_StLinjeTMP.PerId = "AAR" THEN
                            STRING(TT_StLinjeTMP.Aar) ELSE
                        STRING(TT_StLinjeTMP.Aar) + "-" + STRING(TT_StLinjeTMP.PerLinNr,"99").
            CREATE TT_StLinje.
            BUFFER-COPY TT_StLinjeTMP TO TT_StLinje.
            IF NOT AVAIL ArtBas THEN DO:
              ASSIGN 
                 TT_StLinje.VgLopNr = "Ukjent"
                 TT_StLinje.LevKod = ""
                 TT_StLinje.Beskrivelse = "Ukjent"
                 TT_StLinje.LevNr = 0
                 TT_StLinje.Levnamn = "Ukjent"
                 TT_StLinje.Vg = 0
                 TT_StLinje.VgBeskr = "Ukjent"
                 TT_StLinje.Hg = 0
                 TT_StLinje.HgBeskr = "Ukjent"
                 TT_StLinje.Sasong = 0
                 TT_StLinje.SasBeskr = "Ukjent"
                 TT_StLinje.Farg = 0
                 TT_StLinje.FarBeskr = "Ukjent"
                 TT_StLinje.Matkod = 0
                 TT_StLinje.MatBeskr = "Ukjent"
                 TT_StLinje.VMId = 0
                 TT_StLinje.VMBeskr = "Ukjent".
              FIND levbas WHERE levbas.levnr = TT_StLinjeTMP.artLevNr NO-LOCK NO-ERROR.
              FIND VarGr WHERE vargr.vg      = TT_StLinjeTMP.artVg    NO-LOCK NO-ERROR.
              IF AVAIL vargr THEN
                  FIND HuvGr OF vargr NO-LOCK NO-ERROR.
              ELSE
                  RELEASE HuvGr.
              FIND SaSong   WHERE Sasong.sasong   = TT_StLinjeTMP.artsasong NO-LOCK NO-ERROR.
              FIND Farg     WHERE farg.farg       = TT_StLinjeTMP.artFarg   NO-LOCK NO-ERROR.
              FIND Material WHERE material.matkod = TT_StLinjeTMP.artMatkod NO-LOCK NO-ERROR.
              FIND Varemerke WHERE varemerke.VMid = TT_StLinjeTMP.artVMid   NO-LOCK NO-ERROR.
              ASSIGN
                 TT_StLinje.VgLopNr = "Ukjent"
                 TT_StLinje.LevKod = TT_StLinjeTMP.artLevkod
                 TT_StLinje.Beskrivelse = TT_StLinjeTMP.art_Beskr
                 TT_StLinje.LevNr = TT_StLinjeTMP.artLevNr 
                 TT_StLinje.Levnamn = IF AVAIL LevBas THEN LevBas.LevNamn ELSE "Ukjent"
                 TT_StLinje.Vg = TT_StLinjeTMP.artVg
                 TT_StLinje.VgBeskr = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent"
                 TT_StLinje.Hg = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 0
                 TT_StLinje.HgBeskr = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent"
                 TT_StLinje.Sasong = TT_StLinjeTMP.artsasong
                 TT_StLinje.SasBeskr = IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent"
                 TT_StLinje.Farg = TT_StLinjeTMP.artFarg
                 TT_StLinje.FarBeskr = IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent"
                 TT_StLinje.LevFargKod = TT_StLinje.LevFargKod
                 TT_StLinje.Matkod = TT_StLinjeTMP.artMatkod
                 TT_StLinje.MatBeskr = IF AVAIL Material THEN Material.MatBeskr ELSE  "Ukjent"
                 TT_StLinje.VMId = TT_StLinjeTMP.artVMid
                 TT_StLinje.VMBeskr = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE  "Ukjent".
                  
            END.
            ELSE DO:
              FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
              FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
              FIND Farg OF ArtBas NO-LOCK NO-ERROR.
              FIND Material OF ArtBas NO-LOCK NO-ERROR.
              FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
/*               ASSIGN TT_StLinje.VgLopNr = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
              ASSIGN 
                TT_StLinje.VgLopNr = STRING(ArtBas.Vg) + "/ " + STRING(ArtBas.LopNr,"999999")
                TT_StLinje.VgLopNr = IF TT_StLinje.VgLopNr = ? THEN STRING(ArtBas.Vg) + "/?" ELSE TT_StLinje.VgLopNr
                TT_StLinje.LevKod = ArtBas.LevKod
                TT_StLinje.Beskrivelse = ArtBas.Beskr
                TT_StLinje.LevNr = ArtBas.LevNr 
                TT_StLinje.Levnamn = IF AVAIL LevBas THEN LevBas.LevNamn ELSE "Ukjent"
                TT_StLinje.Vg = ArtBas.Vg
                TT_StLinje.VgBeskr = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent"
                TT_StLinje.Hg = ArtBas.Hg
                TT_StLinje.HgBeskr = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent"
                TT_StLinje.Sasong = ArtBas.Sasong
                TT_StLinje.SasBeskr = IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent"
                TT_StLinje.Farg = ArtBas.Farg
                TT_StLinje.FarBeskr = IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent"
                TT_StLinje.LevFargKod = Artbas.LevFargKod
                TT_StLinje.Matkod = ArtBas.MatKod
                TT_StLinje.MatBeskr = IF AVAIL Material THEN Material.MatBeskr ELSE  "Ukjent"
                TT_StLinje.VMId = ArtBas.VMId
                TT_StLinje.VMBeskr = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE  "Ukjent".
            END.
            IF TG-VisPerBut:CHECKED THEN DO:
                FIND Butiker WHERE Butiker.Butik = TT_StLinje.Butik NO-LOCK NO-ERROR.
                ASSIGN TT_StLinje.Butnamn = IF AVAIL Butiker THEN Butiker.Butnamn ELSE "Ukjent".
            END.
          END.
          ELSE
            ASSIGN
                TT_StLinje.AntSolgt    = TT_StLinje.AntSolgt    + TT_StLinjeTMP.AntSolgt
                TT_StLinje.VerdiSolgt  = TT_StLinje.VerdiSolgt  + TT_StLinjeTMP.VerdiSolgt
                TT_StLinje.MvaVerdi    = TT_StLinje.MvaVerdi    + TT_StLinjeTMP.MvaVerdi
                TT_StLinje.DbKr        = TT_StLinje.DbKr        + TT_StLinjeTMP.DbKr
                TT_StLinje.AntRab      = TT_StLinje.AntRab      + TT_StLinjeTMP.AntRab
                TT_StLinje.VerdiRabatt = TT_StLinje.VerdiRabatt + TT_StLinjeTMP.VerdiRabatt
                TT_StLinje.AntTilbSolgt   = TT_StLinje.AntTilbSolgt  + TT_StLinjeTMP.AntTilbSolgt  
                TT_StLinje.BrekkAnt       = TT_StLinje.BrekkAnt      + TT_StLinjeTMP.BrekkAnt      
                TT_StLinje.BrekkVerdi     = TT_StLinje.BrekkVerdi    + TT_StLinjeTMP.BrekkVerdi    
                TT_StLinje.GjenkjopAnt    = TT_StLinje.GjenkjopAnt   + TT_StLinjeTMP.GjenkjopAnt   
                TT_StLinje.GjenkjopVerdi  = TT_StLinje.GjenkjopVerdi + TT_StLinjeTMP.GjenkjopVerdi 
                TT_StLinje.IntAnt         = TT_StLinje.IntAnt        + TT_StLinjeTMP.IntAnt        
                TT_StLinje.IntVerdi       = TT_StLinje.IntVerdi      + TT_StLinjeTMP.IntVerdi      
                TT_StLinje.JustAnt        = TT_StLinje.JustAnt       + TT_StLinjeTMP.JustAnt       
                TT_StLinje.JustVerdi      = TT_StLinje.JustVerdi     + TT_StLinjeTMP.JustVerdi     
                TT_StLinje.KjopAnt        = TT_StLinje.KjopAnt       + TT_StLinjeTMP.KjopAnt       
                TT_StLinje.KjopVerdi      = TT_StLinje.KjopVerdi     + TT_StLinjeTMP.KjopVerdi     
                TT_StLinje.LagerAnt       = TT_StLinje.LagerAnt      + TT_StLinjeTMP.LagerAnt      
                TT_StLinje.LagerVerdi     = TT_StLinje.LagerVerdi    + TT_StLinjeTMP.LagerVerdi    
                TT_StLinje.ReklAnt        = TT_StLinje.ReklAnt        + TT_StLinjeTMP.ReklAnt       
                TT_StLinje.ReklLAnt       = TT_StLinje.ReklLAnt       + TT_StLinjeTMP.ReklLAnt      
                TT_StLinje.ReklLVerdi     = TT_StLinje.ReklLVerdi     + TT_StLinjeTMP.ReklLVerdi    
                TT_StLinje.ReklVerdi      = TT_StLinje.ReklVerdi      + TT_StLinjeTMP.ReklVerdi     
                TT_StLinje.OvAnt       = TT_StLinje.OvAnt       + TT_StLinjeTMP.OvAnt       
                TT_StLinje.OvVerdi     = TT_StLinje.OvVerdi     + TT_StLinjeTMP.OvVerdi     
                TT_StLinje.SvinnAnt       = TT_StLinje.SvinnAnt       + TT_StLinjeTMP.SvinnAnt       
                TT_StLinje.SvinnVerdi     = TT_StLinje.SvinnVerdi     + TT_StLinjeTMP.SvinnVerdi     
                TT_StLinje.VerdiTilbSolgt = TT_StLinje.VerdiTilbSolgt + TT_StLinjeTMP.VerdiTilbSolgt
                TT_StLinje.VVarekost      = TT_StLinje.VVarekost      + TT_StLinjeTMP.VVarekost
                TT_StLinje.BruttoSolgt    = TT_StLinje.BruttoSolgt    + TT_StLinjeTMP.BruttoSolgt.
        END.
      END.
    END. /* butikkloop */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArtDyn fFrameWin 
PROCEDURE StartSokArtDyn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER qh         AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER lLocal     AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStatus        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKriterier     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ipKriterierTot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry2     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerId    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFraAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iButik       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTilAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE pcFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPeriode     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cDataobjekt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lEkstern     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE IbY          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDato        AS DATE       NO-UNDO.
  EMPTY TEMP-TABLE TT_StLinje.
  ASSIGN tmp_qh       = qh      
         tmp_lLocal   = lLocal  
         tmp_cButiker = cButiker.
  IF cButiker BEGINS "HENTINTERNT" THEN DO:
      /* vid HENTINTERNT: cStatus innehåller antal poster i tempdb kan visas */
      ASSIGN cStatus =  ENTRY(2,cButiker,CHR(1))
             cButiker = ENTRY(1,cButiker,CHR(1)).
      IF NOT DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
         OUTPUT ipKriterierTot /* CHARACTER */) THEN
          RETURN.
/*       ASSIGN cButiker   = (ENTRY(1,ipKriterierTot,CHR(1))) */
/*              cKriterier = (ENTRY(2,ipKriterierTot,CHR(1))) */
/*              cPerId = ENTRY(1,cKriterier).                 */
  END.
  ELSE DO:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      ASSIGN lEkstern = TRUE.
      RUN gvelgkriterier.w (INPUT cButiker,OUTPUT ipKriterierTot).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
      IF ipKriterierTot = "" THEN DO:
          MESSAGE "Ingen kriterier valgt."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "CLOSE" TO h_Window.
          RETURN.
      END.
  END.
  ASSIGN cButiker   = (ENTRY(1,ipKriterierTot,CHR(1)))
         cKriterier = (ENTRY(2,ipKriterierTot,CHR(1)))
         cPerId = ENTRY(1,cKriterier).
  IF lEkstern = TRUE AND NUM-ENTRIES(cButiker) > 1 THEN DO WITH FRAME {&FRAME-NAME}:
      MESSAGE "Ønsker du resultatet per butikk? "
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVisPerBut AS LOGICAL.
      IF lVisPerBut THEN
          ASSIGN Tg-VisPerBut:CHECKED = TRUE.
  END.
  CASE cPerId:
      WHEN "AAR" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,cKriterier)
                 cTilAar      = ENTRY(3,cKriterier)
                 cFraPerLinNr = ENTRY(4,cKriterier)
                 cTilPerLinNr = ENTRY(5,cKriterier).
      END.
      WHEN "MANED" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,cKriterier)
                 cTilAar      = ENTRY(3,cKriterier)
                 cFraPerLinNr = ENTRY(4,cKriterier)
                 cTilPerLinNr = ENTRY(5,cKriterier).
      END.
      WHEN "UKE" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,cKriterier)
                 cTilAar      = ENTRY(3,cKriterier)
                 cFraPerLinNr = ENTRY(4,cKriterier)
                 cTilPerLinNr = ENTRY(5,cKriterier).
      END.
      WHEN "DAG" THEN DO:
          ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,cKriterier)))).
                 cTilAar      = STRING(YEAR(DATE(ENTRY(3,cKriterier)))).
                 cFraPerLinNr = STRING(DATE(ENTRY(2,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,cKriterier))) - 1)).
                 cTilPerLinNr = STRING(DATE(ENTRY(3,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,cKriterier))) - 1)).
      END.
      OTHERWISE DO:
          ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,cKriterier))))
                 cTilAar      = STRING(YEAR(DATE(ENTRY(3,cKriterier))))
                 cFraPerLinNr = STRING(DATE(ENTRY(2,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,cKriterier))) - 1))
                 cTilPerLinNr = STRING(DATE(ENTRY(3,cKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,cKriterier))) - 1)).
      END.
  END CASE.
  ASSIGN iFraAarPer = INT(cFraAar) * 1000 + INT(cFraPerLinNr)
         iTilAarPer = INT(cTilAar) * 1000 + INT(cTilPerLinNr)
         dTotSum    = 0
         iBy        = IF cPerId = "AAR" THEN 1000 ELSE 1.
  PUBLISH "VisTxtBox" ("Søker data......").
  qh:QUERY-OPEN().
  DO WITH FRAME {&FRAME-NAME}:
    REPEAT:
        qh:GET-NEXT().
        IF qh:QUERY-OFF-END THEN
            LEAVE.
        cDataobjekt = STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE(),"9999999999999").
        FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cDataobjekt) NO-LOCK NO-ERROR.
        FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
        FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
        FIND Material OF ArtBas NO-LOCK NO-ERROR.
        FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        DO iCount = 1 TO NUM-ENTRIES(cButiker):
          iButik = INT(ENTRY(iCount,cButiker)).
          DO iPeriode = iFraAarPer TO iTilAarPer:
              IF cPerId = "AAR" AND SUBSTR(STRING(iPeriode),5) = "002" THEN
                  iPeriode = iPeriode + 999.
              ELSE IF cPerId = "MANED" AND SUBSTR(STRING(iPeriode),5) = "013" THEN
                  iPeriode = iPeriode + 988.
              ELSE IF cPerId = "UKE" AND SUBSTR(STRING(iPeriode),5) = "054" THEN
                  iPeriode = iPeriode + 947.
              ELSE IF SUBSTR(STRING(iPeriode),5) = "367" THEN
                  iPeriode = iPeriode + 634.
              FOR EACH StLinje WHERE Stlinje.Butik       = iButik    AND 
                                     StLinje.StTypeId    = cStTypeId AND
                                     StLinje.PerId       = cPerId    AND
                                     StLinje.AarPerLinNr = iPeriode  AND
                                     StLinje.Dataobjekt  = cDataobjekt USE-INDEX AarPerLinNr NO-LOCK.
                  IF AVAIL TT_StLinjeTMP THEN
                      DELETE TT_StLinjeTMP.
                  BUFFER-COPY StLinje TO TT_StLinjeTMP.
                  ASSIGN TT_StLinjeTMP.DbKr  = TT_StLinjeTMP.VerdiSolgt - TT_StLinjeTMP.VVareKost
                         TT_StLinjeTMP.Butik = IF TG-VisPerBut:CHECKED THEN TT_StLinjeTMP.Butik ELSE 0
                         TT_StLinjeTMP.AarPerLinNr = IF INPUT Tg-VisPeriode = TRUE THEN TT_StLinjeTMP.AarPerLinNr ELSE 0
                         TT_StLinjeTMP.BruttoSolgt = TT_StLinjeTMP.VerdiSolgt + TT_StLinjeTMP.MvaVerdi.
                  ASSIGN dTotSum = dTotSum + TT_StLinjeTMP.VerdiSolgt.
                  DO:
                      IF RS-Level:SCREEN-VALUE <> "1" THEN DO:
                          TT_StLinjeTMP.DataObjekt = newDataObjekt().
                      END.
                      FIND TT_StLinje WHERE TT_StLinje.Butik      = TT_StLinjeTMP.Butik      AND /* allt på en butik = 0 */
                                            TT_StLinje.StTypeId   = TT_StLinjeTMP.StTypeId   AND
                                            TT_StLinje.PerId      = TT_StLinjeTMP.PerId      AND
                                            TT_StLinje.AarPerLinNr = TT_StLinjeTMP.AarPerLinNr AND
                                            TT_StLinje.DataObjekt = TT_StLinjeTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
                      IF NOT AVAIL TT_Stlinje THEN DO:
                          IF INPUT Tg-VisPeriode = TRUE THEN
                              ASSIGN dDato = IF TT_StLinjeTMP.PerId = "DAG" THEN DATE(1,1,TT_StLinjeTMP.Aar) + TT_StLinjeTMP.PerLinNr - 1 ELSE ?
                                 TT_StLinjeTMP.PerLinTxt = IF TT_StLinjeTMP.PerId = "DAG" THEN
                                     STRING(YEAR(dDato)) + "/" + STRING(MONTH(dDato),"99") + "/" + STRING(DAY(dDato),"99") ELSE 
                                        IF TT_StLinjeTMP.PerId = "AAR" THEN
                                            STRING(TT_StLinjeTMP.Aar) ELSE
                                        STRING(TT_StLinjeTMP.Aar) + "-" + STRING(TT_StLinjeTMP.PerLinNr,"99").
                          CREATE TT_StLinje.
                          BUFFER-COPY TT_StLinjeTMP TO TT_StLinje.
                          IF NOT AVAIL ArtBas THEN
                              ASSIGN TT_StLinje.VgLopNr = "Ukjent"
                                     TT_StLinje.LevKod = ""
                                     TT_StLinje.Beskrivelse = "Ukjent"
                                     TT_StLinje.LevNr = 0
                                     TT_StLinje.Levnamn = "Ukjent"
                                     TT_StLinje.Vg = 0
                                     TT_StLinje.VgBeskr = "Ukjent"
                                     TT_StLinje.Sasong = 0
                                     TT_StLinje.SasBeskr = "Ukjent"
                                     TT_StLinje.Farg = 0
                                     TT_StLinje.FarBeskr = "Ukjent"
                                     TT_StLinje.Matkod = 0
                                     TT_StLinje.MatBeskr = "Ukjent"
                                     TT_StLinje.VMId = 0
                                     TT_StLinje.VMBeskr = "Ukjent".
                          ELSE DO:
                              ASSIGN TT_StLinje.VgLopNr = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ")
                                     TT_StLinje.LevKod = ArtBas.LevKod
                                     TT_StLinje.Beskrivelse = ArtBas.Beskr
                                     TT_StLinje.LevNr = ArtBas.LevNr 
                                     TT_StLinje.Levnamn = IF AVAIL LevBas THEN LevBas.LevNamn ELSE "Ukjent"
                                     TT_StLinje.Vg = ArtBas.Vg
                                     TT_StLinje.VgBeskr = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent"
                                     TT_StLinje.Hg = ArtBas.Hg
                                     TT_StLinje.HgBeskr = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent"
                                     TT_StLinje.Sasong = ArtBas.Sasong
                                     TT_StLinje.SasBeskr = IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent"
                                     TT_StLinje.Farg = ArtBas.Farg
                                     TT_StLinje.FarBeskr = IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent"
                                     TT_StLinje.LevFargKod = Artbas.LevFargKod
                                     TT_StLinje.Matkod = ArtBas.MatKod
                                     TT_StLinje.MatBeskr = IF AVAIL Material THEN Material.MatBeskr ELSE  "Ukjent"
                                     TT_StLinje.VMId = ArtBas.VMId
                                     TT_StLinje.VMBeskr = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE  "Ukjent".
                          END.
                          IF TG-VisPerBut:CHECKED THEN DO:
                              FIND Butiker WHERE Butiker.Butik = TT_StLinje.Butik NO-LOCK NO-ERROR.
                              ASSIGN TT_StLinje.Butnamn = IF AVAIL Butiker THEN Butiker.Butnamn ELSE "Ukjent".
                          END.
                      END.
                      ELSE
                          ASSIGN TT_StLinje.AntSolgt    = TT_StLinje.AntSolgt    + TT_StLinjeTMP.AntSolgt
                                 TT_StLinje.VerdiSolgt  = TT_StLinje.VerdiSolgt  + TT_StLinjeTMP.VerdiSolgt
                                 TT_StLinje.MvaVerdi    = TT_StLinje.MvaVerdi    + TT_StLinjeTMP.MvaVerdi
                                 TT_StLinje.DbKr        = TT_StLinje.DbKr        + TT_StLinjeTMP.DbKr
                                 TT_StLinje.AntRab      = TT_StLinje.AntRab      + TT_StLinjeTMP.AntRab
                                 TT_StLinje.VerdiRabatt = TT_StLinje.VerdiRabatt + TT_StLinjeTMP.VerdiRabatt
                                 TT_StLinje.AntTilbSolgt   = TT_StLinje.AntTilbSolgt  + TT_StLinjeTMP.AntTilbSolgt  
                                 TT_StLinje.BrekkAnt       = TT_StLinje.BrekkAnt      + TT_StLinjeTMP.BrekkAnt      
                                 TT_StLinje.BrekkVerdi     = TT_StLinje.BrekkVerdi    + TT_StLinjeTMP.BrekkVerdi    
                                 TT_StLinje.GjenkjopAnt    = TT_StLinje.GjenkjopAnt   + TT_StLinjeTMP.GjenkjopAnt   
                                 TT_StLinje.GjenkjopVerdi  = TT_StLinje.GjenkjopVerdi + TT_StLinjeTMP.GjenkjopVerdi 
                                 TT_StLinje.IntAnt         = TT_StLinje.IntAnt        + TT_StLinjeTMP.IntAnt        
                                 TT_StLinje.IntVerdi       = TT_StLinje.IntVerdi      + TT_StLinjeTMP.IntVerdi      
                                 TT_StLinje.JustAnt        = TT_StLinje.JustAnt       + TT_StLinjeTMP.JustAnt       
                                 TT_StLinje.JustVerdi      = TT_StLinje.JustVerdi     + TT_StLinjeTMP.JustVerdi     
                                 TT_StLinje.KjopAnt        = TT_StLinje.KjopAnt       + TT_StLinjeTMP.KjopAnt       
                                 TT_StLinje.KjopVerdi      = TT_StLinje.KjopVerdi     + TT_StLinjeTMP.KjopVerdi     
                                 TT_StLinje.LagerAnt       = TT_StLinje.LagerAnt      + TT_StLinjeTMP.LagerAnt      
                                 TT_StLinje.LagerVerdi     = TT_StLinje.LagerVerdi    + TT_StLinjeTMP.LagerVerdi    
                                 TT_StLinje.ReklAnt        = TT_StLinje.ReklAnt        + TT_StLinjeTMP.ReklAnt       
                                 TT_StLinje.ReklLAnt       = TT_StLinje.ReklLAnt       + TT_StLinjeTMP.ReklLAnt      
                                 TT_StLinje.ReklLVerdi     = TT_StLinje.ReklLVerdi     + TT_StLinjeTMP.ReklLVerdi    
                                 TT_StLinje.ReklVerdi      = TT_StLinje.ReklVerdi      + TT_StLinjeTMP.ReklVerdi     
                                 TT_StLinje.SvinnAnt       = TT_StLinje.SvinnAnt       + TT_StLinjeTMP.SvinnAnt       
                                 TT_StLinje.SvinnVerdi     = TT_StLinje.SvinnVerdi     + TT_StLinjeTMP.SvinnVerdi     
                                 TT_StLinje.VerdiTilbSolgt = TT_StLinje.VerdiTilbSolgt + TT_StLinjeTMP.VerdiTilbSolgt
                                 TT_StLinje.VVarekost      = TT_StLinje.VVarekost      + TT_StLinjeTMP.VVarekost
                                 TT_StLinje.BruttoSolgt    = TT_StLinje.BruttoSolgt    + TT_StLinjeTMP.BruttoSolgt.
                  END.
              END.
          END.
        END. /* butikkloop */
    END.
    IF NOT CAN-FIND(FIRST TT_STLinje) THEN DO:
        MESSAGE "Ingen statistikk funnet"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        PUBLISH "VisTxtBox" ("").
        IF lEkstern = TRUE THEN
            APPLY "CLOSE" TO h_Window.
        RETURN.
    END.
    RUN EksportLesInn.
    EMPTY TEMP-TABLE TT_StLinje.
/*     PUBLISH "VisTxtBox" ("Leser ut data......"). */
/*     RUN ExporterTTStLinje.                       */
/*     RUN LesInnIGrid.                             */
/*     PUBLISH "VisTxtBox" ("").                    */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TagButiker fFrameWin 
PROCEDURE TagButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT  PARAMETER cButiker AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSDelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where true",
                      INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  cButiker = cButikerIdList.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject fFrameWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "ClearGrid" (cLabels).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockVindu fFrameWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldDefsLevel fFrameWin 
FUNCTION getFieldDefsLevel RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTmpDefs AS CHARACTER  NO-UNDO.

  ASSIGN cTmpDefs = IF CAN-DO("2,3",RS-Level:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
                                    "Hg;Hovedgr;;1,HgBeskr;HgBeskr;;," ELSE "Vg;Varegr;;1,VgBeskr;VgBeskr;;,"
         cTmpDefs = cTmpDefs + IF CAN-DO("3,5",RS-Level:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
                                    "Sasong;Sesong;;1,SasBeskr;SesBeskr;;," ELSE "".
  RETURN cTmpDefs.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFeltNumListe AS CHARACTER  NO-UNDO.
  ASSIGN cFeltNumListe = FILL(",",NUM-ENTRIES(cFeltnavnListe) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cFeltnavnListe):
      ASSIGN ENTRY(iCount,cFeltNumListe) = STRING(LOOKUP(ENTRY(iCount,cFeltnavnListe),cFelter)).
  END.
  RETURN cFeltNumListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION newDataObjekt fFrameWin 
FUNCTION newDataObjekt RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cNewDataObjekt AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF AVAIL ArtBas THEN DO:
          CASE RS-Level:SCREEN-VALUE:
              WHEN "2" THEN cNewDataObjekt = STRING(Artbas.Hg,"99999999").
              WHEN "3" THEN cNewDataObjekt = STRING(Artbas.Hg,"99999999") + STRING(ArtBas.Sasong,"9999").
              WHEN "4" THEN cNewDataObjekt = STRING(Artbas.Vg,"99999999").
              WHEN "5" THEN cNewDataObjekt = STRING(Artbas.Vg,"99999999")  + STRING(ArtBas.Sasong,"9999").
          END CASE.
      END.
      ELSE DO:
          CASE RS-Level:SCREEN-VALUE:
              WHEN "2" THEN cNewDataObjekt = "99999999".
              WHEN "3" THEN cNewDataObjekt = "99999999" + "9999".
              WHEN "4" THEN cNewDataObjekt = "99999999".
              WHEN "5" THEN cNewDataObjekt = "99999999" + "9999".
          END CASE.
      END.
  END.
  RETURN cNewDataObjekt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

