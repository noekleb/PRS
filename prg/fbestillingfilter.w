&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
                
DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAnropButiker     AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

DEFINE TEMP-TABLE TT_BigListItem NO-UNDO
    FIELD Butiker AS CHARACTER.

DEFINE TEMP-TABLE TT_BestStat NO-UNDO
    FIELD BestStat AS INTE
    FIELD Beskr    AS CHAR
    INDEX BestStat BestStat.


/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi" */
/*        cLabels = "Butikk,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr"                         */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2"                                                                                                                                                                                                           */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1". /* Fält som skall högerjust i XPrint */                                                                                                                                                         */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter = "TotAntPar,TotInnLev,TotMakulert,TotOverLev,TotInnkjVerdi,TotDbKr,TotSalgsVerdi,Rest".
ASSIGN cFieldDefs = 
/*  1 */ "BestNr;BestNr;;," +
/*  2 */ "CL;Butikk;;," +
/*  3 */ "BestStat;BestStat;;," +
/*  4 */ "OrdreNr;OrdreNr;;," +
/*  5 */ "VgLopNr;VgLopNr;;," +
/*  6 */ "ArtikkelNr;ArtikkelNr;;," +
/*  7 */ "ArtBeskr;ArtBeskr;;," +
/*  8 */ "BestillingsDato;Best.dato;;," +
/*  9 */ "LevDato;LevDato;;," +
/* 10 */ "TotAntPar;AntPar;;1," +
/* 11 */ "TotInnLev;InnLev;;1," +
/* 12 */ "Rest;Rest;;1," +
/* 13 */ "TotMakulert;Makulert;;1," +
/* 14 */ "TotOverLev;Overlev;;1," +
/* 15 */ "TotInnkjVerdi;Innkjverdi;;1," +
/* 16 */ "TotDbKr;DbKr;;1," +
/* 17 */ "TotSalgsVerdi;Salgsverdi;;1," +
/* 18 */ "LevNavn;LevNavn;;," +
/* 19 */ "LevKod;LevKod;;," +
/* 20 */ "LevFargKod;LevFargKod;;," +
/* 21 */ "TeamNr;TeamNr;;," +
/* 22 */ "AnonseArtikkel;AnonseArtikkel;;," +
/* 23 */ "DirekteLev;DirekteLev;;," +
/* 24 */ "AvdelingNr;AvdelingNr;;," +
/* 25 */ "AvdBeskr;AvdBeskr;;," +
/* 26 */ "Hg;Hg;;," +
/* 27 */ "HgBeskr;HgBeskr;;," +
/* 28 */ "Vg;Vg;;," +
/* 29 */ "VgBeskr;VgBeskr;;," +
/* 30 */ "Sasong;Sesong;;," +
/* 31 */ "KategoriBeskr;KategoriBeskr;;," +
/* 32 */ "LapTop;LapTop;;," +
/* 33 */ "Beskrivelse;Beskrivelse;;," +
/* 34 */ "BestType;BestType;;," +
/* 35 */ "StrTypeID;StrTypeID;;," +
/* 36 */ "Merknad;Merknad;;," +
/* 37 */ "EkstId;EkstId;;," +
/* 38 */ "EDato;EDato;;," +
/* 39 */ "BrukerID;BrukerID;;," +
/* 40 */ "RegistrertDato;RegistrertDato;;," +
/* 41 */ "RegistrertAv;RegistrertAv;;".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StLinje

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH StLinje NO-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH StLinje NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain StLinje
&Scoped-define FIRST-TABLE-IN-QUERY-fMain StLinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Butikker B-AvdelingBlank B-KategoriBlank ~
CB-ButikkTeam B-Aktiver FI-LevPeriode1 FI-LevPeriode2 B-HgBlank B-Blank ~
FI-BestDato1 FI-BestDato2 B-VgBlank B-BestDato B-VisTrans CB-BestType ~
B-LevNrBlank Tg-VisButikker CB-BestStat B-SesongBlank B-AktivitetBlank ~
B-Aktivitet B-Kategori B-Avdeling BUTTON-SokBut B-Sesong B-HuvGr B-VarGr ~
B-LevNr 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikker FI-Avdeling FI-Kategori ~
CB-ButikkTeam FI-HuvGr FI-LevPeriode1 FI-LevPeriode2 FI-VarGr FI-BestDato1 ~
FI-BestDato2 FI-LevNr CB-BestType Tg-VisButikker FI-Sesong CB-BestStat ~
FI-Aktivitet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddWhere fFrameWin 
FUNCTION AddWhere RETURNS CHARACTER
  ( INPUT cWhereString AS CHARACTER,INPUT cAddToWhere AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBestHodeFeltNr fFrameWin 
FUNCTION getBestHodeFeltNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButiker fFrameWin 
FUNCTION getButiker RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dbesthode AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Aktivitet  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AktivitetBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Avdeling  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AvdelingBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-BestDato 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-BestStat  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-BestStatBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Blank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Kategori  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KategoriBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

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

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-BestStat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bestillingsstatus" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 49.2 BY 1 NO-UNDO.

DEFINE VARIABLE CB-BestType AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Bestillingstype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0,
                     "Grunnbestilling",1,
                     "Tilleggsbestilling",2
     DROP-DOWN-LIST
     SIZE 37.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Aktivitet AS CHARACTER FORMAT "X(10)":U 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestDato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Bestillingsdato" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestDato2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestStat AS CHARACTER FORMAT "X(10)":U 
     LABEL "Bestillingsstatus" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kategori AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kategori" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevPeriode1 AS DATE FORMAT "99/99/99":U 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevPeriode2 AS DATE FORMAT "99/99/99":U 
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

DEFINE VARIABLE Tg-VisButikker AS LOGICAL INITIAL yes 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      StLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Butikker AT ROW 1.19 COL 14 COLON-ALIGNED
     FI-Avdeling AT ROW 1.19 COL 75.6 COLON-ALIGNED
     B-BestStat AT ROW 6.24 COL 138.4 NO-TAB-STOP 
     FI-Kategori AT ROW 1.19 COL 121.6 COLON-ALIGNED
     B-AvdelingBlank AT ROW 1.24 COL 97.2
     B-KategoriBlank AT ROW 1.24 COL 143.2
     CB-ButikkTeam AT ROW 2.19 COL 14 COLON-ALIGNED
     B-Aktiver AT ROW 2.19 COL 47
     FI-HuvGr AT ROW 2.19 COL 75.6 COLON-ALIGNED
     FI-LevPeriode1 AT ROW 2.19 COL 121.6 COLON-ALIGNED
     FI-LevPeriode2 AT ROW 2.19 COL 136.6 COLON-ALIGNED NO-LABEL
     B-HgBlank AT ROW 2.24 COL 97.2
     B-Blank AT ROW 2.24 COL 153
     FI-VarGr AT ROW 3.19 COL 75.6 COLON-ALIGNED
     FI-BestDato1 AT ROW 3.19 COL 121.6 COLON-ALIGNED
     FI-BestDato2 AT ROW 3.19 COL 136.6 COLON-ALIGNED NO-LABEL
     B-VgBlank AT ROW 3.24 COL 97.2
     B-BestDato AT ROW 3.24 COL 153
     B-VisTrans AT ROW 3.33 COL 47
     FI-LevNr AT ROW 4.19 COL 75.6 COLON-ALIGNED
     CB-BestType AT ROW 4.19 COL 121.6 COLON-ALIGNED
     B-LevNrBlank AT ROW 4.24 COL 97.2
     Tg-VisButikker AT ROW 4.91 COL 42
     FI-Sesong AT ROW 5.19 COL 75.6 COLON-ALIGNED
     CB-BestStat AT ROW 5.19 COL 121.6 COLON-ALIGNED
     B-SesongBlank AT ROW 5.24 COL 97.2
     FI-Aktivitet AT ROW 6.19 COL 75.6 COLON-ALIGNED
     FI-BestStat AT ROW 6.19 COL 121.6 COLON-ALIGNED
     B-AktivitetBlank AT ROW 6.24 COL 97.2
     B-BestStatBlank AT ROW 6.24 COL 143.4
     B-Aktivitet AT ROW 6.24 COL 92.2 NO-TAB-STOP 
     B-Kategori AT ROW 1.24 COL 138.2 NO-TAB-STOP 
     B-Avdeling AT ROW 1.24 COL 92.2 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 1.19 COL 31.8 NO-TAB-STOP 
     B-Sesong AT ROW 5.24 COL 92.2 NO-TAB-STOP 
     B-HuvGr AT ROW 2.24 COL 92.2 NO-TAB-STOP 
     B-VarGr AT ROW 3.24 COL 92.2 NO-TAB-STOP 
     B-LevNr AT ROW 4.24 COL 92.2 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 187.2 BY 6.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 6.48
         WIDTH              = 187.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-BestStat IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-BestStat:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-BestStatBlank IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-BestStatBlank:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN FI-Aktivitet IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BestStat IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-BestStat:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       FI-Butikker:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kategori IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
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
  DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
  ASSIGN INPUT FI-LevPeriode1
         INPUT FI-LevPeriode2
         INPUT FI-BestDato1
         INPUT FI-BestDato2.

  ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                       IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE 
                       IF FI-VarGr    <> "*" THEN "VarGr"      ELSE ""
         pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                       IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE 
                       IF FI-VarGr    <> "*" THEN FI-VarGr     ELSE "".
    IF FI-Sesong <> "*" THEN
      ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "Sasong" 
             pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-Sesong.
    IF FI-Aktivitet <> "*" THEN
      ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "Aktivitet" 
             pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-Aktivitet.
    IF FI-Kategori <> "*" THEN
      ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "Kategori" 
             pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-Kategori.
    

  IF FI-LevPeriode1 <> ? AND FI-LevPeriode2 <> ? AND FI-LevPeriode1 > FI-LevPeriode2 THEN DO:
      MESSAGE "Fra > Til"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-LevPeriode1.
      RETURN NO-APPLY.
  END.
  IF FI-BestDato1 <> ? AND FI-BestDato2 <> ? AND FI-BestDato1 > FI-BestDato2 THEN DO:
      MESSAGE "Fra > Til"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-BestDato1.
      RETURN NO-APPLY.
  END.
  PUBLISH "VisTxtBox" ("Søker data......").
  RUN StartSok.
  RUN BestHodeToTT IN h_dbesthode
    ( OUTPUT TTH,cStTypeId,getButiker(),pcFeltListe + ";" + pcVerdier,CHR(1) + STRING(Tg-VisButikker:CHECKED,"J/")).
  CREATE QUERY qh.
  qh:SET-BUFFERS(TTH).
  qh:QUERY-PREPARE("for each TT_BestHode").
  qh:QUERY-OPEN().
  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("TT_BestHode",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dbesthode),cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
  PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
  PUBLISH "LoadGrid" (cFileName,4).  /* 4 = antall frozen cols  */
  /* getSumFelter ger colnr för resp fält */
  ASSIGN cSumCols   = getSumFelter(cSummerFelter)
         cSumString = getSumFelter("BestStat") + ",SUM" .
  PUBLISH "Summer" (cSumCols,cSumString).
  /* nästa rad måste stå före 'Summer' */
/*   PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")). */
/*   PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).                                  */
  qh:QUERY-CLOSE().
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  DELETE OBJECT TTH.
  DELETE OBJECT qh.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Aktivitet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktivitet fFrameWin
ON CHOOSE OF B-Aktivitet IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Aktivitet;AktNr;Beskrivelse",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "AktNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Aktivitet:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Aktivitet     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Aktivitet:TOOLTIP = IF FI-Aktivitet = "*" THEN "" ELSE FI-Aktivitet.
        IF FI-Aktivitet <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Aktivitet:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Aktivitet:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AktivitetBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktivitetBlank fFrameWin
ON CHOOSE OF B-AktivitetBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Aktivitet:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Aktivitet:SCREEN-VALUE = cAlle
               FI-Aktivitet              = "*"
               FI-Aktivitet:TOOLTIP      = ""
               FI-Aktivitet:BGCOLOR      = ?
               B-Aktivitet:PRIVATE-DATA  = "".
    END.
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
               FI-Avdeling:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Avdeling:BGCOLOR      = ?.
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


&Scoped-define SELF-NAME B-BestDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestDato fFrameWin
ON CHOOSE OF B-BestDato IN FRAME fMain /* Blank */
DO:
  assign
    FI-BestDato1 = ?
    FI-BestDato2 = ?
    .
  display 
      FI-BestDato1 
      FI-BestDato2
      with frame {&FRAME-NAME}.

  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestStat fFrameWin
ON CHOOSE OF B-BestStat IN FRAME fMain /* ... */
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
          FI-BestStat:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-BestStat     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-BestStat:TOOLTIP = IF FI-BestStat = "*" THEN "" ELSE FI-BestStat.
        IF FI-BestStat <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-BestStat:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-BestStat:BGCOLOR   = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BestStatBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestStatBlank fFrameWin
ON CHOOSE OF B-BestStatBlank IN FRAME fMain /* Blank */
DO:
    IF FI-BestStat:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-BestStat:SCREEN-VALUE = cAlle
               FI-BestStat              = "*"
               FI-BestStat:TOOLTIP      = ""
               FI-BestStat:BGCOLOR      = ?
               B-BestStat:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank fFrameWin
ON CHOOSE OF B-Blank IN FRAME fMain /* Blank */
DO:
  assign
    FI-LevPeriode1 = ?
    FI-LevPeriode2 = ?
    .
  display 
      FI-LevPeriode1 
      FI-LevPeriode2
      with frame {&FRAME-NAME}.

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
          FI-HuvGr:TOOLTIP = IF FI-HuvGr = "*" THEN "" ELSE FI-HuvGr.
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


&Scoped-define SELF-NAME B-Kategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kategori fFrameWin
ON CHOOSE OF B-Kategori IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Kategori;KatNr;Beskrivelse",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "KatNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Kategori:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Kategori     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Kategori:TOOLTIP = IF FI-Kategori = "*" THEN "" ELSE FI-Kategori.
        IF FI-Kategori <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Kategori:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Kategori:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KategoriBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KategoriBlank fFrameWin
ON CHOOSE OF B-KategoriBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Kategori:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Kategori:SCREEN-VALUE = cAlle
               FI-Kategori              = "*"
               FI-Kategori:TOOLTIP      = ""
               FI-Kategori:BGCOLOR      = ?
               B-Kategori:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
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
                   FI-LevNr:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-LevNr:BGCOLOR  = ?.
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
               FI-Sesong:BGCOLOR      = 11.
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


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans fFrameWin
ON CHOOSE OF B-VisTrans IN FRAME fMain /* Vis transaksjon */
DO:
    RUN BestInnlev.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut fFrameWin
ON CHOOSE OF BUTTON-SokBut IN FRAME fMain /* ... */
or F10 of BUTTON-SokBut
DO:
/*    DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO. */
/*    DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO. */
   DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik;ButNamn",
                        "where CAN-DO('" + cTillgButikker + "',STRING(Butiker.Butik))",
                        INPUT-OUTPUT cButikerRowIdList,
                        "Butik",
                        INPUT-OUTPUT cButikerIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        RUN FixButikVis.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-BestType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-BestType fFrameWin
ON VALUE-CHANGED OF CB-BestType IN FRAME fMain /* Bestillingstype */
DO:
  ASSIGN CB-BestType.
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ButikkTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ButikkTeam fFrameWin
ON VALUE-CHANGED OF CB-ButikkTeam IN FRAME fMain /* Butikkteam */
DO:
  ASSIGN SELF:TOOLTIP = IF SELF:SCREEN-VALUE = "INGEN" THEN "" ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,";") = 2 THEN
      ENTRY(1,SELF:SCREEN-VALUE,";") ELSE REPLACE(SELF:SCREEN-VALUE,CHR(1),",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BestDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestDato1 fFrameWin
ON LEAVE OF FI-BestDato1 IN FRAME fMain /* Bestillingsdato */
DO:
  ASSIGN INPUT FI-BestDato1 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      ASSIGN FI-BestDato1 = ?.
  display FI-BestDato1 
      with frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BestDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestDato2 fFrameWin
ON LEAVE OF FI-BestDato2 IN FRAME fMain
DO:
    ASSIGN INPUT FI-BestDato2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN FI-BestDato2 = ?.
    display FI-BestDato2 
        with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestDato2 fFrameWin
ON TAB OF FI-BestDato2 IN FRAME fMain
OR "RETURN" OF FI-LevPeriode2
DO:
  ASSIGN
      FI-BestDato1
      FI-BestDato2.
  IF FI-BestDato2 < FI-BestDAto1 THEN
  DO:
      MESSAGE "Til dato er mindre enn fra dato!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevPeriode1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevPeriode1 fFrameWin
ON LEAVE OF FI-LevPeriode1 IN FRAME fMain /* Lev.dato */
DO:
  ASSIGN INPUT FI-LevPeriode1 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      ASSIGN FI-LevPeriode1 = ?.
  display 
      FI-LevPeriode1 
      with frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevPeriode2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevPeriode2 fFrameWin
ON LEAVE OF FI-LevPeriode2 IN FRAME fMain
DO:
    ASSIGN INPUT FI-LevPeriode2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN FI-LevPeriode2 = ?.
    display 
        FI-LevPeriode2 
        with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevPeriode2 fFrameWin
ON TAB OF FI-LevPeriode2 IN FRAME fMain
OR "RETURN" OF FI-LevPeriode2
DO:
  ASSIGN
      FI-LevPeriode1
      FI-LevPeriode2.
  IF FI-LevPeriode2 < FI-LevPeriode1 THEN
  DO:
      MESSAGE "Til dato er mindre enn fra dato!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.
  RETURN NO-APPLY.    
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dbesthode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbesthodeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dbesthode ).
       RUN repositionObject IN h_dbesthode ( 3.86 , 15.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 10.80 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestInnlev fFrameWin 
PROCEDURE BestInnlev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cVerdier       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iTest          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE wBestHodeRecid AS RECID  NO-UNDO.
    PUBLISH "FeltVerdier" (OUTPUT cVerdier,DYNAMIC-FUNCTION('getBestHodeFeltNr':U),"SAME"). 
    IF cVerdier = "" THEN
        RETURN.
    ASSIGN iTest = INT(cVerdier) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN.
    FIND BestHode WHERE BestHode.BestNr = INT(cVerdier) NO-LOCK NO-ERROR.
    IF NOT AVAIL BestHode THEN
        RETURN.
    FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN
        RETURN.
    ASSIGN wBestHodeRecid = RECID(BestHode).
    DYNAMIC-FUNCTION('fLockvindu':U IN h_Window,
      INPUT TRUE /* LOGICAL */).
/*     fLockvindu(TRUE). */
    IF BestHode.BestStat < 4 THEN
        run w-gridord.w  (input recid(ArtBas), input-output wBestHodeRecid, "ENDRE").
    ELSE
        run w-gridinnlev.w  (input recid(ArtBas), input-output wBestHodeRecid, "INLEV").
    DYNAMIC-FUNCTION('fLockvindu':U IN h_Window,
          INPUT FALSE /* LOGICAL */).
/*     fLockvindu(FALSE). */
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
  DISPLAY FI-Butikker FI-Avdeling FI-Kategori CB-ButikkTeam FI-HuvGr 
          FI-LevPeriode1 FI-LevPeriode2 FI-VarGr FI-BestDato1 FI-BestDato2 
          FI-LevNr CB-BestType Tg-VisButikker FI-Sesong CB-BestStat FI-Aktivitet 
      WITH FRAME fMain.
  ENABLE FI-Butikker B-AvdelingBlank B-KategoriBlank CB-ButikkTeam B-Aktiver 
         FI-LevPeriode1 FI-LevPeriode2 B-HgBlank B-Blank FI-BestDato1 
         FI-BestDato2 B-VgBlank B-BestDato B-VisTrans CB-BestType B-LevNrBlank 
         Tg-VisButikker CB-BestStat B-SesongBlank B-AktivitetBlank B-Aktivitet 
         B-Kategori B-Avdeling BUTTON-SokBut B-Sesong B-HuvGr B-VarGr B-LevNr 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikVis fFrameWin 
PROCEDURE FixButikVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF cButikerIdList <> "" THEN
          ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = ",INGEN"
                 FI-Butikker = cButikerIdList
                 FI-Butikker:BGCOLOR = 15
                 FI-Butikker:SCREEN-VALUE = "(" + STRING(NUM-ENTRIES(cButikerIdList,"|")) + ")"
                 FI-Butikker:TOOLTIP = REPLACE(cButikerIdList,"|",",")
                 CB-ButikkTeam:SCREEN-VALUE = "INGEN"
                 CB-ButikkTeam:SENSITIVE    = FALSE.
      ELSE
          ASSIGN FI-Butikker:BGCOLOR = ?
                 FI-Butikker = ""
                 FI-Butikker:SCREEN-VALUE = ""
                 FI-Butikker:TOOLTIP = ""
                 CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
                 CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cListItemPairs,cUserDefaultBut) THEN 
                                                    cUserDefaultBut ELSE ENTRY(2,cListItemPairs)
                 CB-ButikkTeam:SENSITIVE    = TRUE.
                 .
     APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker fFrameWin 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cButString   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOkButiker   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButStringListe AS CHARACTER  NO-UNDO.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    ASSIGN cUserDefaultBut = STRING(Bruker.ButikkNr)
           cListItemPairs  = "".
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                              ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
        ASSIGN cButString = "".
        FOR EACH ButikkKobling OF ButikkTeam.
            ASSIGN cButString = cButString + (IF cButString = "" THEN "" ELSE CHR(1)) 
                              + STRING(ButikkKobling.butik).
            FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
            IF NOT AVAIL TT_TillgButikker THEN DO:
                    CREATE TT_TillgButikker.
                    ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
            END.
        END.
        IF NUM-ENTRIES(cButString,CHR(1)) > 25 THEN DO:
            CREATE TT_BigListItem.
            ASSIGN TT_BigListItem.Butiker = cButString
                   cButString = "(" + STRING(NUM-ENTRIES(cButString,CHR(1))) + ");" + STRING(ROWID(TT_BigListItem)).
            RELEASE TT_BigListItem.
        END.
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                             ButikkTeam.Beskrivelse + "," + cButString
               cButStringListe = cButStringListe + (IF cButStringListe <> "" THEN "," ELSE "") + cButString.
    END.
    FOR EACH TT_TillgButikker:
        ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(TT_TillgButikker.Butik).
    END.
    /* Om vi har fått en butiklista genom proc SetIpButiker skall vi kontrollera att listan */
    /* innehåller butiker vi har tillgång till och ev strippa bort andra butiker */
    IF cAnropButiker <> "" AND cAnropButiker <> cUserDefaultBut THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cAnropButiker,"|"):
            IF CAN-DO(cTillgButikker,ENTRY(iCount,cAnropButiker,"|")) THEN
                ASSIGN cOkButiker = cOkButiker + (IF cOkButiker <> "" THEN "|" ELSE "") + ENTRY(iCount,cAnropButiker,"|").
        END.
        /* om cOkButiker finns i teamlistan i combo väljer vi det teamet som cUserdefault */
/*         MESSAGE "cTillgbutikker  : " cTillgbutikker SKIP              */
/*                 "cOkButiker     : " cOkButiker SKIP                   */
/*                 "cButStringListe:" cButStringListe                    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*         IF cOkButiker <> "" THEN DO:                                  */
/*             IF CAN-DO(cButStringListe,cOkButiker) THEN                */
/*                 ASSIGN cUserDefaultBut = REPLACE(cOkButiker,"|",","). */
/*             ELSE                                                      */
/*                 cButStringListe = cOkButiker.                         */
/*         END.                                                          */
        IF cOkButiker <> "" THEN DO:
            ASSIGN cButikerIdList = cOkButiker.
            DO iCount = 1 TO NUM-ENTRIES(cOkButiker,"|"):
                FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCOunt,cOkButiker,"|")) NO-LOCK NO-ERROR.
                IF AVAIL Butiker THEN
                   ASSIGN cButikerRowIdList = cButikerRowIdList + (IF cButikerRowIdList <> "" THEN "," ELSE "") + STRING(ROWID(Butiker)).
            END.
        END.
    END.
    RUN FixButikVis.
/*     ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs                                                           */
/*            CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN */
/*                                              cUserDefaultBut ELSE ENTRY(2,cListItemPairs).                          */
/*     APPLY "VALUE-CHANGED" TO CB-ButikkTeam.                                                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB fFrameWin 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
--------------------------------------------------------------------------------*/
  def var wBestStatus as char no-undo.
  DEFINE VARIABLE cBestType AS CHARACTER  NO-UNDO.
  ASSIGN wBestStatus = cAlle.

  SYSPARA:
  for each SysPAra no-lock where
    SysPara.SysHId = 5 and
    SysPara.SysGr  = 2 and
    SysPara.ParaNr < 99:
    
    assign
      wBestStatus = wBestStatus + 
                   (if wBestStatus = "" 
                      then ""
                      else ",") +
                   string(SysPara.ParaNr,"z9") + ": " + SysPara.Parameter1.    
  end. /* SYSPARA */
  SYSPARA2:
  for each SysPAra no-lock where
    SysPara.SysHId = 5 and
    SysPara.SysGr  = 5 and
    SysPara.ParaNr < 99:
    
    assign
      cBestType = cBestType + 
                   (if cBestType = "" 
                      then ""
                      else ",") +
                   string(SysPara.ParaNr,"z9") + ": " + SysPara.Beskrivelse + "," + string(SysPara.ParaNr).    
  end. /* SYSPARA */

  assign
    CB-BestStat = entry(1,wBestStatus)
    CB-BestStat:List-Items in frame {&FRAME-NAME} = wBestStatus.
  
  display CB-BestStat with frame {&FRAME-NAME}.
  ASSIGN CB-BestType:LIST-ITEM-PAIRS = cAlle + ",0," + cBestType
         CB-BestType:SCREEN-VALUE = "0".
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
  ASSIGN h_Window = SOURCE-PROCEDURE.
  RUN FixStrings.
  RUN SUPER.
  {syspara.i 1 100 1 cAlle}
  ASSIGN cAlle = TRIM(cAlle).
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN InitCB.
  RUN InitButiker.
  DO WITH FRAME {&FRAME-NAME}:
      APPLY "CHOOSE" TO B-LevNrBlank.
      APPLY "CHOOSE" TO B-AvdelingBlank.
      APPLY "CHOOSE" TO B-HgBlank.
      APPLY "CHOOSE" TO B-VgBlank.
      APPLY "CHOOSE" TO B-SesongBlank.
      APPLY "CHOOSE" TO B-AktivitetBlank.
      APPLY "CHOOSE" TO B-KategoriBlank.
  END.
  /* Code placed here will execute AFTER standard behavior.    */
/*   PUBLISH "GetWindowH" (OUTPUT h_Window ). */

  ASSIGN cStTypeId = "NONE"
         cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt".

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
DEFINE        VARIABLE  cFraAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFraPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTTId          AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:

      ASSIGN cFilterVerdier = CB-BestStat:LABEL + ": " + CB-BestStat:SCREEN-VALUE + CHR(10) +
             CB-BestType:LABEL + ": " + ENTRY((CB-BestType:LOOKUP(CB-BestType:SCREEN-VALUE) * 2) - 1,CB-BestType:LIST-ITEM-PAIRS) + CHR(10) +
          (IF FI-LevNr <> "*" THEN FI-LevNr:LABEL + ": " + FI-LevNr + CHR(10) ELSE "") +
          (IF FI-LevPeriode1 <> ? AND FI-LevPeriode2 <> ? THEN FI-LevPeriode1:LABEL + ": " + 
                                       FI-LevPeriode1:SCREEN-VALUE + " - " + FI-LevPeriode2:SCREEN-VALUE + CHR(10) ELSE "") +
          (IF FI-LevPeriode1 <> ? AND FI-LevPeriode2 = ? THEN FI-LevPeriode1:LABEL + ": " + 
                                       FI-LevPeriode1:SCREEN-VALUE + " -> "  + CHR(10) ELSE "") +
          (IF FI-LevPeriode1 = ? AND FI-LevPeriode2 <> ? THEN FI-LevPeriode1:LABEL + ": " + 
                                       " -> " + FI-LevPeriode2:SCREEN-VALUE + CHR(10) ELSE "") +
          (IF FI-BestDato1 <> ? AND FI-BestDato2 <> ? THEN FI-BestDato1:LABEL + ": " + 
                                       FI-BestDato1:SCREEN-VALUE + " - " + FI-BestDato2:SCREEN-VALUE + CHR(10) ELSE "") +
          (IF FI-BestDato1 <> ? AND FI-BestDato2 = ? THEN FI-BestDato1:LABEL + ": " + 
                                       FI-BestDato1:SCREEN-VALUE + " -> "  + CHR(10) ELSE "") +
          (IF FI-BestDato1 = ? AND FI-BestDato2 <> ? THEN FI-BestDato1:LABEL + ": " + 
                                       " -> " + FI-BestDato2:SCREEN-VALUE + CHR(10) ELSE "")
          cFilterVerdier = REPLACE(REPLACE(cFilterVerdier,"[",""),"]","")
          cColAlign = cRightCols.
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
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQryString   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWhereString AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cQryString   = "FOR EACH BestHode NO-LOCK"
             cWhereString = IF CB-BestStat:SCREEN-VALUE <> cAlle THEN AddWhere(cWhereString,"BestHode.BestStat = " + TRIM(ENTRY(1,CB-BestStat:SCREEN-VALUE,":"))) ELSE cWhereString
             cWhereString = IF FI-LevNr <> "*" AND NUM-ENTRIES(FI-LevNr) = 1 THEN AddWhere(cWhereString,"BestHode.Levnr = " + TRIM(FI-LevNr)) ELSE cWhereString
             cWhereString = IF FI-LevNr <> "*" AND NUM-ENTRIES(FI-LevNr) > 1 THEN AddWhere(cWhereString,'CAN-DO("' + FI-Levnr + '",STRING(BestHode.LevNr))') ELSE cWhereString
             cWhereString = IF CB-BestType:SCREEN-VALUE <> "0" THEN AddWhere(cWhereString,"BestHode.BestType = " + TRIM(CB-BestType:SCREEN-VALUE)) ELSE cWhereString
             cWhereString = IF INPUT FI-LevPeriode1 <> ? AND INPUT FI-LevPeriode2 = ? THEN AddWhere(cWhereString,'BestHode.LevDato >= DATE("' + STRING(INPUT FI-LevPeriode1) + '")') ELSE cWhereString
             cWhereString = IF INPUT FI-LevPeriode1 = ? AND INPUT FI-LevPeriode2 <> ? THEN AddWhere(cWhereString,'BestHode.LevDato <= DATE("' + STRING(INPUT FI-LevPeriode2) + '")') ELSE cWhereString
             cWhereString = IF INPUT FI-LevPeriode1 <> ? AND INPUT FI-LevPeriode2 <> ? 
                        THEN AddWhere(cWhereString,'BestHode.LevDato >= DATE("' + STRING(INPUT FI-LevPeriode1) + '") AND BestHode.LevDato <= DATE("' + STRING(INPUT FI-LevPeriode2) + '")') ELSE cWhereString
             cWhereString = IF INPUT FI-BestDato1 <> ? AND INPUT FI-BestDato2 = ? THEN AddWhere(cWhereString,'BestHode.BestillingsDato >= DATE("' + STRING(INPUT FI-BestDato1) + '")') ELSE cWhereString
             cWhereString = IF INPUT FI-BestDato1 = ? AND INPUT FI-BestDato2 <> ? THEN AddWhere(cWhereString,'BestHode.BestillingsDato <= DATE("' + STRING(INPUT FI-BestDato2) + '")') ELSE cWhereString
             cWhereString = IF INPUT FI-BestDato1 <> ? AND INPUT FI-BestDato2 <> ? 
                        THEN AddWhere(cWhereString,'BestHode.BestillingsDato >= DATE("' + STRING(INPUT FI-BestDato1) + '") AND BestHode.BestillingsDato <= DATE("' + STRING(INPUT FI-BestDato2) + '")') ELSE cWhereString
             cQryString   = cQryString + (IF cWhereString <> "" THEN " WHERE " + cWhereString ELSE "").
  END.
  DYNAMIC-FUNCTION('setQueryString':U IN h_dbesthode,
     INPUT cQryString /* CHARACTER */).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddWhere fFrameWin 
FUNCTION AddWhere RETURNS CHARACTER
  ( INPUT cWhereString AS CHARACTER,INPUT cAddToWhere AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cWhereString + (IF cWhereString <> "" THEN " AND " ELSE "") + cAddToWhere.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBestHodeFeltNr fFrameWin 
FUNCTION getBestHodeFeltNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN STRING(LOOKUP("BestNr",cFelter)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButiker fFrameWin 
FUNCTION getButiker RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
 DO WITH FRAME {&FRAME-NAME}:
     IF FI-Butikker = "" THEN DO:
          IF NUM-ENTRIES(CB-ButikkTeam:SCREEN-VALUE,";") = 2 THEN DO:
              FIND TT_BigListItem WHERE ROWID(TT_BigListItem) = TO-ROWID(ENTRY(2,CB-ButikkTeam:SCREEN-VALUE,";")).
              ASSIGN cButiker = REPLACE(TT_BigListItem.Butiker,CHR(1),",").
          END.
          ELSE
              ASSIGN cButiker = REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",").
      END.
      ASSIGN cButiker = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE cButiker).
 END.

  RETURN cButiker.   /* Function return value. */

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

