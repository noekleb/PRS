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
DEFINE VARIABLE cRightColsNy  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
                
DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAnropButiker     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabelsOrg AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lRapport AS LOGICAL    NO-UNDO.

DEFINE VARIABLE hSA AS HANDLE     NO-UNDO.
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

DEFINE TEMP-TABLE TT_BigListItem NO-UNDO
    FIELD Butiker AS CHARACTER.

DEFINE TEMP-TABLE TT_BestStat NO-UNDO
    FIELD BestStat AS INTE
    FIELD Beskr    AS CHAR
    FIELD iRowNr   AS INTE
    FIELD RowSelected AS LOG
    INDEX BestStat BestStat.


/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi" */
/*        cLabels = "Butikk,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr"                         */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2"                                                                                                                                                                                                           */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1". /* Fält som skall högerjust i XPrint */                                                                                                                                                         */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter = "TotAntPar,TotInnLev,TotMakulert,TotOverLev,TotInnkjVerdi,TotDbKr,TotSalgsVerdi,Rest".
ASSIGN cFieldDefs = 
/*  1 */ "Vg;Vg;;," +
/*  2 */ "Vgbeskr;Beskr;;," +
/*  3 */ "Verdier;Verdier;;".

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
&Scoped-Define ENABLED-OBJECTS RECT-67 FI-Butikker B-BestStat ~
FI-SalgsDato1-1 FI-SalgsDato1-2 B-SalgsDato B-SesongBlank CB-ButikkTeam ~
B-Aktiver B-SesongBlank2 FI-SalgsDato2-1 FI-SalgsDato2-2 B-SalgsDato-2 ~
B-AvdelingBlank B-VisTrans B-Rapport FI-BestDato1 FI-BestDato2 B-BestDato ~
B-HgBlank B-BestStatBlank B-VgBlank B-LevNrBlank RS-Type B-KategoriBlank ~
B-Sesong2 B-Kategori B-Avdeling BUTTON-SokBut B-Sesong B-HuvGr B-VarGr ~
B-LevNr FI-Rstext 
&Scoped-Define DISPLAYED-OBJECTS Tg-VisButikker FI-Butikker FI-Sesong ~
FI-SalgsDato1-1 FI-SalgsDato1-2 CB-ButikkTeam FI-Sesong2 FI-SalgsDato2-1 ~
FI-SalgsDato2-2 FI-Avdeling FI-HuvGr FI-BestDato1 FI-BestDato2 FI-VarGr ~
FI-BestStat FI-LevNr RS-Type FI-Kategori FI-Rstext 

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
  ( INPUT cButikListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dbesthode AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

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

DEFINE BUTTON B-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SalgsDato 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SalgsDato-2 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Sesong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Sesong2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SesongBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SesongBlank2 
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

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

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
     LABEL "Beststat" 
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

DEFINE VARIABLE FI-Rstext AS CHARACTER FORMAT "X(256)":U INITIAL "Bestilling=Antall ulike artikkler" 
      VIEW-AS TEXT 
     SIZE 37 BY .62 NO-UNDO.

DEFINE VARIABLE FI-SalgsDato1-1 AS DATE FORMAT "99/99/99":U 
     LABEL "Salgsdato S1" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SalgsDato1-2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SalgsDato2-1 AS DATE FORMAT "99/99/99":U 
     LABEL "Salgsdato S2" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SalgsDato2-2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sesong1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong2 AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sesong2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Type AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bestilling", "1",
"Salg", "2"
     SIZE 37 BY 1.14 NO-UNDO.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 2.86.

DEFINE VARIABLE Tg-VisButikker AS LOGICAL INITIAL no 
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
     Tg-VisButikker AT ROW 1 COL 42
     FI-Butikker AT ROW 1.19 COL 14 COLON-ALIGNED
     FI-Sesong AT ROW 1.19 COL 75.6 COLON-ALIGNED
     B-BestStat AT ROW 5.19 COL 138.4 NO-TAB-STOP 
     FI-SalgsDato1-1 AT ROW 1.19 COL 121.6 COLON-ALIGNED
     FI-SalgsDato1-2 AT ROW 1.19 COL 136.6 COLON-ALIGNED NO-LABEL
     B-SalgsDato AT ROW 1.19 COL 153
     B-SesongBlank AT ROW 1.24 COL 97.2
     CB-ButikkTeam AT ROW 2.19 COL 14 COLON-ALIGNED
     B-Aktiver AT ROW 2.19 COL 42
     FI-Sesong2 AT ROW 2.19 COL 75.6 COLON-ALIGNED
     B-SesongBlank2 AT ROW 2.19 COL 97.2
     FI-SalgsDato2-1 AT ROW 2.19 COL 121.6 COLON-ALIGNED
     FI-SalgsDato2-2 AT ROW 2.19 COL 136.6 COLON-ALIGNED NO-LABEL
     B-SalgsDato-2 AT ROW 2.19 COL 153
     FI-Avdeling AT ROW 3.19 COL 75.6 COLON-ALIGNED
     B-AvdelingBlank AT ROW 3.24 COL 97.2
     B-VisTrans AT ROW 3.33 COL 42
     B-Rapport AT ROW 3.86 COL 21
     FI-HuvGr AT ROW 4.19 COL 75.6 COLON-ALIGNED
     FI-BestDato1 AT ROW 4.19 COL 121.6 COLON-ALIGNED
     FI-BestDato2 AT ROW 4.19 COL 136.6 COLON-ALIGNED NO-LABEL
     B-BestDato AT ROW 4.19 COL 153
     B-HgBlank AT ROW 4.24 COL 97.2
     FI-VarGr AT ROW 5.19 COL 75.6 COLON-ALIGNED
     FI-BestStat AT ROW 5.19 COL 121.6 COLON-ALIGNED
     B-BestStatBlank AT ROW 5.19 COL 143.4
     B-VgBlank AT ROW 5.24 COL 97.2
     FI-LevNr AT ROW 6.19 COL 75.6 COLON-ALIGNED
     B-LevNrBlank AT ROW 6.24 COL 97.2
     RS-Type AT ROW 7 COL 23 NO-LABEL
     FI-Kategori AT ROW 7.19 COL 75.6 COLON-ALIGNED
     B-KategoriBlank AT ROW 7.19 COL 97.2
     B-Sesong2 AT ROW 2.19 COL 92.2 NO-TAB-STOP 
     B-Kategori AT ROW 7.19 COL 92.2 NO-TAB-STOP 
     B-Avdeling AT ROW 3.24 COL 92.2 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 1.19 COL 31.8 NO-TAB-STOP 
     B-Sesong AT ROW 1.24 COL 92.2 NO-TAB-STOP 
     B-HuvGr AT ROW 4.24 COL 92.2 NO-TAB-STOP 
     B-VarGr AT ROW 5.24 COL 92.2 NO-TAB-STOP 
     B-LevNr AT ROW 6.24 COL 92.2 NO-TAB-STOP 
     FI-Rstext AT ROW 6.24 COL 20.4 COLON-ALIGNED NO-LABEL
     RECT-67 AT ROW 5.52 COL 15.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 187.2 BY 7.57.


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
         HEIGHT             = 7.57
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

/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BestStat IN FRAME fMain
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN FI-Sesong2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Tg-VisButikker IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       Tg-VisButikker:HIDDEN IN FRAME fMain           = TRUE.

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
  DEFINE VARIABLE cFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cSesong AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dDatoFra1 AS DATE       NO-UNDO.
  DEFINE VARIABLE dDatoTil1 AS DATE       NO-UNDO.
  DEFINE VARIABLE dDatoFra2 AS DATE       NO-UNDO.
  DEFINE VARIABLE dDatoTil2 AS DATE       NO-UNDO.
  ASSIGN INPUT FI-BestDato1
         INPUT FI-BestDato2
         INPUT FI-SalgsDato1-1
         INPUT FI-SalgsDato1-2
         INPUT FI-SalgsDato2-1
         INPUT FI-SalgsDato2-2.
  IF FI-Sesong = "*" AND FI-Sesong2 = "*" THEN DO:
      MESSAGE "Angi sesong!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
      cTmpFeltListe = "AvdelingNr,HuvGr,VarGr,LevNr,Kategori".
      DO iCount = 1 TO NUM-ENTRIES(cTmpFeltListe):
          CASE ENTRY(iCount,cTmpFeltListe):
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
              WHEN "Kategori" THEN
                  IF FI-Kategori  <> "*" THEN
                      ASSIGN cFeltListe = cFeltListe + (IF cFeltListe <> "" THEN CHR(1) ELSE "") + "Kategori"
                             cVerdier   = cVerdier   + (IF cVerdier <> "" THEN CHR(1) ELSE "") + FI-Kategori.
          END CASE.
      END.


  IF RS-Type:SCREEN-VALUE = "1" AND FI-BestDato1 <> ? AND FI-BestDato2 <> ? AND FI-BestDato1 > FI-BestDato2 THEN DO:
      MESSAGE "Fra > Til"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-BestDato1.
      RETURN NO-APPLY.
  END.
  ELSE IF RS-Type:SCREEN-VALUE = "2" AND FI-SalgsDato1-1 <> ? AND FI-SalgsDato1-2 <> ? AND FI-SalgsDato1-1 > FI-SalgsDato1-2 THEN DO:
      MESSAGE "Dato S1: Fra > Til"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-SalgsDato1-1.
      RETURN NO-APPLY.
  END.
  ELSE IF RS-Type:SCREEN-VALUE = "2" AND FI-SalgsDato2-1 <> ? AND FI-SalgsDato2-2 <> ? AND FI-SalgsDato2-1 > FI-SalgsDato2-2 THEN DO:
      MESSAGE "Dato S2: Fra > Til"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-SalgsDato2-1.
      RETURN NO-APPLY.
  END.

  PUBLISH "VisTxtBox" ("Søker data......").
  cSesong = REPLACE(FI-Sesong + "," + FI-Sesong2,"*","").
  dDatoFra1 = IF RS-Type:SCREEN-VALUE = "1" THEN FI-BestDato1 ELSE FI-SalgsDato1-1.
  dDatoTil1 = IF RS-Type:SCREEN-VALUE = "1" THEN FI-BestDato2 ELSE FI-SalgsDato1-2.
  dDatoFra2 = IF RS-Type:SCREEN-VALUE = "2" THEN FI-SalgsDato1-1 ELSE ?.
  dDatoTil2 = IF RS-Type:SCREEN-VALUE = "2" THEN FI-SalgsDato1-2 ELSE ?.
  RUN sesonganalyse.p PERSISTENT SET hSA
    (cSesong,getButiker(),cFeltListe + ";" + cVerdier,CHR(1) + STRING(Tg-VisButikker:CHECKED,"J/"),FI-Beststat,dDatoFra1,dDatoTil1,dDatoFra2,dDatoTil2,lRapport).
  IF RS-Type:SCREEN-VALUE = "1" THEN
      RUN Bestanalys1 IN hSA (OUTPUT TTH).
  ELSE IF RS-Type:SCREEN-VALUE = "2" THEN
      RUN Salgsanalys1 IN hSA (OUTPUT TTH).
/*   MESSAGE VALID-HANDLE(TTH)              */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF lRapport = FALSE THEN DO:
      CREATE QUERY qh.
      qh:SET-BUFFERS(TTH).
      qh:QUERY-PREPARE("for each TT_Vg").
      qh:QUERY-OPEN().
      PUBLISH "VisTxtBox" ("Leser ut data......").
      cLabelsOrg = cLabels.
      RUN GetNylabels (getButiker()).
      RUN rappgenqry.p ("TT_Vg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
      cLabels = cLabelsOrg.
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      PUBLISH "LoadGrid" (cFileName,3).  /* 4 = antall frozen cols  */
      /* getSumFelter ger colnr för resp fält */
      ASSIGN cSumCols   = getSumFelter(getButiker()).
             cSumString = getSumFelter("Vg") + ",SUM" .
      PUBLISH "Summer" (cSumCols,cSumString).
      /* nästa rad måste stå före 'Summer' */
    /*   PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")). */
    /*   PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).                                  */
      qh:QUERY-CLOSE().
      TTH:EMPTY-TEMP-TABLE().
      PUBLISH "VisTxtBox" ("").
      DELETE OBJECT TTH.
      DELETE OBJECT qh.
  END.
  ELSE DO:
      RUN dynsesonganalyse.p (TTH,"","",getbutiker()).
  END.
  PUBLISH "VisTxtBox" ("").
  DELETE PROCEDURE hSA.
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
    IF NOT cIdList = "" THEN DO:
        FOR EACH TT_BestStat:
            IF CAN-DO(REPLACE(cIdList,"|",","),STRING(TT_BestStat.Beststat)) THEN
                TT_Beststat.RowSelected = TRUE.
        END.
    END.
    RUN JBoxDSelector.w (THIS-PROCEDURE,50,
                         "temp-table"
                         + ";BestStat|INTEGER|>>9||Beststat"          /* Fieldname|Datatype|Format|Initial value|Label */
                         + ";Beskr|CHARACTER|x(30)||Beskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "BestStat",
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
        FOR EACH TT_BestStat:
            TT_Beststat.RowSelected = FALSE.
        END.
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


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport fFrameWin
ON CHOOSE OF B-Rapport IN FRAME fMain /* Rapport */
DO:
  ASSIGN lRapport = TRUE.
  APPLY "CHOOSE" TO B-Aktiver.
  ASSIGN lRapport = FALSE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SalgsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SalgsDato fFrameWin
ON CHOOSE OF B-SalgsDato IN FRAME fMain /* Blank */
DO:
  assign
    FI-SalgsDato2-1 = ?
    FI-SalgsDato2-2 = ?
    .
  display 
      FI-SalgsDato2-1 
      FI-SalgsDato2-2
      with frame {&FRAME-NAME}.

  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SalgsDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SalgsDato-2 fFrameWin
ON CHOOSE OF B-SalgsDato-2 IN FRAME fMain /* Blank */
DO:
  assign
    FI-SalgsDato2-1 = ?
    FI-SalgsDato2-2 = ?
    .
  display 
      FI-SalgsDato2-1 
      FI-SalgsDato2-2
      with frame {&FRAME-NAME}.

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
    DEFINE VARIABLE bOK          AS LOGICAL    NO-UNDO.
    DEF    VAR      cLookupValue AS CHAR NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    

    /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
    /*          Param2: <Where sats> m/Join                                              */
    /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
    /* Kalkulerte felt kan også benyttes, label, format o.l..       */
    cLookupValue = "Sasong".
    RUN JBoxDLookup.w ("SaSong;Sasong;SasBeskr","where true",INPUT-OUTPUT cLookupValue).
    IF cLookupValue = "" THEN
        RETURN.
    FI-Sesong:SCREEN-VALUE = cLookupValue.
    FI-Sesong = FI-Sesong:SCREEN-VALUE.
/*     RUN JBoxDSelector.w (THIS-PROCEDURE,0,                                    */
/*                         "SaSong;Sasong;SasBeskr",                             */
/*                         "WHERE TRUE",                                         */
/*                         INPUT-OUTPUT cRowIdList,                              */
/*                         "Sasong",                                             */
/*                         INPUT-OUTPUT cIdList,                                 */
/*                         "","",                                                */
/*                         OUTPUT bOK).                                          */
/*     IF bOK THEN DO:                                                           */
/*         assign                                                                */
/*           FI-Sesong:SCREEN-VALUE = if cIdList = ""                            */
/*                             then cAlle                                        */
/*                           else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )" */
/*           FI-Sesong     = if cIdList = ""                                     */
/*                             then "*"                                          */
/*                             else REPLACE(cIdList,"|",",")                     */
/*           FI-Sesong:TOOLTIP = IF FI-Sesong = "*" THEN "" ELSE FI-Sesong.      */
/*         IF FI-Sesong <> "*" THEN                                              */
/*             ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList          */
/*                FI-Sesong:BGCOLOR      = 11.                                   */
/*         ELSE                                                                  */
/*             ASSIGN SELF:PRIVATE-DATA = ""                                     */
/*                FI-Sesong:BGCOLOR      = ?.                                    */
/*      END.                                                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sesong2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sesong2 fFrameWin
ON CHOOSE OF B-Sesong2 IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK          AS LOGICAL    NO-UNDO.
    DEF    VAR      cLookupValue AS CHAR NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    

    /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
    /*          Param2: <Where sats> m/Join                                              */
    /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
    /* Kalkulerte felt kan også benyttes, label, format o.l..       */
    cLookupValue = "Sasong".
    RUN JBoxDLookup.w ("SaSong;Sasong;SasBeskr","where true",INPUT-OUTPUT cLookupValue).
    IF cLookupValue = "" THEN
        RETURN.
    FI-Sesong2:SCREEN-VALUE = cLookupValue.
    FI-Sesong2 = FI-Sesong2:SCREEN-VALUE.
/*     RUN JBoxDSelector.w (THIS-PROCEDURE,0,                                    */
/*                         "SaSong;Sasong;SasBeskr",                             */
/*                         "WHERE TRUE",                                         */
/*                         INPUT-OUTPUT cRowIdList,                              */
/*                         "Sasong",                                             */
/*                         INPUT-OUTPUT cIdList,                                 */
/*                         "","",                                                */
/*                         OUTPUT bOK).                                          */
/*     IF bOK THEN DO:                                                           */
/*         assign                                                                */
/*           FI-Sesong2:SCREEN-VALUE = if cIdList = ""                            */
/*                             then cAlle                                        */
/*                           else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )" */
/*           FI-Sesong2     = if cIdList = ""                                     */
/*                             then "*"                                          */
/*                             else REPLACE(cIdList,"|",",")                     */
/*           FI-Sesong2:TOOLTIP = IF FI-Sesong2 = "*" THEN "" ELSE FI-Sesong2.      */
/*         IF FI-Sesong2 <> "*" THEN                                              */
/*             ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList          */
/*                FI-Sesong2:BGCOLOR      = 11.                                   */
/*         ELSE                                                                  */
/*             ASSIGN SELF:PRIVATE-DATA = ""                                     */
/*                FI-Sesong2:BGCOLOR      = ?.                                    */
/*      END.                                                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongBlank fFrameWin
ON CHOOSE OF B-SesongBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Sesong:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Sesong:SCREEN-VALUE = ""
               FI-Sesong              = "*"
               FI-Sesong:TOOLTIP      = ""
               FI-Sesong:BGCOLOR      = ?
               B-Sesong:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongBlank2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongBlank2 fFrameWin
ON CHOOSE OF B-SesongBlank2 IN FRAME fMain /* Blank */
DO:
    IF FI-Sesong2:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Sesong2:SCREEN-VALUE = ""
               FI-Sesong2              = "*"
               FI-Sesong2:TOOLTIP      = ""
               FI-Sesong2:BGCOLOR      = ?
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
OR "RETURN" OF FI-BestDato2
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


&Scoped-define SELF-NAME FI-SalgsDato1-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SalgsDato1-1 fFrameWin
ON LEAVE OF FI-SalgsDato1-1 IN FRAME fMain /* Salgsdato S1 */
DO:
  ASSIGN INPUT FI-BestDato1 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      ASSIGN FI-BestDato1 = ?.
  display FI-BestDato1 
      with frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-SalgsDato1-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SalgsDato1-2 fFrameWin
ON LEAVE OF FI-SalgsDato1-2 IN FRAME fMain
DO:
    ASSIGN INPUT FI-BestDato2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN FI-BestDato2 = ?.
    display FI-BestDato2 
        with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SalgsDato1-2 fFrameWin
ON TAB OF FI-SalgsDato1-2 IN FRAME fMain
OR "RETURN" OF FI-BestDato2
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


&Scoped-define SELF-NAME FI-SalgsDato2-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SalgsDato2-1 fFrameWin
ON LEAVE OF FI-SalgsDato2-1 IN FRAME fMain /* Salgsdato S2 */
DO:
  ASSIGN INPUT FI-BestDato1 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      ASSIGN FI-BestDato1 = ?.
  display FI-BestDato1 
      with frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-SalgsDato2-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SalgsDato2-2 fFrameWin
ON LEAVE OF FI-SalgsDato2-2 IN FRAME fMain
DO:
    ASSIGN INPUT FI-BestDato2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN FI-BestDato2 = ?.
    display FI-BestDato2 
        with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SalgsDato2-2 fFrameWin
ON TAB OF FI-SalgsDato2-2 IN FRAME fMain
OR "RETURN" OF FI-BestDato2
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


&Scoped-define SELF-NAME RS-Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Type fFrameWin
ON VALUE-CHANGED OF RS-Type IN FRAME fMain
DO:
    DEFINE VARIABLE cValue AS CHARACTER  NO-UNDO.
    cValue = SELF:SCREEN-VALUE.
    B-Rapport:SENSITIVE = cVALUE = "1".
  IF cVALUE = "1" THEN
      APPLY "CHOOSE" TO B-BestDato.
  ELSE IF cVALUE = "2" THEN
      APPLY "CHOOSE" TO B-SalgsDato.
  ASSIGN
      FI-BestDato1:SENSITIVE = cValue = "1"
      FI-BestDato2:SENSITIVE = cValue = "1"
      B-BestDato:SENSITIVE = cValue = "1"
      B-BestStat:SENSITIVE = cValue = "1"
      B-BestStatBlank:SENSITIVE = cValue = "1"
      B-SalgsDato:SENSITIVE = cValue = "2"
      B-SalgsDato-2:SENSITIVE = cValue = "2"
      FI-SalgsDato1-1:SENSITIVE = cValue = "2"
      FI-SalgsDato1-2:SENSITIVE = cValue = "2"
      FI-SalgsDato2-1:SENSITIVE = cValue = "2"
      FI-SalgsDato2-2:SENSITIVE = cValue = "2".
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
       RUN repositionObject IN h_dbesthode ( 3.86 , 6.00 ) NO-ERROR.
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
  DISPLAY Tg-VisButikker FI-Butikker FI-Sesong FI-SalgsDato1-1 FI-SalgsDato1-2 
          CB-ButikkTeam FI-Sesong2 FI-SalgsDato2-1 FI-SalgsDato2-2 FI-Avdeling 
          FI-HuvGr FI-BestDato1 FI-BestDato2 FI-VarGr FI-BestStat FI-LevNr 
          RS-Type FI-Kategori FI-Rstext 
      WITH FRAME fMain.
  ENABLE RECT-67 FI-Butikker B-BestStat FI-SalgsDato1-1 FI-SalgsDato1-2 
         B-SalgsDato B-SesongBlank CB-ButikkTeam B-Aktiver B-SesongBlank2 
         FI-SalgsDato2-1 FI-SalgsDato2-2 B-SalgsDato-2 B-AvdelingBlank 
         B-VisTrans B-Rapport FI-BestDato1 FI-BestDato2 B-BestDato B-HgBlank 
         B-BestStatBlank B-VgBlank B-LevNrBlank RS-Type B-KategoriBlank 
         B-Sesong2 B-Kategori B-Avdeling BUTTON-SokBut B-Sesong B-HuvGr B-VarGr 
         B-LevNr FI-Rstext 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetNylabels fFrameWin 
PROCEDURE GetNylabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cButikListe AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cNyLabels   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cLabel2     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmp AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmpRight AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iWhere AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cBut  AS CHARACTER  NO-UNDO.
    cRightColsNy = cRightCols.
    DO ii = 1 TO NUM-ENTRIES(cButikListe).
        cBut = ENTRY(ii,cButikListe).
        FIND Butiker WHERE Butiker.butik = INT(cBut) NO-LOCK NO-ERROR.
        IF AVAIL Butiker AND Butiker.Kortnavn <> "" THEN
            cBut = butiker.kortnavn.
        cNyLabels = cNyLabels + (IF cNyLabels <> "" THEN "|" ELSE "") + (IF FI-Sesong  = "*" THEN "" ELSE cBut + "-" + FI-Sesong) + "|" + 
                                                                        (IF FI-Sesong2 = "*" THEN "" ELSE cBut + "-" + FI-Sesong2).
    END.
    cNyLabels = cNyLabels + "|" + string(FI-Sesong  = "*","/TOT") + (IF FI-Sesong  = "*" THEN "" ELSE "-" + FI-Sesong) + "|" + 
                                  string(FI-Sesong2 = "*","/TOT") + (IF FI-Sesong2 = "*" THEN "" ELSE "-" + FI-Sesong2).
    cLabels = REPLACE(cLabels,"Verdier",cNyLabels).
    cLabel2 = REPLACE(cLabels,",","|").
    iWhere = LOOKUP("Verdier",cLabelsOrg).
    IF iWhere <> 0 THEN DO:
        DO ii = 1 TO iWhere - 1.
            ENTRY(ii,cLabel2,"|") = " ".
        END.
        DO ii = iWhere TO NUM-ENTRIES(cLabel2,"|") BY 2.
            ENTRY(ii,cLabel2,"|") = "S1".
            ENTRY(ii + 1,cLabel2,"|") = "S2".
        END.
/*         cLabels = cLabels + CHR(1) + " " + cLabel2. */
    END.
/*     cTmp = REPLACE(cLabels,"|",",").             */
/*     cTmpRight = FILL(",",NUM-ENTRIES(cTmp) - 1). */
/*     DO ii = 3 TO NUM-ENTRIES(cTmp):              */
/*         IF ENTRY(ii,cTmp) <> "" THEN             */
/*             ENTRY(ii,cTmpRight) = "1".           */
/*     END.                                         */
/*     cRightCols = cTmpRight.                      */
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
  ASSIGN wBestStatus = cAlle.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.

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
    ii = ii + 1.
    CREATE TT_Beststat.
    ASSIGN TT_Beststat.BestStat = SysPara.ParaNr
           TT_Beststat.Beskr    = SysPara.Parameter1
           TT_Beststat.iRowNr   = ii.

  end. /* SYSPARA */

/*   assign                                                                                   */
/*     CB-BestStat = entry(1,wBestStatus)                                                     */
/*     CB-BestStat:List-Items in frame {&FRAME-NAME} = wBestStatus.                           */
/*                                                                                            */
/*   display CB-BestStat with frame {&FRAME-NAME}.                                            */
/*   ASSIGN CB-BestType:LIST-ITEM-PAIRS = cAlle + ",0,Grunnbestilling,1,Tilleggsbestilling,2" */
/*          CB-BestType:SCREEN-VALUE = "0".                                                   */
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
      APPLY "CHOOSE" TO B-SesongBlank2.
      APPLY "CHOOSE" TO B-KategoriBlank.
      APPLY "CHOOSE" TO B-BestStatBlank.
  END.
  /* Code placed here will execute AFTER standard behavior.    */
/*   PUBLISH "GetWindowH" (OUTPUT h_Window ). */

  ASSIGN cStTypeId = "NONE"
         cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt".
  APPLY "VALUE-CHANGED" TO RS-Type.
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
   DEFINE OUTPUT PARAMETER cDummy AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER cAlign AS CHARACTER  NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes fFrameWin 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrwTarget AS HANDLE NO-UNDO.

DEF VAR httBufferSource AS HANDLE NO-UNDO.
DEF VAR httBufferTarget AS HANDLE NO-UNDO.
    
    IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "BestStat" THEN DO:
        httBufferSource = ihBrwSource:QUERY:GET-BUFFER-HANDLE(1).
        httBufferTarget = ihBrwTarget:QUERY:GET-BUFFER-HANDLE(1).
        FOR EACH TT_BestStat BY TT_BestStat.iRowNr:
            httBufferSource:BUFFER-CREATE().
           ASSIGN httBufferSource:BUFFER-FIELD("rowident1"):BUFFER-VALUE = TT_BestStat.iRowNr
                  httBufferSource:BUFFER-FIELD("Beststat"):BUFFER-VALUE = TT_BestStat.Beststat
                  httBufferSource:BUFFER-FIELD("Beskr"):BUFFER-VALUE = TT_BestStat.Beskr.
           IF TT_BestStat.RowSelected = TRUE THEN DO:
               httBufferTarget:BUFFER-CREATE().
              ASSIGN httBufferTarget:BUFFER-FIELD("rowident1"):BUFFER-VALUE = TT_BestStat.iRowNr
                     httBufferTarget:BUFFER-FIELD("Beststat"):BUFFER-VALUE = TT_BestStat.Beststat
                     httBufferTarget:BUFFER-FIELD("Beskr"):BUFFER-VALUE = TT_BestStat.Beskr.
           END.
        END.
        DYNAMIC-FUNCTION("setCurrentObject",ihBrwSource). 
        RUN OpenQuery.
        DYNAMIC-FUNCTION("setCurrentObject",ihBrwTarget).
        RUN OpenQuery.
    END.
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
  ( INPUT cButikListe AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: vi skall göra en lista antal butiker * 2 + 2 tot 
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntSumfelt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFeltNumListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iFirstSumCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iBy AS INTEGER    NO-UNDO.
  iAntsumfelt = NUM-ENTRIES(cButikListe) * 2 + 2.
  iFirstSumCol = 3.
  iBy = IF FI-Sesong = "*" OR FI-Sesong2 = "*" THEN 2 ELSE 1.
  IF iBy = 1 THEN
      DO iCount = (IF FI-Sesong = "*" THEN 2 ELSE 1) TO iAntSumfelt:
          ASSIGN cFeltNumListe = cFeltNumListe + (IF cFeltNumListe <> "" THEN "," ELSE "") + STRING(iFirstSumCol + iCount - 1).
      END.
  ELSE
      DO iCount = (IF FI-Sesong = "*" THEN 2 ELSE 1) TO iAntSumfelt BY 2:
          ASSIGN cFeltNumListe = cFeltNumListe + (IF cFeltNumListe <> "" THEN "," ELSE "") + STRING(iFirstSumCol + iCount - 1).
      END.
  RETURN cFeltNumListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

