&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Lager NO-UNDO LIKE Lager
       FIELD UtSolgt% AS DECI
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
       FIELD Beskrivelse AS CHARACTER
       FIELD CharButik AS CHARACTER
       FIELD Sasong LIKE ArtBas.Sasong
       FIELD SasBeskr LIKE Sasong.SasBeskr
       FIELD Farg LIKE ArtBas.Farg
       FIELD FarBeskr LIKE Farg.FarBeskr
       FIELD DbKr LIKE StLinje.DbKr
       FIELD Db% LIKE StLinje.Db%
       FIELD LagerVerdi LIKE Lager.VVarekost
       FIELD Hg LIKE HuvGr.Hg
       FIELD VgLopNr AS CHARACTER
       FIELD T_db% AS DECIMAL FORMAT "->>,>>9.99"
       FIELD Pris  AS DECIMAL FORMAT "->>,>>9.99"
       FIELD T_LagerVerdi  AS DECIMAL FORMAT "->>>,>>>,>>9.99"
       FIELD MatKod LIKE Material.MatKod
       FIELD MatBeskr LIKE Material.MatBeskr
       FIELD VMId LIKE Varemerke.VMId
       FIELD VMBeskr LIKE Varemerke.Beskrivelse
       FIELD Butnamn LIKE Butiker.Butnamn
       FIELD HarNeg AS CHARACTER
       FIELD LevKod AS CHARACTER
       FIELD Solgt% AS DECI
       FIELD DBandel% AS DECI
       FIELD Rabandel% AS DECI
       FIELD Kjopandel% AS DECI
       .


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

DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtRightCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStTypeId  AS CHARACTER  NO-UNDO.
def var wTittel    as char no-undo.
DEFINE VARIABLE h_Window      AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_dstlager    AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE     NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hUtskriftProg AS HANDLE     NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dSolgtTot      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dDBTot         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dRabTot        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKjopTot       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefsBut AS CHARACTER  NO-UNDO.


DEFINE TEMP-TABLE TT_LagerTMP NO-UNDO LIKE TT_Lager.
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

DEFINE TEMP-TABLE TT_BigListItem NO-UNDO
    FIELD Butiker AS CHARACTER.

DEF VAR hTTArt  AS HANDLE NO-UNDO.
/* DEFINE TEMP-TABLE TT_Art NO-UNDO                   */
/*     FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr        */
/*     INDEX ArtikkelNr IS PRIMARY UNIQUE ArtikkelNr. */

/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter =
    "Antall,ordresum".

ASSIGN cFieldDefs = 
  /*  1 */ "Butikknr;Butikknr;;1," +
  /*  2 */ "Butnamn;Navn;;,"       +
  /*  3 */ "obj1;obj1;;,"            +
  /*  4 */ "obj1Namn;Beskr;;1,"    +
  /*  5 */ "Antall;Antall;;1,"    +
  /*  6 */ "ordresum;Tot;2;1".
ASSIGN cFieldDefsBut = 
  /*  3 */ "obj1;obj1;;,"            +
  /*  4 */ "obj1Namn;Beskr;;1,"    +
  /*  5 */ "Antall;Antall;;1,"    +
  /*  6 */ "ordresum;Tot;2;1".

DEFINE TEMP-TABLE TT_Rapport NO-UNDO
    FIELD butikknr   AS INTE
    FIELD butnamn    AS CHAR
    FIELD obj1       AS DECI
    FIELD obj1Namn   AS CHAR
    FIELD antall     AS DECI
    FIELD ordresum   AS DECI
    INDEX butobj IS PRIMARY UNIQUE butikknr obj1.
    .

DEFINE TEMP-TABLE tt_beskr NO-UNDO
    FIELD obj AS DECI
    FIELD beskr AS CHAR
    INDEX obj IS PRIMARY UNIQUE obj.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Avdeling B-HuvGr B-VarGr CB-Varebok ~
RS-Aggregering B-AvdelingBlank FI-LevNr B-Aktiver B-LevNrBlank B-VMId ~
CB-ButikkTeam B-HgBlank FI-VMId B-VMIdBlank B-VgBlank B-LevNr BUTTON-SokBut 
&Scoped-Define DISPLAYED-OBJECTS CB-Varebok RS-Aggregering FI-Butikker ~
FI-Avdeling FI-LevNr CB-ButikkTeam FI-HuvGr FI-VMId FI-VarGr Tg-VisButikker ~
TG-AvFilter FI-VisPer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBeskr fFrameWin 
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLabel fFrameWin 
FUNCTION getLabel RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

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

DEFINE VARIABLE CB-Varebok AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Varebok" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Velg varebok",      0
     DROP-DOWN-LIST
     SIZE 117 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VisPer AS CHARACTER FORMAT "X(256)":U INITIAL "Vis per:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE FI-VMId AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Aggregering AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Avdeling", "AVD",
"Hovedgruppe", "HUVGR",
"Varegruppe", "VARGR",
"Leverandør", "LEV",
"Butikk", "BUT",
"Artikkel", "ART",
"Varemerke", "VMID"
     SIZE 19.6 BY 7 NO-UNDO.

DEFINE VARIABLE TG-AvFilter AS LOGICAL INITIAL no 
     LABEL "Avansert filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.4 BY .81 NO-UNDO.

DEFINE VARIABLE Tg-VisButikker AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Avdeling AT ROW 2.81 COL 92.8 NO-TAB-STOP 
     B-HuvGr AT ROW 3.81 COL 92.8 NO-TAB-STOP 
     B-VarGr AT ROW 4.81 COL 92.8 NO-TAB-STOP 
     CB-Varebok AT ROW 1.19 COL 14 COLON-ALIGNED
     RS-Aggregering AT ROW 1.38 COL 152.4 NO-LABEL
     FI-Butikker AT ROW 2.81 COL 14 COLON-ALIGNED
     FI-Avdeling AT ROW 2.81 COL 75.8 COLON-ALIGNED
     B-AvdelingBlank AT ROW 2.81 COL 97.8
     FI-LevNr AT ROW 2.81 COL 116.6 COLON-ALIGNED
     B-Aktiver AT ROW 2.86 COL 42
     B-LevNrBlank AT ROW 2.86 COL 138.6
     B-VMId AT ROW 3.81 COL 133.6 NO-TAB-STOP 
     CB-ButikkTeam AT ROW 3.81 COL 14 COLON-ALIGNED
     FI-HuvGr AT ROW 3.81 COL 75.8 COLON-ALIGNED
     B-HgBlank AT ROW 3.81 COL 97.8
     FI-VMId AT ROW 3.81 COL 116.6 COLON-ALIGNED
     B-VMIdBlank AT ROW 3.81 COL 138.6
     B-Artikkelkort AT ROW 4.19 COL 42
     FI-VarGr AT ROW 4.81 COL 75.8 COLON-ALIGNED
     B-VgBlank AT ROW 4.81 COL 97.8
     Tg-VisButikker AT ROW 5.48 COL 42
     TG-AvFilter AT ROW 6.48 COL 42
     B-LevNr AT ROW 2.86 COL 133.6 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 2.81 COL 31.6 NO-TAB-STOP 
     FI-VisPer AT ROW 1.48 COL 139.6 COLON-ALIGNED NO-LABEL
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
   Temp-Tables and Buffers:
      TABLE: TT_Lager T "?" NO-UNDO SkoTex Lager
      ADDITIONAL-FIELDS:
          FIELD UtSolgt% AS DECI
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
          FIELD Beskrivelse AS CHARACTER
          FIELD CharButik AS CHARACTER
          FIELD Sasong LIKE ArtBas.Sasong
          FIELD SasBeskr LIKE Sasong.SasBeskr
          FIELD Farg LIKE ArtBas.Farg
          FIELD FarBeskr LIKE Farg.FarBeskr
          FIELD DbKr LIKE StLinje.DbKr
          FIELD Db% LIKE StLinje.Db%
          FIELD LagerVerdi LIKE Lager.VVarekost
          FIELD Hg LIKE HuvGr.Hg
          FIELD VgLopNr AS CHARACTER
          FIELD T_db% AS DECIMAL FORMAT "->>,>>9.99"
          FIELD Pris  AS DECIMAL FORMAT "->>,>>9.99"
          FIELD T_LagerVerdi  AS DECIMAL FORMAT "->>>,>>>,>>9.99"
          FIELD MatKod LIKE Material.MatKod
          FIELD MatBeskr LIKE Material.MatBeskr
          FIELD VMId LIKE Varemerke.VMId
          FIELD VMBeskr LIKE Varemerke.Beskrivelse
          FIELD Butnamn LIKE Butiker.Butnamn
          FIELD HarNeg AS CHARACTER
          FIELD LevKod AS CHARACTER
          FIELD Solgt% AS DECI
          FIELD DBandel% AS DECI
          FIELD Rabandel% AS DECI
          FIELD Kjopandel% AS DECI
          
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
         HEIGHT             = 7.57
         WIDTH              = 187.2.
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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Artikkelkort IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Artikkelkort:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VisPer IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-AvFilter IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       TG-AvFilter:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Tg-VisButikker IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       Tg-VisButikker:HIDDEN IN FRAME fMain           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "NO-LOCK INDEXED-REPOSITION KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
  DEFINE VARIABLE cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hQry   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE TTH         AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh          AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qhParam     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cXParam     AS CHARACTER  NO-UNDO.

  IF INPUT CB-Varebok = 0 THEN DO:
      MESSAGE "Velg varebok"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  PUBLISH "ClearGrid" (cLabels).
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT DYNAMIC-FUNCTION('getKriterier':U,
         OUTPUT cKriterier /* CHARACTER */) THEN
          RETURN.
        ASSIGN cButiker   = (ENTRY(1,cKriterier,CHR(1))).
      ASSIGN B-Artikkelkort:SENSITIVE = RS-Aggregering:SCREEN-VALUE = "ART".
      IF RS-Aggregering:SCREEN-VALUE = "ART" AND TG-AvFilter:CHECKED THEN DO:
          RUN Avancerat.
          RETURN.
      END.
      CASE RS-Aggregering:SCREEN-VALUE:
          WHEN "AVD" THEN DO:
              IF FI-Avdeling <> "*" THEN
                  ASSIGN pcFeltListe = "AvdelingNr"
                         pcVerdier   = FI-Avdeling.
          END.
          WHEN "HUVGR" THEN DO:
              ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                                   IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE ""
                     pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                                   IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE "".
          END.
          WHEN "VARGR" THEN DO:
              ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                                   IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE 
                                   IF FI-VarGr    <> "*" THEN "VarGr"      ELSE ""
                     pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                                   IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE 
                                   IF FI-VarGr    <> "*" THEN FI-VarGr     ELSE "".
          END.
          WHEN "LEV" THEN DO:
              IF FI-LevNr <> "*" THEN
                  ASSIGN pcFeltListe = "LevNr"
                         pcVerdier   = FI-LevNr.
          END.
          WHEN "ART" THEN DO:
              ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                                   IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE 
                                   IF FI-VarGr    <> "*" THEN "VarGr"      ELSE ""
                     pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                                   IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE 
                                   IF FI-VarGr    <> "*" THEN FI-VarGr     ELSE "".
              DO iCount = 1 TO NUM-ENTRIES("LevNr,VMId"):
                  CASE ENTRY(iCount,"LevNr,VMId,Farg"):
                      WHEN "LevNr" THEN
                          IF FI-LevNr <> "*" THEN
                              ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "LevNr" 
                                     pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-LevNr.
                      WHEN "VMId" THEN
                          IF FI-VMId <> "*" THEN
                              ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "VMId" 
                                     pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-VMId.
                  END CASE.
              END.
          END.
      END CASE.
      PUBLISH "VisTxtBox" ("Søker data......").
      IF Tg-VisButikker:CHECKED AND NOT RS-Aggregering:SCREEN-VALUE = "ART" THEN DO:
        ASSIGN cTmpFieldDefs       = cFieldDefs
               ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",Butnamn;Navn;;".
        RUN FixStrings.
        ASSIGN cFieldDefs = cTmpFieldDefs.
      END.
      ELSE IF RS-Aggregering:SCREEN-VALUE <> "ART" THEN DO:
          ASSIGN cTmpFieldDefs = cFieldDefs
                 cFieldDefs    = REPLACE(cFieldDefs,"VVarekost;VVarekost;2;1,","").
          RUN FixStrings.
          ASSIGN cFieldDefs = cTmpFieldDefs.
      END.
      ELSE
          RUN FixStrings.
      IF RS-Aggregering:SCREEN-VALUE = "ART" THEN DO:
          /* StartSokArtDyn innehåller allt för att även klara av allt utifrån  */
          RUN StartSokArtDyn (qhParam,TRUE,ENTRY(1,cKriterier,CHR(1))). /* Butiker */
          RETURN NO-APPLY.
      END.
      ELSE DO:
          RUN StartSok.
/*           RUN MesseToTT IN h_dstlager */
          RUN MesseToTT.
          IF CAN-FIND(FIRST TT_Rapport) THEN DO:
              CREATE QUERY qh.
              qh:SET-BUFFERS(BUFFER TT_Rapport:HANDLE).
              qh:QUERY-PREPARE("FOR EACH TT_Rapport").
              qh:QUERY-OPEN().
              PUBLISH "VisTxtBox" ("Leser ut data......").
              RUN rappgenqry.p ("","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
          END.
      END.
      IF CAN-FIND(FIRST TT_Rapport) THEN DO:
          RUN LesInnIGrid.
          qh:QUERY-CLOSE().
          DELETE OBJECT qh NO-ERROR.
          ASSIGN qh  = ?.
      END.
      PUBLISH "VisTxtBox" ("").
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort fFrameWin
ON CHOOSE OF B-Artikkelkort IN FRAME fMain /* Artikkelkort */
DO:
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
/*     "where can-do('" + cTillgButikker + "',string(butik))" , */
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
/*     ASSIGN SELF:TOOLTIP = IF SELF:SCREEN-VALUE = "INGEN" THEN "" ELSE REPLACE(SELF:SCREEN-VALUE,CHR(1),","). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Varebok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Varebok fFrameWin
ON VALUE-CHANGED OF CB-Varebok IN FRAME fMain /* Varebok */
DO:
  FIND Varebehhode WHERE varebehhode.varebehnr = INPUT CB-Varebok NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Aggregering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Aggregering fFrameWin
ON VALUE-CHANGED OF RS-Aggregering IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
/*         ASSIGN B-VMId:SENSITIVE = SELF:SCREEN-VALUE = "ART"                   */
/*                B-VMIdBlank:SENSITIVE = SELF:SCREEN-VALUE = "ART"              */
/*                TG-AvFilter:SENSITIVE     = SELF:SCREEN-VALUE = "ART"          */
/* /*                Tg-VisButikker:SENSITIVE = NOT SELF:SCREEN-VALUE = "ART" */ */
/*             .                                                                 */
/*                                                                               */
/*         CASE SELF:SCREEN-VALUE:                                               */
/*             WHEN "AVD" THEN DO:                                               */
/*                 APPLY "CHOOSE" TO B-HgBlank.                                  */
/*                 APPLY "CHOOSE" TO B-VgBlank.                                  */
/* /*                 APPLY "CHOOSE" TO B-LagerAntBlank. */                      */
/*                 ASSIGN B-Avdeling:SENSITIVE = TRUE                            */
/*                        B-AvdelingBlank:SENSITIVE = TRUE                       */
/*                        B-HuvGr:SENSITIVE = FALSE                              */
/*                        B-HgBlank:SENSITIVE = FALSE                            */
/*                        B-VarGr:SENSITIVE = FALSE                              */
/*                        B-VgBlank:SENSITIVE = FALSE.                           */
/*             END.                                                              */
/*             WHEN "HUVGR" THEN DO:                                             */
/*                 APPLY "CHOOSE" TO B-VgBlank.                                  */
/* /*                 APPLY "CHOOSE" TO B-LagerAntBlank. */                      */
/*                 ASSIGN B-Avdeling:SENSITIVE = FALSE                           */
/*                        B-AvdelingBlank:SENSITIVE = FALSE                      */
/*                        B-HuvGr:SENSITIVE = TRUE                               */
/*                        B-HgBlank:SENSITIVE = TRUE                             */
/*                        B-VarGr:SENSITIVE = FALSE                              */
/*                        B-VgBlank:SENSITIVE = FALSE.                           */
/*             END.                                                              */
/*             WHEN "VARGR" THEN DO:                                             */
/* /*                 APPLY "CHOOSE" TO B-LagerAntBlank. */                      */
/*                 ASSIGN B-Avdeling:SENSITIVE = TRUE                            */
/*                        B-AvdelingBlank:SENSITIVE = TRUE                       */
/*                        B-HuvGr:SENSITIVE = TRUE                               */
/*                        B-HgBlank:SENSITIVE = TRUE                             */
/*                        B-VarGr:SENSITIVE = TRUE                               */
/*                        B-VgBlank:SENSITIVE = TRUE.                            */
/*             END.                                                              */
/*             WHEN "LEV" THEN DO:                                               */
/* /*                 APPLY "CHOOSE" TO B-LagerAntBlank. */                      */
/*                 ASSIGN B-Avdeling:SENSITIVE = TRUE                            */
/*                        B-AvdelingBlank:SENSITIVE = TRUE                       */
/*                        B-HuvGr:SENSITIVE = TRUE                               */
/*                        B-HgBlank:SENSITIVE = TRUE                             */
/*                        B-VarGr:SENSITIVE = TRUE                               */
/*                        B-VgBlank:SENSITIVE = TRUE.                            */
/*             END.                                                              */
/*             WHEN "BUT" THEN DO:                                               */
/* /*                 APPLY "CHOOSE" TO B-LagerAntBlank. */                      */
/*                 ASSIGN B-Avdeling:SENSITIVE = TRUE                            */
/*                        B-AvdelingBlank:SENSITIVE = TRUE                       */
/*                        B-HuvGr:SENSITIVE = TRUE                               */
/*                        B-HgBlank:SENSITIVE = TRUE                             */
/*                        B-VarGr:SENSITIVE = TRUE                               */
/*                        B-VgBlank:SENSITIVE = TRUE.                            */
/*             END.                                                              */
/*             WHEN "ART" THEN DO:                                               */
/* /*                 APPLY "CHOOSE" TO B-AvdelingBlank. */                      */
/*                 ASSIGN B-Avdeling:SENSITIVE = TRUE                            */
/*                        B-AvdelingBlank:SENSITIVE = TRUE                       */
/*                        B-HuvGr:SENSITIVE = TRUE                               */
/*                        B-HgBlank:SENSITIVE = TRUE                             */
/*                        B-VarGr:SENSITIVE = TRUE                               */
/*                        B-VgBlank:SENSITIVE = TRUE.                            */
/* /*                        Tg-VisButikker:CHECKED = FALSE. */                  */
/*             END.                                                              */
/*             WHEN "VMID" THEN DO:                                              */
/*             END.                                                              */
/*         END CASE.                                                             */
        ASSIGN B-Artikkelkort:SENSITIVE = FALSE.
        IF NOT TG-AvFilter:SENSITIVE THEN
            ASSIGN TG-AvFilter:CHECKED = FALSE.
    END.
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
  
  ASSIGN cGetVerdier = getSumFelter("DataObjekt").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject fFrameWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/*   DELETE OBJECT hTTArt. */
/*   ASSIGN hTTArt = ?.    */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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
  DISPLAY CB-Varebok RS-Aggregering FI-Butikker FI-Avdeling FI-LevNr 
          CB-ButikkTeam FI-HuvGr FI-VMId FI-VarGr Tg-VisButikker TG-AvFilter 
          FI-VisPer 
      WITH FRAME fMain.
  ENABLE B-Avdeling B-HuvGr B-VarGr CB-Varebok RS-Aggregering B-AvdelingBlank 
         FI-LevNr B-Aktiver B-LevNrBlank B-VMId CB-ButikkTeam B-HgBlank FI-VMId 
         B-VMIdBlank B-VgBlank B-LevNr BUTTON-SokBut 
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
                 CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN 
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
DEFINE VARIABLE iLookup AS INTEGER    NO-UNDO.
DEFINE VARIABLE cRSvalue AS CHARACTER  NO-UNDO.
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
DO WITH FRAME {&FRAME-NAME}:
    IF RS-Aggregering:SCREEN-VALUE <> "BUT" THEN DO:
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
        cRSvalue = REPLACE(RS-Aggregering:RADIO-BUTTONS IN FRAME {&FRAME-NAME}," ","").
        iLookup = LOOKUP(RS-Aggregering:SCREEN-VALUE IN FRAME {&FRAME-NAME},cRSvalue).
        ASSIGN ENTRY(3,cLabels) = ENTRY(iLookup - 1,cRSvalue).
    END.
    ELSE DO:
        ASSIGN cFelter = FILL(",",NUM-ENTRIES(cFieldDefsBut) - 1)
               cLabels = cFelter
               cDecimaler = cFelter
               cRightCols = cFelter.
        DO iCount = 1 TO NUM-ENTRIES(cFieldDefsBut):
            ASSIGN ENTRY(iCount,cFelter) = ENTRY(1,ENTRY(iCount,cFieldDefsBut),";")
                   ENTRY(iCount,cLabels) = ENTRY(2,ENTRY(iCount,cFieldDefsBut),";")
                   ENTRY(iCount,cDecimaler) = ENTRY(3,ENTRY(iCount,cFieldDefsBut),";")
                   ENTRY(iCount,cRightCols) = ENTRY(4,ENTRY(iCount,cFieldDefsBut),";").
        END.
        cRSvalue = REPLACE(RS-Aggregering:RADIO-BUTTONS IN FRAME {&FRAME-NAME}," ","").
        iLookup = LOOKUP(RS-Aggregering:SCREEN-VALUE IN FRAME {&FRAME-NAME},cRSvalue).
        ASSIGN ENTRY(3,cLabels) = ENTRY(iLookup - 1,cRSvalue).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlle fFrameWin 
PROCEDURE GetAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cListe AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
  IF cType = "VARGR" THEN DO:
      FOR EACH VarGr NO-LOCK:
          ASSIGN cListe = cListe + (IF cListe = "" THEN "" ELSE ",") + STRING(VarGr.Vg).
      END.
  END.
  ELSE IF cType = "LEV" THEN DO:
      FOR EACH LevBas NO-LOCK:
          ASSIGN cListe = cListe + (IF cListe = "" THEN "" ELSE ",") + STRING(LevBas.LevNr).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVareGrupper fFrameWin 
PROCEDURE GetVareGrupper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cVareGrupper AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cVerdier     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cType        AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    
  IF cType = "AVDELING" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cVerdier):
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(ENTRY(iCount,cVerdier)) NO-LOCK NO-ERROR.
          FOR EACH HuvGr OF Avdeling NO-LOCK:
              FOR EACH VarGr OF HuvGr NO-LOCK.
                  ASSIGN cVareGrupper = cVareGrupper + (IF cVareGrupper = "" THEN "" ELSE ",") + 
                      STRING(VarGr.Vg).
              END.
          END.
      END.
  END.
  ELSE IF cType = "HOVEDGR" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cVerdier):
          FIND HuvGr WHERE HuvGr.Hg = INT(ENTRY(iCount,cVerdier)) NO-LOCK NO-ERROR.
          IF AVAIL HuvGr THEN DO:
              FOR EACH VarGr OF HuvGr NO-LOCK.
                  ASSIGN cVareGrupper = cVareGrupper + (IF cVareGrupper = "" THEN "" ELSE ",") + 
                      STRING(VarGr.Vg).
              END.
          END.
      END.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB fFrameWin 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStTypeId         AS CHARACTER INIT "AVDELING" NO-UNDO.
    DEFINE VARIABLE cMesseList AS CHARACTER  NO-UNDO.
/*     DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cButString        AS CHARACTER  NO-UNDO.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    ASSIGN cUserDefaultBut = STRING(bruker.butikknr)
           cListItemPairs = "".
  DO WITH FRAME {&FRAME-NAME}:
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
                             ButikkTeam.Beskrivelse + "," + cButString.
    END.
    FOR EACH TT_TillgButikker:
        ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(TT_TillgButikker.Butik).
    END.
    ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
           CB-ButikkTeam:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
    /* Messe */
    FOR EACH messe NO-LOCK WHERE Messe.MesseType = 1 BY messe.fradato DESCENDING.
        FOR EACH varebehhode OF messe NO-LOCK.
            ASSIGN cMesseList = cMesseList + (IF cMesseList <> "" THEN "," ELSE "") + 
                           REPLACE(VareBehHode.VareBehBeskrivelse,","," ") + "-" + STRING(VareBehHode.VareBehNr) + 
                               "(" + STRING(messe.messenr) + ")" + "," + STRING(VareBehHode.VareBehNr).
            ASSIGN CB-Varebok:LIST-ITEM-PAIRS = CB-Varebok:LIST-ITEM-PAIRS + "," + cMesseList
                   CB-Varebok:SCREEN-VALUE = ENTRY(2,CB-Varebok:LIST-ITEM-PAIRS).
        END.
    END.
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
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN InitCB.
  RUN FixStrings.
/*   RUN SkapaTTArt. */
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN DO:
      ASSIGN h_frapportgrid = DYNAMIC-FUNCTION('geth_frapportgrid':U IN h_Window).
  END.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridmesse.txt".
/*              FI-Avdeling  = "*"                */
/*              FI-Avdeling:SCREEN-VALUE = cAlle. */
      APPLY "CHOOSE" TO B-AvdelingBlank.
      APPLY "CHOOSE" TO B-HgBlank.
      APPLY "CHOOSE" TO B-VgBlank.
      APPLY "CHOOSE" TO B-LevNrBlank.
      APPLY "CHOOSE" TO B-VMIdBlank.

      APPLY "VALUE-CHANGED" TO RS-Aggregering.
      IF INT(cUserDefaultBut) > 0 AND CAN-DO(CB-ButikkTeam:LIST-ITEM-PAIRS,cUserDefaultBut) THEN
          ASSIGN CB-ButikkTeam:SCREEN-VALUE = cUserDefaultBut NO-ERROR.
      APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inndeling fFrameWin 
PROCEDURE Inndeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER iAntFord AS INTEGER    NO-UNDO.

  ASSIGN iAntFord      = 1.
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinje.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
     ,FIRST LevSort OF ArtSort NO-LOCK:

    ASSIGN iAntFord   = 0.
    FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
      ASSIGN iAntFord      = iAntFord + LevSAnt.SoAnt.
    END.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkuleraTTLager fFrameWin 
PROCEDURE KalkuleraTTLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iLnNr AS INTEGER    NO-UNDO.
/* OUTPUT TO VALUE(cFileName).                                                                                                                          */
/* PUT UNFORMATTED "|" REPLACE(REPLACE(cLabels + "," + cArtLabels,",","|"),"DataobjektTXT",getLabel(RS-Aggregering:SCREEN-VALUE IN FRAME {&FRAME-NAME})). */
/*                                                                                                                                                      */
/* ASSIGN iLnNr = 0.                                                                                                                                    */
  FOR EACH TT_Lager:
      ASSIGN TT_Lager.UtSolgt% = IF TT_Lager.KjopAnt > 0 THEN ROUND(TT_Lager.AntSolgt / TT_Lager.KjopAnt * 100,1)
                                    ELSE 0
             TT_Lager.Db%      = IF TT_Lager.VerdiSolgt <> 0 THEN ROUND(TT_Lager.DbKr / TT_Lager.VerdiSolgt * 100,1)
                                    ELSE 0
             TT_Lager.Solgt%    = IF dSolgtTot <> 0 THEN ROUND(TT_Lager.VerdiSolgt / dSolgtTot * 100,1)
                                     ELSE 0
             TT_Lager.DBandel%  = IF dDBTot <> 0 THEN ROUND(TT_Lager.DbKr / dDBTot * 100,1)
                                    ELSE 0
             TT_Lager.Rabandel% = IF dRabTot <> 0 THEN ROUND(TT_Lager.VerdiRabatt / dRabTot * 100,1)
                                    ELSE 0
             TT_Lager.Kjopandel% = IF dKjopTot <> 0 THEN ROUND(TT_Lager.KjopVerdi / dKjopTot * 100,1)
                                    ELSE 0.
/*                TT_Lager.VVarekost = IF TT_Lager.LagAnt <> 0 THEN ROUND(TT_Lager.LagerVerdi / TT_Lager.LagAnt,2) ELSE 0. */
      FIND ArtPris WHERE ArtPris.ArtikkelNr = TT_Lager.ArtikkelNr AND
                         ArtPris.Profil     = 1 NO-LOCK NO-ERROR.
      IF AVAIL ArtPris THEN DO:
          ASSIGN TT_Lager.T_db% = ArtPris.db%[IF NOT ArtPris.Tilbud THEN 1 ELSE 2]
                 TT_Lager.Pris  = ArtPris.Pris[IF NOT ArtPris.Tilbud THEN 1 ELSE 2].
                 TT_Lager.T_Lagerverdi = TT_Lager.LagAnt * TT_Lager.Pris.
      END.
/*       iLnNr = iLnNr + 1.                                                                 */
/*       PUT CONTROL CHR(10).                                                               */
/*       PUT UNFORMATTED iLnNr                                 "|"                          */
/*              /*  1 */ TT_Lager.ArtikkelNr                   "|"                          */
/*              /*  2 */ REPLACE(TT_Lager.Beskrivelse,"|","ö") "|"                          */
/*              /*  3 */ TT_Lager.CharButik                    "|"                          */
/*              /*  4 */ ROUND(TT_Lager.AntSolgt,0)       "|"                               */
/*              /*  5 */ ROUND(TT_Lager.VerdiSolgt,0)     "|"                               */
/*              /*  6 1 */ TRIM(STRING(ROUND(TT_Lager.UtSolgt%,1),"->>>>>>>>>>>9.9"))  "|"  */
/*              /*  7 */ ROUND(TT_Lager.DbKr,0)       "|"                                   */
/*              /*  8 1 */ TRIM(STRING(ROUND(TT_Lager.Db%,1),"->>>>>>>>>>>9.9"))       "|"  */
/*              /*  9 */ ROUND(TT_Lager.AntRab,0)      "|"                                  */
/*              /* 10 */ ROUND(TT_Lager.VerdiRabatt,0) "|"                                  */
/*              /* 11 */ ROUND(TT_Lager.LagAnt,0)      "|"                                  */
/*              /* 12 2 */ TRIM(STRING(ROUND(TT_Lager.LagerVerdi,2),"->>>>>>>>>>9.99")) "|" */
/*              /* 13 2 */ TRIM(STRING(ROUND(TT_Lager.VVarekost,2),"->>>>>>>>>>9.99"))  "|" */
/*              /* 14 */ ROUND(TT_Lager.ReklAnt,0)       "|"                                */
/*              /* 15 */ ROUND(TT_Lager.ReklVerdi,0)     "|"                                */
/*              /* 16 */ ROUND(TT_Lager.ReklLAnt,0)      "|"                                */
/*              /* 17 */ ROUND(TT_Lager.ReklLVerdi,0)    "|"                                */
/*              /* 18 */ ROUND(TT_Lager.RetLAnt,0)       "|"                                */
/*              /* 19 */ ROUND(TT_Lager.SvinnAnt,0)      "|"                                */
/*              /* 20 */ ROUND(TT_Lager.SvinnVerdi,0)    "|"                                */
/*              /* 21 */ ROUND(TT_Lager.GjenkjopAnt,0)   "|"                                */
/*              /* 22 */ ROUND(TT_Lager.GjenkjopVerdi,0) "|"                                */
/*              /* 23 */ ROUND(TT_Lager.KjopAnt,0)       "|"                                */
/*              /* 24 */ ROUND(TT_Lager.KjopVerdi,0)     "|"                                */
/*              /* 25 */ ROUND(TT_Lager.BrekkAnt,0)      "|"                                */
/*              /* 26 */ ROUND(TT_Lager.BrekkVerdi,0)    "|"                                */
/*              /* 27 */ ROUND(TT_Lager.IntAnt,0)        "|"                                */
/*              /* 28 */ ROUND(TT_Lager.IntVerdi,0)      "|"                                */
/*              /* 29 */ ROUND(TT_Lager.JustAnt,0)       "|"                                */
/*              /* 30 */ ROUND(TT_Lager.JustVerdi,0)     "|"                                */
/*              /* 31 */ ROUND(TT_Lager.OvAnt,0)         "|"                                */
/*              /* 32 */ ROUND(TT_Lager.OvVerdi,0)       "|"                                */
/*              /* 33 1 */ TRIM(STRING(ROUND(TT_Lager.T_db%,1),"->>>>>>>>>>>9.9")) "|"      */
/*              /* 34 2 */ TRIM(STRING(ROUND(TT_Lager.Pris,2),"->>>>>>>>>>9.99"))  "|"      */
/*              /* 35 */ ROUND(TT_Lager.T_Lagerverdi,0)   "|"                               */
/*              /* 36 */ TT_Lager.AvdelingNr              "|"                               */
/*              /* 37 */ TT_Lager.AvdelingNavn            "|"                               */
/*              /* 38 */ TT_Lager.Hg                      "|"                               */
/*              /* 39 */ TT_Lager.HgBeskr                 "|"                               */
/*              /* 40 */ TT_Lager.Vg                      "|"                               */
/*              /* 41 */ TT_Lager.VgBeskr                 "|"                               */
/*              /* 42 */ TT_Lager.VgLopNr                 "|"                               */
/*              /* 43 */ TT_Lager.LevNr                   "|"                               */
/*              /* 44 */ TT_Lager.LevNamn                 "|"                               */
/*              /* 45 */ TT_Lager.SasBeskr                "|"                               */
/*              /* 46 */ TT_Lager.FarBeskr.                                                 */
  END.
/*   OUTPUT CLOSE. */
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
  DO WITH FRAME {&FRAME-NAME}:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      DO:
          PUBLISH "LoadGrid" (cFileName,INT(getSumFelter("CharButik")) + 1).
          ASSIGN cSumCols = getSumFelter(cSummerFelter)
              /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
/*                  cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";" + */
/*                              "2," + getSumFelter("Utsolgt%") + "," + getSumFelter("AntSolgt") + "," + getSumFelter("KjopAnt")   */
                 cSumString = getSumFelter("obj1Namn") + ",SUM" .
/*           IF RS-Aggregering:SCREEN-VALUE = "ART" THEN */
/*               ASSIGN cSumCols = cSumCols + ",35".     */
          PUBLISH "Summer" (cSumCols,cSumString).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MesseToTT fFrameWin 
PROCEDURE MesseToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE        ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cAggType AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE      dObj AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cObjbeskr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE   dAntall AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE      dSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE  iAntFord AS INTEGER    NO-UNDO.
    ASSIGN cAggtype = RS-Aggregering:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    EMPTY TEMP-TABLE tt_rapport.
    EMPTY TEMP-TABLE tt_beskr.
    FOR EACH varebehlinje WHERE varebehlinje.varebehnr = varebehhode.varebehnr NO-LOCK.
        IF NOT CAN-DO(FI-Avdeling,STRING(varebehlinje.avdelingnr)) THEN
            NEXT.
        IF NOT CAN-DO(FI-HuvGr,STRING(varebehlinje.hg)) THEN
            NEXT.
        IF NOT CAN-DO(FI-Vargr,STRING(varebehlinje.vg)) THEN
            NEXT.
        IF NOT CAN-DO(FI-levnr,STRING(varebehlinje.levnr)) THEN
            NEXT.
        IF FI-vmid <> "*" THEN DO:
            FIND artbas OF varebehlinje NO-LOCK NO-ERROR.
            IF NOT AVAIL artbas OR NOT CAN-DO(FI-vmid,STRING(artbas.vmid)) THEN
                NEXT.
            IF cAggtype = "VMID" THEN DO:
                FIND tt_beskr WHERE tt_beskr.obj = artbas.vmid NO-ERROR.
                IF NOT AVAIL tt_beskr THEN DO:
                    FIND varemerke OF Artbas NO-LOCK NO-ERROR.
                    CREATE tt_beskr.
                    ASSIGN tt_beskr.obj   = artbas.vmid
                           tt_beskr.beskr = IF AVAIL varemerke THEN varemerke.beskrivelse ELSE "*Ukjent*".
                END.
            END.
        END.
        DO ii = 1 TO NUM-ENTRIES(cButiker):
            FIND butiker WHERE butiker.butik = INT(ENTRY(ii,cButiker)) NO-LOCK NO-ERROR.
            FOR EACH VareBehLinjeTrans WHERE VareBehLinjeTrans.ButikkNr    = INT(ENTRY(ii,cButiker)) AND
                                             VareBehLinjeTrans.VareBehNr   = varebehhode.varebehnr   AND
                                             VareBehLinjeTrans.ArtikkelNr  = varebehlinje.artikkelnr NO-LOCK.
                CASE cAggType:
                    WHEN "AVD" THEN DO:
                        FIND tt_beskr WHERE tt_beskr.obj = varebehlinje.avdelingnr NO-ERROR.
                        IF NOT AVAIL tt_beskr THEN DO:
                            FIND avdeling OF varebehlinje NO-LOCK NO-ERROR.
                            CREATE tt_beskr.
                            ASSIGN tt_beskr.obj   = varebehlinje.avdelingnr
                                   tt_beskr.beskr = IF AVAIL avdeling THEN Avdeling.AvdelingNavn ELSE "*Ukjent*".
                        END.
                        ASSIGN dObj      = tt_beskr.obj
                               cObjbeskr = tt_beskr.beskr.
                    END.
                    WHEN "HUVGR"  THEN DO:
                        FIND tt_beskr WHERE tt_beskr.obj = varebehlinje.hg NO-ERROR.
                        IF NOT AVAIL tt_beskr THEN DO:
                            FIND huvgr OF varebehlinje NO-LOCK NO-ERROR.
                            CREATE tt_beskr.
                            ASSIGN tt_beskr.obj   = varebehlinje.hg
                                   tt_beskr.beskr = IF AVAIL huvgr THEN huvgr.hgbeskr ELSE "*Ukjent*".
                        END.
                        ASSIGN dObj      = tt_beskr.obj
                               cObjbeskr = tt_beskr.beskr.
                    END.
                    WHEN "VARGR"   THEN DO:
                        FIND tt_beskr WHERE tt_beskr.obj = varebehlinje.vg NO-ERROR.
                        IF NOT AVAIL tt_beskr THEN DO:
                            FIND vargr OF varebehlinje NO-LOCK NO-ERROR.
                            CREATE tt_beskr.
                            ASSIGN tt_beskr.obj   = varebehlinje.vg
                                   tt_beskr.beskr = IF AVAIL vargr THEN vargr.vgbeskr ELSE "*Ukjent*".
                        END.
                        ASSIGN dObj      = tt_beskr.obj
                               cObjbeskr = tt_beskr.beskr.
                    END.
                    WHEN "LEV"  THEN DO:
                        FIND tt_beskr WHERE tt_beskr.obj = varebehlinje.levnr NO-ERROR.
                        IF NOT AVAIL tt_beskr THEN DO:
                            FIND levbas OF varebehlinje NO-LOCK NO-ERROR.
                            CREATE tt_beskr.
                            ASSIGN tt_beskr.obj   = varebehlinje.levnr
                                   tt_beskr.beskr = IF AVAIL levbas THEN levbas.levnamn ELSE "*Ukjent*".
                        END.
                        ASSIGN dObj      = tt_beskr.obj
                               cObjbeskr = tt_beskr.beskr.
                    END.
                    WHEN "BUT"  THEN DO:
                        FIND tt_beskr WHERE tt_beskr.obj = varebehlinjeTrans.butikknr NO-ERROR.
                        IF NOT AVAIL tt_beskr THEN DO:
                            FIND butiker WHERE butiker.butik = varebehlinjeTrans.butikknr NO-LOCK NO-ERROR.
                            CREATE tt_beskr.
                            ASSIGN tt_beskr.obj   = varebehlinjeTrans.butikknr
                                   tt_beskr.beskr = IF AVAIL butiker THEN butiker.butnamn ELSE "*Ukjent*".
                        END.
                        ASSIGN dObj      = tt_beskr.obj
                               cObjbeskr = tt_beskr.beskr.
                    END.
                    WHEN "ART" THEN DO:
                        FIND tt_beskr WHERE tt_beskr.obj = varebehlinjeTrans.Artikkelnr NO-ERROR.
                        IF NOT AVAIL tt_beskr THEN DO:
                            FIND Artbas OF varebehlinjeTrans NO-LOCK NO-ERROR.
                            CREATE tt_beskr.
                            ASSIGN tt_beskr.obj   = varebehlinjeTrans.butikknr
                                   tt_beskr.beskr = IF AVAIL butiker THEN butiker.butnamn ELSE "*Ukjent*".
                        END.
                        ASSIGN dObj      = tt_beskr.obj
                               cObjbeskr = tt_beskr.beskr.
                    END.
                    WHEN "VMID"     THEN DO:
                        /* om aggregering 0 "VMID" så är tt_beskr skapad redan */
                        FIND artbas OF varebehlinje NO-LOCK NO-ERROR.
                        FIND tt_beskr WHERE tt_beskr.obj = artbas.vmid NO-ERROR.
                        ASSIGN dObj      = IF AVAIL tt_beskr THEN tt_beskr.obj ELSE 0
                               cObjbeskr = IF AVAIL tt_beskr THEN tt_beskr.beskr ELSE "*UKJENT*".
                    END.
                END CASE.
                
                FIND tt_rapport WHERE tt_rapport.butikknr = INT(ENTRY(ii,cButiker)) AND
                                      tt_rapport.obj1     = dObj NO-ERROR.
                IF NOT AVAIL tt_rapport THEN DO:
                    CREATE tt_rapport.
                    ASSIGN tt_rapport.butikknr  = INT(ENTRY(ii,cButiker))
                           tt_rapport.butnamn   = IF AVAIL butiker THEN butiker.butnamn ELSE "*Ukjent*"
                           tt_rapport.obj1      = dObj
                           tt_rapport.obj1Namn  = cObjbeskr.
                END.
                RUN Inndeling (OUTPUT iAntFord).
                dAntall = (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * iAntFord.
                   dSum = dAntall * VareBehLinje.VareKost.
                ASSIGN tt_rapport.antall   = tt_rapport.antall + dAntall
                       tt_rapport.ordresum = tt_rapport.ordresum + dSum.
            END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFilterValues fFrameWin 
PROCEDURE SendFilterValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cFilterVerdier AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cColAlign      AS CHARACTER  NO-UNDO.
/* DEFINE        VARIABLE  cFstPeriode    AS CHARACTER  NO-UNDO.  */
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
/* DEFINE        VARIABLE  cPeriodeTmp    AS CHARACTER  NO-UNDO.  */
/* DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.  */
/* DEFINE        VARIABLE  cFraAar         AS CHARACTER  NO-UNDO. */
/* DEFINE        VARIABLE  cTilAar         AS CHARACTER  NO-UNDO. */
/* DEFINE        VARIABLE  cFraPerLinNr    AS CHARACTER  NO-UNDO. */
/* DEFINE        VARIABLE  cTilPerLinNr    AS CHARACTER  NO-UNDO. */
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT TG-Avfilter:CHECKED THEN DO:
            ASSIGN cButikker = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")).
            RELEASE Butiker.
            IF NUM-ENTRIES(cButikker) = 1 THEN
                FIND Butiker WHERE Butiker.Butik = INT(cButikker) NO-LOCK NO-ERROR.
            ASSIGN cFilterVerdier = IF AVAIL Butiker THEN "Butikk: " + Butiker.Butnamn ELSE "Butikker: " + cButikker.
        END.
        IF RS-Aggregering:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ART" THEN
            ASSIGN cColAlign = cRightCols + "," +  cArtRightCols.
        ELSE
            ASSIGN cColAlign = cRightCols.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAVfilter fFrameWin 
PROCEDURE setAVfilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN TG-AvFilter:CHECKED IN FRAME {&FRAME-NAME} = TRUE
          RS-Aggregering:SCREEN-VALUE = "ART".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTArt fFrameWin 
PROCEDURE SkapaTTArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE TEMP-TABLE hTTArt.
    hTTArt:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
    hTTArt:ADD-NEW-INDEX("Artikkel",YES).
    hTTArt:ADD-INDEX-FIELD("Artikkel","ArtikkelNr").
    hTTArt:TEMP-TABLE-PREPARE("TTArt").

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
  DEFINE VARIABLE cQryString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLagAntStr AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
/*     IF RS-Aggregering:SCREEN-VALUE = "ART" THEN DO:                                                     */
/*     END.                                                                                                     */
/*     ELSE DO:                                                                                                 */
/*         ASSIGN cQryString = SUBSTITUTE("FOR EACH StLager WHERE StTypeId = '&1'",RS-Aggregering:SCREEN-VALUE) */
/*                               + cLagAntStr + " AND SUBSTBUTIK NO-LOCK".                                      */
/* /*                                           + cLagAntStr. */                                                */
/*         DYNAMIC-FUNCTION('setQueryString':U IN h_dstlager,                                                   */
/*            INPUT cQryString /* CHARACTER */).                                                                */
/*     END.                                                                                                     */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArt fFrameWin 
PROCEDURE StartSokArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cLagAnt        AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lOK            AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  logTMP AS LOGICAL    NO-UNDO.
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    DO WITH FRAME {&FRAME-NAME}:
        CREATE TT_LagerTMP. /* en temporär record för att kunna summera */
/*         FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE NO-LOCK: */
        FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE AND CAN-DO(FI-Levnr,STRING(Artbas.LevNr)) AND
                              CAN-DO(FI-VMId,STRING(ArtBas.VMId)) NO-LOCK:

            FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
            IF FI-Avdeling <> "*" THEN DO:
                IF NOT AVAIL HuvGr OR NOT CAN-DO(FI-Avdeling,STRING(HuvGr.AvdelingNr)) THEN
                    NEXT.
            END.
            DO iCount = 1 TO NUM-ENTRIES(cButiker):
                FIND Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND Lager.Butik = INT(ENTRY(iCount,cButiker)) NO-LOCK NO-ERROR.
                IF NOT AVAIL Lager THEN
                    NEXT.
                IF cLagAnt <> "*" THEN DO:
                    ASSIGN lOK = FALSE.
                    CASE cLagAnt:
                        WHEN "<" THEN 
                            IF Lager.Lagant < 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "<=" THEN 
                            IF Lager.Lagant <= 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "=" THEN 
                            IF Lager.Lagant = 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN ">=" THEN 
                            IF Lager.Lagant >= 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN ">" THEN 
                            IF Lager.Lagant > 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "<>" THEN 
                            IF Lager.Lagant <> 0 THEN 
                                ASSIGN lOK = TRUE.
                    END CASE.
                    IF lOK = FALSE THEN
                        NEXT.
                END.
                BUFFER-COPY Lager TO TT_LagerTMP.
                FIND CURRENT TT_LagerTMP.
                RELEASE TT_Lager.
                IF NOT Tg-VisButikker:CHECKED THEN DO:
                    ASSIGN TT_LagerTMP.Butik = 0.
                    FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr AND TT_Lager.Butik = TT_LagerTMP.Butik NO-ERROR.
                END.
                IF AVAIL TT_Lager THEN
                    RUN Summera.
                ELSE DO:
                    CREATE TT_Lager.
                    BUFFER-COPY TT_LagerTMP TO TT_Lager.
                    ASSIGN TT_Lager.Beskrivelse  = ArtBas.Beskr
                           TT_Lager.AvdelingNr   = IF AVAIL HuvGr THEN HuvGr.AvdelingNr ELSE 0
                           TT_Lager.AvdelingNavn = getBeskr("AVDELING",STRING(TT_Lager.AvdelingNr))
                           TT_Lager.Hg           = ArtBas.Hg
                           TT_Lager.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_Lager.Hg))
                           TT_Lager.Vg           = ArtBas.Vg
                           TT_Lager.VgBeskr      = getBeskr("VAREGR",STRING(TT_Lager.Vg))
        /*                    TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
                           TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(ArtBas.LopNr))) + STRING(ArtBas.LopNr)
                           TT_Lager.LevNr       = ArtBas.LevNr
                           TT_Lager.LevNamn     = getBeskr("LEVERAN",STRING(TT_Lager.LevNr))
                           TT_Lager.VMId      = ArtBas.VMId
                           TT_Lager.SasBeskr    = getBeskr("VMId",STRING(ArtBas.VMId))
                           TT_Lager.Farg        = ArtBas.Farg
                           TT_Lager.FarBeskr    = getBeskr("FARG",STRING(ArtBas.Farg))
                           TT_Lager.CharButik   = IF Tg-VisButikker:CHECKED THEN STRING(TT_Lager.Butik) ELSE ""
                           TT_Lager.DbKr        = TT_Lager.VerdiSolgt - (TT_Lager.AntSolgt * TT_Lager.VVarekost)
                          /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                          /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                          /* som ger vvarekost/st */
                           TT_Lager.LagerVerdi     = TT_Lager.LagAnt * TT_Lager.VVarekost.
                    IF Tg-VisButikker:CHECKED THEN 
                        TT_Lager.Butnamn = getBeskr("GETBUTNAVN",STRING(TT_Lager.Butik)).
                END.
            END.
        END.
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
    DEFINE INPUT  PARAMETER qh        AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER lLocal    AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cStatus   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cLagAnt        AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lOK            AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cQry           AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dArtikkelNr    AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iHg            LIKE HuvGr.Hg NO-UNDO.
    DEFINE        VARIABLE  lAvancert      AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cFelterTmp  AS CHARACTER  NO-UNDO.
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    ASSIGN dSolgtTot = 0
           dDBTot    = 0
           dRabTot   = 0
           dKjopTot  = 0.
    DO WITH FRAME {&FRAME-NAME}:
        IF cButiker BEGINS "HENTINTERNT" THEN DO:
            ASSIGN cStatus =  ENTRY(2,cButiker,CHR(1))
                   cButiker = ENTRY(1,cButiker,CHR(1)).
      /* vid HENTINTERNT: cStatus innehåller antal poster i tempdb */
            ASSIGN cButiker  = IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")
                   lAvancert = TRUE.
        END.
        CREATE TT_LagerTMP. /* en temporär record för att kunna summera */
/*         FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE NO-LOCK: */
        IF lLocal THEN DO:
            cQry =
            "FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE" + 
                 (IF FI-LevNr <> "*"  THEN (IF NUM-ENTRIES(FI-LevNr) = 1  THEN " AND ArtBas.LevNr = " + FI-LevNr   ELSE " AND CAN-DO('" + FI-LevNr + "',STRING(ArtBas.LevNr))") ELSE "") +
                 (IF FI-VarGr <> "*"  THEN (IF NUM-ENTRIES(FI-VarGr) = 1  THEN " AND ArtBas.Vg = " + FI-VarGr   ELSE " AND CAN-DO('" + FI-VarGr + "',STRING(ArtBas.Vg))") ELSE "") +
                 (IF FI-VMId <> "*" THEN (IF NUM-ENTRIES(FI-VMId) = 1 THEN " AND ArtBas.VMId = " + FI-VMId ELSE " AND CAN-DO('" + FI-VMId + "',STRING(ArtBas.VMId))") ELSE "") + " NO-LOCK".
            CREATE QUERY qh.
            qh:SET-BUFFERS(BUFFER ArtBas:HANDLE).
            qh:QUERY-PREPARE(cQry).
            qh:QUERY-OPEN().
        END.
        ELSE DO:
            IF cButiker = "" AND NUM-ENTRIES(cTillgButikker) = 1 THEN
                ASSIGN cButiker = cTillgButikker.
            ELSE IF cButiker = "" THEN DO:
                RUN TagButiker IN THIS-PROCEDURE (OUTPUT cButiker).
                ASSIGN cButiker = REPLACE(cButiker,"|",",").
                IF cButiker = "" THEN DO:
                    MESSAGE "Ingen butikker valgt."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    APPLY "CLOSE" TO h_Window.
                    RETURN.
                END.
            END.
            IF NUM-ENTRIES(cButiker) > 1 AND NOT Tg-VisButikker:CHECKED THEN DO:
                MESSAGE "Ønsker du resultatet per butikk? "
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVisPerBut AS LOGICAL.
                ASSIGN Tg-VisButikker:CHECKED = lVisPerBut.
            END.
            PUBLISH "VisTxtBox" ("Søker data......").
            qh:QUERY-OPEN().
        END.
        ASSIGN cTmpFieldDefs       = cFieldDefs.
        IF Tg-VisButikker:CHECKED THEN DO:
            ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",Butnamn;Navn;;".
            ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",HarNeg;Neg;;".
        END.
        ELSE 
            ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",HarNeg;Neg;;".
        /* Lägg in LevKod */
        ASSIGN ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + ",LevKod;Lev.artnr;;".
        RUN FixStrings.
        ASSIGN cFieldDefs = cTmpFieldDefs.
        REPEAT: 
            RELEASE VarGr.
            RELEASE HuvGr.
            qh:GET-NEXT().
            IF qh:QUERY-OFF-END THEN
                LEAVE.
/*                  MESSAGE qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE() */
/*                       VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
            IF lLocal = FALSE AND NOT (qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OPris"):BUFFER-VALUE() = FALSE AND
                                       qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lager"):BUFFER-VALUE() = TRUE)     THEN
                NEXT.
            FIND VarGr WHERE VarGr.Vg = INT(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Vg"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
            IF NOT AVAIL VarGr THEN
                NEXT.
            IF lLocal = TRUE AND (FI-HuvGr <> "*" OR FI-Avdeling <> "*") THEN DO:
                IF FI-HuvGr <> "*" AND NOT CAN-DO(FI-HuvGr,STRING(VarGr.Hg)) THEN
                    NEXT.
                ELSE IF FI-Avdeling <> "*" THEN DO:
                    FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                    IF NOT AVAIL HuvGr OR NOT CAN-DO(FI-Avdeling,STRING(HuvGr.AvdelingNr)) THEN
                        NEXT.
                END.
            END.
            IF NOT AVAIL HuvGr THEN
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            DO iCount = 1 TO NUM-ENTRIES(cButiker):
                FIND Lager WHERE Lager.ArtikkelNr = DECI(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE()) AND 
                                 Lager.Butik = INT(ENTRY(iCount,cButiker)) NO-LOCK NO-ERROR.
                IF NOT AVAIL Lager THEN
                    NEXT.
                ASSIGN lOK = TRUE.
                IF lLocal = FALSE AND lAvancert = FALSE AND Lager.Lagant <= 0 THEN
                    NEXT.
                IF (lLocal = TRUE OR lAvancert) AND cLagAnt <> "*" THEN DO:
                    ASSIGN lOK = FALSE.
                    CASE cLagAnt:
                        WHEN "<" THEN 
                            IF Lager.Lagant < 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "<=" THEN 
                            IF Lager.Lagant <= 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "=" THEN 
                            IF Lager.Lagant = 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN ">=" THEN 
                            IF Lager.Lagant >= 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN ">" THEN 
                            IF Lager.Lagant > 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "<>" THEN 
                            IF Lager.Lagant <> 0 THEN 
                                ASSIGN lOK = TRUE.
                    END CASE.
                    IF lOK = FALSE THEN
                        NEXT.
                END.
                IF lOK = FALSE THEN
                    NEXT.
                BUFFER-COPY Lager TO TT_LagerTMP.
                FIND CURRENT TT_LagerTMP.
                RELEASE TT_Lager.
                IF NOT Tg-VisButikker:CHECKED THEN DO:
                    ASSIGN TT_LagerTMP.Butik = 0.
                    FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr AND TT_Lager.Butik = TT_LagerTMP.Butik NO-ERROR.
                END.
                IF AVAIL TT_Lager AND TT_LagerTMP.Butik = 0 THEN
                    RUN Summera.
                ELSE DO:
                    CREATE TT_Lager.
                    BUFFER-COPY TT_LagerTMP TO TT_Lager.
                    ASSIGN dSolgtTot = dSolgtTot + TT_LagerTMP.VerdiSolgt
                           dDBTot    = dDBTot    + TT_LagerTMP.VerdiSolgt - TT_LagerTMP.Svk
                           dRabTot   = dRabTot   + TT_LagerTMP.VerdiRabatt
                           dKjopTot  = dKjopTot  + TT_LagerTMP.KjopVerdi.
                    ASSIGN TT_Lager.Beskrivelse  = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Beskr"):BUFFER-VALUE()
                           TT_Lager.AvdelingNr   = IF AVAIL HuvGr THEN HuvGr.AvdelingNr ELSE 0
                           TT_Lager.AvdelingNavn = getBeskr("AVDELING",STRING(TT_Lager.AvdelingNr))
                           TT_Lager.Hg           = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hg"):BUFFER-VALUE()
                           TT_Lager.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_Lager.Hg))
                           TT_Lager.Vg           = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Vg"):BUFFER-VALUE()
                           TT_Lager.VgBeskr      = getBeskr("VAREGR",STRING(TT_Lager.Vg))
        /*                    TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
                           TT_Lager.VgLopNr       = STRING(TT_Lager.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lopnr"):BUFFER-VALUE())))
                                                                                         + STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lopnr"):BUFFER-VALUE())
                           TT_Lager.LevNr       = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE()
                           TT_Lager.LevNamn     = getBeskr("LEVERAN",STRING(TT_Lager.LevNr))
                           TT_Lager.VMId      = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("VMId"):BUFFER-VALUE()
                           TT_Lager.SasBeskr    = getBeskr("VMId",STRING(TT_Lager.VMId))
                           TT_Lager.Farg        = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Farg"):BUFFER-VALUE()
                           TT_Lager.FarBeskr    = getBeskr("FARG",STRING(TT_Lager.Farg))
                           TT_Lager.CharButik   = IF Tg-VisButikker:CHECKED THEN STRING(TT_Lager.Butik) ELSE ""
                           TT_Lager.DbKr        = TT_Lager.VerdiSolgt - TT_Lager.SVK /* (TT_Lager.AntSolgt * TT_Lager.VVarekost) */
                          /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                          /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                          /* som ger vvarekost/st */
                           TT_Lager.LagerVerdi     = TT_Lager.LagAnt * TT_Lager.VVarekost
                           TT_Lager.HarNeg = STRING(CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = TT_Lager.artikkelnr AND artlag.lagant < 0),"J/")
                           TT_Lager.LevKod = TRIM(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevKod"):BUFFER-VALUE()).
                           
                     IF Tg-VisButikker:CHECKED THEN 
                         ASSIGN TT_Lager.Butnamn = IF Tg-VisButikker:CHECKED THEN getBeskr("GETBUTNAVN",STRING(TT_Lager.Butik)) ELSE "".
                END.
            END.
        END.
    END.
    IF lLocal = TRUE THEN DO:
        qh:QUERY-CLOSE().
        DELETE OBJECT qh NO-ERROR.
        ASSIGN qh = ?.
    END.
    RUN KalkuleraTTLager.
    PUBLISH "VisTxtBox" ("Leser ut data......").
    CREATE QUERY qh.
    qh:SET-BUFFERS(BUFFER TT_Lager:HANDLE).
    qh:QUERY-PREPARE("FOR EACH TT_Lager").
    qh:QUERY-OPEN().
    PUBLISH "VisTxtBox" ("Leser ut data......").
    ASSIGN cFelterTmp = REPLACE(cFelter,"Dataobjekt","ArtikkelNr").
    RUN rappgenqry.p ("dynttStLager","FOR EACH TT_Lager",
                      cFileName,
                      REPLACE(cLabels,"DataobjektTXT",getLabel(RS-Aggregering:SCREEN-VALUE)) + "," + cArtLabels,
                      cFelterTmp + "," + cArtFelter,
                      cDecimaler + "," + cArtDecimaler,
                      cTidFelter,qh).
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    DELETE OBJECT qh NO-ERROR.
    ASSIGN qh = ?.
    RUN LesInnIGrid.
    RUN AlignCol IN h_frapportgrid (3,1) NO-ERROR.
    PUBLISH "VisTxtBox" ("").
/*     ELSE DO:                                         */
/*         PUBLISH "VisTxtBox" ("Leser ut data......"). */
/*         RUN ExporterTTLager.                         */
/*         RUN LesInnIGrid.                             */
/*         PUBLISH "VisTxtBox" ("").                    */
/*     END.                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Summera fFrameWin 
PROCEDURE Summera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN TT_Lager.LagAnt        = TT_Lager.LagAnt        + TT_LagerTMP.LagAnt       
       TT_Lager.AntSolgt      = TT_Lager.AntSolgt      + TT_LagerTMP.AntSolgt     
       TT_Lager.VerdiSolgt    = TT_Lager.VerdiSolgt    + TT_LagerTMP.VerdiSolgt   
       TT_Lager.AntRab        = TT_Lager.AntRab        + TT_LagerTMP.AntRab       
/**/   TT_Lager.DbKr          = TT_Lager.DbKr          + TT_LagerTMP.VerdiSolgt - TT_LagerTMP.SVK
                                     /*  TT_LagerTMP.VerdiSolgt - (TT_LagerTMP.AntSolgt * TT_LagerTMP.VVarekost) */
       TT_Lager.VerdiRabatt   = TT_Lager.VerdiRabatt   + TT_LagerTMP.VerdiRabatt  
    /* Här summerar vi varekosten för att få totala lagervärdet, */
    /* Innan vi kör visning delas den med LagAnt och ger VVarekost */
/**/   TT_Lager.LagerVerdi    = TT_Lager.LagerVerdi    + (TT_LagerTMP.LagAnt * TT_LagerTMP.VVarekost)    
       TT_Lager.ReklAnt       = TT_Lager.ReklAnt       + TT_LagerTMP.ReklAnt      
       TT_Lager.ReklVerdi     = TT_Lager.ReklVerdi     + TT_LagerTMP.ReklVerdi    
       TT_Lager.ReklLAnt      = TT_Lager.ReklLAnt      + TT_LagerTMP.ReklLAnt     
       TT_Lager.ReklLVerdi    = TT_Lager.ReklLVerdi    + TT_LagerTMP.ReklLVerdi   
       TT_Lager.RetLAnt       = TT_Lager.RetLAnt       + TT_LagerTMP.RetLAnt      
       TT_Lager.SvinnAnt      = TT_Lager.SvinnAnt      + TT_LagerTMP.SvinnAnt     
       TT_Lager.SvinnVerdi    = TT_Lager.SvinnVerdi    + TT_LagerTMP.SvinnVerdi   
       TT_Lager.GjenkjopAnt   = TT_Lager.GjenkjopAnt   + TT_LagerTMP.GjenkjopAnt  
       TT_Lager.GjenkjopVerdi = TT_Lager.GjenkjopVerdi + TT_LagerTMP.GjenkjopVerdi
       TT_Lager.KjopAnt       = TT_Lager.KjopAnt       + TT_LagerTMP.KjopAnt      
       TT_Lager.KjopVerdi     = TT_Lager.KjopVerdi     + TT_LagerTMP.KjopVerdi    
       TT_Lager.BrekkAnt      = TT_Lager.BrekkAnt      + TT_LagerTMP.BrekkAnt     
       TT_Lager.BrekkVerdi    = TT_Lager.BrekkVerdi    + TT_LagerTMP.BrekkVerdi   
       TT_Lager.IntAnt        = TT_Lager.IntAnt        + TT_LagerTMP.IntAnt       
       TT_Lager.IntVerdi      = TT_Lager.IntVerdi      + TT_LagerTMP.IntVerdi     
       TT_Lager.JustAnt       = TT_Lager.JustAnt       + TT_LagerTMP.JustAnt      
       TT_Lager.JustVerdi     = TT_Lager.JustVerdi     + TT_LagerTMP.JustVerdi    
       TT_Lager.NedAnt        = TT_Lager.NedAnt        + TT_LagerTMP.NedAnt       
       TT_Lager.NedVerdi      = TT_Lager.NedVerdi      + TT_LagerTMP.NedVerdi     
       TT_Lager.OvAnt         = TT_Lager.OvAnt         + TT_LagerTMP.OvAnt        
       TT_Lager.OvVerdi       = TT_Lager.OvVerdi       + TT_LagerTMP.OvVerdi.      

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
  DEFINE OUTPUT PARAMETER cButiker AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where CAN-DO('" + cTillgButikker + "',STRING(Butiker.Butik))",
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
  PUBLISH "ClearGrid" (REPLACE(cLabels,"DataobjektTXT",getLabel(RS-Aggregering:SCREEN-VALUE IN FRAME {&FRAME-NAME}))).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBeskr fFrameWin 
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBeskrivelse AS CHARACTER  NO-UNDO.
  CASE cStTypeId:
      WHEN "GETBUTNAVN" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.Butnamn ELSE "Ukjent".
      END.
      WHEN "AVDELING" THEN DO:
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "Ukjent".
      END.
      WHEN "HOVEDGR" THEN DO:
          FIND HuvGr WHERE HuvGr.Hg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent".
      END.
      WHEN "BUTSTAT" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".
      END.
      WHEN "VAREGR" THEN DO:
          FIND VarGr WHERE VarGr.Vg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
      END.
      WHEN "LEVERAN" THEN DO:
          FIND LevBas WHERE LevBas.LevNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Levbas THEN LevBas.levnamn ELSE "Ukjent".
      END.
      WHEN "VMId" THEN DO:
          FIND Varemerke WHERE Varemerke.VMId = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE "Ukjent".
      END.
      WHEN "FARG" THEN DO:
          FIND Farg WHERE Farg.Farg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent".
      END.
/*       WHEN "ART" THEN DO:                                             */
/*           FIND ArtBas WHERE ArtBas.ForsNr = INT(cObjekt) NO-LOCK NO-ERROR. */
/*           RETURN IF AVAIL Forsalj THEN Forsalj.FoNamn ELSE "Ukjent".       */
/*       END.                                                                 */
      OTHERWISE RETURN "".
  END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF FI-Butikker = "" THEN DO:
          IF NUM-ENTRIES(CB-ButikkTeam:SCREEN-VALUE,";") = 2 THEN DO:
              FIND TT_BigListItem WHERE ROWID(TT_BigListItem) = TO-ROWID(ENTRY(2,CB-ButikkTeam:SCREEN-VALUE,";")).
              ASSIGN cButiker = REPLACE(TT_BigListItem.Butiker,CHR(1),",").
          END.
          ELSE
              ASSIGN cButiker = REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",").
      END.
      ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE cButiker) + CHR(1) + RS-Aggregering:SCREEN-VALUE.
/*       ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")) + CHR(1) + RS-Aggregering:SCREEN-VALUE. */
      RETURN TRUE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLabel fFrameWin 
FUNCTION getLabel RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  CASE cStTypeId:
      WHEN "AVDELING" THEN
          ASSIGN cTxt = "Avdeling".
      WHEN "HOVEDGR"  THEN
          ASSIGN cTxt = "Hovedgr".
      WHEN "VAREGR"   THEN
          ASSIGN cTxt = "Varegr".
      WHEN "LEVERAN"  THEN
          ASSIGN cTxt = "Leverandør".
      WHEN "BUTSTAT"  THEN
          ASSIGN cTxt = "Butikk".
      WHEN "ART" THEN
          ASSIGN cTxt = "Artikkel".
      OTHERWISE
          ASSIGN cTxt = "??".
  END CASE.
  RETURN cTxt.   /* Function return value. */

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

