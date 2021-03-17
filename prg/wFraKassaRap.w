&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

/* DEFINE VARIABLE cFilKatalog       AS CHARACTER  NO-UNDO. */
DEFINE VARIABLE dFileModDate      AS DATE       NO-UNDO.
DEFINE VARIABLE cTidFelter        AS CHARACTER  NO-UNDO.
/* DEFINE VARIABLE cTmpFile          AS CHARACTER  NO-UNDO. */
DEFINE VARIABLE cColAlign AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iWidthPix  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER    NO-UNDO.
DEFINE VARIABLE cTillgbutiker   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cFelter   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSeAllaiButStat AS CHARACTER   NO-UNDO. /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHitrate AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVgList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cSEfolder AS CHARACTER INIT
 "Avdelning|Huvudgrupp|Varugrupp|Leverantör|Säljare|Timfsg|Varufsg|Butik|Budget D/V|Budget M/Å"     
                 NO-UNDO.
DEFINE VARIABLE iAntkunder   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntkvitt    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAarPerLinNr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iWdOff     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDayString AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE TT_akt_rapp NO-UNDO LIKE akt_rapp
    FIELD bruttosalg AS DECI
    FIELD DBKr       AS DECI
    FIELD DB%        AS DECI
    INDEX tid_txt tid_txt.



DEFINE TEMP-TABLE TT_Butiker NO-UNDO
  FIELD Butik         AS  INTE LABEL "Butikk"
  FIELD butnamn       AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD AntSolgt      AS  DECI LABEL "Solgt"
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD mvaverdi      AS  DECI LABEL "Mva"
  FIELD vvarekost     AS  DECI LABEL "Varekost"
  FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
  FIELD Antkunder     AS  INTE LABEL "Kunder"
  FIELD Antkvitt      AS  INTE LABEL "Kvitteringer"
    INDEX butik IS UNIQUE PRIMARY butik.

DEFINE TEMP-TABLE TT_Avdeling NO-UNDO
  FIELD avdelingnr    AS  INTE LABEL "Avdeling"
  FIELD AvdelingNavn  AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD AntSolgt      AS  DECI LABEL "Solgt"
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD mvaverdi      AS  DECI LABEL "Mva"
  FIELD vvarekost     AS  DECI LABEL "Varekost"
  FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
    INDEX avdelingnr IS UNIQUE PRIMARY avdelingnr.

DEFINE TEMP-TABLE TT_Lev NO-UNDO
  FIELD levnr         AS  INTE LABEL "Levnr"
  FIELD LevNavn       AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD AntSolgt      AS  DECI LABEL "Solgt"
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD mvaverdi      AS  DECI LABEL "Mva"
  FIELD vvarekost     AS  DECI LABEL "Varekost"
  FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
    INDEX levnr IS UNIQUE PRIMARY levnr.

DEFINE TEMP-TABLE TT_Selger NO-UNDO
  FIELD selgernr      AS  INTE LABEL "Selger"
  FIELD Navn      AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD AntSolgt      AS  DECI LABEL "Solgt"
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD mvaverdi      AS  DECI LABEL "Mva"
  FIELD vvarekost     AS  DECI LABEL "Varekost"
  FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
  FIELD AntKunder     AS  INTE LABEL "Kunder"
  FIELD Hitrate       AS  DECI LABEL "Hitrate %"
  FIELD Merfsg        AS  DECI LABEL "Mer fsg %"
  FIELD AntUtens      AS  INTE
  FIELD SumUtensBrutto AS DECI
    INDEX selgernr IS UNIQUE PRIMARY Selgernr.


DEFINE TEMP-TABLE TT_HuvGr NO-UNDO
  FIELD hg            AS  INTE LABEL "Hg"
  FIELD HgBeskr       AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD AntSolgt      AS  DECI LABEL "Solgt"
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD mvaverdi      AS  DECI LABEL "Mva"
  FIELD vvarekost     AS  DECI LABEL "Varekost"
  FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
    INDEX hg IS UNIQUE PRIMARY hg.

DEFINE TEMP-TABLE TT_Vg NO-UNDO
  FIELD vg            AS  INTE LABEL "Vg"
  FIELD VgBeskr       AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD AntSolgt      AS  DECI LABEL "Solgt"
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD mvaverdi      AS  DECI LABEL "Mva"
  FIELD vvarekost     AS  DECI LABEL "Varekost"
  FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
    INDEX vg IS UNIQUE PRIMARY vg.

DEFINE TEMP-TABLE TT_VareSalg NO-UNDO
    FIELD Artikkelnr    AS  DECI LABEL "Artikkelnr"
    FIELD LevKod        AS  CHAR LABEL "Levkod"
    FIELD Farg          AS  CHAR LABEL "Farge"
    FIELD Beskr         AS  CHAR LABEL "Beskrivelse"
    FIELD Varemerke     AS  CHAR LABEL "Varemerke"
    FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
    FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
    FIELD AntSolgt      AS  DECI LABEL "Solgt"
    FIELD VerdiRabatt   AS  DECI LABEL "Rabatter"
    FIELD DBkr          AS  DECI LABEL "DB kr"
    FIELD DB%           AS  DECI LABEL "DB%"
    FIELD mvaverdi      AS  DECI LABEL "Mva"
    FIELD vvarekost     AS  DECI LABEL "Varekost"
    FIELD GjenkjopAnt   AS  DECI LABEL "Returer"
    FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"
      INDEX Artikkelnr  IS UNIQUE PRIMARY Artikkelnr.

DEFINE TEMP-TABLE TT_SBud NO-UNDO
  FIELD Butik         AS  INTE LABEL "Butikk"
  FIELD butnamn       AS  CHAR LABEL "Beskrivelse"
  FIELD salgbrutto    AS  DECI LABEL "Salgssum brutto"
  FIELD VerdiSolgt    AS  DECI LABEL "Salgssum netto"
  FIELD salgBudsjett  AS  DECI LABEL "Salg budsjett"
  FIELD DBkr          AS  DECI LABEL "DB kr"
  FIELD DB%           AS  DECI LABEL "DB%"
  FIELD DbKrBudsjett  AS  DECI LABEL "Db kr budsjett"
  FIELD Db%Budsjett   AS  DECI LABEL "Db% budsjett"
    INDEX butik IS UNIQUE PRIMARY butik.
DEFINE TEMP-TABLE TT_SBudDW NO-UNDO   SERIALIZE-NAME "Budsjett"
    FIELD Butik            AS  INTE LABEL "Butikk"
    FIELD butnamn          AS  CHAR FORMAT "x(30)" LABEL "Beskrivelse"
    FIELD DAGsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD DAGsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD DAGSalgAvvikKr   AS  DECI LABEL "Avvik kr"
    FIELD DAGSalgAvvikproc AS  DECI LABEL "Avvik Oms%" 
    FIELD Skille           AS CHARACTER LABEL '' FORMAT "x(1)" INITIAL ' '
    FIELD UKEsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD UKEsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD UKESalgAvvikKr   AS  DECI FORMAT ">,>>>,>>9" LABEL "Avvik kr"
    FIELD UKESalgAvvikproc AS  DECI LABEL "Avvik Oms%" 
    INDEX butik IS UNIQUE PRIMARY butik.

DEFINE TEMP-TABLE TT_SBudMY NO-UNDO   SERIALIZE-NAME "Budsjett"
    FIELD Butik            AS  INTE LABEL "Butikk"
    FIELD butnamn          AS  CHAR FORMAT "x(30)" LABEL "Beskrivelse"
    FIELD MNDsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD MNDsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD MNDSalgAvvikKr   AS  DECI LABEL "Avvik kr"
    FIELD MNDSalgAvvikproc AS  DECI LABEL "Avvik Oms%" 
    FIELD Skille           AS CHARACTER LABEL '' FORMAT "x(1)" INITIAL ' '
    FIELD AARsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD AARsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD AARSalgAvvikKr   AS  DECI FORMAT ">,>>>,>>9" LABEL "Avvik kr"
    FIELD AARSalgAvvikproc AS  DECI LABEL "Avvik Oms%" 
    INDEX butik IS UNIQUE PRIMARY butik.

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define QUERY-NAME QAvd

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Avdeling TT_Butiker TT_HuvGr TT_Lev ~
TT_SBudDW TT_SBudMY TT_Selger TT_akt_rapp TT_VareSalg TT_Vg

/* Definitions for QUERY QAvd                                           */
&Scoped-define SELF-NAME QAvd
&Scoped-define QUERY-STRING-QAvd FOR EACH TT_Avdeling
&Scoped-define OPEN-QUERY-QAvd OPEN QUERY {&SELF-NAME} FOR EACH TT_Avdeling.
&Scoped-define TABLES-IN-QUERY-QAvd TT_Avdeling
&Scoped-define FIRST-TABLE-IN-QUERY-QAvd TT_Avdeling


/* Definitions for QUERY QButiker                                       */
&Scoped-define SELF-NAME QButiker
&Scoped-define QUERY-STRING-QButiker FOR EACH TT_Butiker
&Scoped-define OPEN-QUERY-QButiker OPEN QUERY {&SELF-NAME} FOR EACH TT_Butiker.
&Scoped-define TABLES-IN-QUERY-QButiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-QButiker TT_Butiker


/* Definitions for QUERY QHuvGr                                         */
&Scoped-define SELF-NAME QHuvGr
&Scoped-define QUERY-STRING-QHuvGr FOR EACH TT_HuvGr
&Scoped-define OPEN-QUERY-QHuvGr OPEN QUERY {&SELF-NAME} FOR EACH TT_HuvGr.
&Scoped-define TABLES-IN-QUERY-QHuvGr TT_HuvGr
&Scoped-define FIRST-TABLE-IN-QUERY-QHuvGr TT_HuvGr


/* Definitions for QUERY QLev                                           */
&Scoped-define SELF-NAME QLev
&Scoped-define QUERY-STRING-QLev FOR EACH TT_Lev
&Scoped-define OPEN-QUERY-QLev OPEN QUERY {&SELF-NAME} FOR EACH TT_Lev.
&Scoped-define TABLES-IN-QUERY-QLev TT_Lev
&Scoped-define FIRST-TABLE-IN-QUERY-QLev TT_Lev


/* Definitions for QUERY QSBudDW                                        */
&Scoped-define SELF-NAME QSBudDW
&Scoped-define QUERY-STRING-QSBudDW FOR EACH TT_SBudDW
&Scoped-define OPEN-QUERY-QSBudDW OPEN QUERY {&SELF-NAME} FOR EACH TT_SBudDW.
&Scoped-define TABLES-IN-QUERY-QSBudDW TT_SBudDW
&Scoped-define FIRST-TABLE-IN-QUERY-QSBudDW TT_SBudDW


/* Definitions for QUERY QSBudMY                                        */
&Scoped-define SELF-NAME QSBudMY
&Scoped-define QUERY-STRING-QSBudMY FOR EACH TT_SBudMY
&Scoped-define OPEN-QUERY-QSBudMY OPEN QUERY {&SELF-NAME} FOR EACH TT_SBudMY.
&Scoped-define TABLES-IN-QUERY-QSBudMY TT_SBudMY
&Scoped-define FIRST-TABLE-IN-QUERY-QSBudMY TT_SBudMY


/* Definitions for QUERY QSelger                                        */
&Scoped-define SELF-NAME QSelger
&Scoped-define QUERY-STRING-QSelger FOR EACH TT_Selger
&Scoped-define OPEN-QUERY-QSelger OPEN QUERY {&SELF-NAME} FOR EACH TT_Selger.
&Scoped-define TABLES-IN-QUERY-QSelger TT_Selger
&Scoped-define FIRST-TABLE-IN-QUERY-QSelger TT_Selger


/* Definitions for QUERY QTime                                          */
&Scoped-define SELF-NAME QTime
&Scoped-define QUERY-STRING-QTime FOR EACH TT_akt_rapp
&Scoped-define OPEN-QUERY-QTime OPEN QUERY {&SELF-NAME} FOR EACH TT_akt_rapp.
&Scoped-define TABLES-IN-QUERY-QTime TT_akt_rapp
&Scoped-define FIRST-TABLE-IN-QUERY-QTime TT_akt_rapp


/* Definitions for QUERY QVare                                          */
&Scoped-define SELF-NAME QVare
&Scoped-define QUERY-STRING-QVare FOR EACH TT_VareSalg
&Scoped-define OPEN-QUERY-QVare OPEN QUERY {&SELF-NAME} FOR EACH TT_VareSalg.
&Scoped-define TABLES-IN-QUERY-QVare TT_VareSalg
&Scoped-define FIRST-TABLE-IN-QUERY-QVare TT_VareSalg


/* Definitions for QUERY QVg                                            */
&Scoped-define SELF-NAME QVg
&Scoped-define QUERY-STRING-QVg FOR EACH TT_Vg
&Scoped-define OPEN-QUERY-QVg OPEN QUERY {&SELF-NAME} FOR EACH TT_Vg.
&Scoped-define TABLES-IN-QUERY-QVg TT_Vg
&Scoped-define FIRST-TABLE-IN-QUERY-QVg TT_Vg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Help RECT-3 RECT-1 RECT-2 B-Chart ~
RS-Type CB-Butik B-Excel B-Artikkelkort BUTTON-1 B-Oppdater BUTTON-Ok 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato RS-Type CB-Butik 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockVindu wWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHeltal wWin 
FUNCTION getHeltal RETURNS CHARACTER
  ( INPUT cListe AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Artikkelkort 
     LABEL "Arti&kkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Chart 
     IMAGE-UP FILE "icon/e-chart.bmp":U
     LABEL "Button 1" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Eksporter til Excel.".

DEFINE BUTTON B-Oppdater  NO-FOCUS
     LABEL "Oppdater" 
     SIZE 15 BY 1.05.

DEFINE BUTTON B-QuickPrint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "Enkel rapport".

DEFINE BUTTON Btn_Help 
     IMAGE-UP FILE "icon/e-help.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "XPrint rapport".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE VARIABLE CB-Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 46.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Idag", 1,
"Tidligere", 2
     SIZE 28 BY 1.05 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202.8 BY .14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202.8 BY .14.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 4.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QAvd FOR 
      TT_Avdeling SCROLLING.

DEFINE QUERY QButiker FOR 
      TT_Butiker SCROLLING.

DEFINE QUERY QHuvGr FOR 
      TT_HuvGr SCROLLING.

DEFINE QUERY QLev FOR 
      TT_Lev SCROLLING.

DEFINE QUERY QSBudDW FOR 
      TT_SBudDW SCROLLING.

DEFINE QUERY QSBudMY FOR 
      TT_SBudMY SCROLLING.

DEFINE QUERY QSelger FOR 
      TT_Selger SCROLLING.

DEFINE QUERY QTime FOR 
      TT_akt_rapp SCROLLING.

DEFINE QUERY QVare FOR 
      TT_VareSalg SCROLLING.

DEFINE QUERY QVg FOR 
      TT_Vg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_Help AT ROW 1.33 COL 192.8 NO-TAB-STOP 
     B-Chart AT ROW 1.24 COL 76
     FI-Dato AT ROW 4.05 COL 18.2 COLON-ALIGNED
     RS-Type AT ROW 4.14 COL 43.4 NO-LABEL
     CB-Butik AT ROW 5.48 COL 18.2 COLON-ALIGNED HELP
          "Butikknummer"
     B-Excel AT ROW 1.33 COL 13.2
     B-Artikkelkort AT ROW 6.24 COL 85
     B-QuickPrint AT ROW 1.33 COL 18
     BUTTON-1 AT ROW 1.33 COL 22.4
     B-Oppdater AT ROW 1.33 COL 27.6
     BUTTON-Next AT ROW 1.33 COL 8.6 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 198 NO-TAB-STOP 
     RECT-3 AT ROW 3.14 COL 3.6
     RECT-1 AT ROW 1.14 COL 1
     RECT-2 AT ROW 2.43 COL 1
     SPACE(0.00) SKIP(27.96)
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 9
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "On-line rapport kasse"
         HEIGHT             = 30
         WIDTH              = 203.2
         MAX-HEIGHT         = 36
         MAX-WIDTH          = 203.6
         VIRTUAL-HEIGHT     = 36
         VIRTUAL-WIDTH      = 203.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}
{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON B-QuickPrint IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-QuickPrint:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Next IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Prev IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Dato IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QAvd
/* Query rebuild information for QUERY QAvd
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Avdeling.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 5.52 , 106 )
*/  /* QUERY QAvd */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QButiker
/* Query rebuild information for QUERY QButiker
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Butiker.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 5.57 , 122.2 )
*/  /* QUERY QButiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QHuvGr
/* Query rebuild information for QUERY QHuvGr
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_HuvGr.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 5.52 , 113 )
*/  /* QUERY QHuvGr */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QLev
/* Query rebuild information for QUERY QLev
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Lev.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 3.86 , 132 )
*/  /* QUERY QLev */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QSBudDW
/* Query rebuild information for QUERY QSBudDW
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_SBudDW.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 3.86 , 146 )
*/  /* QUERY QSBudDW */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QSBudMY
/* Query rebuild information for QUERY QSBudMY
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_SBudMY.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 5.52 , 145 )
*/  /* QUERY QSBudMY */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QSelger
/* Query rebuild information for QUERY QSelger
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Selger.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 5.52 , 132 )
*/  /* QUERY QSelger */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QTime
/* Query rebuild information for QUERY QTime
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_akt_rapp.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 3.86 , 114 )
*/  /* QUERY QTime */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QVare
/* Query rebuild information for QUERY QVare
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_VareSalg.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 3.86 , 122 )
*/  /* QUERY QVare */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QVg
/* Query rebuild information for QUERY QVg
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Vg.
     _END_FREEFORM
     _Design-Parent    is FRAME fMain @ ( 3.86 , 106 )
*/  /* QUERY QVg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* On-line rapport kasse */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* On-line rapport kasse */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* On-line rapport kasse */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
            {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort wWin
ON CHOOSE OF B-Artikkelkort IN FRAME fMain /* Artikkelkort */
DO:
  RUN ArtikkelKort.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Chart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Chart wWin
ON CHOOSE OF B-Chart IN FRAME fMain /* Button 1 */
DO:
    DEFINE VARIABLE cColverdier AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cVerdierTmp AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSolgt      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRowLabel   AS CHAR NO-UNDO.
    DEFINE VARIABLE cColLabel   AS CHAR NO-UNDO.
    DEFINE VARIABLE cColValues  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cColEnable  AS CHAR NO-UNDO.
    DEFINE VARIABLE cTitel      AS CHARACTER  NO-UNDO.

    RUN ColVerdier IN h_frapportgrid (OUTPUT cRowLabel,1).
    IF cRowLabel = "" THEN
        RETURN.
    ASSIGN cRowLabel = REPLACE(cRowLabel,CHR(1),",").
    CASE INT(DYNAMIC-FUNCTION('getCurrentPage':U)):
        WHEN 1 THEN DO:
            ASSIGN cTitel    = "Salg avdeling"
                   cColLabel = "Salg,Rabatt"
                   cColEnable = "1,1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).
            ASSIGN cVerdierTmp = REPLACE(cVerdierTmp,CHR(1),",").
            DO iCount = 1 TO NUM-ENTRIES(cRowLabel):
                ASSIGN ENTRY(iCount,cRowLabel) = ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp).
            END.
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,7).
            ASSIGN cColValues = cColValues + ";" + REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
        WHEN 2 THEN DO:
            ASSIGN cTitel    = "Salg hovedgrupper"
                   cColLabel = "Salg,Rabatt"
                   cColEnable = "1,1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).
            ASSIGN cVerdierTmp = REPLACE(cVerdierTmp,CHR(1),",").
            DO iCount = 1 TO NUM-ENTRIES(cRowLabel):
                ASSIGN ENTRY(iCount,cRowLabel) = ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp).
            END.
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,7).
            ASSIGN cColValues = cColValues + ";" + REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
        WHEN 3 THEN DO:
            ASSIGN cTitel    = "Salg varegrupper"
                   cColLabel = "Salg,Rabatt"
                   cColEnable = "1,1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).
            ASSIGN cVerdierTmp = REPLACE(cVerdierTmp,CHR(1),",").
            DO iCount = 1 TO NUM-ENTRIES(cRowLabel):
                ASSIGN ENTRY(iCount,cRowLabel) = ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp).
            END.
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,7).
            ASSIGN cColValues = cColValues + ";" + REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
        WHEN 4 THEN DO:
            ASSIGN cTitel    = "Salg leverandør"
                   cColLabel = "Salg,Rabatt"
                   cColEnable = "1,1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).
            ASSIGN cVerdierTmp = REPLACE(cVerdierTmp,CHR(1),",").
            DO iCount = 1 TO NUM-ENTRIES(cRowLabel):
                ASSIGN ENTRY(iCount,cRowLabel) = ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp).
            END.
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,7).
            ASSIGN cColValues = cColValues + ";" + REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
        WHEN 5 THEN DO:
            ASSIGN cTitel    = "Salg time"
                   cColLabel = "Salg"
                   cColEnable = "1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
        WHEN 6 THEN DO:
            ASSIGN cTitel    = "Salg artikkler"
                   cColLabel = "Salg,Rabatt"
                   cColEnable = "1,1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).
            ASSIGN cVerdierTmp = REPLACE(cVerdierTmp,CHR(1),",").
            DO iCount = 1 TO NUM-ENTRIES(cRowLabel):
                ASSIGN ENTRY(iCount,cRowLabel) = ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp).
            END.
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,7).
            ASSIGN cColValues = cColValues + ";" + REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
        WHEN 7 THEN DO:
            ASSIGN cTitel    = "Salg Butikker"
                   cColLabel = "Salg,Rabatt"
                   cColEnable = "1,1".
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).
            ASSIGN cVerdierTmp = REPLACE(cVerdierTmp,CHR(1),",").
            DO iCount = 1 TO NUM-ENTRIES(cRowLabel):
                ASSIGN ENTRY(iCount,cRowLabel) = ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp).
            END.
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,3).
            ASSIGN cColValues = REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
            RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,7).
            ASSIGN cColValues = cColValues + ";" + REPLACE(getHeltal(cVerdierTmp),CHR(1),",").
        END.
    END CASE.
/*     RUN ColVerdier IN h_frapportgrid (OUTPUT cVerdierTmp,2).           */
/*     DO iCount = 1 TO NUM-ENTRIES(cRowLabel):                           */
/*         ASSIGN ENTRY(iCount,cRowLabel) =                               */
/*             ENTRY(iCount,cRowLabel) + " " + ENTRY(iCount,cVerdierTmp). */
/*     END.                                                               */

    RUN d-mschart.w ("Online rapport",cTitel + " " + FI-Dato:SCREEN-VALUE,cRowLabel,cColLabel,cColValues,cColEnable).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel wWin
ON CHOOSE OF B-Excel IN FRAME fMain /* Excel... */
DO:
    RUN VisaIExcel IN h_frapportgrid.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater wWin
ON CHOOSE OF B-Oppdater IN FRAME fMain /* Oppdater */
DO:
  APPLY "VALUE-CHANGED" TO CB-Butik.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-QuickPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-QuickPrint wWin
ON CHOOSE OF B-QuickPrint IN FRAME fMain /* Print */
DO:
  DEFINE VARIABLE cDocName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER    NO-UNDO.
/*   FIND Butiker WHERE Butiker.Butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK NO-ERROR. */
  ASSIGN iPage = DYNAMIC-FUNCTION('getCurrentPage':U)
         cDocName = "Statistikkrapport " + ENTRY(iPage,DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") + " " +
                   /* STRING(FI-Dato:SCREEN-VALUE) + */ 
        (IF INPUT RS-Type = 1 THEN " FORELØBIG" ELSE "") + " " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "") +
         " - Polygon Software AS / PRS   " + STRING(TODAY) + " " + SUBSTR(STRING(TIME,"HH:MM:SS"),1,5).
  PUBLISH "PrintGrid" ("QUICK",cDocName,2,"","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help wWin
ON CHOOSE OF Btn_Help IN FRAME fMain /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
/*    {winhlp.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Print */
DO:
  DEFINE VARIABLE cDocName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER    NO-UNDO.
  FIND Butiker WHERE Butiker.Butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK NO-ERROR.
  ASSIGN iPage = DYNAMIC-FUNCTION('getCurrentPage':U)
      cDocName = "Salgsrapport " + ENTRY(iPage,DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") + " " + CHR(2) +
                 STRING(FI-Dato:SCREEN-VALUE) + " " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "").
/*          cDocName = "Salgsrapport " + ENTRY(iPage,DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") + " " + */
/*                     STRING(FI-Dato:SCREEN-VALUE) + " " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "") +       */
/*          " - Polygon Software AS / PRS   " + STRING(TODAY) + " " + SUBSTR(STRING(TIME,"HH:MM:SS"),1,5).   */
  PUBLISH "PrintGrid" ("XPRINT",cDocName,2,"",cColAlign).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next wWin
ON CHOOSE OF BUTTON-Next IN FRAME fMain /* Neste */
DO:
    DEFINE VARIABLE ii       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTmpbut  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iMinPer  AS INTEGER INIT 9999999 NO-UNDO.
    DEFINE VARIABLE iButik    AS INTEGER    NO-UNDO.
    ASSIGN cTmpbut = IF CB-Butik:SCREEN-VALUE = "0" THEN cTillgbutiker ELSE CB-Butik:SCREEN-VALUE.
            
    DO ii = 1 TO NUM-ENTRIES(ctmpBut):
        FIND FIRST StLinje WHERE StLinje.Butik = INT(ENTRY(ii,cTmpBut)) AND StTypeId = 'AVDELING' AND
                       PerId = 'DAG' AND AarPerLinNr > YEAR(FI-Dato) * 1000 + FI-Dato - DATE(12,31,YEAR(FI-Dato) - 1)
                   AND AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK NO-ERROR.
        IF AVAIL StLinje AND iMinPer > AarPerLinNr THEN
            ASSIGN iMinPer   = AarPerLinNr
                   iButik    = StLinje.Butik.
    END.
    RELEASE StLinje.
    IF iMinPer < 9999999 THEN
        FIND FIRST StLinje WHERE StLinje.Butik = iButik AND StTypeId = 'AVDELING' AND
                       PerId = 'DAG' AND AarPerLinNr = iMinPer NO-LOCK NO-ERROR.
    IF AVAIL StLinje THEN DO:
        ASSIGN FI-Dato = DATE(12,31,StLinje.Aar - 1) + StLinje.PerLinNr
               FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
        APPLY "VALUE-CHANGED" TO CB-Butik.
    END.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok wWin
ON CHOOSE OF BUTTON-Ok IN FRAME fMain /* Ok */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.  
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev wWin
ON CHOOSE OF BUTTON-Prev IN FRAME fMain /* Forrige */
DO:
    DEFINE VARIABLE ii       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTmpbut  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iMaxPer  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iButik    AS INTEGER    NO-UNDO.
    ASSIGN cTmpbut = IF CB-Butik:SCREEN-VALUE = "0" THEN cTillgbutiker ELSE CB-Butik:SCREEN-VALUE.
    DO ii = 1 TO NUM-ENTRIES(ctmpBut):
        FIND LAST StLinje WHERE StLinje.Butik = INT(ENTRY(ii,cTmpBut)) AND StTypeId = 'AVDELING' AND
                       PerId = 'DAG' AND AarPerLinNr < YEAR(FI-Dato) * 1000 + FI-Dato - DATE(12,31,YEAR(FI-Dato) - 1) 
                     AND AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK NO-ERROR.
        IF AVAIL StLinje AND iMaxPer < AarPerLinNr THEN
            ASSIGN iMaxPer   = AarPerLinNr
                   iButik    = StLinje.Butik.
    END.
    RELEASE StLinje.
    IF iMaxPer > 0 THEN
    FIND FIRST StLinje WHERE StLinje.Butik = iButik AND StTypeId = 'AVDELING' AND
                   PerId = 'DAG' AND AarPerLinNr = iMaxPer NO-LOCK NO-ERROR.
    IF AVAIL StLinje THEN DO:
        ASSIGN FI-Dato = DATE(12,31,StLinje.Aar - 1) + StLinje.PerLinNr
               FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
        APPLY "VALUE-CHANGED" TO CB-Butik.
    END.
    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butik wWin
ON VALUE-CHANGED OF CB-Butik IN FRAME fMain /* Butikk */
DO:
    ASSIGN CB-Butik.
    RUN Oppdater.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato wWin
ON RETURN OF FI-Dato IN FRAME fMain /* Dato */
OR "TAB" OF FI-Dato DO:
    ASSIGN INPUT FI-Dato NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feil dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
        RETURN NO-APPLY.
    END.
    APPLY "VALUE-CHANGED" TO CB-Butik.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME QAvd
&Scoped-define SELF-NAME QButiker
&Scoped-define SELF-NAME QHuvGr
&Scoped-define SELF-NAME QLev
&Scoped-define SELF-NAME QSBudDW
&Scoped-define SELF-NAME QSBudMY
&Scoped-define SELF-NAME QSelger
&Scoped-define SELF-NAME QTime
&Scoped-define SELF-NAME QVare
&Scoped-define SELF-NAME QVg
&Scoped-define SELF-NAME RS-Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Type wWin
ON VALUE-CHANGED OF RS-Type IN FRAME fMain
DO:
  ASSIGN /* chTimer:ENABLED = SELF:SCREEN-VALUE = "1" */
         INPUT RS-Type
         FI-Dato:SENSITIVE = SELF:SCREEN-VALUE = "2"
         B-Oppdater:SENSITIVE = SELF:SCREEN-VALUE = "1"
         BUTTON-Prev:SENSITIVE = SELF:SCREEN-VALUE = "2"
         BUTTON-Next:SENSITIVE = BUTTON-Prev:SENSITIVE.
  IF SELF:SCREEN-VALUE = "1" THEN DO:
      ASSIGN FI-Dato = TODAY
             FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
      APPLY "VALUE-CHANGED" TO CB-Butik.
      RETURN NO-APPLY.
  END.
  ELSE DO:
      APPLY "CHOOSE" TO BUTTON-Prev.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */

{src/adm2/windowmn.i}

{lng.i &SDO = "SDO"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
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
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Avdeling|Hovedgrupper|Varegrupper|Leverandør|Selgere|Timesalg|Varesalg|Butikker|Budget D/V|Budget M/Å' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 7.91 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 22.62 , 202.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/frapportgrid.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_frapportgrid ).
       RUN repositionObject IN h_frapportgrid ( 9.33 , 1.40 ) NO-ERROR.
       /* Size in AB:  ( 21.14 , 201.20 ) */

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartFrame h_frapportgrid. */
       RUN addLink ( THIS-PROCEDURE , 'AlignCol':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'FeltVerdier':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'LoadGrid':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'PrintGrid':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'ShowBold':U , h_frapportgrid ).
       RUN addLink ( THIS-PROCEDURE , 'Summer':U , h_frapportgrid ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             B-Artikkelkort:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_frapportgrid ,
             h_folder , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort wWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY FI-Dato RS-Type CB-Butik 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_Help RECT-3 RECT-1 RECT-2 B-Chart RS-Type CB-Butik B-Excel 
         B-Artikkelkort BUTTON-1 B-Oppdater BUTTON-Ok 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FyllTT wWin 
PROCEDURE FyllTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dNetRab      AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cButListe    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dAntUtens       AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumUtensBrutto AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cDataObjekt AS CHARACTER   NO-UNDO.
  DEFINE BUFFER bufStlinje FOR StLinje.

  EMPTY TEMP-TABLE TT_Vg.
  EMPTY TEMP-TABLE TT_Lev.
  EMPTY TEMP-TABLE TT_Selger.
  EMPTY TEMP-TABLE TT_HuvGr.
  EMPTY TEMP-TABLE TT_Avdeling.
  EMPTY TEMP-TABLE TT_VareSalg.
  EMPTY TEMP-TABLE TT_akt_rapp.
  EMPTY TEMP-TABLE TT_butiker.
  EMPTY TEMP-TABLE TT_SBudDW.
  EMPTY TEMP-TABLE TT_SBudMY.
  {sww.i}
  cButliste = cTillgbutiker.
  IF CB-Butik:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "0" THEN
      cButListe = CB-Butik:SCREEN-VALUE.
  ASSIGN iAarPerLinNr = YEAR(FI-Dato) * 1000 + FI-Dato - DATE(12,31,YEAR(FI-Dato) - 1).
  FOR EACH Butiker NO-LOCK:
      /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
      IF cSeAllaiButStat <> "1" AND NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN
          NEXT.
      ASSIGN iAntkunder = 0
             iAntkvitt  = 0.
      RUN TT_Butiker.
  END.

  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(icount,cButliste)) AND StTypeId = 'LEVERAN' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
    /*    PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND StLinje.AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK: */
          FIND TT_Lev WHERE TT_Lev.levnr = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Lev THEN DO:
              FIND LevBas WHERE LevBas.levnr = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Lev.
              ASSIGN
                TT_Lev.levnr         = INT(StLinje.DataObjekt)
                TT_Lev.levnavn       = IF AVAIL Levbas THEN levbas.levnamn ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_Lev.salgbrutto  = TT_Lev.salgbrutto    + StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_Lev.VerdiSolgt  = TT_Lev.VerdiSolgt    + StLinje.VerdiSolgt
            TT_Lev.AntSolgt    = TT_Lev.AntSolgt      + StLinje.AntSolgt
            TT_Lev.VerdiRabatt = TT_Lev.VerdiRabatt   + StLinje.VerdiRabatt
            TT_Lev.DBkr        = TT_Lev.DBkr          + StLinje.VerdiSolgt - StLinje.VVarekost
/*             TT_Lev.DB%           = IF TT_Lev.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_Lev.DBkr / TT_Lev.VerdiSolgt * 100,2) */
    /*      TT_Lev.DB%           = IF TT_Lev.VerdiSolgt = 0 THEN 0 ELSE ROUND(TT_Lev.DBkr / TT_Lev.VerdiSolgt * 100,2) */
            TT_Lev.mvaverdi      = TT_Lev.mvaverdi      + StLinje.MvaVerdi
            TT_Lev.vvarekost     = TT_Lev.vvarekost     + StLinje.VVarekost
            TT_Lev.GjenkjopAnt   = TT_Lev.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Lev.GjenkjopVerdi = TT_Lev.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.
  FOR EACH TT_Lev:
      TT_Lev.DB%           = IF TT_Lev.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_Lev.DBkr / TT_Lev.VerdiSolgt * 100,2).
  END.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(icount,cButliste)) AND StTypeId = 'SELGER' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
    /*    PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND StLinje.AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK: */
          /* Skal ikke ha med selgere som ikke har solgt. */
          IF StLinje.VerdiSolgt = 0 AND StLinje.ReklVerdi = 0 AND StLinje.GjenkjopVerdi = 0 THEN 
            NEXT.
          FIND TT_Selger WHERE TT_Selger.selgernr = INTE(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Selger THEN DO:
              FIND Selger WHERE Selger.selgernr = INTE(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Selger.
              ASSIGN
                TT_Selger.selgernr  = INT(StLinje.DataObjekt)
                TT_Selger.navn      = IF AVAIL Selger THEN selger.navn ELSE "?? " + StLinje.Beskrivelse.
          END.
          IF cHitrate = "1" THEN DO:
            ASSIGN dAntUtens       = 0
                   dSumUtensBrutto = 0.
            DO i2 = 1 TO NUM-ENTRIES(cVgList):
                FOR EACH bufStlinje WHERE bufStLinje.Butik = INT(ENTRY(icount,cButliste)) AND 
                                          bufStlinje.sttypeid   = "SELGER-VG"    AND
                                          bufStlinje.perid      = "DAG"       AND
                                          bufStlinje.aarperlinnr = iAarPerLinNr AND
                                          bufStlinje.dataobjekt = StLinje.Dataobjekt + CHR(1) + STRING(INT(ENTRY(i2,cVgList)),"999999")
                                                                USE-INDEX AarPerLinNr NO-LOCK:

                    ASSIGN dAntUtens       = dAntUtens    + bufStlinje.antsolgt
                           dSumUtensBrutto = dSumUtensBrutto + bufStlinje.VerdiSolgt + bufStlinje.MvaVerdi.
/*                     ASSIGN tt_hitrate.antutens    = tt_hitrate.antutens    + stlinje.antsolgt                       */
/*                            tt_hitrate.utensbrutto = tt_hitrate.utensbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi. */
                END.
            END.

          END.
          ASSIGN
            TT_Selger.salgbrutto  = TT_Selger.salgbrutto    + StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_Selger.VerdiSolgt  = TT_Selger.VerdiSolgt    + StLinje.VerdiSolgt
            TT_Selger.AntSolgt    = TT_Selger.AntSolgt      + StLinje.AntSolgt
            TT_Selger.VerdiRabatt = TT_Selger.VerdiRabatt   + StLinje.VerdiRabatt
            TT_Selger.DBkr        = TT_Selger.DBkr          + StLinje.VerdiSolgt - StLinje.VVarekost
            TT_Selger.mvaverdi      = TT_Selger.mvaverdi      + StLinje.MvaVerdi
            TT_Selger.vvarekost     = TT_Selger.vvarekost     + StLinje.VVarekost
            TT_Selger.GjenkjopAnt   = TT_Selger.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Selger.GjenkjopVerdi = TT_Selger.GjenkjopVerdi + StLinje.GjenkjopVerdi.
          IF dAntUtens > 0 AND TT_Selger.AntSolgt > 0 THEN
              ASSIGN TT_Selger.AntUtens = TT_Selger.AntUtens + dAntUtens
                      TT_Selger.SumUtensBrutto  = TT_Selger.SumUtensBrutto + dSumUtensBrutto.
/* ASSIGN TT_Selger.Hitrate = ROUND(dAntUtens / TT_Selger.AntSolgt * 100,1)
       TT_Selger.Merfsg  = ROUND(dSumUtensBrutto / TT_Selger.salgbrutto * 100,1). */

      END.
  END.
  FOR EACH TT_Selger:
      TT_Selger.DB%           = IF TT_Selger.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_Selger.DBkr / TT_Selger.VerdiSolgt * 100,2).
      IF TT_Selger.AntUtens > 0 THEN
            ASSIGN TT_Selger.Hitrate = ROUND(TT_Selger.AntUtens / TT_Selger.AntSolgt * 100,1)
                   TT_Selger.Merfsg  = ROUND(TT_Selger.SumUtensBrutto / TT_Selger.salgbrutto * 100,1).
  END.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH kasse NO-LOCK WHERE kasse.butikknr = INT(ENTRY(icount,cButliste)) AND kasse.kassenr < 99:
          FOR EACH Bonghode WHERE bonghode.butikknr = kasse.butikknr AND
                                  bonghode.gruppenr = 1              AND
                                  bonghode.kassenr  = kasse.kassenr  AND
                                  bonghode.dato     = FI-Dato        NO-LOCK:
              IF bonghode.belop <= 0 OR bonghode.makulert = 2 THEN
                  NEXT.
              IF CAN-FIND(FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.ttid = 1) THEN DO:
                  FIND TT_Selger WHERE TT_Selger.SelgerNr = bonghode.selgernr NO-ERROR.
                  IF AVAIL TT_Selger THEN
                      TT_Selger.AntKunder = TT_Selger.AntKunder + 1.
              END.
          END.
      END.
  END.

  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(icount,cButliste)) AND StTypeId = 'VAREGR' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
    /*    PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND StLinje.AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK: */
          FIND TT_Vg WHERE TT_Vg.vg = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Vg THEN DO:
              FIND VarGr WHERE VarGr.Vg = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Vg.
              ASSIGN
                TT_Vg.vg            = INT(StLinje.DataObjekt)
                TT_Vg.VgBeskr       = IF AVAIL VarGr THEN VarGr.vgbeskr ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_Vg.salgbrutto    = TT_Vg.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_Vg.VerdiSolgt    = TT_Vg.VerdiSolgt  + StLinje.VerdiSolgt
            TT_Vg.AntSolgt      = TT_Vg.AntSolgt    + StLinje.AntSolgt
            TT_Vg.VerdiRabatt   = TT_Vg.VerdiRabatt + StLinje.VerdiRabatt
            TT_Vg.DBkr          = TT_Vg.DBkr        + StLinje.VerdiSolgt - StLinje.VVarekost
            TT_Vg.mvaverdi      = TT_Vg.mvaverdi      + StLinje.MvaVerdi
            TT_Vg.vvarekost     = TT_Vg.vvarekost     + StLinje.VVarekost
            TT_Vg.GjenkjopAnt   = TT_Vg.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Vg.GjenkjopVerdi = TT_Vg.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.
  FOR EACH TT_Vg:
      TT_Vg.DB%           = IF TT_Vg.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_Vg.DBkr / TT_Vg.VerdiSolgt * 100,2).
  END.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(icount,cButliste)) AND StTypeId = 'HOVEDGR' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
    /*    PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND StLinje.AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK: */
          FIND TT_HuvGr WHERE TT_HuvGr.hg = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_HuvGr THEN DO:
              FIND HuvGr WHERE HuvGr.Hg = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_HuvGr.
              ASSIGN
                TT_HuvGr.hg            = INT(StLinje.DataObjekt)
                TT_HuvGr.HgBeskr       = IF AVAIL HuvGr THEN HuvGr.hgbeskr ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_HuvGr.salgbrutto    = TT_HuvGr.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_HuvGr.VerdiSolgt    = TT_HuvGr.VerdiSolgt  + StLinje.VerdiSolgt
            TT_HuvGr.AntSolgt      = TT_HuvGr.AntSolgt    + StLinje.AntSolgt
            TT_HuvGr.VerdiRabatt   = TT_HuvGr.VerdiRabatt + StLinje.VerdiRabatt
            TT_HuvGr.DBkr          = TT_HuvGr.DBkr        + StLinje.VerdiSolgt - StLinje.VVarekost
/*             TT_HuvGr.DB%           = IF TT_HuvGr.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_HuvGr.DBkr / TT_HuvGr.VerdiSolgt * 100,2) */
    /*      TT_HuvGr.DB%           = IF TT_HuvGr.VerdiSolgt = 0 THEN 0 ELSE ROUND(TT_HuvGr.DBkr / TT_HuvGr.VerdiSolgt * 100,2) */
            TT_HuvGr.mvaverdi      = TT_HuvGr.mvaverdi      + StLinje.MvaVerdi
            TT_HuvGr.vvarekost     = TT_HuvGr.vvarekost     + StLinje.VVarekost
            TT_HuvGr.GjenkjopAnt   = TT_HuvGr.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_HuvGr.GjenkjopVerdi = TT_HuvGr.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.
  FOR EACH TT_HuvGr:
      TT_HuvGr.DB%           = IF TT_HuvGr.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_HuvGr.DBkr / TT_HuvGr.VerdiSolgt * 100,2).
  END.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(icount,cButliste)) AND StTypeId = 'AVDELING' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
    /*    PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND StLinje.AntSolgt <> 0 USE-INDEX AarPerLinNr NO-LOCK: */
          FIND TT_Avdeling WHERE TT_Avdeling.avdelingnr    = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Avdeling THEN DO:
              FIND Avdeling WHERE Avdeling.AvdelingNr = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Avdeling.
              ASSIGN
                TT_Avdeling.avdelingnr    = INT(StLinje.DataObjekt)
                TT_Avdeling.AvdelingNavn  = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_Avdeling.salgbrutto    = TT_Avdeling.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_Avdeling.VerdiSolgt    = TT_Avdeling.VerdiSolgt  + StLinje.VerdiSolgt
            TT_Avdeling.AntSolgt      = TT_Avdeling.AntSolgt    + StLinje.AntSolgt
            TT_Avdeling.VerdiRabatt   = TT_Avdeling.VerdiRabatt + StLinje.VerdiRabatt
            TT_Avdeling.DBkr          = TT_Avdeling.DBkr        + StLinje.VerdiSolgt - StLinje.VVarekost
/*             TT_Avdeling.DB%           = IF TT_Avdeling.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_Avdeling.DBkr / TT_Avdeling.VerdiSolgt * 100,2) */
    /*      TT_Avdeling.DB%           = IF TT_Avdeling.VerdiSolgt = 0 THEN 0 ELSE ROUND(TT_Avdeling.DBkr / TT_Avdeling.VerdiSolgt * 100,2) */
            TT_Avdeling.mvaverdi      = TT_Avdeling.mvaverdi      + StLinje.MvaVerdi
            TT_Avdeling.vvarekost     = TT_Avdeling.vvarekost     + StLinje.VVarekost
            TT_Avdeling.GjenkjopAnt   = TT_Avdeling.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Avdeling.GjenkjopVerdi = TT_Avdeling.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.
  FOR EACH TT_Avdeling:
      TT_Avdeling.DB%           = IF TT_Avdeling.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_Avdeling.DBkr / TT_Avdeling.VerdiSolgt * 100,2).
  END.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(icount,cButliste)) AND StTypeId = 'ARTIKKEL' AND
    /*       PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK: */
       PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND (StLinje.AntSolgt <> 0 OR (StLinje.AntSolgt = 0 AND StLinje.GjenkjopAnt <> 0)) USE-INDEX AarPerLinNr NO-LOCK:
          FIND TT_VareSalg WHERE TT_VareSalg.ArtikkelNr    = DECI(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_VareSalg THEN DO:
              RELEASE Farg.
              RELEASE Varemerke.
              FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              IF AVAIL ArtBas THEN DO:
                  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
                  FIND varemerke OF artbas NO-LOCK NO-ERROR.
              END.
              CREATE TT_VareSalg.
              ASSIGN
                TT_VareSalg.ArtikkelNr    = DECI(StLinje.DataObjekt)
                TT_VareSalg.LevKod        = IF AVAIL ArtBas THEN ArtBas.LevKod ELSE " "
                TT_VareSalg.Farg          = IF AVAIL Farg THEN Farg.FarBeskr ELSE " "
                TT_VareSalg.Beskr         = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "?? " + StLinje.Beskrivelse
                TT_VareSalg.Varemerke    = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE "".
          END.
          ASSIGN
            TT_VareSalg.salgbrutto    = TT_VareSalg.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_VareSalg.VerdiSolgt    = TT_VareSalg.VerdiSolgt  + StLinje.VerdiSolgt
            TT_VareSalg.AntSolgt      = TT_VareSalg.AntSolgt    + StLinje.AntSolgt
            TT_VareSalg.VerdiRabatt   = TT_VareSalg.VerdiRabatt + StLinje.VerdiRabatt
            TT_VareSalg.DBkr          = TT_VareSalg.DBkr        + StLinje.VerdiSolgt - StLinje.VVarekost
/*             TT_VareSalg.DB%           = IF TT_VareSalg.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_VareSalg.DBkr / TT_VareSalg.VerdiSolgt * 100,2) */
    /*      TT_VareSalg.DB%           = IF TT_VareSalg.VerdiSolgt = 0 THEN 0 ELSE ROUND(TT_VareSalg.DBkr / TT_VareSalg.VerdiSolgt * 100,2) */
            TT_VareSalg.mvaverdi      = TT_VareSalg.mvaverdi      + StLinje.MvaVerdi
            TT_VareSalg.vvarekost     = TT_VareSalg.vvarekost     + StLinje.VVarekost
            TT_VareSalg.GjenkjopAnt   = TT_VareSalg.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_VareSalg.GjenkjopVerdi = TT_VareSalg.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.
  FOR EACH TT_VareSalg:
      TT_VareSalg.DB%           = IF TT_VareSalg.VerdiSolgt <= 0 THEN 0 ELSE ROUND(TT_VareSalg.DBkr / TT_VareSalg.VerdiSolgt * 100,2).
  END.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FOR EACH akt_rapp NO-LOCK WHERE akt_rapp.dato = FI-Dato AND akt_rapp.butik = INT(ENTRY(icount,cButliste)) AND
             (akt_rapp.oms_verd <> 0 OR akt_rapp.ant_kunder <> 0):
          FIND FIRST TT_akt_rapp WHERE TT_akt_rapp.tid_txt = akt_rapp.tid_txt NO-ERROR.
          IF AVAIL TT_akt_rapp THEN DO:
              ASSIGN
              TT_akt_rapp.ant_kunder = TT_akt_rapp.ant_kunder + akt_rapp.ant_kunder
              TT_akt_rapp.ant_kvitto = TT_akt_rapp.ant_kvitto + akt_rapp.ant_kvitto
              TT_akt_rapp.mva_kr     = TT_akt_rapp.mva_kr     + akt_rapp.mva_kr
              TT_akt_rapp.oms_ant    = TT_akt_rapp.oms_ant    + akt_rapp.oms_ant
              TT_akt_rapp.oms_verd   = TT_akt_rapp.oms_verd   + akt_rapp.oms_verd
              TT_akt_rapp.svk        = TT_akt_rapp.svk        + akt_rapp.svk
              TT_akt_rapp.verd_ret   = TT_akt_rapp.verd_ret   + akt_rapp.verd_ret
              TT_akt_rapp.ant_ret    = TT_akt_rapp.ant_ret    + akt_rapp.ant_ret.
          END.
          ELSE DO:
              CREATE TT_akt_rapp.
              ASSIGN
                 TT_akt_rapp.dato       = akt_rapp.dato
                 TT_akt_rapp.ant_kunder = akt_rapp.ant_kunder
                 TT_akt_rapp.ant_kvitto = akt_rapp.ant_kvitto
                 TT_akt_rapp.butik      = akt_rapp.butik
                 TT_akt_rapp.kasse      = akt_rapp.kasse
                 TT_akt_rapp.mnd        = akt_rapp.mnd
                 TT_akt_rapp.mva_kr     = akt_rapp.mva_kr
                 TT_akt_rapp.oms_verd   = akt_rapp.oms_verd
                 TT_akt_rapp.svk        = akt_rapp.svk
                 TT_akt_rapp.tid        = akt_rapp.tid
                 TT_akt_rapp.tid_txt    = akt_rapp.tid_txt
                 TT_akt_rapp.verd_ret   = akt_rapp.verd_ret
                 TT_akt_rapp.ant_ret    = akt_rapp.ant_ret.
          END.
      END.
  END.
  RUN SalgsbudsjettDWFlik (cButListe).
  RUN SalgsbudsjettMYFlik (cButListe).
  FOR EACH TT_akt_rapp:
      ASSIGN 
             TT_akt_rapp.ant_kunder = ABS(TT_akt_rapp.ant_kunder)
             TT_akt_rapp.ant_kvitto = ABS(TT_akt_rapp.ant_kvitto)
             TT_akt_rapp.oms_ant    = ABS(TT_akt_rapp.oms_ant)
             TT_akt_rapp.ant_ret    = ABS(TT_akt_rapp.ant_ret)
             TT_akt_rapp.bruttosalg = TT_akt_rapp.oms_verd + TT_akt_rapp.mva_kr
             TT_akt_rapp.DBKr       = TT_akt_rapp.oms_verd - TT_akt_rapp.svk
             TT_akt_rapp.DB%        = IF TT_akt_rapp.oms_verd = 0 THEN 0 ELSE ROUND(TT_akt_rapp.DBKr / TT_akt_rapp.oms_verd * 100,2).
  END.
  {swn.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getWeekOffset wWin 
PROCEDURE getWeekOffset :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:    
                                  1,2,3,4,5,6,7 
                        wdString  7,1,2,3,4,5,6                                                                                                   
        ------------------------------------------------------------------------------*/

  ASSIGN
    cDayString = '7,1,2,3,4,5,6'
    iWdOff     = WEEKDAY(FI-Dato)
    iWdOff     = INT(ENTRY(iWdOff,cDayString)) - 1
    . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB wWin 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iMaxLen        AS INTEGER INIT 0 NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cBut           AS CHARACTER   NO-UNDO.
    
    FIND LAST Butiker NO-LOCK NO-ERROR.
    
    DO icount = 1 TO NUM-ENTRIES(cTillgbutiker):
        FIND butiker WHERE butiker.butik = INT(ENTRY(icount,cTillgbutiker)) NO-ERROR.
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                   STRING(Butiker.Butik,"zzzzz9") + " " + Butiker.Butnamn + "," + STRING(Butiker.Butik).
    END.
    ASSIGN CB-Butik:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = (IF NUM-ENTRIES(cTillgbutiker) > 1 THEN "Alle tilgjengelige,0," ELSE "") + cListItemPairs
           CB-Butik = INT(ENTRY(2,CB-Butik:LIST-ITEM-PAIRS)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS
         iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS.

  ASSIGN FI-Dato = TODAY.
  
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
  FIND BrukerGrp WHERE BrukerGrp.BrGrpNr = bruker.BrGrpNr NO-LOCK.

  FOR EACH ButikkTilgang OF BrukerGrp NO-LOCK:
      IF CAN-FIND(FIRST Kasse WHERE Kasse.Butik = ButikkTilgang.Butik) THEN
          cTillgbutiker = cTillgbutiker + (IF cTillgbutiker <> "" THEN "," ELSE "") + STRING(ButikkTilgang.Butik).
  END.
  IF cTillgbutiker = "" THEN DO:
      MESSAGE "Bruker mangler butikktilgang (Brukergruppe)."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  {syspara.i 210 275 1 cTmp}
  /* Bara JF tillsvidare */
  IF cTmp <> "" THEN DO:
      cHitrate = "1".
      FOR EACH vargr WHERE vargr.hg = 5 NO-LOCK.
          IF NOT CAN-DO(cVgList,STRING(vargr.vg)) THEN
              cVgList = cVgList + (IF cVgList <> "" THEN "," ELSE "") + STRING(vargr.vg).
      END.
  END.
  
  ASSIGN cUserDefaultBut = IF CAN-DO(cTillgbutiker,STRING(Bruker.ButikkNr)) THEN STRING(Bruker.ButikkNr) ELSE ENTRY(1,cTillgbutiker).

  /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
  {syspara.i 6 10 1 cSeAllaiButStat}
  RUN InitCB.
  
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker AND CAN-DO("SE,SVE",Bruker.Lng) THEN 
  DO:
      IF NUM-ENTRIES(DYNAMIC-FUNCTION('getFolderLabels':U IN h_folder),"|") =
         NUM-ENTRIES(cSEfolder,"|") THEN
          DYNAMIC-FUNCTION('setFolderLabels':U IN h_folder,
     INPUT cSEfolder /* CHARACTER */).
/*       &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN                      */
/*           RUN oversettGrid2SE.p PERSISTENT SET hOversettGrid2SE. */
/*           THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hOversettGrid2SE).  */
/*       &ENDIF                                                     */
  END.
  
  RUN SUPER.
  
  CB-Butik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cUserDefaultBut.
  RUN SetDivResize.

  /* Code placed here will execute AFTER standard behavior.    */
/*     ASSIGN cFilKatalog = "\home\Lindbak\Tmp\"                    */
/*            cTmpFile    = SESSION:TEMP-DIRECTORY + STRING(TIME)   */
/*            cCopyFil    = SESSION:TEMP-DIRECTORY + "copyfil.txt". */
/*     RUN FyllTT. */
    RUN selectPage (1).
    APPLY "VALUE-CHANGED" TO CB-Butik IN FRAME {&FRAME-NAME}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Oppdater wWin 
PROCEDURE Oppdater :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF RS-Type:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" AND FI-Dato <> TODAY THEN DO:
        ASSIGN FI-Dato = TODAY
               FI-Dato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
    END.
    RUN FyllTT.
    {swn.i}
    RUN VisFlip (DYNAMIC-FUNCTION('getCurrentPage':U)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext wWin 
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

    ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,cRettning).      
        IF cArtikkelNr = "" THEN
          RETURN.
        PUBLISH "ByttArtikkel" (DECI(cArtikkelNr)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SalgsbudsjettDWFlik wWin 
PROCEDURE SalgsbudsjettDWFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
        DEFINE INPUT  PARAMETER cButListe AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
        DEFINE VARIABLE dDateLoop AS DATE        NO-UNDO.

  RUN getWeekOffset. /* Ukedag offset */
/*   BUDSJETT Butiker NO-LOCK: */
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
/*       IF cSeAllaiButStat <> "1" AND NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN */
/*           NEXT.                                                                      */
      FIND Butiker WHERE Butiker.butik = INT(ENTRY(icount,cButliste)) NO-LOCK NO-ERROR.      
      IF NOT AVAIL Butiker THEN
          NEXT.
      IF AVAILABLE TT_SBudDW THEN RELEASE TT_SBudDW.  
          
      FIND FIRST SBudHode NO-LOCK WHERE 
          SBudHode.ButikkNr = Butiker.Butik AND 
          SBudHode.Aar      = YEAR(FI-Dato) AND 
          SBudHode.Aktiv    = TRUE NO-ERROR.
      /* Dagen i dag */       
      FOR EACH StLinje WHERE 
          StLinje.Butik         = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
          StLinje.AarPerLinNr   = iAarPerLinNr 
          USE-INDEX AarPerLinNr NO-LOCK:

         FIND FIRST TT_SBudDW WHERE 
             TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudDW THEN 
           DO:                 
             CREATE TT_SBudDW.
             ASSIGN 
                 TT_SBudDW.butik   = Butiker.butik
                 TT_SBudDW.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudDW.DAGsalgbrutto    = StLinje.VerdiSolgt + StLinje.MvaVerdi.
      END.
      /* om ingen budget skall dessa uppdateras */
      IF NOT AVAIL SBudHode AND AVAIL TT_SBudDW THEN
          ASSIGN
/*             TT_SBudDW.DAGSalgBudsjett  = TT_SBudDW.DAGSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0) */
            TT_SBudDW.DAGSalgAvvikKr   = TT_SBudDW.DAGsalgbrutto - TT_SBudDW.DAGSalgBudsjet.
/*             TT_SBudDW.DAGSalgAvvikproc = ROUND((TT_SBudDW.DAGSalgAvvikKr / TT_SBudDW.DAGSalgBudsjett) * 100,2) */
            .

      IF AVAILABLE SBudHode THEN
      FOR EACH SBudDag NO-LOCK WHERE 
               SBudDag.SBudId    = SBudHode.SBudId AND 
               SBudDag.AarMnd    = INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99")) AND 
               SBudDag.AarMndDag = INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99") + STRING(DAY(FI-Dato),"99")):
         IF NOT AVAILABLE TT_SBudDW THEN 
             FIND FIRST TT_SBudDW WHERE 
                 TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudDW THEN 
           DO:                 
             CREATE TT_SBudDW.
             ASSIGN 
                 TT_SBudDW.butik   = Butiker.butik
                 TT_SBudDW.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudDW.DAGSalgBudsjett  = TT_SBudDW.DAGSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
           TT_SBudDW.DAGSalgAvvikKr   = TT_SBudDW.DAGsalgbrutto - TT_SBudDW.DAGSalgBudsjett  
           TT_SBudDW.DAGSalgAvvikproc = ROUND((TT_SBudDW.DAGSalgAvvikKr / TT_SBudDW.DAGSalgBudsjett) * 100,2)
           .
      END.       
      
      /* Hitil denne uken */       
      FOR EACH StLinje WHERE 
          StLinje.Butik            = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
          StLinje.AarPerLinNr   >= iAarPerLinNr - iWdOff AND  
          StLinje.AarPerLinNr   <= iAarPerLinNr  
          USE-INDEX AarPerLinNr NO-LOCK:
              
          IF NOT AVAILABLE TT_SBudDW THEN 
              FIND FIRST TT_SBudDW WHERE 
                  TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
          IF NOT AVAILABLE TT_SBudDW THEN 
          DO:                 
              CREATE TT_SBudDW.
              ASSIGN 
                  TT_SBudDW.butik   = Butiker.butik
                  TT_SBudDW.butnamn = Butiker.butnamn.
          END.
          ASSIGN
            TT_SBudDW.UKEsalgbrutto    = TT_SBudDW.UKEsalgbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi
            .
      END.
      /* om ingen budget skall dessa uppdateras */
      IF NOT AVAIL SBudHode AND AVAIL TT_SBudDW THEN DO:
          ASSIGN
/*             TT_SBudDW.UKESalgBudsjett  = TT_SBudDW.UKESalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0) */
            TT_SBudDW.UKESalgAvvikKr   = TT_SBudDW.UKEsalgbrutto - TT_SBudDW.UKESalgBudsjett.
/*             TT_SBudDW.UKESalgAvvikproc = ROUND((TT_SBudDW.UKESalgAvvikKr / TT_SBudDW.UKESalgBudsjett) * 100,2) */
      END.

      IF AVAILABLE SBudHode THEN DO dDateLoop = FI-Dato - iWdOff TO FI-Dato:
          FOR EACH SBudDag NO-LOCK WHERE SBudDag.SBudId     = SBudHode.SBudId AND 
                                         SBudDag.AarMnd     = INT(STRING(YEAR(dDateLoop),"9999") + STRING(MONTH(dDateLoop),"99")) AND 
                                         SBudDag.AarMndDag = INT(STRING(YEAR(dDateLoop),"9999") + STRING(MONTH(dDateLoop),"99") + STRING(DAY(dDateLoop),"99")):
             FIND FIRST TT_SBudDW WHERE TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
             IF NOT AVAILABLE TT_SBudDW THEN DO:                 
                 CREATE TT_SBudDW.
                 ASSIGN TT_SBudDW.butik   = Butiker.butik
                     TT_SBudDW.butnamn = Butiker.butnamn.
             END.
             ASSIGN TT_SBudDW.UKESalgBudsjett  = TT_SBudDW.UKESalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
                    TT_SBudDW.UKESalgAvvikKr   = TT_SBudDW.UKEsalgbrutto - TT_SBudDW.UKESalgBudsjett 
                    TT_SBudDW.UKESalgAvvikproc = ROUND((TT_SBudDW.UKESalgAvvikKr / TT_SBudDW.UKESalgBudsjett) * 100,2).
          END.
      END.
      
/*       IF AVAILABLE SBudHode THEN FOR EACH SBudDag NO-LOCK WHERE                                                                                */
/*                SBudDag.SBudId     = SBudHode.SBudId AND                                                                                        */
/*                SBudDag.AarMnd     = INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99")) AND                                        */
/*                SBudDag.AarMndDag >= INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99") +  STRING(DAY(FI-Dato -  iWdOff),"99")) AND */
/*                SBudDag.AarMndDag <= INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99") + STRING(DAY(FI-Dato),"99")):               */
/*          IF NOT AVAILABLE TT_SBudDW THEN                                                                                                       */
/*              FIND FIRST TT_SBudDW WHERE                                                                                                        */
/*                  TT_SBudDW.Butik = Butiker.Butik NO-ERROR.                                                                                     */
/*          IF NOT AVAILABLE TT_SBudDW THEN                                                                                                       */
/*            DO:                                                                                                                                 */
/*              CREATE TT_SBudDW.                                                                                                                 */
/*              ASSIGN                                                                                                                            */
/*                  TT_SBudDW.butik   = Butiker.butik                                                                                             */
/*                  TT_SBudDW.butnamn = Butiker.butnamn.                                                                                          */
/*            END.                                                                                                                                */
/*          ASSIGN                                                                                                                                */
/*            TT_SBudDW.UKESalgBudsjett  = TT_SBudDW.UKESalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)                    */
/*            TT_SBudDW.UKESalgAvvikKr   = TT_SBudDW.UKEsalgbrutto - TT_SBudDW.UKESalgBudsjett                                                    */
/*            TT_SBudDW.UKESalgAvvikproc = ROUND((TT_SBudDW.UKESalgAvvikKr / TT_SBudDW.UKESalgBudsjett) * 100,2).                                 */
/*       END.                                                                                                                                     */
      
  END.
  FOR EACH TT_SBudDW:
      ASSIGN 
      TT_SBudDW.DAGSalgBudsjett  = ROUND(TT_SBudDW.DAGSalgBudsjett,0)
      TT_SBudDW.DAGsalgbrutto    = ROUND(TT_SBudDW.DAGsalgbrutto,0)
      TT_SBudDW.DAGSalgAvvikKr   = TT_SBudDW.DAGsalgbrutto - TT_SBudDW.DAGSalgBudsjett  
      TT_SBudDW.DAGSalgAvvikproc = IF TT_SBudDW.DAGSalgBudsjett > 0 THEN ROUND((TT_SBudDW.DAGSalgAvvikKr / TT_SBudDW.DAGSalgBudsjett) * 100,2) ELSE 0
      TT_SBudDW.UKESalgBudsjett  = ROUND(TT_SBudDW.UKESalgBudsjett,0)
      TT_SBudDW.UKEsalgbrutto    = ROUND(TT_SBudDW.UKEsalgbrutto,0)
      TT_SBudDW.UKESalgAvvikKr   = TT_SBudDW.UKEsalgbrutto - TT_SBudDW.UKESalgBudsjett 
      TT_SBudDW.UKESalgAvvikproc = IF TT_SBudDW.UKESalgBudsjett > 0 THEN ROUND((TT_SBudDW.UKESalgAvvikKr / TT_SBudDW.UKESalgBudsjett) * 100,2) ELSE 0.
  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SalgsbudsjettMYFlik wWin 
PROCEDURE SalgsbudsjettMYFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
        DEFINE INPUT  PARAMETER cButListe AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.

/*   BUDSJETT Butiker NO-LOCK: */
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
/*       IF cSeAllaiButStat <> "1" AND NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN */
/*           NEXT.                                                                      */
      FIND Butiker WHERE Butiker.butik = INT(ENTRY(icount,cButliste)) NO-LOCK NO-ERROR.      
      IF NOT AVAIL Butiker THEN
          NEXT.

      IF AVAILABLE TT_SBudMY THEN RELEASE TT_SBudMY.  
          
      FIND FIRST SBudHode NO-LOCK WHERE 
          SBudHode.ButikkNr = Butiker.Butik AND 
          SBudHode.Aar      = YEAR(FI-Dato) AND 
          SBudHode.Aktiv    = TRUE NO-ERROR.

      /* Hitil i måneden */       
      FOR EACH StLinje WHERE 
          StLinje.Butik         = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
                                   /* År */             /* Første dag i mnd    */   /* Første dag i året      */
          StLinje.AarPerLinNr   >= YEAR(FI-Dato) * 1000 + ((FI-Dato - DAY(FI-Dato) + 1) - DATE(12,31,YEAR(FI-Dato) - 1)) AND 
                                   /* Dagens dato */
          StLinje.AarPerLinNr   <= iAarPerLinNr  
          USE-INDEX AarPerLinNr NO-LOCK:
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudMY.MNDsalgbrutto    = TT_SBudMY.MNDsalgbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi
           .
      END.
      /* om vi inte har budget skall dessa uppdateras */
      IF NOT AVAIL SBudHode AND AVAIL TT_SBudMY THEN
          ASSIGN
/*             TT_SBudMY.MNDSalgBudsjett  = TT_SBudMY.MNDSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0) */
            TT_SBudMY.MNDSalgAvvikKr   = TT_SBudMY.MNDsalgbrutto - TT_SBudMY.MNDSalgBudsjett.
/*             TT_SBudMY.MNDSalgAvvikproc = ROUND((TT_SBudMY.MNDSalgAvvikKr / TT_SBudMY.MNDSalgBudsjett) * 100,2) */
      
      IF AVAILABLE SBudHode THEN
      FOR EACH SBudDag NO-LOCK WHERE 
               SBudDag.SBudId    = SBudHode.SBudId AND 
               SBudDag.AarMnd    = INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99")) AND 
               SBudDag.AarMndDag >= INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99") + '01') AND 
               SBudDag.AarMndDag <= INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99") + STRING(DAY(FI-Dato),"99")):
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.         
         ASSIGN
           TT_SBudMY.MNDSalgBudsjett  = TT_SBudMY.MNDSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
           TT_SBudMY.MNDSalgAvvikKr   = TT_SBudMY.MNDsalgbrutto - TT_SBudMY.MNDSalgBudsjett
           TT_SBudMY.MNDSalgAvvikproc = ROUND((TT_SBudMY.MNDSalgAvvikKr / TT_SBudMY.MNDSalgBudsjett) * 100,2)
           .
/*       IF bLogg THEN                                                                          */
/*           RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: Hitil i måneden '              */
/*                                 + ' BudId: '  + STRING(SBudDag.SBudId)                       */
/*                                 + ' AaMnd: '  + STRING(SBudDag.AarMnd)                       */
/*                                 + ' AaMndDag: '  + STRING(SBudDag.AarMndDag)                 */
/*                                 + ' Butikk: ' + STRING(TT_SBudMY.butik)                      */
/*                                 + ' Navn: '   + TT_SBudMY.butnamn                            */
/*                                 + ' MNDSalgBudsjett: '  + STRING(TT_SBudMY.MNDSalgBudsjett)  */
/*                                 + ' MNDSalgAvvikKr: '   + STRING(TT_SBudMY.MNDSalgAvvikKr)   */
/*                                 + ' MNDSalgAvvikproc: ' + STRING(TT_SBudMY.MNDSalgAvvikproc) */
/*                                 ).                                                           */
      END.       
           
      /* Hitil dette år */       
      FOR EACH StLinje WHERE 
          StLinje.Butik         = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
          StLinje.AarPerLinNr   >= YEAR(FI-Dato) * 1000 + 1 AND 
                                   /* Dagens dato */
          StLinje.AarPerLinNr   <= iAarPerLinNr  
          USE-INDEX AarPerLinNr NO-LOCK:
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudMY.AARsalgbrutto    = TT_SBudMY.AARsalgbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi
           .
      END.
      /* om vi inte har budget skall dessa uppdateras */
      IF NOT AVAIL SBudHode AND AVAIL TT_SBudMY THEN
          ASSIGN
/*             TT_SBudMY.AARSalgBudsjett  = TT_SBudMY.AARSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0) */
            TT_SBudMY.AARSalgAvvikKr   = TT_SBudMY.AARsalgbrutto - TT_SBudMY.AARSalgBudsjett.
/*             TT_SBudMY.AARSalgAvvikproc = ROUND((TT_SBudMY.AARSalgAvvikKr / TT_SBudMY.AARSalgBudsjett) * 100,2) */
      
      IF AVAILABLE SBudHode THEN
      FOR EACH SBudDag NO-LOCK WHERE 
               SBudDag.SBudId    = SBudHode.SBudId AND 
               SBudDag.AarMnd    >= INT(STRING(YEAR(FI-Dato),"9999") + '01') AND 
               SBudDag.AarMnd    <= INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99")) AND 
               SBudDag.AarMndDag >= INT(STRING(YEAR(FI-Dato),"9999") + '0101') AND 
               SBudDag.AarMndDag <= INT(STRING(YEAR(FI-Dato),"9999") + STRING(MONTH(FI-Dato),"99") + STRING(DAY(FI-Dato),"99")):
         
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.
         
         ASSIGN
           TT_SBudMY.AARSalgBudsjett  = TT_SBudMY.AARSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
           TT_SBudMY.AARSalgAvvikKr   = TT_SBudMY.AARsalgbrutto - TT_SBudMY.AARSalgBudsjett
           TT_SBudMY.AARSalgAvvikproc = ROUND((TT_SBudMY.AARSalgAvvikKr / TT_SBudMY.AARSalgBudsjett) * 100,2)
           .
/*       IF bLogg THEN                                                                          */
/*           RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: Hitil i året '                 */
/*                                 + ' BudId: '  + STRING(SBudDag.SBudId)                       */
/*                                 + ' AaMnd: '  + STRING(SBudDag.AarMnd)                       */
/*                                 + ' AaMndDag: '  + STRING(SBudDag.AarMndDag)                 */
/*                                 + ' Butikk: ' + STRING(TT_SBudMY.butik)                      */
/*                                 + ' Navn: '   + TT_SBudMY.butnamn                            */
/*                                 + ' AARSalgBudsjett: '  + STRING(TT_SBudMY.AARSalgBudsjett)  */
/*                                 + ' AARSalgAvvikKr: '   + STRING(TT_SBudMY.AARSalgAvvikKr)   */
/*                                 + ' AARSalgAvvikproc: ' + STRING(TT_SBudMY.AARSalgAvvikproc) */
/*                                 ).                                                           */
      END.
      
             
  END.
  FOR EACH TT_SBudMY:
      ASSIGN TT_SBudMY.AARSalgBudsjett = ROUND(TT_SBudMY.AARSalgBudsjett,0)
             TT_SBudMY.AARsalgbrutto   = ROUND(TT_SBudMY.AARsalgbrutto,0)
             TT_SBudMY.AARSalgAvvikKr  = TT_SBudMY.AARsalgbrutto - TT_SBudMY.AARSalgBudsjett
             TT_SBudMY.AARSalgAvvikproc = IF TT_SBudMY.AARSalgBudsjett > 0 THEN ROUND((TT_SBudMY.AARSalgAvvikKr / TT_SBudMY.AARSalgBudsjett) * 100,2) ELSE 0
             TT_SBudMY.MNDSalgBudsjett  = ROUND(TT_SBudMY.MNDSalgBudsjett,0)
             TT_SBudMY.MNDsalgbrutto    = ROUND(TT_SBudMY.MNDsalgbrutto,0)
             TT_SBudMY.MNDSalgAvvikKr   = TT_SBudMY.MNDsalgbrutto - TT_SBudMY.MNDSalgBudsjett
             TT_SBudMY.MNDSalgAvvikproc = IF TT_SBudMY.MNDSalgBudsjett > 0 THEN ROUND((TT_SBudMY.MNDSalgAvvikKr / TT_SBudMY.MNDSalgBudsjett) * 100,2) ELSE 0.


  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.
  DEFINE       VARIABLE iPageBefore AS INTEGER    NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  
  ASSIGN iPageBefore = (DYNAMIC-FUNCTION('getCurrentPage':U)).
  
  RUN SUPER( INPUT piPageNum).
  
  IF iPageBefore <> piPageNum THEN
      RUN VisFlip (piPageNum).
  
  ASSIGN B-Chart:SENSITIVE IN FRAME {&FRAME-NAME} = CAN-DO("1,2,3,4,5,6,7,8,9,10",STRING(piPageNum)).
  B-Artikkelkort:SENSITIVE = piPageNum = 7.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDivResize wWin 
PROCEDURE SetDivResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME fMain:HANDLE,
                                "RECT-3").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME fMain:HANDLE,
                                "RECT-1,RECT-2,RECT-3").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME fMain:HANDLE,
                                 "Btn_Ok,Btn_Help").
 DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). 
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_Butiker wWin 
PROCEDURE TT_Butiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      FOR EACH StLinje WHERE StLinje.Butik = Butiker.Butik AND StTypeId = 'BUTSTAT' AND
                     PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.AntSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:

          FOR EACH akt_rapp NO-LOCK WHERE akt_rapp.dato = FI-Dato AND akt_rapp.butik = StLinje.Butik AND
                 (akt_rapp.oms_verd <> 0 OR akt_rapp.ant_kunder <> 0):
              ASSIGN iAntkunder = iAntkunder + akt_rapp.ant_kunder
                     iAntkvitt  = iAntkvitt + (akt_rapp.ant_kvitto * -1).
          END.
          CREATE TT_Butiker.
          ASSIGN
            TT_Butiker.butik         = Butiker.butik
            TT_Butiker.butnamn       = Butiker.butnamn
            TT_Butiker.salgbrutto    = StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_Butiker.VerdiSolgt    = StLinje.VerdiSolgt
            TT_Butiker.AntSolgt      = StLinje.AntSolgt
            TT_Butiker.VerdiRabatt   = StLinje.VerdiRabatt
            TT_Butiker.DBkr          = TT_Butiker.VerdiSolgt - StLinje.VVarekost
            TT_Butiker.DB%           = IF TT_Butiker.VerdiSolgt = 0 THEN 0 ELSE ROUND(TT_Butiker.DBkr / TT_Butiker.VerdiSolgt * 100,2)
            TT_Butiker.mvaverdi      = StLinje.MvaVerdi
            TT_Butiker.vvarekost     = StLinje.VVarekost
            TT_Butiker.GjenkjopAnt   = StLinje.GjenkjopAnt
            TT_Butiker.GjenkjopVerdi = StLinje.GjenkjopVerdi
            TT_Butiker.Antkunder     = iAntkunder
            TT_Butiker.Antkvitt      = iAntkvitt.
          /*
          CREATE TT_SBud.
          ASSIGN
              TT_SBud.Butik          = Butiker.butik       
              TT_SBud.butnamn        = Butiker.butnamn                         
              TT_SBud.salgbrutto     = StLinje.VerdiSolgt + StLinje.MvaVerdi                      
              TT_SBud.VerdiSolgt     = StLinje.VerdiSolgt 
              /*
              TT_SBud.salgBudsjett   =  
              TT_SBud.DBkr        
              TT_SBud.DB%         
              TT_SBud.DbKrBudsjett
              TT_SBud.Db%Budsjett 
              */
              .
          */
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisFlip wWin 
PROCEDURE VisFlip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iFlipNum AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  hQry     AS HANDLE     NO-UNDO.
/*   DEFINE        VARIABLE cFelter   AS CHARACTER  NO-UNDO. FLYTTAT TIL DEFINE  */
  DEFINE        VARIABLE cLabels   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cSumCols  AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cSumWhat  AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cColLabel AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cKalkCols  AS CHARACTER  NO-UNDO.

  CASE iFlipNum:
      WHEN 1 THEN DO:
          {&OPEN-QUERY-QAvd}
          ASSIGN hQry    = QUERY QAvd:HANDLE
              cLabels    = "Avdeling,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr"
              cFelter    = "avdelingnr,AvdelingNavn,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi"
              cSumWhat   = "3,4,5,6,7,9,10,11,12"
              cDecimaler = ",,3,2,2,2,2,2,2,2,3,2"
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1"
              cKalkCols  = "2,8,7,5".
      END.
      WHEN 2 THEN DO:
          {&OPEN-QUERY-QHuvGr}
          ASSIGN hQry   = QUERY QHuvGr:HANDLE
              cLabels    = "Hg,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr"
              cFelter    = "hg,hgbeskr,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi"
              cSumWhat   = "3,4,5,6,7,9,10,11,12"
              cDecimaler = ",,3,2,2,2,2,2,2,2,3,2"
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1"
              cKalkCols  = "2,8,7,5".
      END.
      WHEN 3 THEN DO:
          {&OPEN-QUERY-QVg}
          ASSIGN hQry   = QUERY QVg:HANDLE
              cLabels    = "Vg,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr"
              cFelter    = "vg,vgbeskr,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi"
              cSumWhat   = "3,4,5,6,7,9,10,11,12"
              cDecimaler = ",,3,2,2,2,2,2,2,2,3,2"
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1"
              cKalkCols  = "2,8,7,5".
      END.
      WHEN 4 THEN DO:
          {&OPEN-QUERY-QLev}
          ASSIGN hQry   = QUERY QLev:HANDLE
              cLabels    = "Levnr,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr"
              cFelter    = "levnr,levnavn,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi"
              cSumWhat   = "3,4,5,6,7,9,10,11,12"
              cDecimaler = ",,3,2,2,2,2,2,2,2,3,2"
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1"
              cKalkCols  = "2,8,7,5".
      END.
      WHEN 5 THEN DO:
          {&OPEN-QUERY-QSelger}
          ASSIGN hQry   = QUERY QSelger:HANDLE
              cLabels    = "Selgere,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr,Kvitteringer,Hitrate %,Mer fsg %"
              cFelter    = "selgernr,navn,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi,AntKunder,Hitrate,Merfsg"
              cSumWhat   = "3,4,5,6,7,9,10,11,12,13"
              cDecimaler = ",,3,2,2,2,2,2,2,2,3,2,,1,1"
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1,1,1,1"
              cKalkCols  = "2,8,7,5".
      END.
      WHEN 6 THEN DO:
          {&OPEN-QUERY-QTime}
          ASSIGN hQry = QUERY QTime:HANDLE
              /* oms_ant ! vad är det ? det kan inte vara antal varer */
/*                  cLabels = "Time,Kasse,Salgsum brutto,Salgsum netto,Varekost,Mva,DB kr,DB%,Ant varer,Kunder,Ant kvitt.,Returer,Returer kr" */
/*                  cFelter = "tid_txt,kasse,bruttosalg,oms_verd,svk,mva_kr,DBKr,DB%,oms_ant,ant_kunder,ant_kvitto,ant_ret,verd_ret"                      */
                 cLabels = "Time,Salgssum brutto,Salgssum netto,Varekost,Mva,DB kr,DB%,Kunder,Kvitteringer,Returer,Returer kr"
                 cFelter = "tid_txt,bruttosalg,oms_verd,svk,mva_kr,DBKr,DB%,ant_kunder,ant_kvitto,ant_ret,verd_ret"
                 cDecimaler = ",2,2,2,2,2,2,,,3,2"
                 cSumWhat   = "2,3,4,5,6,8,9,10,11"
                 cColLabel = "2,Sum"
                 cColAlign  = "1,1,1,1,1,1,1,1,1,1,1"
                 cKalkCols  = "2,7,6,3".
      END.
      WHEN 7 THEN DO:
          {&OPEN-QUERY-QVare}
          ASSIGN hQry = QUERY QVare:HANDLE
/*               cLabels    = "ArtikkelNr,Levkod,Beskrivelse,Salgssum brutto,Salgssum netto,Solgt,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr" */
/*               cFelter    = "Artikkelnr,levkod,beskr,salgbrutto,VerdiSolgt,AntSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi"          */
/*               cSumWhat   = "4,5,6,7,8,10,11,12,13"                                                                                                             */
/*               cDecimaler = ",,,2,2,,2,2,2,2,2,,2"                                                                                                              */
/*               cColLabel  = "3,Sum"                                                                                                                             */
/*               cColAlign  = "1,,,1,1,1,1,1,1,1,1,1,1"                                                                                                           */
/*               cKalkCols  = "2,9,8,5".                                                                                                                          */
              cLabels    = "ArtikkelNr,Beskrivelse,Farve,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr,Levkod,Varemerke"
              cFelter    = "Artikkelnr,beskr,Farg,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi,levkod,Varemerke"
              cSumWhat   = "4,5,6,7,8,10,11,12,13"
              cDecimaler = ",,,3,2,2,2,2,2,2,2,3,2,,"
              cColLabel  = "3,Sum"
              cColAlign  = "1,,,1,1,1,1,1,1,1,1,1,1,,"
              cKalkCols  = "2,9,8,6".
      END.
      WHEN 8 THEN DO:
          {&OPEN-QUERY-QButiker}
          ASSIGN hQry    = QUERY QButiker:HANDLE
              cLabels    = "Butikk,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr,Kunder" /* ,Ant kvitt" */
              cFelter    = "butik,butnamn,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DB%,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi,Antkunder"          /* ,Antkvitt"  */
              cSumWhat   = "3,4,5,6,7,9,10,11,12"
              cDecimaler = ",,3,2,2,2,2,2,2,2,3,2,"
/*               cDecimaler = ",,2,2,,2,2,2,2,2,,2,," */
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1,1"
/*               cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1,1,1" */
              cKalkCols  = "2,8,7,5".
      END.
      WHEN 9 THEN DO:
          {&OPEN-QUERY-QSBudDW}
          ASSIGN hQry    = QUERY QSBudDW:HANDLE
              cLabels    = "Butik,Namn,Försäljning,Bud försäljning,Avvik kr,Avvik %, ,Försäljning,Bud försäljning,Avvik kr,Avvik %"
              cFelter    = "Butik,butnamn,DAGsalgbrutto,DAGsalgBudsjett,DAGSalgAvvikKr,DAGSalgAvvikproc,Skille,UKEsalgbrutto,UKEsalgBudsjett,UKESalgAvvikKr,UKESalgAvvikproc"
              cSumWhat   = "3,4,5,8,9,10"
              cDecimaler = ",,,,,,,,,,"
/*               cDecimaler = ",,2,2,,2,2,2,2,2,,2,," */
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,,1,1,1,1"
/*               cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1,1,1" */
              cKalkCols  = "2,6,5,4;2,11,10,9".
/*               cKalkCols  = "2,8,7,5". */
      END.
      WHEN 10 THEN DO:
          {&OPEN-QUERY-QSBudMY}
          ASSIGN hQry    = QUERY QSBudMY:HANDLE
              cLabels    = "Butik,Namn,Försäljning,Bud försäljning,Avvik kr,Avvik %, ,Försäljning,Bud försäljning,Avvik kr,Avvik %"
              cFelter    = "Butik,butnamn,MNDsalgbrutto,MNDsalgBudsjett,MNDSalgAvvikKr,MNDSalgAvvikproc,Skille,AARsalgbrutto,AARsalgBudsjett,AARSalgAvvikKr,AARSalgAvvikproc"
              cSumWhat   = "3,4,5,8,9,10"
              cDecimaler = ",,,,,,,,,,"
/*               cDecimaler = ",,2,2,,2,2,2,2,2,,2,," */
              cColLabel  = "2,Sum"
              cColAlign  = "1,,1,1,1,1,,1,1,1,1"
/*               cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1,1,1" */
              cKalkCols  = "2,6,5,4;2,11,10,9".
/*               cKalkCols  = "2,8,7,5". */
      END.
  END CASE.

/*   RUN kassrappqry.p ("","",SESSION:TEMP-DIRECTORY + "gridkasse.txt",cLabels,cFelter,hQry). */
  RUN rappgenqry.p ("","",SESSION:TEMP-DIRECTORY + "gridkasse.txt",cLabels,cFelter,cDecimaler,cTidFelter,hQry).

  PUBLISH "LoadGrid" (SESSION:TEMP-DIRECTORY + "gridkasse.txt",IF iFlipNum = 6 THEN 2 ELSE IF iFlipNum = 7 THEN 4 ELSE 3). /*  = frozencols */
/*   PUBLISH "LoadGrid" (SESSION:TEMP-DIRECTORY + "gridkasse.txt",IF iFlipNum = 4 THEN 2 ELSE IF iFlipNum = 5 THEN 5 ELSE 3). /*  = frozencols */ */
  IF cSumWhat <> "" THEN
      PUBLISH "Summer" (cSumWhat + ";" + cKalkCols,cColLabel).
  IF iFlipNum = 7 THEN
      PUBLISH "AlignCol" (2,2).
  IF cSumCols <> "" THEN
      PUBLISH "ShowBold" (cSumCols).
/* 
   ASSIGN cSumCols = getSumFelter(cSummerFelter)
         /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
         cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"
                   + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt")
         /* Col för SummaRadTxt, SUM = txt  */
         cSumString = getSumFelter("PerLinTxt") + ",SUM" .
  /* nästa rad måste stå före 'Summer' */
  PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")).
  PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).

 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockVindu wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHeltal wWin 
FUNCTION getHeltal RETURNS CHARACTER
  ( INPUT cListe AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCOunt AS INTEGER    NO-UNDO.
  DO iCount = 1 TO NUM-ENTRIES(cListe,CHR(1)):
      ASSIGN ENTRY(iCount,cListe,CHR(1)) = ENTRY(1,ENTRY(iCount,cListe,CHR(1))).
  END.
  RETURN cListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

