&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hToolbarPris      AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hGyldigKodeCol    AS HANDLE NO-UNDO.
DEF VAR hArtBilde         AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame    AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.
DEF VAR iCurrBildNr       AS INT    NO-UNDO.

DEF VAR hLevAnt           AS HANDLE NO-UNDO.
DEF VAR hStrekkode        AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort     AS HANDLE NO-UNDO.
DEF VAR cButikkNr         AS CHAR   NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmPkSdlPris

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiUnderlineInnkjPris Beskr OverstyrPris ~
NyInnkjopsPris NyRab1% NyFrakt NyPris NyVarekost Tilbud InnkjopsPris Rab1% ~
DB% Pris GjInnkjopsPris GjRab1% fiInnkjPrisLabel fi%Rab1Label GjDB% ~
fiIDB%Label GjPris fiPrisLabel Varekost GjVarekost NyDB% fiVarekostLabel ~
GjFrakt Frakt fiFraktLabel fiTotAntall fiTotVerdi fiTotAntRest ~
fiTotVerdiRest fiUnderlinePris fiUnderlineNyRab1% Kommentar ArtikkelBilde ~
tbPksdlPris 
&Scoped-Define DISPLAYED-OBJECTS fiUnderlineInnkjPris Beskr OverstyrPris ~
NyInnkjopsPris NyRab1% NyFrakt NyPris NyVarekost Tilbud InnkjopsPris Rab1% ~
DB% Pris GjInnkjopsPris GjRab1% fiInnkjPrisLabel fi%Rab1Label GjDB% ~
fiIDB%Label GjPris fiPrisLabel Varekost GjVarekost NyDB% fiVarekostLabel ~
GjFrakt Frakt fiFraktLabel fiTotAntall fiTotVerdi fiTotAntRest ~
fiTotVerdiRest fiUnderlinePris fiUnderlineNyRab1% Kommentar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcTotals C-Win 
FUNCTION CalcTotals RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButikkNr C-Win 
FUNCTION setButikkNr RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setInputSensitive C-Win 
FUNCTION setInputSensitive RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Beskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54.4 BY .76 NO-UNDO.

DEFINE VARIABLE DB% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE VARIABLE fi%Rab1Label AS CHARACTER FORMAT "X(256)":U INITIAL "Rab1%:" 
      VIEW-AS TEXT 
     SIZE 7.8 BY .62 NO-UNDO.

DEFINE VARIABLE fiFraktLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Frakt Kr:" 
      VIEW-AS TEXT 
     SIZE 9.6 BY .62 NO-UNDO.

DEFINE VARIABLE fiIDB%Label AS CHARACTER FORMAT "X(256)":U INITIAL "DB%:" 
      VIEW-AS TEXT 
     SIZE 6.6 BY .62 NO-UNDO.

DEFINE VARIABLE fiInnkjPrisLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Innkj.pris:" 
      VIEW-AS TEXT 
     SIZE 10.6 BY .62 NO-UNDO.

DEFINE VARIABLE fiPrisLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Pris:" 
      VIEW-AS TEXT 
     SIZE 4.6 BY .62 NO-UNDO.

DEFINE VARIABLE fiTotAntall AS DECIMAL FORMAT "->>>>9":U INITIAL 0 
     LABEL "Antall levert" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotAntRest AS DECIMAL FORMAT "->>>>9":U INITIAL 0 
     LABEL "Antall rest" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotVerdi AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Sum varekost" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE fiTotVerdiRest AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Varekost, rest" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE fiUnderlineInnkjPris AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY .29
     BGCOLOR 12 FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fiUnderlineNyRab1% AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .29
     BGCOLOR 12 FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fiUnderlinePris AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY .29
     BGCOLOR 12 FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fiVarekostLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Varekost:" 
      VIEW-AS TEXT 
     SIZE 10.6 BY .62 NO-UNDO.

DEFINE VARIABLE Frakt AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.8 BY 1.

DEFINE VARIABLE GjDB% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE VARIABLE GjFrakt AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.8 BY 1.

DEFINE VARIABLE GjInnkjopsPris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Butikkpris" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE GjPris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 TOOLTIP "Gjeldende pris".

DEFINE VARIABLE GjRab1% AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1.

DEFINE VARIABLE GjVarekost AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE InnkjopsPris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Kjedepris" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE Kommentar AS CHARACTER FORMAT "x(64)" 
     LABEL "Art.merknad" 
     VIEW-AS FILL-IN 
     SIZE 107 BY 1 TOOLTIP "Kommentar".

DEFINE VARIABLE NyDB% AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1
     FONT 6.

DEFINE VARIABLE NyFrakt AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.8 BY 1
     FONT 6.

DEFINE VARIABLE NyInnkjopsPris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Ny pris" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     FONT 6.

DEFINE VARIABLE NyPris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1
     FONT 6.

DEFINE VARIABLE NyRab1% AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1
     FONT 6.

DEFINE VARIABLE NyVarekost AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     FONT 6.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE Rab1% AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1.

DEFINE VARIABLE Varekost AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 4.57.

DEFINE RECTANGLE tbPksdlPris
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 1.19.

DEFINE VARIABLE OverstyrPris AS LOGICAL INITIAL NO 
     LABEL "Korrigert kalkyle" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.6 BY .81 NO-UNDO.

DEFINE VARIABLE Tilbud AS LOGICAL INITIAL NO 
     LABEL "Tilb" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmPkSdlPris
     fiUnderlineInnkjPris AT ROW 5.76 COL 16.2 COLON-ALIGNED NO-LABEL
     Beskr AT ROW 1.1 COL 35.6 COLON-ALIGNED NO-LABEL
     OverstyrPris AT ROW 1.1 COL 18.6
     NyInnkjopsPris AT ROW 4.81 COL 10.6 HELP
          "Innkjøpspris"
     NyRab1% AT ROW 4.81 COL 31.2 COLON-ALIGNED HELP
          "Rabatt i % regnes av innkjøpspris." NO-LABEL
     NyFrakt AT ROW 4.81 COL 40.4 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-LABEL
     NyPris AT ROW 4.81 COL 67 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-LABEL
     NyVarekost AT ROW 4.81 COL 51.8 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-LABEL
     Tilbud AT ROW 2.05 COL 75.4
     InnkjopsPris AT ROW 3.76 COL 8.4 HELP
          "Innkjøpspris"
     Rab1% AT ROW 3.76 COL 31.2 COLON-ALIGNED HELP
          "Rabatt i % regnes av innkjøpspris." NO-LABEL
     DB% AT ROW 3.76 COL 82.6 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %" NO-LABEL
     Pris AT ROW 3.76 COL 67 COLON-ALIGNED HELP
          "Pris (Til kunde)" NO-LABEL
     GjInnkjopsPris AT ROW 2.71 COL 7.8 HELP
          "Innkjøpspris"
     GjRab1% AT ROW 2.71 COL 31.2 COLON-ALIGNED HELP
          "Rabatt i % regnes av innkjøpspris." NO-LABEL
     fiInnkjPrisLabel AT ROW 2 COL 18.2 NO-LABEL
     fi%Rab1Label AT ROW 2 COL 31.4 COLON-ALIGNED NO-LABEL
     GjDB% AT ROW 2.71 COL 82.6 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %" NO-LABEL
     fiIDB%Label AT ROW 2 COL 83 COLON-ALIGNED NO-LABEL
     GjPris AT ROW 2.71 COL 67 COLON-ALIGNED HELP
          "Pris (Til kunde)" NO-LABEL
     fiPrisLabel AT ROW 2 COL 67.4 COLON-ALIGNED NO-LABEL
     Varekost AT ROW 3.76 COL 53.8 HELP
          "Innkjøpspris" NO-LABEL
     GjVarekost AT ROW 2.71 COL 53.8 HELP
          "Innkjøpspris" NO-LABEL
     NyDB% AT ROW 4.81 COL 82.6 COLON-ALIGNED HELP
          "Dekningsbidrag oppgitt i %" NO-LABEL
     fiVarekostLabel AT ROW 2 COL 53.8 NO-LABEL
     GjFrakt AT ROW 2.71 COL 40.4 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-LABEL
     Frakt AT ROW 3.76 COL 40.4 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-LABEL
     fiFraktLabel AT ROW 2 COL 40.6 COLON-ALIGNED NO-LABEL
     fiTotAntall AT ROW 1.38 COL 108.2 COLON-ALIGNED NO-TAB-STOP 
     fiTotVerdi AT ROW 2.48 COL 108.2 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-TAB-STOP 
     fiTotAntRest AT ROW 3.76 COL 108.2 COLON-ALIGNED NO-TAB-STOP 
     fiTotVerdiRest AT ROW 4.81 COL 108.2 COLON-ALIGNED HELP
          "Ny pris (Til kunde)" NO-TAB-STOP 
     fiUnderlinePris AT ROW 5.76 COL 67 COLON-ALIGNED NO-LABEL
     fiUnderlineNyRab1% AT ROW 5.76 COL 31.2 COLON-ALIGNED NO-LABEL
     Kommentar AT ROW 6.05 COL 16 COLON-ALIGNED
     ArtikkelBilde AT ROW 1.38 COL 126
     tbPksdlPris AT ROW 1.1 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147.2 BY 6.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 6.24
         WIDTH              = 147.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmPkSdlPris
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fiInnkjPrisLabel IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
ASSIGN 
       fiTotAntall:READ-ONLY IN FRAME frmPkSdlPris        = TRUE.

ASSIGN 
       fiTotAntRest:READ-ONLY IN FRAME frmPkSdlPris        = TRUE.

ASSIGN 
       fiTotVerdi:READ-ONLY IN FRAME frmPkSdlPris        = TRUE.

ASSIGN 
       fiTotVerdiRest:READ-ONLY IN FRAME frmPkSdlPris        = TRUE.

/* SETTINGS FOR FILL-IN fiVarekostLabel IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN GjInnkjopsPris IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN GjVarekost IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN InnkjopsPris IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN NyInnkjopsPris IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Varekost IN FRAME frmPkSdlPris
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmPkSdlPris
/* Query rebuild information for FRAME frmPkSdlPris
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmPkSdlPris */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NyFrakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyFrakt C-Win
ON ALT-L OF NyFrakt IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyFrakt C-Win
ON CTRL-S OF NyFrakt IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyFrakt C-Win
ON CURSOR-DOWN OF NyFrakt IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyFrakt C-Win
ON CURSOR-UP OF NyFrakt IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN prevRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NyInnkjopsPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyInnkjopsPris C-Win
ON ALT-L OF NyInnkjopsPris IN FRAME frmPkSdlPris /* Ny pris */
DO:
  APPLY "leave" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyInnkjopsPris C-Win
ON CTRL-S OF NyInnkjopsPris IN FRAME frmPkSdlPris /* Ny pris */
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyInnkjopsPris C-Win
ON CURSOR-DOWN OF NyInnkjopsPris IN FRAME frmPkSdlPris /* Ny pris */
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyInnkjopsPris C-Win
ON CURSOR-UP OF NyInnkjopsPris IN FRAME frmPkSdlPris /* Ny pris */
DO:
  APPLY "leave" TO SELF.
  RUN prevRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NyPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyPris C-Win
ON ALT-L OF NyPris IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyPris C-Win
ON CTRL-S OF NyPris IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyPris C-Win
ON CURSOR-DOWN OF NyPris IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyPris C-Win
ON CURSOR-UP OF NyPris IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN prevRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NyRab1%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyRab1% C-Win
ON ALT-L OF NyRab1% IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyRab1% C-Win
ON CTRL-S OF NyRab1% IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyRab1% C-Win
ON CURSOR-DOWN OF NyRab1% IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN nextRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NyRab1% C-Win
ON CURSOR-UP OF NyRab1% IN FRAME frmPkSdlPris
DO:
  APPLY "leave" TO SELF.
  RUN prevRow IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hStrekkode)    THEN APPLY "close" TO hStrekkode.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBilde)     THEN APPLY "close" TO hArtBilde.
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
{incl/supptrigg.i hQuery}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "AltSKundeOrdre" (THIS-PROCEDURE:FILE-NAME).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelKort C-Win 
PROCEDURE ArtikkelKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hArtikkelkort) THEN 
DO:
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMap:BUFFER-FIELD("RowIdent3"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CurrPrisFillIn C-Win 
PROCEDURE CurrPrisFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowseFillIn AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF VALID-HANDLE(ihBrowseFillIn) THEN 
    CASE ihBrowseFillIn:NAME:
      WHEN "NyInnkjopsPris" THEN 
        ASSIGN fiUnderlineInnkjPris:HIDDEN = NO
               fiUnderlinePris:HIDDEN = YES
               fiUnderlineNyRab1%:HIDDEN = YES.
      WHEN "NyPris" THEN 
        ASSIGN fiUnderlinePris:HIDDEN = NO
               fiUnderlineInnkjPris:HIDDEN = YES
               fiUnderlineNyRab1%:HIDDEN = YES.
      WHEN "NyRab1%" THEN 
        ASSIGN fiUnderlinePris:HIDDEN = YES
               fiUnderlineInnkjPris:HIDDEN = YES
               fiUnderlineNyRab1%:HIDDEN = NO.
      OTHERWISE
        ASSIGN fiUnderlineInnkjPris:HIDDEN = YES
               fiUnderLinePris:HIDDEN = YES
               fiUnderlineNyRab1%:HIDDEN = YES.
    END CASE.
  ELSE
    ASSIGN fiUnderlineInnkjPris:HIDDEN = YES
           fiUnderLinePris:HIDDEN = YES
           fiUnderlineNyRab1%:HIDDEN = YES.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME frmPkSdlPris.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bEditPkSdl AS LOG  NO-UNDO.
DEF VAR cTot       AS CHAR NO-UNDO.

IF VALID-HANDLE(hArtikkelkort) THEN DO:    
  DYNAMIC-FUNCTION("DoLockWindow",hArtikkelkort:CURRENT-WINDOW).  
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
END.

RUN SUPER.

ASSIGN NyPris:SENSITIVE IN FRAME {&FRAME-NAME} = hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("MottaksId"):BUFFER-VALUE = 0 /* AND hFieldMap:BUFFER-FIELD("OverstyrPris"):BUFFER-VALUE */
       NyVarekost:SENSITIVE = NyPris:SENSITIVE
       fiUnderlineInnkjPris:HIDDEN = YES
       fiUnderLinePris:HIDDEN = YES
       .

IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("Tilbud"):BUFFER-VALUE THEN
  Tilbud:BGCOLOR IN FRAME {&FRAME-NAME} = 12.
ELSE
  Tilbud:BGCOLOR IN FRAME {&FRAME-NAME} = ?.

IF hFieldMap:AVAIL THEN DO:      
    /* Farger på KjedePris linjen ------------------------------------------------------*/
    IF hFieldMap:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjInnkjopsPris"):BUFFER-VALUE THEN
      InnkjopsPris:BGCOLOR = 17.
    ELSE IF hFieldMap:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjInnkjopsPris"):BUFFER-VALUE THEN
      InnkjopsPris:BGCOLOR = 18.
    ELSE
      InnkjopsPris:BGCOLOR = ?.

    IF hFieldMap:BUFFER-FIELD("Rab1%"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjRab1%"):BUFFER-VALUE THEN
      Rab1%:BGCOLOR = 17.
    ELSE IF hFieldMap:BUFFER-FIELD("Rab1%"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjRab1%"):BUFFER-VALUE THEN
      Rab1%:BGCOLOR = 18.
    ELSE
      Rab1%:BGCOLOR = ?.

    IF hFieldMap:BUFFER-FIELD("Frakt"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjFrakt"):BUFFER-VALUE THEN
      Frakt:BGCOLOR = 17.
    ELSE IF hFieldMap:BUFFER-FIELD("Frakt"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjFrakt"):BUFFER-VALUE THEN
      Frakt:BGCOLOR = 18.
    ELSE
      Frakt:BGCOLOR = ?.

    IF hFieldMap:BUFFER-FIELD("Varekost"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjVarekost"):BUFFER-VALUE THEN
      Varekost:BGCOLOR = 17.
    ELSE IF hFieldMap:BUFFER-FIELD("Varekost"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjVarekost"):BUFFER-VALUE THEN
      Varekost:BGCOLOR = 18.
    ELSE
      Varekost:BGCOLOR = ?.

    IF hFieldMap:BUFFER-FIELD("Pris"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjPris"):BUFFER-VALUE THEN
      Pris:BGCOLOR = 17.
    ELSE IF hFieldMap:BUFFER-FIELD("Pris"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjPris"):BUFFER-VALUE THEN
      Pris:BGCOLOR = 18.
    ELSE
      Pris:BGCOLOR = ?.

    IF hFieldMap:BUFFER-FIELD("Db%"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjDb%"):BUFFER-VALUE THEN
      Db%:BGCOLOR = 17.
    ELSE IF hFieldMap:BUFFER-FIELD("Db%"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjDb%"):BUFFER-VALUE THEN
      Db%:BGCOLOR = 18.
    ELSE
      Db%:BGCOLOR = ?.
  
  /* Farger på NyPris linjen ------------------------------------------------------*/
  IF hFieldMap:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjInnkjopsPris"):BUFFER-VALUE THEN
    NyInnkjopsPris:BGCOLOR = 17.
  ELSE IF hFieldMap:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjInnkjopsPris"):BUFFER-VALUE THEN
    NyInnkjopsPris:BGCOLOR = 18.
  ELSE
    NyInnkjopsPris:BGCOLOR = ?.
  
  IF hFieldMap:BUFFER-FIELD("NyRab1%"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjRab1%"):BUFFER-VALUE THEN
    NyRab1%:BGCOLOR = 17.
  ELSE IF hFieldMap:BUFFER-FIELD("NyRab1%"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjRab1%"):BUFFER-VALUE THEN
    NyRab1%:BGCOLOR = 18.
  ELSE
    NyRab1%:BGCOLOR = ?.

  IF hFieldMap:BUFFER-FIELD("NyFrakt"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjFrakt"):BUFFER-VALUE THEN
    NyFrakt:BGCOLOR = 17.
  ELSE IF hFieldMap:BUFFER-FIELD("NyFrakt"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjFrakt"):BUFFER-VALUE THEN
    NyFrakt:BGCOLOR = 18.
  ELSE
    NyFrakt:BGCOLOR = ?.

  IF hFieldMap:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjVarekost"):BUFFER-VALUE THEN
    NyVarekost:BGCOLOR = 17.
  ELSE IF hFieldMap:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjVarekost"):BUFFER-VALUE THEN
    NyVarekost:BGCOLOR = 18.
  ELSE
    NyVarekost:BGCOLOR = ?.

  IF hFieldMap:BUFFER-FIELD("NyPris"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjPris"):BUFFER-VALUE THEN
    NyPris:BGCOLOR = 17.
  ELSE IF hFieldMap:BUFFER-FIELD("NyPris"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjPris"):BUFFER-VALUE THEN
    NyPris:BGCOLOR = 18.
  ELSE
    NyPris:BGCOLOR = ?.

  IF hFieldMap:BUFFER-FIELD("NyDb%"):BUFFER-VALUE > hFieldMap:BUFFER-FIELD("GjDb%"):BUFFER-VALUE THEN
    NyDb%:BGCOLOR = 17.
  ELSE IF hFieldMap:BUFFER-FIELD("NyDb%"):BUFFER-VALUE < hFieldMap:BUFFER-FIELD("GjDb%"):BUFFER-VALUE THEN
    NyDb%:BGCOLOR = 18.
  ELSE
    NyDb%:BGCOLOR = ?.

  RUN VisMiniBilde IN hArtBilde (hFieldMap:BUFFER-FIELD("BildNr"):BUFFER-VALUE).
  IF DYNAMIC-FUNCTION("runproc","pksdl_sumart.p",
                      hFieldMap:BUFFER-FIELD("PkSdlId"):STRING-VALUE + "|" 
                    + hFieldMap:BUFFER-FIELD("ArtikkelNr"):STRING-VALUE + "|"
                    + cButikkNr
                      ,?) THEN DO:                          
    cTot = DYNAMIC-FUNCTION("getTransactionMessage").
    ASSIGN fiTotAntall:SCREEN-VALUE     = ENTRY(1,cTot,"|")
           fiTotVerdi:SCREEN-VALUE      = ENTRY(2,cTot,"|")
           fiTotAntRest:SCREEN-VALUE    = ENTRY(3,cTot,"|")
           fiTotVerdiRest:SCREEN-VALUE  = ENTRY(4,cTot,"|")
           .
  END.
END. 
ELSE DO:
  RUN VisMiniBilde IN hArtBilde (0).
  ASSIGN fiTotAntall:SCREEN-VALUE    = "0"
         fiTotVerdi:SCREEN-VALUE     = "0"
         fiTotAntRest:SCREEN-VALUE   = "0"
         fiTotVerdiRest:SCREEN-VALUE = "0"
         .
END. 

PUBLISH "EditPkSdl" (THIS-PROCEDURE:CURRENT-WINDOW,OUTPUT bEditPkSdl).
/* OverstyrPris:SENSITIVE = bEditPkSdl AND hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("MottaksId"):BUFFER-VALUE = 0.  */

setInputSensitive().

IF VALID-HANDLE(hArtikkelkort) THEN DO:
  RUN ByttArtikkel IN hArtikkelkort (IF hFieldMap:AVAIL THEN hFieldMap:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                     ELSE 0).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

hArtBildeFrame:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiUnderlineInnkjPris Beskr OverstyrPris NyInnkjopsPris NyRab1% NyFrakt 
          NyPris NyVarekost Tilbud InnkjopsPris Rab1% DB% Pris GjInnkjopsPris 
          GjRab1% fiInnkjPrisLabel fi%Rab1Label GjDB% fiIDB%Label GjPris 
          fiPrisLabel Varekost GjVarekost NyDB% fiVarekostLabel GjFrakt Frakt 
          fiFraktLabel fiTotAntall fiTotVerdi fiTotAntRest fiTotVerdiRest 
          fiUnderlinePris fiUnderlineNyRab1% Kommentar 
      WITH FRAME frmPkSdlPris.
  ENABLE fiUnderlineInnkjPris Beskr OverstyrPris NyInnkjopsPris NyRab1% NyFrakt 
         NyPris NyVarekost Tilbud InnkjopsPris Rab1% DB% Pris GjInnkjopsPris 
         GjRab1% fiInnkjPrisLabel fi%Rab1Label GjDB% fiIDB%Label GjPris 
         fiPrisLabel Varekost GjVarekost NyDB% fiVarekostLabel GjFrakt Frakt 
         fiFraktLabel fiTotAntall fiTotVerdi fiTotAntRest fiTotVerdiRest 
         fiUnderlinePris fiUnderlineNyRab1% Kommentar ArtikkelBilde tbPksdlPris 
      WITH FRAME frmPkSdlPris.
  {&OPEN-BROWSERS-IN-QUERY-frmPkSdlPris}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.
DEF VAR cProfilNr   AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN fiUnderlineInnkjPris:HIDDEN = YES
         fiUnderLinePris:HIDDEN = YES
         fiUnderlineNyRab1%:HIDDEN = YES.

  SUBSCRIBE TO "CurrPrisFillIn" IN hParent.

  RUN VisMiniBilde.w PERSIST SET hArtBilde.
  RUN InitializeObject IN hArtBilde (ArtikkelBilde:HANDLE).
  hArtBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hArtBilde).
/*   PUBLISH "removeFollowSplitBar" (THIS-PROCEDURE:CURRENT-WINDOW,                                    */
/*                                   STRING(hArtBildeFrame)                                            */
/*                            + "," + STRING(DYNAMIC-FUNCTION("getControlFrameHandle" IN hArtBilde))). */

  cProfilnr = DYNAMIC-FUNCTION("getFieldList","SysPara;,Butiker;ProfilNr",
                               "WHERE SysPara.SysHId = 5 AND SysPara.SysGr = 1 AND SysPara.ParaNr = 1"
                             + ",FIRST Butiker WHERE Butiker.Butik = INT(SysPara.Parameter1)"
                               ).

  hQuery = DYNAMIC-FUNCTION("NewQuery"
                            ,100
                            ,""
                            ,"PkSdlPris"
                          + ",ArtPris"
                             + ";+GjPris|DECIMAL|>>><>>9.99|artpris_pris(ArtikkelNr)"
                             + ";Tilbud"
                             + ";+GjInnkjopsPris|DECIMAL|>>><>>9.99|artpris_innkjopspris(ArtikkelNr)"
                             + ";+GjFrakt|DECIMAL|>>><>>9.99|artpris_frakt(ArtikkelNr)"
                             + ";+GjRab1%|DECIMAL|>>><>>9.99|artpris_rab1%(ArtikkelNr)"
                             + ";+GjVarekost|DECIMAL|>>><>>9.99|artpris_varekost(ArtikkelNr)"
                             + ";+GjDB%|DECIMAL|>>><>>9.99|artpris_db%(ArtikkelNr)"
                          + ",ArtBas"
                            + ";!ArtikkelNr;!BildNr;Beskr;Kommentar"
                          + ",buf1_PkSdlLinje"
                            + ";MottaksId"
                            ,"WHERE false"
                          + ",FIRST ArtPris WHERE ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND ProfilNr = " + cProfilnr + " NO-LOCK OUTER-JOIN"
                          + ",FIRST ArtBas OF ArtPris NO-LOCK"
                          + ",FIRST buf1_PkSdlLinje WHERE buf1_PkSdlLinje.PkSdlId = PkSdlPris.PkSdlId AND buf1_PkSdlLinje.ArtikkelNr = PkSdlPris.ArtikkelNr AND buf1_PkSdlLinje.MottaksId > 0 NO-LOCK OUTER-JOIN"
                            ,"").

  DYNAMIC-FUNCTION("setAttribute",hQuery,"calcfieldproc","artpris_brwcalc.p").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
                            ,hQuery
                            ,FRAME {&FRAME-NAME}:HANDLE
                            ,"NyInnkjopsPris,NyRab1%,NyFrakt,NyPris",""
                            ,"OverstyrPris,InnkjopsPris,Pris,Frakt,Varekost,Rab1%,DB%,GjPris,Tilbud,GjInnkjopsPris,GjRab1%,GjFrakt,GjDB%,GjVarekost,NyVarekost,NyDB%,Beskr,Kommentar",""
                            ,"").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"saveOnlyModified","yes").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"PostUpdateProc","pksdlpris_post_update.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).

  IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "PkSdlLinje" THEN
    DYNAMIC-FUNCTION("CreateParentLink",hQuery,hParentBrowse,"PkSdlId,ArtikkelNr").
  ELSE
    DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hParentBrowse,"PkSdlId,ArtikkelNr").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
           ,tbPksdlPris:HANDLE
           ,""
           ,"VisBestilling;Vis bestilling"
           ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).

  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hArtBildeFrame,hArtBildeFrame:NAME).
  DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, hArtBildeFrame,hArtBildeFrame:NAME).

END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR hWidget AS HANDLE NO-UNDO.


hWidget = DYNAMIC-FUNCTION("getFieldHandle",hFieldMap,icFieldName).

IF NOT VALID-HANDLE(hWidget) OR NOT hWidget:MODIFIED THEN RETURN.

DYNAMIC-FUNCTION("setCurrentObject",hQuery).
RUN SaveRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
hArtBildeFrame:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icDir AS CHARACTER  NO-UNDO.

RUN PrevNext IN hParent (icDir).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnOfWidget C-Win 
PROCEDURE ReturnOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.
hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF hCurrWidget:NAME = "NyPris" THEN
  RUN LeaveOfField ("NyPris").
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF icFieldName = "OverstyrPris" THEN DO:      
    DYNAMIC-FUNCTION("setCurrentObject",hQuery).
    RUN SaveRecord.
    setInputSensitive().
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBestillingRecord C-Win 
PROCEDURE VisBestillingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ioRecId AS RECID NO-UNDO.
DEF VAR cBestNr AS CHAR  NO-UNDO.

cBestNr = DYNAMIC-FUNCTION("getFieldValues","FIRST PkSdlLinje",
                           "WHERE PkSdlLinje.PkSdlId = " + hFieldMap:BUFFER-FIELD("PkSdlId"):STRING-VALUE
                         + "  AND PkSdlLinje.ArtikkelNr = " + hFieldMap:BUFFER-FIELD("ArtikkelNr"):STRING-VALUE,
                           "BestNr").

ioRecId = DYNAMIC-FUNCTION("getRecId","BestHode",
                           DYNAMIC-FUNCTION("getRowidList","BestHode","","WHERE bestnr = " + cBestNr)
                           ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

RUN w-gridord.w (DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMap:BUFFER-FIELD("RowIdent3"):BUFFER-VALUE),
                 INPUT-OUTPUT ioRecId,
                 "").

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcTotals C-Win 
FUNCTION CalcTotals RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN InvokeMethod(hQuery,"DisplayRecord").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN REPLACE(
       REPLACE(
       DYNAMIC-FUNCTION("getFieldList","PkSdlLinje;,StrKonv;Storl",
                        "WHERE ArtikkelNr = " + STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                      + ",FIRST StrKonv NO-LOCK WHERE StrKonv.StrKode = PkSdlLinje.StrKode")
      ,"|",",")
      ," ","").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"frmPkSdlPris"). 
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"ArtikkelBilde,frmPkSdlPris").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"ArtikkelBilde,frmPkSdlPris").
DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"ArtikkelBilde").
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButikkNr C-Win 
FUNCTION setButikkNr RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cButikkNr = icButikkNr.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setInputSensitive C-Win 
FUNCTION setInputSensitive RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bSensitive AS LOG NO-UNDO.

IF hFieldMap:AVAIL THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN bSensitive = hFieldMap:BUFFER-FIELD("MottaksId"):BUFFER-VALUE = 0 /* OverstyrPris:SENSITIVE AND OverstyrPris:CHECKED */
         NyInnkjopsPris:SENSITIVE = bSensitive
         NyRab1%:SENSITIVE        = bSensitive
         NyFrakt:SENSITIVE        = bSensitive
         NyPris:SENSITIVE         = bSensitive
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  hParent = ihParent.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hParentBrowse = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

