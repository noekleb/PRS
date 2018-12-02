&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk                   AS LOG    NO-UNDO.
DEF VAR ix                    AS INT    NO-UNDO.
DEF VAR hBrowse               AS HANDLE NO-UNDO.
DEF VAR hToolbar              AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBest      AS HANDLE NO-UNDO.
DEF VAR hBestBilde            AS HANDLE NO-UNDO.
DEF VAR hBestBildeFrame       AS HANDLE NO-UNDO.
DEF VAR hPakkliste            AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwOrdre brwArtikler rectToolBar ~
rectWinToolbar BestBilde sokBestOrdreStatus sokBestOrdreStatusTil ~
sokOrdreNr sokEkstId sokOrdreLevDato SokOrdreLevNr btnLev-3 ~
sokOrdremottaker sokOpphav sokOrdreCL btnOrdreLev sokOrdreArtikkel ~
btnOrdreArtikkel sokOrdreLevKod sokOrdreLevFargKod 
&Scoped-Define DISPLAYED-OBJECTS sokBestOrdreStatus sokBestOrdreStatusTil ~
sokOrdreNr sokEkstId sokOrdreLevDato SokOrdreLevNr SokOrdreLevNamn ~
sokOrdremottaker sokOpphav sokOrdreCL SokOrdreCLNamn sokOrdreArtikkel ~
SokOrdreArtBeskr sokOrdreLevKod sokOrdreLevFargKod 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLev-3 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOrdreArtikkel 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOrdreLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE sokBestOrdreStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokBestOrdreStatusTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "til" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokOpphav AS CHARACTER FORMAT "X(256)":U 
     LABEL "Opphav" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 11.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdremottaker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordremottak" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokEkstId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekst.id" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Ekstern id for enkeltbestilling" NO-UNDO.

DEFINE VARIABLE SokOrdreArtBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreArtikkel AS DECIMAL FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreCL AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "CL" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreCLNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreLevDato AS DATE FORMAT "99/99/99":U 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreLevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.fargek" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokOrdreLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.artnr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE SokOrdreLevNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1.

DEFINE VARIABLE sokOrdreNr AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1 NO-UNDO.

DEFINE RECTANGLE BestBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.2 BY 3.43.

DEFINE RECTANGLE brwArtikler
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 12.86.

DEFINE RECTANGLE brwOrdre
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY 12.86.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 12.86
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sokBestOrdreStatus AT ROW 2.48 COL 10.8 COLON-ALIGNED
     sokBestOrdreStatusTil AT ROW 2.48 COL 39 COLON-ALIGNED
     sokOrdreNr AT ROW 2.48 COL 73.6 COLON-ALIGNED
     sokEkstId AT ROW 2.48 COL 95.2 COLON-ALIGNED
     sokOrdreLevDato AT ROW 2.48 COL 124.2 COLON-ALIGNED
     SokOrdreLevNr AT ROW 3.52 COL 10.8 COLON-ALIGNED HELP
          "Varegruppe"
     btnLev-3 AT ROW 3.52 COL 21.2 NO-TAB-STOP 
     SokOrdreLevNamn AT ROW 3.52 COL 23.8 COLON-ALIGNED NO-LABEL
     sokOrdremottaker AT ROW 3.52 COL 64.4 COLON-ALIGNED
     sokOpphav AT ROW 3.52 COL 98 COLON-ALIGNED
     sokOrdreCL AT ROW 3.52 COL 113.6 COLON-ALIGNED
     btnOrdreLev AT ROW 3.52 COL 121.8 NO-TAB-STOP 
     SokOrdreCLNamn AT ROW 3.52 COL 124.2 COLON-ALIGNED NO-LABEL
     sokOrdreArtikkel AT ROW 4.57 COL 10.8 COLON-ALIGNED
     btnOrdreArtikkel AT ROW 4.57 COL 27 NO-TAB-STOP 
     SokOrdreArtBeskr AT ROW 4.57 COL 29.4 COLON-ALIGNED NO-LABEL
     sokOrdreLevKod AT ROW 4.57 COL 64.4 COLON-ALIGNED
     sokOrdreLevFargKod AT ROW 4.57 COL 95.2 COLON-ALIGNED
     brwOrdre AT ROW 6 COL 2
     brwArtikler AT ROW 6 COL 109
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 151.4
     BestBilde AT ROW 2.48 COL 141
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 161.2 BY 18.14.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 38.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 69.2 ROW 6
         SIZE 62.8 BY 12.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Søk ordre"
         HEIGHT             = 18.14
         WIDTH              = 161.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 161.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 161.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

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
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 18.14
       FRAME DEFAULT-FRAME:WIDTH            = 161.2.

/* SETTINGS FOR FILL-IN SokOrdreArtBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SokOrdreCLNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SokOrdreLevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søk ordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søk ordre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev-3 C-Win
ON CHOOSE OF btnLev-3 IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N",
                    "where true",
                    "",
                    "Levnr,levnamn",
                     OUTPUT cReturnValues,
                     OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


  IF bOk AND cReturnValues NE "" THEN DO:  
    ASSIGN sokOrdreLevnr:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           SokOrdreLevnamn:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrdreArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrdreArtikkel C-Win
ON CHOOSE OF btnOrdreArtikkel IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "ArtBas"
                    + ";ArtikkelNr"
                    + ";Beskr"
                    + ";LevKod"
                    + ";LevFargKod"
                    ,"WHERE true"
                    ,""                                                  
                    ,"ArtikkelNr,Beskr",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:  
    ASSIGN sokOrdreArtikkel:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           SokOrdreArtBeskr:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrdreLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrdreLev C-Win
ON CHOOSE OF btnOrdreLev IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Butiker"
                    + ";Butik"
                    + ";ButNamn"
                    ,"where true"
                    ,""                                                  
                    ,"Butik,ButNamn",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:  
    ASSIGN sokOrdreCL:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           SokOrdreCLNamn:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME sokBestOrdreStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBestOrdreStatus C-Win
ON VALUE-CHANGED OF sokBestOrdreStatus IN FRAME DEFAULT-FRAME /* Fra status */
DO:
  IF sokBestOrdreStatusTil:SCREEN-VALUE NE ? AND sokBestOrdreStatusTil:SCREEN-VALUE NE "" AND sokBestOrdreStatusTil:SCREEN-VALUE LT sokBestOrdreStatus:SCREEN-VALUE THEN
    sokBestOrdreStatusTil:SCREEN-VALUE = sokBestOrdreStatus:SCREEN-VALUE.
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBestOrdreStatusTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBestOrdreStatusTil C-Win
ON VALUE-CHANGED OF sokBestOrdreStatusTil IN FRAME DEFAULT-FRAME /* til */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokEkstId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokEkstId C-Win
ON LEAVE OF sokEkstId IN FRAME DEFAULT-FRAME /* Ekst.id */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOpphav
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOpphav C-Win
ON VALUE-CHANGED OF sokOpphav IN FRAME DEFAULT-FRAME /* Opphav */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreArtikkel C-Win
ON RETURN OF sokOrdreArtikkel IN FRAME DEFAULT-FRAME /* Art.nr */
DO:
  IF sokOrdreArtikkel:MODIFIED THEN DO:
    SokOrdreArtBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","ArtBas;Beskr","WHERE ArtikkelNr = " + sokOrdreArtikkel:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreArtikkel C-Win
ON TAB OF sokOrdreArtikkel IN FRAME DEFAULT-FRAME /* Art.nr */
DO:
  IF sokOrdreArtikkel:MODIFIED THEN 
    SokOrdreArtBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","ArtBas;Beskr","WHERE ArtikkelNr = " + sokOrdreArtikkel:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreCL C-Win
ON RETURN OF sokOrdreCL IN FRAME DEFAULT-FRAME /* CL */
DO:
  IF sokOrdreCL:MODIFIED THEN DO:
    SokOrdreCLNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + sokOrdreCL:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreCL C-Win
ON TAB OF sokOrdreCL IN FRAME DEFAULT-FRAME /* CL */
DO:
  IF sokOrdreCL:MODIFIED THEN 
    SokOrdreCLNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = " + sokOrdreCL:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreLevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreLevDato C-Win
ON RETURN OF sokOrdreLevDato IN FRAME DEFAULT-FRAME /* Lev.dato */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreLevFargKod C-Win
ON RETURN OF sokOrdreLevFargKod IN FRAME DEFAULT-FRAME /* Lev.fargek */
DO:
  IF sokOrdreLevFargKod:MODIFIED THEN 
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreLevKod C-Win
ON RETURN OF sokOrdreLevKod IN FRAME DEFAULT-FRAME /* Lev.artnr */
DO:
  IF sokOrdreLevKod:MODIFIED THEN 
    RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SokOrdreLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreLevNr C-Win
ON RETURN OF SokOrdreLevNr IN FRAME DEFAULT-FRAME /* Levnr */
DO:
  IF sokOrdreLevnr:MODIFIED THEN DO:
    SokOrdreLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokOrdreLevnr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SokOrdreLevNr C-Win
ON TAB OF SokOrdreLevNr IN FRAME DEFAULT-FRAME /* Levnr */
DO:
  IF sokOrdreLevnr:MODIFIED THEN 
    SokOrdreLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokOrdreLevnr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdremottaker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdremottaker C-Win
ON VALUE-CHANGED OF sokOrdremottaker IN FRAME DEFAULT-FRAME /* Ordremottak */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokOrdreNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokOrdreNr C-Win
ON RETURN OF sokOrdreNr IN FRAME DEFAULT-FRAME /* Ordrenr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hBestBilde)    THEN APPLY "close" TO hBestBilde.
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankFilterRecord C-Win 
PROCEDURE BlankFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
      sokBestOrdreStatus:SCREEN-VALUE     = " "
      sokBestOrdreStatusTil:SCREEN-VALUE  = " "
      sokOrdreNr:SCREEN-VALUE             = "0"
      sokEkstId:SCREEN-VALUE              = ""
      sokOrdreLevDato:SCREEN-VALUE        = STRING(?)
      sokOrdreCL:SCREEN-VALUE             = "0"
      SokOrdreCLNamn:SCREEN-VALUE         = ""
      SokOrdreLevNr:SCREEN-VALUE          = "0"
      SokOrdreLevNamn:SCREEN-VALUE        = ""
      sokOrdreArtikkel:SCREEN-VALUE       = "0"
      SokOrdreArtBeskr:SCREEN-VALUE       = ""
      sokOrdreLevKod:SCREEN-VALUE         = ""
      sokOrdreLevFargKod:SCREEN-VALUE     = ""
      sokOrdremottaker:SCREEN-VALUE       = " "
      sokOpphav:SCREEN-VALUE              = " "
      .
  
   RUN StartQuery.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OverforVaremottakRecord.
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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
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
RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBest THEN DO:
  RUN VisMiniBilde IN hBestBilde 
      (IF hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
         hBrowseOrdreBest:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BildNr"):BUFFER-VALUE 
       ELSE 0).
END.

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
  DISPLAY sokBestOrdreStatus sokBestOrdreStatusTil sokOrdreNr sokEkstId 
          sokOrdreLevDato SokOrdreLevNr SokOrdreLevNamn sokOrdremottaker 
          sokOpphav sokOrdreCL SokOrdreCLNamn sokOrdreArtikkel SokOrdreArtBeskr 
          sokOrdreLevKod sokOrdreLevFargKod 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwOrdre brwArtikler rectToolBar rectWinToolbar BestBilde 
         sokBestOrdreStatus sokBestOrdreStatusTil sokOrdreNr sokEkstId 
         sokOrdreLevDato SokOrdreLevNr btnLev-3 sokOrdremottaker sokOpphav 
         sokOrdreCL btnOrdreLev sokOrdreArtikkel btnOrdreArtikkel 
         sokOrdreLevKod sokOrdreLevFargKod 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR cFraStatus AS CHAR NO-UNDO.
DEF VAR cTilStatus AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  RUN VisMiniBilde.w PERSIST SET hBestBilde.
  RUN InitializeObject IN hBestBilde (BestBilde:HANDLE).
  hBestBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hBestBilde).

  ASSIGN sokBestOrdreStatus:DELIMITER          = "|"
         sokBestOrdreStatusTil:DELIMITER       = "|"
         sokBestOrdreStatus:LIST-ITEM-PAIRS    = "||" + DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter2;Parameter1","WHERE SysHId = 21 AND SysGr = 101 BY Parameter1")
         sokBestOrdreStatusTil:LIST-ITEM-PAIRS = sokBestOrdreStatus:LIST-ITEM-PAIRS
         sokOrdremottaker:LIST-ITEM-PAIRS      = ",,Kjedelevert,KJEDE,Gjennomfaktureres,GJENNOM"
         sokOpphav:LIST-ITEM-PAIRS             = ",,ERP,ERP,HK,HK"
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
           ,brwOrdre:HANDLE
           ,100
           ,"multiple"
          ,"Ordre"
         + ";OrdreNr"
         + ";LeveringsDato"
         + ";LevNr|Levnr"
         + ";CL"
         + ";+OrdreTotAnt|INTEGER|>>>>9|ordre_tot_antall|Antall"
         + ";+OrdreTotPris|INTEGER|->>>>>9.99|ordre_tot_pris|Sum innk.pris"
         + ";+OrdreTotDBkr|INTEGER|->>>>>9.99|ordre_tot_dbkr|Sum DB"
         + ";+OrdreTotAntLev|INTEGER|>>>>9|ordre_tot_levert|Ant.lev"
         + ";+OrdreTotRestAnt|INTEGER|->>>9|ordre_tot_rest|Ant.rest"
         + ";Hasteordre|Haster|Ja/Nei"
         + ";RegistrertDato"
         + ";SendtDato"
         + ";BekreftetDato|Bekr.dato"
         + ";OrdreStatus|St"
         + ";OrdreMottaker"
         + ";!VarebehNr"
/*          + ";!+StatusOgButikkSjekk|CHARACTER|x|ordre_for_butikk(ROWID)"  */
       + ",LevBas"
         + ";LevNamn|Levnavn@4"
       + ",buf1_SysPara"
         + ";Beskrivelse|Status@16"
         ,"WHERE false"
        + ",FIRST LevBas OF Ordre NO-LOCK"
        + ",FIRST buf1_SysPara WHERE  buf1_SysPara.SysHId = 5 and buf1_SysPara.SysGr = 3 AND buf1_SysPara.ParaNr = Ordre.OrdreStatus NO-LOCK OUTER-JOIN"
         ,""
          ).
  DYNAMIC-FUNCTION("setSortString",hBrowse,"ordrenr;desc").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"copytoolbartobrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsbrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","ordre_browsekalk.p").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                         
                    "BrowseConfig,Excel;Eksporter til E&xcel"
                    + ",InnlevRapport;Innleveranserapport"
                    + ",OverforVaremottak;Overfør til varemottak"
                    + ",BlankFilter;&Blank filter"
                    ,"maxborder").  
  
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","BlankFilter").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).


  hBrowseOrdreBest = DYNAMIC-FUNCTION("NewBrowse",
                    brwArtikler:HANDLE,
                    100,
                    "",
                    "BestHode"
                    + ";BestStat|BS"
                    + ";LevKod"
                    + ";LevFargKod|Lev.farge"
                    + ";TotAntPar|Ant"
                    + ";BestNr"
                    + ";ArtikkelNr|Art.nr"
                    + ";BekreftetDato|Bekr.dato"
                    + ";EkstId|Ekstern id"
                    + ";TotInnkjVerdi|Sum varekost"
                    + ";TotSalgsVerdi|Sum pris"
                    + ";TotInnLev"
                    + ";TotMakulert"
                    + ";+BestTotRestAnt|INTEGER|->>>9|best_tot_rest.p|Ant.rest"
                    + ";BestType"
                    + ";SendtAv"
                    + ";SendtDato"
                    + ";+Kjedevare|LOGICAL|J/N|kjedevare|Kj.lev"
                    + ";+Gjennomfaktureres|LOGICAL|J/N|gjennomfaktureres|Gj.fakt"
                    + ";!OrdreNr"
                  + ",ArtBas"
                    + ";Beskr@6"
                    + ";!BildNr"
                  + ",buf2_SysPara"
                    + ";Beskrivelse|Status@2"
                    ,"WHERE false"
                   + ",FIRST ArtBas OF BestHode NO-LOCK"
                   + ",FIRST buf2_SysPara WHERE  buf2_SysPara.SysHId = 5 and buf2_SysPara.SysGr = 2 AND buf2_SysPara.ParaNr = BestHode.BestStat NO-LOCK OUTER-JOIN"
                    ,"sort|BestNr").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBest,hBrowse,"OrdreNr").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdreBest,"calcfieldproc","butvarebeh_besthode_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdreBest,"copytoolbartobrowse","yes").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowseOrdreBest
                   ,"MultiSortBrowse;Sorter på flere kolonner"
                  + ",BrowseConfig;Kolonneoppsett"
                  ,"").

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE, 
                    "Fil",                
                    "close;Avslutt",
                    "right,enable").

  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(brwOrdre:HANDLE) + ","
                  + STRING(hBrowse) + ","
                  + STRING(brwArtikler:HANDLE) + ","
                  + STRING(hBrowseOrdreBest)
                    ).
  
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "CustomerNavBrowse,searchField," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "CustomerNavToolbar").
  
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,640,200,0,0).

  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"initOrdreStatusSearch") THEN DO:      
    RUN initOrdreStatusSearch IN SOURCE-PROCEDURE (OUTPUT cFraStatus,OUTPUT cTilStatus, OUTPUT bOk).
    ASSIGN sokBestOrdreStatus:SCREEN-VALUE = cFraStatus
           sokBestOrdreStatusTil:SCREEN-VALUE = cTilStatus
           NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND bOk THEN
      RUN StartQuery.
  END.
  IF SOURCE-PROCEDURE:FILE-NAME = "PkSdlBrw.w" THEN
    hPakkListe = SOURCE-PROCEDURE.
END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevRapportRecord C-Win 
PROCEDURE InnlevRapportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF (DYNAMIC-FUNCTION("getCurrentObject") = hToolbar OR DYNAMIC-FUNCTION("getCurrentObject") = hBrowse)
   AND hBrowse:NUM-SELECTED-ROWS NE 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Innleveranserapport kan bare kjøres for en ordre","","").
  RETURN.
END.
RUN skrivMottaksrapport.p (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordrenr"):BUFFER-VALUE,"","").

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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforVaremottakRecord C-Win 
PROCEDURE OverforVaremottakRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDummy        AS CHAR NO-UNDO.
DEF VAR iReturn       AS INT  NO-UNDO.
DEF VAR cOrdreNrList  AS CHAR NO-UNDO.
DEF VAR cRowIdList    AS CHAR NO-UNDO.
DEF VAR cPkSdlId      AS CHAR NO-UNDO.
DEF VAR cMsg          AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdSelVal.w ("Overfør valgte ordre til varemottaksprogram / pakkliste",
                              hBrowse:NUM-SELECTED-ROWS,
                              INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordcount")),
                              "",
                              OUTPUT cDummy,
                              OUTPUT iReturn).

IF iReturn = 0 THEN RETURN.


IF iReturn = 2 THEN DO: /* Behandle valgte */
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      ASSIGN cOrdreNrList = cOrdreNrList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrdreNr"):BUFFER-VALUE) + "|"
             cRowIdList   = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  END.

  IF DYNAMIC-FUNCTION("runproc","ordre_overfor_pakkliste.p",TRIM(cOrdreNrList,"|"),?) THEN 
  OVERFOR_ORDRE:
  DO:
    ASSIGN cPkSdlId = ENTRY(2,DYNAMIC-FUNCTION("getTransactionMessage"),"|")
           cMsg     = ENTRY(1,DYNAMIC-FUNCTION("getTransactionMessage"),"|").
                    
    IF cMsg NE "" THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,cMsg,"","").

    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
    IF NOT VALID-HANDLE(hPakkliste) THEN 
      DO:        
        RUN PkSdlHode.w PERSIST SET hPakkliste.
        RUN InitializeObject IN hPakkliste.
      END.

    RUN setNyPkSdlQuery IN hPakkliste (cPkSdlId,hBrowse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      RUN setQuery IN hPakkliste (cPkSdlId,hBrowse) NO-ERROR.

    APPLY "close" TO THIS-PROCEDURE.
  END. /* OVERFOR_ORDRE*/
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQuery C-Win 
PROCEDURE StartQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  ASSIGN sokOrdreCL sokOrdreLevDato sokOrdreLevNr sokOrdreNr sokEkstId sokOrdreLevKod sokOrdreLevFargKod
         sokOrdreLevDato:MODIFIED       = FALSE
         sokOrdreLevNr:MODIFIED         = FALSE
         sokOrdreNr:MODIFIED            = FALSE
         sokBestOrdreStatus:MODIFIED    = FALSE
         sokOrdreCL:MODIFIED            = FALSE
         sokEkstId:MODIFIED             = NO
         sokOrdreLevFargKod:MODIFIED    = NO
         sokOrdreLevKod:MODIFIED        = NO
         .

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",
                   (IF sokOrdreCL NE 0 THEN
                     " AND CL = " + sokOrdreCL:SCREEN-VALUE
                    ELSE "")
                 + (IF sokOrdreLevDato NE ? THEN
                     " AND LeveringsDato = DATE('" + sokOrdreLevDato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF sokOrdreLevNr:SCREEN-VALUE NE "0" THEN
                     " AND LevNr = " + sokOrdreLevNr:SCREEN-VALUE
                    ELSE "")
                 + (IF sokOrdreNr NE 0 THEN 
                     " AND OrdreNr = " + sokOrdreNr:SCREEN-VALUE
                    ELSE "")
                 + (IF sokOrdremottaker:SCREEN-VALUE NE "" AND sokOrdremottaker:SCREEN-VALUE NE ? THEN 
                     " AND Ordremottaker = '" + sokOrdremottaker:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF sokOpphav:SCREEN-VALUE NE "" AND sokOpphav:SCREEN-VALUE NE ? THEN 
                     " AND Opphav = '" + sokOpphav:SCREEN-VALUE + "'"
                    ELSE "")
                   ).

/*   IF sokBestOrdreStatus:SCREEN-VALUE NE ? AND sokBestOrdreStatus:SCREEN-VALUE NE "" THEN                            */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamStatusOgButikkSjekk",sokBestOrdreStatus:SCREEN-VALUE + "¤*"). */
/*   ELSE                                                                                                              */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamStatusOgButikkSjekk","¤*").                                   */

  IF sokOrdreArtikkel:SCREEN-VALUE NE "0" OR sokEkstId NE "" OR sokOrdreLevKod NE "" OR sokOrdreLevFargKod NE "" OR
     (sokBestOrdreStatus:SCREEN-VALUE NE ? AND sokBestOrdreStatus:SCREEN-VALUE NE "") OR 
     (sokBestOrdreStatusTil:SCREEN-VALUE NE ? AND sokBestOrdreStatusTil:SCREEN-VALUE NE "") THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter",
                     "BestHode WHERE true"
                   + (IF SokOrdreLevNr:SCREEN-VALUE NE "0" THEN 
                        " AND Levnr = " + SokOrdreLevNr:SCREEN-VALUE
                      ELSE "")
                   + (IF sokOrdreCL NE 0 THEN
                       " AND CL = " + sokOrdreCL:SCREEN-VALUE
                      ELSE "")
                   + (IF sokOrdreArtikkel:SCREEN-VALUE NE "0" THEN 
                        " AND Artikkelnr = " + sokOrdreArtikkel:SCREEN-VALUE
                      ELSE "")
                   + (IF sokEkstId NE "" THEN 
                       " AND EkstId = '" + sokEkstId + "'"
                      ELSE "")
                   + (IF sokOrdreLevKod NE "" THEN 
                       " AND LevKod BEGINS '" + sokOrdreLevKod + "'"
                      ELSE "")
                   + (IF sokOrdreLevFargKod NE "" THEN 
                       " AND LevFargKod BEGINS '" + sokOrdreLevFargKod + "'"
                      ELSE "")
                   + (IF sokBestOrdreStatus:SCREEN-VALUE NE ? AND sokBestOrdreStatus:SCREEN-VALUE NE "" THEN
                       " AND BestHode.BestStat " 
                     + (IF sokBestOrdreStatusTil:SCREEN-VALUE NE ? AND sokBestOrdreStatusTil:SCREEN-VALUE NE "" THEN "GE " ELSE "= ")
                     + (IF sokBestOrdreStatus:SCREEN-VALUE NE "44" THEN sokBestOrdreStatus:SCREEN-VALUE
                        ELSE "4 AND BestHode.BekreftetDato NE ?") 
                      ELSE "")
                   + (IF sokBestOrdreStatusTil:SCREEN-VALUE NE ? AND sokBestOrdreStatusTil:SCREEN-VALUE NE "" THEN
                       " AND BestHode.BestStat " 
                     + (IF sokBestOrdreStatus:SCREEN-VALUE NE ? AND sokBestOrdreStatus:SCREEN-VALUE NE "" THEN "LE " ELSE "= ")
                     + (IF sokBestOrdreStatusTil:SCREEN-VALUE NE "44" THEN sokBestOrdreStatusTil:SCREEN-VALUE
                        ELSE "4 AND BestHode.BekreftetDato NE ?") 
                      ELSE "")
                   + ",FIRST Ordre OF BestHode NO-LOCK").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter","").

END.

RUN InvokeMethod(hBrowse,"OpenQuery").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "BrwArtikler" THEN
  ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 25
         ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 50
         ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 55
         ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 50
         ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 38.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

