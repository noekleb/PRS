&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

&SCOPED-DEFINE PureABLWin 0

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR ihParent             AS HANDLE NO-UNDO.

  DEF VAR iiBatchSize          AS INT    NO-UNDO INIT 200.

  DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "FakturaHode;iFakturaNr;cKundeNavn".
  DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true".

/*   DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "Dokument;iDokNr;cDokType,Sak;cSaksNr;cSaksTittel".          */
/*   DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true,first sak where can-do('80,82',string(iSakId))". */

/*   DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn". */
/*   DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true, first Avdeling OF HuvGr NO-LOCK".       */

  DEF VAR iocSelectedRows      AS CHAR   NO-UNDO.
  DEF VAR icOutputFields       AS CHAR   NO-UNDO.
  DEF VAR iocOutputValues      AS CHAR   NO-UNDO.

  DEF VAR icServerSelectProc   AS CHAR   NO-UNDO. /* INIT "=test_updstrtstr.p".   */
  DEF VAR icServerDeSelectProc AS CHAR   NO-UNDO INIT "".

  DEF VAR obOK                 AS LOG    NO-UNDO.

  icSourceTableAndFlds = 
                    "untGuestInformation"
                    + ";GuestInformationStatusDate"
                    + ";GuestInformationNote"
                    .

  icSourceQuery = 
                  "WHERE GuestObj = DEC('" + STRING(15.1) + "')"
                  .

  icSourceTableAndFlds = 
                    "vargr"
                    + ";!REgistrertAv"
                    + ";vg"
                    + ";vgbeskr"
                    .

  icSourceQuery = 
                  "WHERE true"
                  .
&ELSE
  DEF INPUT PARAM ihParent               AS HANDLE NO-UNDO.

  DEF INPUT PARAM iiBatchSize            AS INT  NO-UNDO.
  DEF INPUT PARAM icSourceTableAndFlds   AS CHAR NO-UNDO.
  DEF INPUT PARAM icSourceQuery          AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocSelectedRows AS CHAR NO-UNDO.
  DEF INPUT PARAM icOutputFields  AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocOutputValues AS CHAR NO-UNDO.

  DEF INPUT PARAM icServerSelectProc     AS CHAR NO-UNDO.
  DEF INPUT PARAM icServerDeSelectProc   AS CHAR NO-UNDO.

  DEF OUTPUT PARAM obOK                  AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR bOK                AS LOG    NO-UNDO.

DEF VAR cSelRowidList      AS CHAR   NO-UNDO.
DEF VAR cDeSelRowidList    AS CHAR   NO-UNDO.

DEF VAR hBrwSource         AS HANDLE NO-UNDO.
DEF VAR hBrwTarget         AS HANDLE NO-UNDO.
DEF VAR hBuffSource        AS HANDLE NO-UNDO.
DEF VAR hBuffTarget        AS HANDLE NO-UNDO.
DEF VAR hSearchFieldSource AS HANDLE NO-UNDO.

DEF VAR httTarget          AS HANDLE NO-UNDO.
DEF VAR cSortColumn        AS CHAR   NO-UNDO.
DEF VAR bDesc              AS LOG    NO-UNDO.
DEF VAR hSourceToolBar     AS HANDLE NO-UNDO.
DEF VAR hFilToolBar        AS HANDLE NO-UNDO.
DEF VAR hWinToolBar        AS HANDLE NO-UNDO.

DEF VAR iSelectIndex       AS INT    NO-UNDO.
DEF VAR iDeSelectIndex     AS INT    NO-UNDO.
DEF VAR cBaseQuerySource   AS CHAR   NO-UNDO.
DEF VAR cCurrMaxValueField AS CHAR   NO-UNDO.
DEF VAR oSelector          AS JBoxDynSelector NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDeSelect rectBrwSource rectSearchSource ~
rectBrwTarget rectWinToolBar SourceToolbar Btn_OK Btn_Cancel btnDeSelectAll ~
btnSelect btnSelectAll 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD adjustBrowseHeight C-Win 
FUNCTION adjustBrowseHeight RETURNS HANDLE
  ( INPUT iiDeltaY AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOutputData C-Win 
FUNCTION getOutputData RETURNS CHARACTER
  ( INPUT icOutputFields AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTargetToolbar C-Win 
FUNCTION getTargetToolbar RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMaxValueField C-Win 
FUNCTION setMaxValueField RETURNS LOGICAL
  ( INPUT icCurrMaxValueField AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectedRowids C-Win 
FUNCTION setSelectedRowids RETURNS LOGICAL
  ( INPUT icSelRowidList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UndoSelection C-Win 
FUNCTION UndoSelection RETURNS LOGICAL
  ( INPUT icUndoType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDeSelect 
     IMAGE-UP FILE "bmp/vcrrew.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnselectall 3" 
     SIZE 4.6 BY 1.14 TOOLTIP "Fjern valgt(e)".

DEFINE BUTTON btnDeSelectAll 
     IMAGE-UP FILE "bmp/vcrall.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnselectall 2" 
     SIZE 4.6 BY 1.14 TOOLTIP "Fjern alle".

DEFINE BUTTON btnSelect 
     IMAGE-UP FILE "bmp/vcrfwd.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnselectall 4" 
     SIZE 4.6 BY 1.14 TOOLTIP "Legg til valgt(e)".

DEFINE BUTTON btnSelectAll 
     IMAGE-UP FILE "bmp/vcrnon.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 2" 
     SIZE 4.6 BY 1.14 TOOLTIP "Legg til alle".

DEFINE BUTTON Btn_Cancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE rectBrwSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 14.76.

DEFINE RECTANGLE rectBrwTarget
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 14.76.

DEFINE RECTANGLE rectSearchSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY .95.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 10.2 BY 1.05.

DEFINE RECTANGLE SourceToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.8 BY .91.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 2 BY 14.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDeSelect AT ROW 11.05 COL 51.6
     Btn_OK AT ROW 17.29 COL 73.2 NO-TAB-STOP 
     Btn_Cancel AT ROW 17.29 COL 89.2 NO-TAB-STOP 
     btnDeSelectAll AT ROW 12.19 COL 51.6
     btnSelect AT ROW 9.91 COL 51.6
     btnSelectAll AT ROW 8.76 COL 51.6
     rectBrwSource AT ROW 3.62 COL 1.8
     rectSearchSource AT ROW 2.48 COL 2
     rectBrwTarget AT ROW 3.62 COL 57.6
     rectWinToolBar AT ROW 1.24 COL 94
     SourceToolbar AT ROW 1.38 COL 43.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.6 BY 17.62.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1.05 COL 10.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 39.6 ROW 3.67
         SIZE 22.8 BY 14.67.


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
         TITLE              = "Velg"
         HEIGHT             = 17.52
         WIDTH              = 104.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MIN-BUTTON         = no
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
ON END-ERROR OF C-Win /* Velg */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Velg */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Velg */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeSelect C-Win
ON CHOOSE OF btnDeSelect IN FRAME DEFAULT-FRAME /* btnselectall 3 */
DO:
  RUN DeSelectRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeSelectAll C-Win
ON CHOOSE OF btnDeSelectAll IN FRAME DEFAULT-FRAME /* btnselectall 2 */
DO:
  RUN DeSelectAllRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* btnselectall 4 */
DO:
  RUN SelectRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll C-Win
ON CHOOSE OF btnSelectAll IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  RUN SelectAllRecord.
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
&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON ANY-PRINTABLE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  IF VALID-HANDLE(hSearchFieldSource) THEN DO:
    APPLY "entry" TO hSearchFieldSource.
    APPLY LASTKEY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  DEF VAR iReturn     AS INT NO-UNDO INIT 6.
  DEF VAR hLastWidget AS HANDLE NO-UNDO.

  hLastWidget = DYNAMIC-FUNCTION("getLinkedObject",hBrwTarget,"browseoverlay","from").

  IF PROGRAM-NAME(2) MATCHES "*JBoxSelector.w" AND VALID-HANDLE(hLastWidget) AND hLastWidget:TYPE = "fill-in" THEN
    APPLY "return" TO hLastWidget.

  JBoxServerAPI:Instance:SelectorDeselectRowidList = TRIM(cDeSelRowidList,",").
  
  IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"getSelectorAttributes") THEN
    RUN getSelectorAttributes IN ihParent(hBrwSource,hBrwTarget,TRIM(cDeSelRowidList,","),OUTPUT iReturn).

  IF iReturn = 2 THEN RETURN NO-APPLY.

  IF icOutputFields NE "" THEN
    iocOutputValues = getOutputData(icOutputFields).

  ASSIGN cSelRowidList   = TRIM(cSelRowidList,",")
         cDeSelRowidList = TRIM(cDeSelRowidList,",")
         iocSelectedRows = cSelRowidList
         obOK            = TRUE
         .

  APPLY "close" TO THIS-PROCEDURE.
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
  IF VALID-HANDLE(ihParent:CURRENT-WINDOW) THEN
    ihParent:CURRENT-WINDOW:MOVE-TO-TOP().
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).

  RUN enable_UI.

  RUN InitWindow.

  SUBSCRIBE PROCEDURE ihParent TO "RowDisplaySelector" IN THIS-PROCEDURE.

  IF VALID-HANDLE(ihParent) THEN
    SUBSCRIBE TO "ExpandSelectorDialog" IN ihParent.
  ELSE
    SUBSCRIBE TO "ExpandSelectorDialog" ANYWHERE.

  IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"mySelectorObject") THEN DO:
    oSelector = NEW JBoxDynSelector(ihParent,THIS-PROCEDURE,hBrwSource,hBrwTarget,SourceToolbar:HANDLE).
    RUN mySelectorObject IN ihParent (oSelector).
  END.  
  ELSE IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"setSelectorAttributes") THEN
    RUN setSelectorAttributes IN ihParent (hBrwSource,hBrwTarget).
  ELSE IF VALID-HANDLE(SOURCE-PROCEDURE) AND CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"setSelectorAttributes") THEN
    RUN setSelectorAttributes IN SOURCE-PROCEDURE (hBrwSource,hBrwTarget).

  IF NOT VALID-HANDLE(hBrwSource:POPUP-MENU) THEN
    DYNAMIC-FUNCTION("NewMenuBand",
                      hBrwSource,  /* parent widget */
                      "MultiSortBrowse;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sorter på flere kolonner" ELSE "Sort on multipe columns")
                    + ",Excel","").     

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  
  JBoxSession:Instance:setDelayedFocus(THIS-PROCEDURE:CURRENT-WINDOW,hBrwSource).
  
  ON 'default-action':U OF hBrwSource
  DO:
    APPLY "choose" TO btnSelect IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
  ON 'default-action':U OF hBrwTarget
  DO:
    APPLY "choose" TO btnDeSelect IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.

  Btn_OK:TAB-STOP = TRUE.

  APPLY "entry" TO hBrwSource.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeSelectAllRecord C-Win 
PROCEDURE DeSelectAllRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrwTarget:QUERY:IS-OPEN THEN RETURN.

hBrwTarget:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwTarget:QUERY:QUERY-OFF-END:
  ASSIGN cSelRowidList = REPLACE(cSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
         cSelRowidList = REPLACE(cSelRowidList,",,",",")
         .

  IF NOT CAN-DO(cDeSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
    cDeSelRowidList = cDeSelRowidList + hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".

  IF icServerDeSelectProc = "" THEN 
    hBuffTarget:BUFFER-DELETE().

  hBrwTarget:QUERY:GET-NEXT().
END.

IF icServerDeSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cDeSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffTarget:NAME,
                  icServerDeSelectProc,
                  "",
                  ENTRY(ix,cDeSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.

RUN OpenQuerySource.
RUN OpenQueryTarget.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeSelectRecord C-Win 
PROCEDURE DeSelectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrwTarget:NUM-SELECTED-ROWS > 0 THEN 
  DO ix = 1 TO hBrwTarget:NUM-SELECTED-ROWS:
    bOk = hBrwTarget:FETCH-SELECTED-ROW(ix).
    IF bOK THEN DO:
      ASSIGN cSelRowidList = REPLACE(cSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
             cSelRowidList = REPLACE(cSelRowidList,",,",",")
             .
  
      IF NOT CAN-DO(cDeSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
        cDeSelRowidList = cDeSelRowidList + hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".
  
      IF icServerDeSelectProc = "" THEN 
        hBuffTarget:BUFFER-DELETE().
    END.
  END.
ELSE RETURN.

IF icServerDeSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cDeSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffTarget:NAME,
                  icServerDeSelectProc,
                  "",
                  ENTRY(ix,cDeSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.
ELSE IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"setSelectorTarget") THEN
  RUN setSelectorTarget IN ihParent (hBrwTarget,NO,"",cDeSelRowidList).

RUN OpenQuerySource.
RUN OpenQueryTarget.
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
  ENABLE btnDeSelect rectBrwSource rectSearchSource rectBrwTarget 
         rectWinToolBar SourceToolbar Btn_OK Btn_Cancel btnDeSelectAll 
         btnSelect btnSelectAll 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expandSelectorDialog C-Win 
PROCEDURE expandSelectorDialog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM iiDeltaX AS INT NO-UNDO.
DEF INPUT PARAM iiDeltaY AS INT NO-UNDO.

IF ihBrowse NE hBrwSource THEN RETURN.

DEF VAR hSelectorSplitB AS HANDLE NO-UNDO.

ASSIGN {&WINDOW-NAME}:WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS + iiDeltaX
       {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS + iiDeltaY
       hSelectorSplitB            = DYNAMIC-FUNCTION("getSplitBarHandle",{&WINDOW-NAME},"x")
       .
APPLY "window-resized" TO {&WINDOW-NAME}.
hSelectorSplitB:X = hSelectorSplitB:FRAME:WIDTH-PIXELS / 2 - hSelectorSplitB:WIDTH-PIXELS / 2.

APPLY "end-move" TO hSelectorSplitB.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cInitSourceSort    AS CHAR NO-UNDO.
DEF VAR cJoin              AS CHAR NO-UNDO.
DEF VAR cBaseQueryTarget   AS CHAR NO-UNDO.
DEF VAR cSelectedQuery     AS CHAR NO-UNDO INIT "WHERE false".
DEF VAR iy                 AS INT  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cSelRowidList = IF iocSelectedRows NE "" THEN iocSelectedRows + "," ELSE "".

  IF NUM-ENTRIES(icSourceQuery,";") = 2 THEN
    ASSIGN cSelectedQuery = ENTRY(2,icSourceQuery,";")
           icSourceQuery  = ENTRY(1,icSourceQuery,";")
           .

  /* If transaction handling outside the selector we need the OK/Cancel buttons: */
  IF icServerSelectProc = "" THEN 
    ASSIGN rectSearchSource:Y  = rectSearchSource:Y - 26
           rectBrwSource:Y     = rectBrwSource:Y - 24
           rectBrwTarget:Y     = rectBrwTarget:Y - 24
           FRAME frSplitBarX:Y = FRAME frSplitBarX:Y - 24
           btnSelectAll:Y      = btnSelectAll:Y - 24
           btnSelect:Y         = btnSelect:Y - 24
           btnDeSelect:Y       = btnDeSelect:Y - 24
           btnDeSelectAll:Y    = btnDeSelectAll:Y - 24
           SourceToolbar:Y     = rectSearchSource:Y
           rectWinToolBar:HIDDEN = TRUE
           .
  ELSE
    ASSIGN Btn_OK:HIDDEN = TRUE
           Btn_Cancel:HIDDEN = TRUE.

  IF NUM-ENTRIES(icSourceTableAndFlds,"@1") = 2 THEN DO:
    cInitSourceSort = ENTRY(1,icSourceTableAndFlds,"@1").
    cInitSourceSort = ENTRY(1,ENTRY(NUM-ENTRIES(cInitSourceSort,";"),cInitSourceSort,";"),"|").
    cSortColumn = cInitSourceSort.
    cInitSourceSort = "SORT|" + cInitSourceSort.
  END.  
  ELSE IF NUM-ENTRIES(ENTRY(1,icSourceTableAndFlds),";") > 1 AND NOT ENTRY(2,ENTRY(1,icSourceTableAndFlds),";") BEGINS "!" THEN
    ASSIGN cInitSourceSort = "SORT|" + ENTRY(1,ENTRY(2,ENTRY(1,icSourceTableAndFlds),";"),"|")
           cSortColumn     = ENTRY(1,ENTRY(2,ENTRY(1,icSourceTableAndFlds),";"),"|")
           .
  ELSE DO: 
    rectSearchSource:HIDDEN = TRUE.
    IF NUM-ENTRIES(ENTRY(1,icSourceTableAndFlds),";") > 1 AND ENTRY(2,ENTRY(1,icSourceTableAndFlds),";") = "!" THEN
      icSourceTableAndFlds = REPLACE(icSourceTableAndFlds,";!;",";").
  END.  

  DO ix = 1 TO LENGTH(icSourceQuery):
    IF SUBSTR(icSourceQuery,ix,6) = "first " OR
       SUBSTR(icSourceQuery,ix,5) = "last " OR
       SUBSTR(icSourceQuery,ix,5) = "each " 
       THEN DO:
      DO iy = ix TO ix - 10 BY -1:
        IF SUBSTR(icSourceQuery,iy,1) = "," THEN LEAVE.
      END.
      LEAVE.
    END.
  END.
  
  IF iy > 1 THEN 
    ASSIGN cBaseQuerySource = SUBSTR(icSourceQuery,1,iy - 1)
           cJoin            = SUBSTR(icSourceQuery,iy)
           .
  ELSE cBaseQuerySource = icSourceQuery.

  /* Define the browsers: */
/*   IF NUM-ENTRIES(icSourceQuery) > 1 THEN DO:                                                                  */
/*     IF (INDEX(icSourceQuery,"can-do") > 0 OR INDEX(icSourceQuery,"dec(") > 0) AND                             */
/*        INDEX(icSourceQuery,"(") < INDEX(icSourceQuery,",") THEN                                               */
/*       ASSIGN cBaseQuerySource = SUBSTR(icSourceQuery,1,INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")) - 1) */
/*              cJoin            = IF INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")) > 0 THEN                 */
/*                                   SUBSTR(icSourceQuery,INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")))     */
/*                                 ELSE ""                                                                       */
/*                                 .                                                                             */
/*     ELSE                                                                                                      */
/*       ASSIGN cBaseQuerySource = ENTRY(1,icSourceQuery)                                                        */
/*              cJoin            = SUBSTR(icSourceQuery,INDEX(icSourceQuery,","))                                */
/*              .                                                                                                */
/*   END.                                                                                                        */
/*   ELSE cBaseQuerySource = icSourceQuery.                                                                      */

  IF NUM-ENTRIES(icSourceTableAndFlds) > 1 THEN
    icSourceTableAndFlds = ENTRY(1,icSourceTableAndFlds) + ";+!iJBoxSelectIndex|INTEGER;+!iJBoxDeSelectIndex|INTEGER" + SUBSTR(icSourceTableAndFlds,INDEX(icSourceTableAndFlds,",")).
  ELSE 
    icSourceTableAndFlds = icSourceTableAndFlds + ";+!iJBoxSelectIndex|INTEGER;+!iJBoxDeSelectIndex|INTEGER".

  hBrwSource = DYNAMIC-FUNCTION("NewBrowse",           /* Create a browse object */
                    rectBrwSource:HANDLE,              /* Rectangle to define coordinates for browse */
                    iiBatchSize,                       /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",   /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    icSourceTableAndFlds,              /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    cBaseQuerySource + 
                       cJoin,
                    cInitSourceSort
                    ).    
  ASSIGN hBrwSource:NAME = "brwSource"
         hBuffSource = hBrwSource:QUERY:GET-BUFFER-HANDLE(1)
         .
  
  IF cBaseQuerySource = "where false" THEN cBaseQuerySource = "where true".
  ELSE DYNAMIC-FUNCTION("setAttribute",hBrwSource,"uselocaldata","yes").

  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource).
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querywhere","").

  IF cInitSourceSort NE "" THEN DO:
    hSearchFieldSource = DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchSource:HANDLE,hBrwSource,1).
    DYNAMIC-FUNCTION("CreateObjectLink",hSearchFieldSource,hBrwSource).
  END.

  hBrwTarget = DYNAMIC-FUNCTION("NewBrowse",           /* Create a browse object */
                    rectBrwTarget:HANDLE,              /* Rectangle to define coordinates for browse */
                    10000,                             /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",   /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    icSourceTableAndFlds,              /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    cSelectedQuery + cJoin,
                    cInitSourceSort
                    ).
  ASSIGN hBrwTarget:NAME = "brwTarget"
         hBuffTarget = hBrwTarget:QUERY:GET-BUFFER-HANDLE(1)
         .

  DYNAMIC-FUNCTION("setAttribute",hBrwTarget,"querywhere","").
  
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"nextTabItem",STRING(hBrwTarget)).
  DYNAMIC-FUNCTION("setAttribute",hBrwTarget,"nextTabItem",STRING(Btn_OK:HANDLE)).

  IF iocSelectedRows NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(iocSelectedRows):
      bOk = hBuffSource:FIND-FIRST("where rowident1 = '" + ENTRY(ix,iocSelectedRows) + "'") NO-ERROR.
      IF bOK THEN DO:
        hBuffTarget:BUFFER-CREATE().
        hBuffTarget:BUFFER-COPY(hBuffSource).
      END.
    END.
    cSelRowidList = iocSelectedRows + ",".
    RUN OpenQuerySource.
    RUN OpenQueryTarget.
  END.

  /* Define the toolbars: */

  hSourceToolBar = DYNAMIC-FUNCTION("NewToolbar",
                   SourceToolbar:HANDLE,
                   "",
                   "filter",
                   "maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrwSource,hSourceToolbar).

  hFilToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,
                    IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File", 
                    IF DYNAMIC-FUNCTION("Scandinavian") THEN "|Fil" ELSE "|File",
                    "").

  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hFilToolBar,"placeholder1")),
                    IF DYNAMIC-FUNCTION("Scandinavian") THEN
                      "SelectAll;Legg til alle,Select;Legg til valgt(e),Deselect;Fjern valgt(e),DeselectAll;Fjern alle"
                    ELSE   
                      "SelectAll;Add all,Select;Add selected,Deselect;Remove selected,DeselectAll;Remove all"
                    ,"").

  IF icServerSelectProc NE "" THEN
    hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                      rectWinToolBar:HANDLE,             
                      "", 
                      IF DYNAMIC-FUNCTION("Scandinavian") THEN
                        "|Innstillinger,Help|Hjelp;Hjelp,Close|Fil;Avslutt"
                      ELSE
                        "|Settings,Help|Help;Help,Close|File;Exit"
                     ,"maxborder"). 


  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,
                    btnSplitBarX:HANDLE IN FRAME frSplitBarX,
                    STRING(hBrwSource) + "," +
                    STRING(rectBrwSource:HANDLE) + "," +
                    STRING(hBrwTarget) + "," +
                    STRING(rectBrwTarget:HANDLE) + "," +
                    STRING(btnSelectAll:HANDLE) + "," +
                    STRING(btnSelect:HANDLE) + "," +
                    STRING(btnDeSelect:HANDLE) + "," +
                    DYNAMIC-FUNCTION("getAttribute",hSourceToolbar,"buttonFilter") + "," +
                    STRING(btnDeSelectAll:HANDLE)
                   ).

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwSource,rectBrwSource,rectSearchSource").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectSearchSource,rectWinToolBar,SourceToolbar").
  DYNAMIC-FUNCTION("setMoveYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,"btnSelect,btnSelectAll,btnDeselect,btnDeselectAll").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,210,230,0,0). 

  LocalTranslation().

  DYNAMIC-FUNCTION("SetToolbar",hWinToolBar,"enable").

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadPanel C-Win 
PROCEDURE LoadPanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icPanelName AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ohPanel     AS HANDLE NO-UNDO.

IF SEARCH(icPanelName) NE ? OR SEARCH(RIGHT-TRIM(icPanelName,".w") + ".r") NE ? THEN DO:
  RUN VALUE(icPanelName) PERSIST SET ohPanel.
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,ohPanel,"procedure").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffEnd C-Win 
PROCEDURE OffEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF icServerSelectProc = "" AND DYNAMIC-FUNCTION("GetCurrentObject") = hBrwTarget THEN
  RETURN.
ELSE RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffHome C-Win 
PROCEDURE OffHome :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF icServerSelectProc = "" AND DYNAMIC-FUNCTION("GetCurrentObject") = hBrwTarget THEN
  RETURN.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuerySource C-Win 
PROCEDURE OpenQuerySource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF TRIM(cSelRowidList,",") NE "" THEN
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource +
                  " AND NOT CAN-DO('" + TRIM(cSelRowidList,",") + "',Rowident1)").

ELSE 
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource).

DYNAMIC-FUNCTION("setCurrentObject",hBrwSource).
RUN OpenQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryTarget C-Win 
PROCEDURE OpenQueryTarget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cModSortColumn AS CHAR NO-UNDO.

IF icServerSelectProc = "" THEN DO:

  IF cSortColumn NE "" AND SUBSTR(cSortColumn,LENGTH(cSortColumn)) = "]" THEN
    cModSortColumn = "jbextent_" + RIGHT-TRIM(SUBSTR(cSortColumn,R-INDEX(cSortColumn,"[") + 1),"]") + "_" + SUBSTR(cSortColumn,1,R-INDEX(cSortColumn,"[") - 1).
  ELSE cModSortColumn = cSortColumn.

  hBrwTarget:QUERY:QUERY-PREPARE("FOR EACH " + hBuffTarget:NAME + 
                                 (IF cSortColumn NE "" THEN " BY " + cModSortColumn +
                                    (IF bDesc THEN " DESC" ELSE "")
                                  ELSE "")).
  hBrwTarget:QUERY:QUERY-OPEN().

  PUBLISH "EndJBoxEvent" (hBrwTarget,THIS-PROCEDURE,hBrwTarget,"OpenQuery").

END.
ELSE DO:
  DYNAMIC-FUNCTION("SetCurrentObject",hBrwTarget).
  RUN OpenQuery.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwSource THEN
  PUBLISH "RowDisplaySelector" (hBrwSource,hBuffSource).
ELSE
  PUBLISH "RowDisplaySelector" (hBrwTarget,hBuffTarget).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectAllRecord C-Win 
PROCEDURE SelectAllRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iNewBatch AS INT NO-UNDO.

IF NOT hBrwSource:QUERY:IS-OPEN THEN RETURN.

hBrwSource:SELECT-ALL().
RUN SelectRecord.
/*

IF DYNAMIC-FUNCTION("getAttribute",hBrwSource,"lastrowid") = "" THEN DO:
  RUN dAskForQueryBatch.w (OUTPUT iNewBatch).
  IF iNewBatch NE 0 THEN DO:
    bDesc = IF DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querydesc") = "desc" THEN TRUE ELSE FALSE.
    DYNAMIC-FUNCTION("setAttribute",hBrwSource,"rowstobatch",STRING(iNewBatch)).

    iNewBatch = DYNAMIC-FUNCTION("fillBrowse",hBrwSource,
                        iNewBatch,
                        INT(DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querystart")),
                        DYNAMIC-FUNCTION("getAttribute",hBrwSource,"buffersandfields"),
                        DYNAMIC-FUNCTION("getAttribute",hBrwSource,"basequery") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrwSource,"queryfilter") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querywhere") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrwSource,"queryjoin"),
                        DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querysort"),
                        bDesc).
    DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querystart",STRING(iNewBatch)).
  END.
  ELSE RETURN.
END.

iSelectIndex = iSelectIndex + 1.
hBrwSource:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwSource:QUERY:QUERY-OFF-END:
  IF icServerSelectProc = "" THEN DO:
    hBuffTarget:BUFFER-CREATE().
    hBuffTarget:BUFFER-COPY(hBuffSource).
    hBuffTarget:BUFFER-FIELD("iJBoxSelectIndex"):BUFFER-VALUE = iSelectIndex.
  END.

  ASSIGN cDeSelRowidList = REPLACE(cDeSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
         cDeSelRowidList = REPLACE(cDeSelRowidList,",,",",")
         .

  IF NOT CAN-DO(cSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
    cSelRowidList = cSelRowidList + hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".


  hBrwSource:QUERY:GET-NEXT().
END.
IF icServerSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffSource:NAME,
                  icServerSelectProc,
                  "",
                  ENTRY(ix,cSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.

RUN OpenQuerySource.
RUN OpenQueryTarget.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRecord C-Win 
PROCEDURE SelectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSelRowList   AS CHAR NO-UNDO.
DEF VAR cCurrMaxValue AS CHAR NO-UNDO.

IF cCurrMaxValueField NE "" THEN
  cCurrMaxValue = DYNAMIC-FUNCTION("getLocalQueryMaxValue",hBrwTarget,cCurrMaxValueField,0).


IF hBrwSource:NUM-SELECTED-ROWS > 0 THEN DO:
  iSelectIndex = iSelectIndex + 1.
  DO ix = 1 TO hBrwSource:NUM-SELECTED-ROWS:
    bOk = hBrwSource:FETCH-SELECTED-ROW(ix).
    IF bOK THEN DO:
      IF icServerSelectProc = "" THEN DO:
        hBuffTarget:BUFFER-CREATE().
        hBuffTarget:BUFFER-COPY(hBuffSource).
        hBuffTarget:BUFFER-FIELD("iJBoxSelectIndex"):BUFFER-VALUE = iSelectIndex.
        cSelRowList = cSelRowList + (IF cSelRowList NE "" THEN "," ELSE "") + hBuffTarget::RowIdent1.
      END.
  
      ASSIGN cDeSelRowidList = REPLACE(cDeSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
             cDeSelRowidList = REPLACE(cDeSelRowidList,",,",",")
             .
  
      IF NOT CAN-DO(cSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
        cSelRowidList = cSelRowidList + hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".
    END.
  END.
END.
ELSE RETURN.

IF icServerSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffSource:NAME,
                  icServerSelectProc,
                  "",
                  ENTRY(ix,cSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.
ELSE IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"setSelectorTarget") THEN
  RUN setSelectorTarget IN ihParent (hBrwTarget,YES,cCurrMaxValue,cSelRowList).


/* RUN OpenQuerySource.  */
hBrwSource:DELETE-SELECTED-ROWS().
IF TRIM(cSelRowidList,",") NE "" THEN
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource +
                  " AND NOT CAN-DO('" + TRIM(cSelRowidList,",") + "',Rowident1)").

ELSE 
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource).


RUN OpenQueryTarget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cModSortColumn AS CHAR NO-UNDO.

IF icServerSelectProc = "" AND DYNAMIC-FUNCTION("GetCurrentObject") = hBrwTarget THEN DO:
  IF cSortColumn = hBrwTarget:CURRENT-COLUMN:NAME THEN bDesc = NOT bDesc.
  ELSE bDesc = FALSE.
  cSortColumn = hBrwTarget:CURRENT-COLUMN:NAME.

  IF SUBSTR(cSortColumn,LENGTH(cSortColumn)) = "]" THEN
    cModSortColumn = "jbextent_" + RIGHT-TRIM(SUBSTR(cSortColumn,R-INDEX(cSortColumn,"[") + 1),"]") + "_" + SUBSTR(cSortColumn,1,R-INDEX(cSortColumn,"[") - 1).
  ELSE cModSortColumn = cSortColumn.
  
  hBrwTarget:QUERY:QUERY-PREPARE("FOR EACH " + hBuffTarget:NAME +
                                 (IF cSortColumn NE "" THEN " BY " + cModSortColumn + (IF bDesc THEN " DESC" ELSE "")
                                  ELSE "")
                                 ).
  hBrwTarget:QUERY:QUERY-OPEN().

  DYNAMIC-FUNCTION("setSortLabel",hBrwTarget,cSortColumn,bDesc).
END.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord C-Win 
PROCEDURE UndoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
UndoSelection("all").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION adjustBrowseHeight C-Win 
FUNCTION adjustBrowseHeight RETURNS HANDLE
  ( INPUT iiDeltaY AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hBrwSource:HEIGHT-PIXELS = hBrwSource:HEIGHT-PIXELS + iiDeltaY
       hBrwTarget:HEIGHT-PIXELS = hBrwTarget:HEIGHT-PIXELS + iiDeltaY
       rectBrwSource:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} = rectBrwSource:HEIGHT-PIXELS + iiDeltaY
       rectBrwTarget:HEIGHT-PIXELS = rectBrwTarget:HEIGHT-PIXELS + iiDeltaY
       btnSplitBarX:HEIGHT-PIXELS IN FRAME frSplitBarX = btnSplitBarX:HEIGHT-PIXELS + iiDeltaY - 1
       FRAME frSplitBarX:HEIGHT-PIXELS = FRAME frSplitBarX:HEIGHT-PIXELS + iiDeltaY + 1
       {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS + 1
       .

APPLY "window-resized" TO {&WINDOW-NAME}. 

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOutputData C-Win 
FUNCTION getOutputData RETURNS CHARACTER
  ( INPUT icOutputFields AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cValueList AS CHAR NO-UNDO.

IF NOT hBrwTarget:QUERY:IS-OPEN THEN RETURN "".

hBrwTarget:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwTarget:QUERY:QUERY-OFF-END:
  DO ix = 1 TO NUM-ENTRIES(icOutputFields):
    cValueList = cValueList + (IF hBuffTarget:BUFFER-FIELD(ENTRY(ix,icOutputFields)):BUFFER-VALUE NE ? THEN 
                                STRING(hBuffTarget:BUFFER-FIELD(ENTRY(ix,icOutputFields)):BUFFER-VALUE)
                               ELSE "") + "|".
  END.
  hBrwTarget:QUERY:GET-NEXT().
END.

RETURN SUBSTR(cValueList,1,LENGTH(cValueList) - 1). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTargetToolbar C-Win 
FUNCTION getTargetToolbar RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN rectWinToolbar:HANDLE IN FRAME {&FRAME-NAME}.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN Btn_Cancel:LABEL         = "Cancel" 
           btnDeSelect:TOOLTIP      = "Remove selected row(s) from selection" 
           btnDeSelectAll:TOOLTIP   = "Remove all rows from selection"
           btnSelect:TOOLTIP        = "Add selected row(s) to selection"
           btnSelectAll:TOOLTIP     = "Add all rows to selection"
           {&WINDOW-NAME}:TITLE     = "Select"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMaxValueField C-Win 
FUNCTION setMaxValueField RETURNS LOGICAL
  ( INPUT icCurrMaxValueField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Specify a field that you would know the max value of in the target brw
           BEFORE new records were added 
    Notes: Passed on to the setSelectorTarget hook (called from selectRecord)
------------------------------------------------------------------------------*/
cCurrMaxValueField = icCurrMaxValueField.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectedRowids C-Win 
FUNCTION setSelectedRowids RETURNS LOGICAL
  ( INPUT icSelRowidList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

cSelRowidList = icSelRowidList.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UndoSelection C-Win 
FUNCTION UndoSelection RETURNS LOGICAL
  ( INPUT icUndoType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icUndoType = "all" THEN DO:
  cSelRowidList = "".
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querywhere","").
  RUN OpenQuerySource.
  hBrwTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrwTarget:QUERY:QUERY-OFF-END:
    IF hBuffTarget:BUFFER-FIELD("iJBoxSelectIndex"):BUFFER-VALUE NE 0 THEN
      hBuffTarget:BUFFER-DELETE.
    hBrwTarget:QUERY:GET-NEXT().
  END.
  RUN OpenQueryTarget.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

