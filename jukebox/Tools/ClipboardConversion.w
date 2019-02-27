&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:         Clipboard conversion

  Description:  Convert copied fields from AppBuilder to various other widgets

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:       brynjar@chemistry.no  

  Created:      04.May.2008

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

DEF VAR cVariable       AS CHAR  NO-UNDO.
DEF VAR cSource         AS CHAR  NO-UNDO.
DEF VAR cPart1          AS CHAR  NO-UNDO.
DEF VAR cPart2          AS CHAR  NO-UNDO.
DEF VAR cPart3          AS CHAR  NO-UNDO.
DEF VAR bNextEntry      AS LOG   NO-UNDO.
DEF VAR ix              AS INT   NO-UNDO.
DEF VAR iy              AS INT   NO-UNDO.
DEF VAR iNumButtons     AS INT   NO-UNDO.
DEF VAR bReadNext       AS LOG   NO-UNDO.
DEF VAR bAppendFromHere AS LOG   NO-UNDO.
DEF VAR rUpdate         AS ROWID NO-UNDO.
DEF VAR iMaxLine        AS INT   NO-UNDO.
DEF VAR cConvClip       AS CHAR  NO-UNDO.

DEF TEMP-TABLE ttClipText NO-UNDO
    FIELD iLineNum  AS INT
    FIELD cClipLine AS CHAR
    FIELD cVarName  AS CHAR
    FIELD cBtnName  AS CHAR
    FIELD bBtnAdded AS LOG
    FIELD fVarSize  AS DEC
    FIELD bSkipLine AS LOG
/*     INDEX idxLineNum IS PRIMARY iLineNum  */
    .

DEF BUFFER bttClipText  FOR ttClipText.
DEF BUFFER bbttClipText FOR ttClipText.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsConvClip cmbLookupType tbOO cmbWidgetType ~
cmbListType tbAlwaysOnTop btnOk btnApply btnCancel 
&Scoped-Define DISPLAYED-OBJECTS rsConvClip cmbLookupType tbOO ~
cmbWidgetType cmbListType tbAlwaysOnTop 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ChangeWidgetType C-Win 
FUNCTION ChangeWidgetType RETURNS LOGICAL
  ( INPUT icWidgetType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreList C-Win 
FUNCTION CreList RETURNS LOGICAL
  ( INPUT icListType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreLookup C-Win 
FUNCTION CreLookup RETURNS LOGICAL
  ( INPUT icLookupType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReadClip C-Win 
FUNCTION ReadClip RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-cmbLookupType 
       MENU-ITEM m_Lookup_with_maintenance_opt LABEL "Lookup samples"
       MENU-ITEM m_Selector_samples LABEL "Selector samples"
       MENU-ITEM m_Selector_on_temp-table LABEL "Selector on temp-table"
       MENU-ITEM m_Calender_usage LABEL "Calendar usage (how to invoke action on date selection)".


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply AUTO-GO 
     LABEL "Apply" 
     SIZE 12 BY .91.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 12 BY .91.

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "Ok" 
     SIZE 12 BY .91.

DEFINE VARIABLE cmbListType AS CHARACTER FORMAT "X(256)" INITIAL "COMMA" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "COMMA","SEMICOLON","SPACE","<name>:SCREEN-VALUE =","<name>:HIDDEN =","JukeBox query def","STRING(<name>:HANDLE)" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Tips: Use STRING(<na.. to build list of handles for oContainer:setFollowSplit.." NO-UNDO.

DEFINE VARIABLE cmbLookupType AS CHARACTER FORMAT "X(256)":U INITIAL "WINDOW w/LEAVE trg" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "DATE","DATE DIALOG","DIALOG","DIALOG w/LEAVE trg","SELECTOR","SELECTOR DIALOG","WINDOW","WINDOW w/LEAVE trg" 
     DROP-DOWN-LIST
     SIZE 28 BY 1 TOOLTIP "Rigth-click on combo to open sample-code" NO-UNDO.

DEFINE VARIABLE cmbWidgetType AS CHARACTER FORMAT "X(256)":U INITIAL "COMBO-BOX List-item-pairs" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "COMBO-BOX List-items","COMBO-BOX List-item-pairs","EDITOR","RADIO-SET","TOGGLE-BOX","FILL-IN","FILL-IN TEXT" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Use cut (ctrl-x) when converting fields" NO-UNDO.

DEFINE VARIABLE rsConvClip AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Add lookup buttons", "lookup",
"Change type (ctrl-x)", "widget",
"Separated field list", "list"
     SIZE 21.8 BY 3.33 NO-UNDO.

DEFINE VARIABLE tbAlwaysOnTop AS LOGICAL INITIAL yes 
     LABEL "Always on top" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 TOOLTIP "Keep clipboard conversion always on top" NO-UNDO.

DEFINE VARIABLE tbOO AS LOGICAL INITIAL yes 
     LABEL "OO" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.2 BY .81 TOOLTIP "OO style (if available)" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsConvClip AT ROW 1.05 COL 1.2 NO-LABEL
     cmbLookupType AT ROW 1.14 COL 21.4 COLON-ALIGNED HELP
          "Rigth-click on combo to open sample-code" NO-LABEL
     tbOO AT ROW 1.24 COL 51.6 WIDGET-ID 2
     cmbWidgetType AT ROW 2.19 COL 21.4 COLON-ALIGNED NO-LABEL
     cmbListType AT ROW 3.29 COL 21.4 COLON-ALIGNED NO-LABEL
     tbAlwaysOnTop AT ROW 4.33 COL 3.4
     btnOk AT ROW 4.33 COL 23
     btnApply AT ROW 4.33 COL 34.8
     btnCancel AT ROW 4.33 COL 46.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 57.8 BY 4.29
         DEFAULT-BUTTON btnOk CANCEL-BUTTON btnCancel.


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
         TITLE              = "Clipboard conversion for fill-in vars"
         HEIGHT             = 4.29
         WIDTH              = 57.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-SMALL-ICON("ico/blocks.ico":U) THEN
    MESSAGE "Unable to load small icon: ico/blocks.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       cmbLookupType:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-cmbLookupType:HANDLE.

ASSIGN 
       tbOO:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "yes".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Clipboard conversion for fill-in vars */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Clipboard conversion for fill-in vars */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply C-Win
ON CHOOSE OF btnApply IN FRAME DEFAULT-FRAME /* Apply */
DO:
  RUN ConvertClip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  RUN ConvertClip.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Calender_usage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Calender_usage C-Win
ON CHOOSE OF MENU-ITEM m_Calender_usage /* Calendar usage (how to invoke action on date selection) */
DO:
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

  cFileName = SEARCH("CalendarUsage.w").

  IF cFileName NE ? THEN
    RUN adeuib/_open-w.p (cFileName,"","open").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Lookup_with_maintenance_opt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Lookup_with_maintenance_opt C-Win
ON CHOOSE OF MENU-ITEM m_Lookup_with_maintenance_opt /* Lookup samples */
DO:
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

  cFileName = SEARCH("dynLookupUsage.w").

  IF cFileName NE ? THEN
    RUN adeuib/_open-w.p (cFileName,"","open").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Selector_on_temp-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Selector_on_temp-table C-Win
ON CHOOSE OF MENU-ITEM m_Selector_on_temp-table /* Selector on temp-table */
DO:
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

  cFileName = SEARCH("SelectorUsageTT.w").

  IF cFileName NE ? THEN
    RUN adeuib/_open-w.p (cFileName,"","open").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Selector_samples
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Selector_samples C-Win
ON CHOOSE OF MENU-ITEM m_Selector_samples /* Selector samples */
DO:
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

  cFileName = SEARCH("SelectorUsage.w").

  IF cFileName NE ? THEN
    RUN adeuib/_open-w.p (cFileName,"","open").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsConvClip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsConvClip C-Win
ON VALUE-CHANGED OF rsConvClip IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsConvClip
         cmbLookupType:HIDDEN   = rsConvClip NE "lookup"
         tbOO:HIDDEN            = cmbLookupType:HIDDEN
         cmbWidgetType:HIDDEN   = rsConvClip NE "widget"
         cmbListType:HIDDEN     = rsConvClip NE "list"
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAlwaysOnTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAlwaysOnTop C-Win
ON VALUE-CHANGED OF tbAlwaysOnTop IN FRAME DEFAULT-FRAME /* Always on top */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = tbAlwaysOnTop:CHECKED.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConvertClip C-Win 
PROCEDURE ConvertClip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bOk AS LOG NO-UNDO.
IF NOT ReadClip() THEN RETURN.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbLookupType cmbListType cmbWidgetType.
  CASE rsConvClip:
    WHEN "lookup" THEN
      bOk = CreLookup(cmbLookupType).
    WHEN "widget" THEN
      bOk = ChangeWidgetType(cmbWidgetType).
    WHEN "list" THEN DO:
      CreList(cmbListType).
      RETURN.
    END.
  END CASE.
END.
IF bOk THEN DO:
  ASSIGN ix        = 0
         cConvClip = "".
  FOR EACH ttClipText 
      WHERE NOT ttClipText.bSkipLine
      BY ttClipText.iLineNum:
    ix = ix + 1.
    IF ix = 1 AND ttClipText.cClipLine = "" THEN NEXT.
    cConvClip = cConvClip + ttClipText.cClipLine + CHR(10).
  END.
  CLIPBOARD:VALUE = cConvClip.
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
  DISPLAY rsConvClip cmbLookupType tbOO cmbWidgetType cmbListType tbAlwaysOnTop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsConvClip cmbLookupType tbOO cmbWidgetType cmbListType tbAlwaysOnTop 
         btnOk btnApply btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindTableAndDesc C-Win 
PROCEDURE FindTableAndDesc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icFK    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocTable AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocDesc  AS CHAR NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR iy      AS INT    NO-UNDO.

FOR EACH _Field NO-LOCK
    WHERE _Field._Field-Name = icFK
   ,FIRST _File OF _Field NO-LOCK
          WHERE _File._Tbl-Type = "T"
    :
  CREATE BUFFER hBuffer FOR TABLE _File._File-Name.
  DO iy = 1 TO 100:
    IF hBuffer:INDEX-INFORMATION(iy) NE ? THEN DO:
      IF ENTRY(5,hBuffer:INDEX-INFORMATION(iy)) = _Field._Field-Name AND
         ENTRY(2,hBuffer:INDEX-INFORMATION(iy)) = "1" AND
         ENTRY(3,hBuffer:INDEX-INFORMATION(iy)) = "1" 
         THEN DO:
             
        ocTable = _File._File-Name.
        LEAVE.
      END.
    END.
    ELSE LEAVE.
  END.
  DELETE OBJECT hBuffer.
  IF ocTable NE "" THEN LEAVE.
END.

IF ocTable = "" THEN
  FOR EACH _Field NO-LOCK
      WHERE _Field._Field-Name = icFK
     ,FIRST _File OF _Field NO-LOCK
            WHERE _File._Tbl-Type = "T"
      :
    CREATE BUFFER hBuffer FOR TABLE _File._File-Name.
    DO iy = 1 TO 100:
      IF hBuffer:INDEX-INFORMATION(iy) NE ? THEN DO:
        IF ENTRY(5,hBuffer:INDEX-INFORMATION(iy)) = _Field._Field-Name AND
           ENTRY(2,hBuffer:INDEX-INFORMATION(iy)) = "1" 
           THEN DO:
          ocTable = _File._File-Name.
          LEAVE.
        END.
      END.
      ELSE LEAVE.
    END.
    DELETE OBJECT hBuffer.
    IF ocTable NE "" THEN LEAVE.
  END.

IF ocTable = "" THEN
  FOR EACH _Field NO-LOCK
      WHERE _Field._Field-Name = icFK
     ,FIRST _File OF _Field NO-LOCK
            WHERE _File._Tbl-Type = "T"
      :
    CREATE BUFFER hBuffer FOR TABLE _File._File-Name.
    DO iy = 1 TO 100:
      IF hBuffer:INDEX-INFORMATION(iy) NE ? THEN DO:
        IF ENTRY(5,hBuffer:INDEX-INFORMATION(iy)) = _Field._Field-Name AND
           ENTRY(1,hBuffer:INDEX-INFORMATION(iy)) MATCHES "*" + _File._File-Name + "*"
           THEN DO:
          ocTable = _File._File-Name.
          LEAVE.
        END.
      END.
      ELSE LEAVE.
    END.
    DELETE OBJECT hBuffer.
    IF ocTable NE "" THEN LEAVE.
  END.

IF ocTable = "" THEN
  FOR EACH _Field NO-LOCK
      WHERE _Field._Field-Name = icFK
     ,FIRST _File OF _Field NO-LOCK
            WHERE _File._Tbl-Type = "T"
      :
    CREATE BUFFER hBuffer FOR TABLE _File._File-Name.
    DO iy = 1 TO 100:
      IF hBuffer:INDEX-INFORMATION(iy) NE ? THEN DO:
        IF ENTRY(5,hBuffer:INDEX-INFORMATION(iy)) = _Field._Field-Name AND
           ENTRY(1,hBuffer:INDEX-INFORMATION(iy)) MATCHES "*" + _Field._Field-Name + "*"
           THEN DO:
          ocTable = _File._File-Name.
          LEAVE.
        END.
      END.
      ELSE LEAVE.
    END.
    DELETE OBJECT hBuffer.
    IF ocTable NE "" THEN LEAVE.
  END.


FOR EACH _File NO-LOCK
    WHERE _File._file-name = ocTable
   ,EACH _Field OF _File NO-LOCK
         WHERE _Field._Field-Name NE icFK
           AND _Field._Data-Type = "character"
    :
  ocDesc = _Field._Field-Name.
  LEAVE.
END.
IF ocDesc = "" THEN
  FOR EACH _File NO-LOCK
      WHERE _File._file-name = ocTable
     ,EACH _Field OF _File NO-LOCK
           WHERE _Field._Field-Name NE icFK
      :
    ocDesc = _Field._Field-Name.
    LEAVE.
  END.

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
DEF VAR hProc AS HANDLE NO-UNDO.
DEF VAR xPos  AS INT    NO-UNDO.
DEF VAR yPos  AS INT    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  hProc = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hProc):
    IF VALID-HANDLE(hProc:CURRENT-WINDOW) AND 
       hProc:CURRENT-WINDOW:TITLE BEGINS "PRO*Tools" THEN DO:
      xPos = hProc:CURRENT-WINDOW:X.      
      IF hProc:CURRENT-WINDOW:Y + hProc:CURRENT-WINDOW:HEIGHT-PIXELS LT
         SESSION:WORK-AREA-HEIGHT-PIXELS THEN
        yPos = hProc:CURRENT-WINDOW:Y + hProc:CURRENT-WINDOW:HEIGHT-PIXELS + 25.
      ELSE
        ASSIGN yPos = hProc:CURRENT-WINDOW:Y
               xPos = xPos + hProc:CURRENT-WINDOW:WIDTH-PIXELS.
      LEAVE.
    END.
    hProc = hProc:NEXT-SIBLING.
  END.

  IF VALID-HANDLE(hProc) THEN
    ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:X = xPos
           THIS-PROCEDURE:CURRENT-WINDOW:Y = yPos.

  APPLY "value-changed" TO rsConvClip.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InsertBtnTemplate C-Win 
PROCEDURE InsertBtnTemplate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAM icBtnName    AS CHAR NO-UNDO.
DEF INPUT        PARAM icLookupType AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ix           AS INT  NO-UNDO.

DEF VAR cText         AS CHAR   NO-UNDO.
DEF VAR cTemplateFile AS CHAR   NO-UNDO.
DEF VAR cField1       AS CHAR   NO-UNDO.
DEF VAR cField2       AS CHAR   NO-UNDO.
DEF VAR cTable        AS CHAR   NO-UNDO.
DEF VAR iy            AS INT    NO-UNDO.
DEF VAR cAddnButtons  AS CHAR   NO-UNDO.
DEF VAR cOO           AS CHAR   NO-UNDO.

IF tbOO:CHECKED IN FRAME {&FRAME-NAME} THEN
  cOO = "OO".

IF icLookupType EQ "SELECTOR DIALOG" THEN icLookupType = "SelectorDialog".

cTemplateFile = "template\" + cOO + ENTRY(1,icLookupType," ") + "LookupTemplate.p".
IF SEARCH(cTemplateFile) = ? THEN 
  cTemplateFile = "template\" + ENTRY(1,icLookupType," ") + "LookupTemplate.p".
IF SEARCH(cTemplateFile) = ? THEN 
  cTemplateFile = cOO + ENTRY(1,icLookupType," ") + "LookupTemplate.p".
IF SEARCH(cTemplateFile) = ? THEN 
  cTemplateFile = ENTRY(1,icLookupType," ") + "LookupTemplate.p".
IF SEARCH(cTemplateFile) = ? THEN DO:
  RUN InsertDefaultBtnTemplate (INPUT-OUTPUT ix).
  RETURN.
END.

IF NUM-ENTRIES(icBtnName) > 1 THEN
  ASSIGN cAddnButtons = SUBSTRING(icBtnName,INDEX(icBtnName,",") + 1)
         icBtnName    = ENTRY(1,icBtnName).

cField1 = SUBSTRING(icBtnName,4).

RUN FindTableAndDesc (cField1,OUTPUT cTable,OUTPUT cField2).

INPUT FROM VALUE(SEARCH(cTemplateFile)).
REPEAT:
  IMPORT UNFORMATTED cText.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         ix = ix + 1.
  bttClipText.cClipLine = REPLACE(cText,"<field1>",cField1).
  IF cField2 NE "" THEN
    bttClipText.cClipLine = REPLACE(bttClipText.cClipLine,"<field2>",cField2).
  IF cTable NE "" THEN
    bttClipText.cClipLine = REPLACE(bttClipText.cClipLine,"<table>",cTable).
  IF icLookupType = "selector dialog" THEN
    bttClipText.cClipLine = REPLACE(bttClipText.cClipLine,"JBoxSelector","JBoxDSelector").
  IF icLookupType = "date dialog" THEN
    bttClipText.cClipLine = REPLACE(bttClipText.cClipLine,"Cal.w","dCal.w").

  IF icLookupType = "LeaveOfField" AND bttClipText.cClipLine BEGINS "    WHEN" THEN DO:
    DO iy = 1 TO NUM-ENTRIES(cAddnButtons):
      cField1 = SUBSTRING(ENTRY(iy,cAddnButtons),4).
      RUN FindTableAndDesc (cField1,OUTPUT cTable,OUTPUT cField2).
      CREATE bttClipText.
      ASSIGN bttClipText.iLineNum = ix
             ix = ix + 1.
      bttClipText.cClipLine = REPLACE(cText,"<field1>",cField1).
      IF cField2 NE "" THEN
        bttClipText.cClipLine = REPLACE(bttClipText.cClipLine,"<field2>",cField2).
      IF cTable NE "" THEN
        bttClipText.cClipLine = REPLACE(bttClipText.cClipLine,"<table>",cTable).
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InsertDefaultBtnTemplate C-Win 
PROCEDURE InsertDefaultBtnTemplate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAM ix AS INT NO-UNDO.

CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  DEF VAR cReturnValues AS CHAR NO-UNDO."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  DEF VAR bOk           AS LOG  NO-UNDO."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = " "
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  RUN JBoxLookup.w (THIS-PROCEDURE,100,"
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '                    "Customer"'
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '                    + ";CustNum;Name"'
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '                   ,"WHERE TRUE"'
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '                    ,""'
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '                    ,"CustNum,Name",   /* <- return values for these fields */ '
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "                    OUTPUT cReturnValues,"
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "                    OUTPUT bOK)."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = " "
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '  IF bOk AND cReturnValues NE "" THEN DO:'
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "    ASSIGN " + LEFT-TRIM(ttClipText.cBtnName,"btn") + ':SCREEN-VALUE = STRING(ENTRY(1,cReturnValues,"|"))'
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "          ."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '    APPLY "ANY-PRINTABLE" TO ' + LEFT-TRIM(ttClipText.cBtnName,"btn") + "."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  END."
       ix = ix + 1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InsertLookupKeyTrigger C-Win 
PROCEDURE InsertLookupKeyTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAM icButtonName AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ix           AS INT  NO-UNDO.

DEF VAR cField AS CHAR NO-UNDO.

cField = SUBSTR(icButtonName,4).

RUN TriggerStart("F3",cField,INPUT-OUTPUT ix).

CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = '  APPLY "choose" TO ' + icButtonName + "."
       ix = ix + 1.

RUN TriggerEnd(INPUT-OUTPUT ix).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TriggerEnd C-Win 
PROCEDURE TriggerEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAM ix AS INT NO-UNDO.

CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "END."
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  "
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "/* _UIB-CODE-BLOCK-END */"
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "&ANALYZE-RESUME"
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "  "
       ix = ix + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TriggerStart C-Win 
PROCEDURE TriggerStart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAM icAction AS CHAR NO-UNDO.
DEF INPUT        PARAM icWidget AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM ix       AS INT  NO-UNDO.

CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = " "
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "&Scoped-define SELF-NAME " + icWidget
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL " + icWidget
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "ON " + icAction + " OF " + icWidget + " IN FRAME DEFAULT-FRAME"
       ix = ix + 1.
CREATE bttClipText.
ASSIGN bttClipText.iLineNum = ix
       bttClipText.cClipLine = "DO:"
       ix = ix + 1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ChangeWidgetType C-Win 
FUNCTION ChangeWidgetType RETURNS LOGICAL
  ( INPUT icWidgetType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cType        AS CHAR NO-UNDO.
DEF VAR cSubType     AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR cDataType    AS CHAR NO-UNDO.
DEF VAR iStartDefVar AS INT  NO-UNDO.
DEF VAR bNextIsText  AS LOG  NO-UNDO.
DEF VAR cText        AS CHAR NO-UNDO.
DEF VAR bNextIsSize  AS LOG  NO-UNDO.
DEF VAR cSize        AS CHAR NO-UNDO.

/* Change definition of the VARIABLE: */
ASSIGN bReadNext = NO
       cType     = ENTRY(1,icWidgetType," ")
       .
IF NUM-ENTRIES(icWidgetType," ") > 1 THEN
  cSubType  = ENTRY(2,icWidgetType," ").

/* Check if the source is a TEXT widget (no variable definition) */
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF bNextIsSize THEN
    ASSIGN cSize = SUBSTRING(ttClipText.cClipLine,1,INDEX(ttClipText.cClipLine," AT "))
           bNextIsSize = NO.
  IF bNextIsText THEN
    ASSIGN cText = TRIM(SUBSTR(ttClipText.cClipLine,1,INDEX(ttClipText.cClipLine,"view-as") - 1))
           bNextIsText = NO
           bNextIsSize = YES.
  IF ttClipText.cClipLine BEGINS "DEFINE VARIABLE" THEN 
    cDataType = ENTRY(5,ttClipText.cClipLine," ").
  IF ttClipText.cClipLine BEGINS "/* Definitions of the field level widgets" THEN 
    iStartDefVar = ttClipText.iLineNum + 50.
  IF TRIM(ttClipText.cClipLine) = "FORM" THEN bNextIsText = YES.
END.
/* It IS a text widget: */
IF cDataType = "" THEN DO:
  cDataType = "TEXT".
  CREATE ttClipText.
  ASSIGN ttClipText.cClipLine = 'DEFINE VARIABLE fiLabelText AS CHARACTER FORMAT "X(256)" INITIAL ' + cText.
         ttClipText.iLineNum  = iStartDefVar.
  CREATE ttClipText.
  ASSIGN ttClipText.cClipLine = "      VIEW-AS TEXT"
         ttClipText.iLineNum  = iStartDefVar + 1.
  CREATE ttClipText.
  ASSIGN ttClipText.cClipLine = cSize + " NO-UNDO."
         ttClipText.iLineNum  = iStartDefVar + 2.
END.

FOR EACH ttClipText BY ttClipText.iLineNum:
  IF cDataType = "" AND ttClipText.cClipLine BEGINS "DEFINE VARIABLE" THEN 
    cDataType = ENTRY(5,ttClipText.cClipLine," ").
       
  IF cDataType = "TEXT" THEN DO:
    IF TRIM(ttClipText.cClipLine) BEGINS cText THEN
      ttClipText.bSkipLine = YES.
    ELSE IF ttClipText.cClipLine BEGINS cSize AND INDEX(ttClipText.cClipLine, "AT") > 1 THEN
      ttClipText.cClipLine = "     fiLabelText " + SUBSTRING(ttClipText.cClipLine,INDEX(ttClipText.cClipLine, "AT")). 
    ELSE IF TRIM(ttClipText.cClipLine) BEGINS "SIDE-LABELS" THEN
      ttClipText.cClipLine = REPLACE(ttClipText.cClipLine,"SIDE-LABELS","NO-LABELS").
  END.

  ELSE IF ttClipText.cClipLine BEGINS "     LABEL" THEN DO:
    ASSIGN bReadNext = YES
           ix        = 1.

    CREATE bbttClipText.
    CASE cType:
      WHEN "combo-box" THEN
        bbttClipText.cClipLine = "     VIEW-AS COMBO-BOX INNER-LINES 25".
      WHEN "editor" THEN
        bbttClipText.cClipLine = "     VIEW-AS EDITOR SCROLLBAR-VERTICAL".
      WHEN "radio-set" THEN
        bbttClipText.cClipLine = "     VIEW-AS RADIO-SET HORIZONTAL".
      WHEN "toggle-box" THEN
        bbttClipText.cClipLine = "     VIEW-AS TOGGLE-BOX".
      WHEN "fill-in" THEN DO:
        IF cSubType = "TEXT" THEN
          bbttClipText.cClipLine = "     VIEW-AS TEXT".          
        ELSE
          bbttClipText.cClipLine = "     VIEW-AS FILL-IN".          
      END.
    END CASE.
    ASSIGN bbttClipText.iLineNum  = ttClipText.iLineNum + ix
           ix = ix + 1.

    IF CAN-DO("combo-box,radio-set",cType) THEN DO:        
      CREATE bbttClipText.
      CASE cSubType:
        WHEN "list-item-pairs" THEN
          bbttClipText.cClipLine = '     LIST-ITEM-PAIRS "Item 1","1"'.
        WHEN "list-items" THEN
          bbttClipText.cClipLine = '     LIST-ITEMS "Item 1"'.
        OTHERWISE  /* radio-set */
          bbttClipText.cClipLine = '     RADIO-BUTTONS'.
      END CASE.
      ASSIGN bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.

      CREATE bbttClipText.
      CASE cSubType:
        WHEN "list-item-pairs" OR WHEN "LIST-ITEMS" THEN
          bbttClipText.cClipLine = '     DROP-DOWN-LIST'.
        OTHERWISE DO: /* radio-set */
          CASE cDataType:
            WHEN "integer" OR WHEN "decimal" THEN
              bbttClipText.cClipLine = '          "Item 1", 1,'.
            OTHERWISE
              bbttClipText.cClipLine = '          "Item 1", "1",'.
          END CASE.
        END.
      END CASE.
      ASSIGN bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.

      IF cType = "radio-set" THEN DO:
        CREATE bbttClipText.
        CASE cDataType:
          WHEN "integer" OR WHEN "decimal" THEN
            bbttClipText.cClipLine = '          "Item 1", 1,'.
          OTHERWISE
            bbttClipText.cClipLine = '          "Item 2", "1"'.
        END CASE.
        ASSIGN bbttClipText.iLineNum  = ttClipText.iLineNum + ix
               ix = ix + 1.
      END.

    END.
  END.
  ELSE IF bReadNext THEN DO:
    IF ttClipText.cClipLine BEGINS "     SIZE" THEN 
      bReadNext = NO.
    ELSE IF NOT ttClipText.cClipLine BEGINS "     LABEL" THEN 
      ttClipText.bSkipLine = YES.
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreList C-Win 
FUNCTION CreList RETURNS LOGICAL
  ( INPUT icListType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix         AS INT  NO-UNDO.
DEF VAR cDelimiter AS CHAR NO-UNDO.
DEF VAR cStart     AS CHAR NO-UNDO.

CASE icListType:
  WHEN "COMMA"                 THEN cDelimiter = ",".
  WHEN "SEMICOLON"             THEN cDelimiter = ";".
  WHEN "SPACE"                 THEN cDelimiter = " ".
  WHEN "JukeBox query def"     THEN cDelimiter = '"' + CHR(10) + "      " + ' + ";'.
  WHEN "<name>:HIDDEN ="       THEN cDelimiter = ":HIDDEN = YES" + CHR(10).
  WHEN "STRING(<name>:HANDLE)" THEN ASSIGN cStart = 'STRING('
                                           cDelimiter = ':HANDLE) + "," + STRING('.
  OTHERWISE                     cDelimiter = ":SCREEN-VALUE = " + CHR(10).
END CASE.

cConvClip = cStart.

FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "DEFINE VARIABLE" 
     OR ttClipText.cClipLine BEGINS "DEFINE BUTTON" 
     OR ttClipText.cClipLine BEGINS "DEFINE BROWSE" 
     THEN 
    cConvClip = cConvClip + ENTRY(3,ttClipText.cClipLine," ") + cDelimiter.
END.

IF cDelimiter MATCHES "*= " THEN
  CLIPBOARD:VALUE = cConvClip.
ELSE IF cStart NE "" THEN
  CLIPBOARD:VALUE = SUBSTR(cConvClip,1,LENGTH(cConvClip) - 16).
ELSE 
  CLIPBOARD:VALUE = TRIM(cConvClip,cDelimiter).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreLookup C-Win 
FUNCTION CreLookup RETURNS LOGICAL
  ( INPUT icLookupType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cBtnList AS CHAR NO-UNDO.
DEF VAR iy       AS INT  NO-UNDO.
DEF VAR fColumn  AS DEC  NO-UNDO.

ASSIGN bReadNext = NO
       rUpdate   = ?.

/* Find the size of the vars (FK) that should have the lookup button defined so we can place the button in the right column: */
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "DEFINE VARIABLE" THEN
    ASSIGN ttClipText.cVarName  = ENTRY(3,ttClipText.cClipLine," ")
           ttClipText.bSkipLine = YES
           rUpdate              = ROWID(ttClipText)
           bReadNext            = YES.

  IF bReadNext THEN DO:
    ttClipText.bSkipLine = YES.
    IF ttClipText.cClipLine BEGINS "     SIZE" THEN DO:
      FIND bttClipText
           WHERE ROWID(bttClipText) = rUpdate.
      cPart1 = ENTRY(2,TRIM(ttClipText.cClipLine)," ").
      IF SESSION:NUMERIC-DECIMAL-POINT = "," THEN
        cPart1 = REPLACE(cPart1,".",",").
      ASSIGN bttClipText.fVarSize = DEC(cPart1)
             bReadNext            = NO
             .
    END.
  END.
END.


/* REPLACE the VARIABLE (FK) in the frame definition with the button: */
bReadNext = NO.
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "FORM" THEN
    bReadNext            = YES.
  ELSE IF bReadNext THEN DO:
    IF ttClipText.cClipLine MATCHES "*AT ROW*" THEN DO:
      ASSIGN cVariable = ENTRY(1,TRIM(ttClipText.cClipLine)," ")
             ttClipText.cBtnName  = "btn" + cVariable
             ttClipText.cClipLine = SUBSTR(ttClipText.cClipLine,1,5) + "btn" + SUBSTR(ttClipText.cClipLine,6)
             .
      FIND bttClipText
           WHERE bttClipText.cVarName = cVariable.
      ASSIGN cSource     = TRIM(ttClipText.cClipLine)
             cPart1      = ""
             cPart2      = ""
             bNextEntry  = NO 
             .
      DO iy = 1 TO NUM-ENTRIES(cSource," "):
        IF NOT bNextEntry THEN
          cPart1 = cPart1 + ENTRY(iy,cSource," ") + " ".
        IF ENTRY(iy,cSource," ") = "COL" THEN 
          bNextEntry = YES.  
        IF bNextEntry AND ENTRY(iy,cSource," ") NE "COL" AND cPart2 = "" THEN DO:
          cPart2 = ENTRY(iy,cSource," ").
          IF SESSION:NUMERIC-DECIMAL-POINT = "," THEN
            cPart2 = REPLACE(cPart2,".",",").
          fColumn = DEC(cPart2).
          cPart2 = REPLACE(STRING(fColumn + bttClipText.fVarSize + 2),",",".") + " ".
          LEAVE.
        END.
      END.
      ttClipText.cClipLine = "    " + cPart1 + cPart2 + " NO-TAB-STOP".
    END.
    ELSE IF NOT ttClipText.cClipLine BEGINS "    WITH" THEN
      ttClipText.bSkipLine = YES.
    ELSE
      bReadNext = NO.
  END.
END.

/* Add new buttons to definition of enabled objects: */
ASSIGN bReadNext       = NO
       bAppendFromHere = NO.
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "&Scoped-Define ENABLED-OBJECTS" THEN DO:
    IF SUBSTR(ttClipText.cClipLine,LENGTH(ttClipText.cClipLine)) = CHR(126) THEN
      bReadNext = YES.
    ELSE 
      bAppendFromHere = YES.
  END.
  IF ttClipText.cClipLine BEGINS "&Scoped-Define DISPLAYED-OBJECTS" THEN 
    ASSIGN bReadNext = NO
           bAppendFromHere = NO.
      
  IF bReadNext AND SUBSTR(ttClipText.cClipLine,LENGTH(ttClipText.cClipLine)) NE CHR(126) THEN 
    bAppendFromHere = YES.
  
  IF bAppendFromHere THEN DO:
    FIND bbttClipText WHERE ROWID(bbttClipText) = ROWID(ttClipText).
    ASSIGN ix = 0
           iy = 0.
    FOR EACH bttClipText
        WHERE bttClipText.cBtnName NE "":
      iNumButtons = iNumButtons + 1.
    END.
    FOR EACH bttClipText
        WHERE bttClipText.cBtnName NE "":
      iy = iy + 1.
      IF LENGTH(bbttClipText.cClipLine + " " + bttClipText.cBtnName) LT 71  THEN
        bbttClipText.cClipLine = bbttClipText.cClipLine + " " + bttClipText.cBtnName.
      ELSE IF iy LE iNumButtons THEN DO:
        bbttClipText.cClipLine = bbttClipText.cClipLine + " " + CHR(126).
        CREATE bbttClipText.
        ASSIGN bbttClipText.iLineNum  = ttClipText.iLineNum + iy * 10
               bbttClipText.cClipLine = bttClipText.cBtnName.
      END.
    END.
  END.
END.

/* Add definitions of new buttons and remove VARIABLE definitions: */
bReadNext = NO.
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "/* Definitions of the field level widgets" THEN DO:
    bReadNext = YES.
    ix = 1.
    FOR EACH bttClipText
        WHERE bttClipText.cBtnName NE "":
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = "DEFINE BUTTON " + bttClipText.cBtnName
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
      IF icLookupType BEGINS "date" THEN DO:
        CREATE bbttClipText.
        ASSIGN bbttClipText.cClipLine = '     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON'
               bbttClipText.iLineNum  = ttClipText.iLineNum + ix
               ix = ix + 1.
      END.
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = '     LABEL "..."'
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = (IF icLookupType BEGINS "date" THEN "     SIZE 3.8 BY 1." ELSE "     SIZE 4 BY 1.")
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = " "
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
    END.

  END.
  ELSE IF bReadNext THEN DO:
    IF ttClipText.cClipLine BEGINS "/* ************************  Frame Definitions" THEN 
      bReadNext = NO.
    ELSE
      ttClipText.bSkipLine = YES.
  END.
END.

/* Add trigger code for buttons: */
FIND FIRST ttClipText
     WHERE ttClipText.cClipLine BEGINS "/* ************************  Control Triggers  ************************ */"
     NO-ERROR.
IF NOT AVAIL ttClipText THEN DO:
  CREATE ttClipText.
  ASSIGN ix = iMaxLine + 1
         ttClipText.iLineNum  = ix
         ttClipText.cClipLine = "/* ************************  Control Triggers  ************************ */".
  CREATE ttClipText.
  ASSIGN ix = ix + 1
         ttClipText.iLineNum  = ix
         ttClipText.cClipLine = " ".
END.
ELSE ix = ttClipText.iLineNum + 1.

iy = 0.
FOR EACH ttClipText
    WHERE ttClipText.cBtnName NE "":

  cBtnList = cBtnList + (IF cBtnList NE "" THEN "," ELSE "") + ttClipText.cBtnName.

  RUN TriggerStart ("CHOOSE",ttClipText.cBtnName,INPUT-OUTPUT ix).

  RUN InsertBtnTemplate(ttClipText.cBtnName,icLookupType,INPUT-OUTPUT ix).

  RUN TriggerEnd (INPUT-OUTPUT ix).

/*   IF icLookupType BEGINS "window" OR icLookupType BEGINS "dialog" THEN  */
/*     RUN InsertLookupKeyTrigger(ttClipText.cBtnName,INPUT-OUTPUT ix).    */

  iy = iy + 1.
  IF iy = iNumButtons THEN DO:
    CREATE bttClipText.
    ASSIGN bttClipText.iLineNum = ix
           bttClipText.cClipLine = "&UNDEFINE SELF-NAME"
           ix = ix + 1.
  END.
END.
  
/* Opt. add procedure to capture leave event */
IF icLookupType MATCHES "*w/LEAVE trg" AND cBtnList NE "" THEN
  RUN InsertBtnTemplate(cBtnList,"LeaveOfField",INPUT-OUTPUT ix).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReadClip C-Win 
FUNCTION ReadClip RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
OUTPUT TO VALUE(SESSION:TEMP-DIR + "jbClip.txt").
PUT UNFORMATTED CLIPBOARD:VALUE.
OUTPUT CLOSE.

EMPTY TEMP-TABLE ttClipText.

INPUT FROM VALUE(SESSION:TEMP-DIR + "jbClip.txt").
REPEAT :
  ix = ix + 100.
  CREATE ttClipText.
  IMPORT UNFORMATTED ttClipText.cClipLine.
  ttClipText.iLineNum = ix.
END.
INPUT CLOSE.

iMaxLine = ix.
  
RETURN iMaxLine > 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

