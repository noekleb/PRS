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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* &IF DEFINED(UIB_is_Running) = 0 &THEN */
/*   {incl/ttdatadict.i}                 */
/* &ENDIF                                */

DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowseTrans      AS HANDLE NO-UNDO.
DEF VAR hBuffTrans        AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hMenu             AS HANDLE NO-UNDO.
DEF VAR cBuffersAndFields AS CHAR   NO-UNDO
    INIT "JBoxTranslation;cLanguage;+bExist|logical|y/n|no|Trans;cObjectName;cFileName|File/table;cObjectType;cLabel;cTooltip;cTitle;cInitValue;!cHelpText;!cHelpTag;!iMsgDataPos;!iMsgNo;!cMessage;+!chWidget|character".

DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR cMode             AS CHAR   NO-UNDO.
DEF VAR bDesc             AS LOG    NO-UNDO.
DEF VAR bSortChanged      AS LOG    NO-UNDO.
DEF VAR iMaxCount         AS INT    NO-UNDO.
DEF VAR cOperMode         AS CHAR   NO-UNDO.
DEF VAR hBuffTranslation  AS HANDLE NO-UNDO. 
DEF VAR hMyTransBuffer    AS HANDLE NO-UNDO.
DEF VAR hQueryTranslation AS HANDLE NO-UNDO.
DEF VAR cSourceProcFile   AS CHAR   NO-UNDO.
DEF VAR hQueryTest        AS HANDLE NO-UNDO.
DEF VAR hCurrWindow       AS HANDLE NO-UNDO.
DEF VAR hMenuLoadDb       AS HANDLE NO-UNDO.
DEF VAR hMenuLoadMsg      AS HANDLE NO-UNDO.
DEF VAR hMenuListWinTrans AS HANDLE NO-UNDO.
DEF VAR hCurrWidget       AS HANDLE NO-UNDO.
DEF VAR iOrgBgColor       AS INT    NO-UNDO.
DEF VAR hCurrRect         AS HANDLE NO-UNDO.
DEF VAR iOrgRectBgColor   AS INT    NO-UNDO.
DEF VAR rRepos            AS ROWID  NO-UNDO.
DEF VAR hMsgDetail        AS HANDLE NO-UNDO.

DEF VAR cWidgetType       AS CHAR NO-UNDO.
DEF VAR cWidgetName       AS CHAR NO-UNDO.
DEF VAR cLabelList                AS CHAR NO-UNDO
    INIT "BROWSE-COLUMN,BUTTON,COMBO-BOX,DBFIELD,FILL-IN,MENU-ITEM,RADIO-SET,SUB-MENU,TOGGLE-BOX,TEXT-FILL-IN".
DEF VAR cToolTipList              AS CHAR NO-UNDO
    INIT "BROWSE,BUTTON,COMBO-BOX,DBFIELD,EDITOR,FILL-IN,RADIO-SET,SELECTION-LIST,SLIDER,TOGGLE-BOX".
DEF VAR cTitleList                AS CHAR NO-UNDO
    INIT "DIALOG-BOX,FRAME,WINDOW".


hBuffTranslation = DYNAMIC-FUNCTION("getTranslationBuffer").

IF NOT VALID-HANDLE(hBuffTranslation) THEN DO:
  MESSAGE "Translation table is not installed"
          VIEW-AS ALERT-BOX ERROR.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnReload BrwSource RECT-69 RECT-72 BrwTrans ~
cFileName cmbObjectType cObjectName rsTransStatus tbSetDefNameType ~
tbSetDefName cLabel cTitle cTooltip cHelpText cInitValue cHelpTag iMsgNo ~
iMsgButtonType cMessage btnTry btnClearFilter iMsgDataPos tbOnlyTarget ~
btnDelete btnSave fi-cMessageLabel fi-cDataPosLabel 
&Scoped-Define DISPLAYED-OBJECTS cFileName cLanguage cmbObjectType ~
cObjectName rsTransStatus tbSetDefNameType tbSetDefName cLabel cTitle ~
cTooltip cHelpText cInitValue cHelpTag iMsgNo iMsgButtonType cMessage ~
iMsgDataPos tbOnlyTarget fi-cMessageLabel fi-cDataPosLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cLabel cTitle cTooltip cHelpText cInitValue cHelpTag 
&Scoped-define List-2 iMsgNo iMsgButtonType cMessage iMsgDataPos 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSourceBrowse C-Win 
FUNCTION getSourceBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTranManBuffer C-Win 
FUNCTION getTranManBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWidgets C-Win 
FUNCTION getWidgets RETURNS LOGICAL
  ( INPUT ihWidget  AS HANDLE, 
    INPUT iiLevel   AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RefreshTranslation C-Win 
FUNCTION RefreshTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ResetTranslation C-Win 
FUNCTION ResetTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldStatus C-Win 
FUNCTION setFieldStatus RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD swapValue C-Win 
FUNCTION swapValue RETURNS LOGICAL ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearFilter 
     IMAGE-UP FILE "bmp/vipe16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&S" 
     SIZE 4 BY 1.14 TOOLTIP "Lagre (ALT-S)".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "bmp/del16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&D" 
     SIZE 4 BY 1.14 TOOLTIP "Slett (ALT-D)".

DEFINE BUTTON btnReload  NO-FOCUS FLAT-BUTTON
     LABEL "Reload all" 
     SIZE 12.2 BY 1.05 TOOLTIP "Reload translations for window".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "bmp/save.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&S" 
     SIZE 4 BY 1.14 TOOLTIP "Lagre (ALT-S)".

DEFINE BUTTON btnTry  NO-FOCUS FLAT-BUTTON
     LABEL "Refresh all" 
     SIZE 12.2 BY 1.05 TOOLTIP "Refresh translations for window".

DEFINE VARIABLE cLanguage AS CHARACTER FORMAT "X(256)":U 
     LABEL "Target lng" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE cmbObjectType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Object type" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE iMsgButtonType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buttons" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEM-PAIRS "OK/Cancel","1",
                     "Abort/Retry/Ignore","2",
                     "Yes/No/Cancel","3",
                     "Yes/No","4",
                     "Retry/Cancel","5",
                     "Cancel/Try Again/Continue","6",
                     "Notepad","7"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE cMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55 BY 1.81 NO-UNDO.

DEFINE VARIABLE cFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cHelpTag AS CHARACTER FORMAT "X(60)" 
     LABEL "Helptag" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE cHelpText AS CHARACTER FORMAT "X(100)" 
     LABEL "Help" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE cInitValue AS CHARACTER FORMAT "X(100)" 
     LABEL "Initial value" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE cLabel AS CHARACTER FORMAT "x(256)" 
     LABEL "Label" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE cObjectName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Object name" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cTitle AS CHARACTER FORMAT "X(100)" 
     LABEL "Title" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE cTooltip AS CHARACTER FORMAT "X(100)" 
     LABEL "Tooltip" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1.

DEFINE VARIABLE fi-cDataPosLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Data pos in msg:" 
      VIEW-AS TEXT 
     SIZE 16.2 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cMessageLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Message:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE iMsgNo AS INTEGER FORMAT "->>>>>9" INITIAL 0 
     LABEL "Msg num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE iMsgDataPos AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "First", 0,
"Last", 1,
"At tag(s): <1> <2> etc", 2
     SIZE 41 BY .67 NO-UNDO.

DEFINE VARIABLE rsTransStatus AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All", 1,
"Translated", 2,
"Not translated", 3
     SIZE 41 BY .81 NO-UNDO.

DEFINE RECTANGLE BrwSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67.4 BY 10.48.

DEFINE RECTANGLE BrwTrans
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67.4 BY 4.33.

DEFINE RECTANGLE RECT-69
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 2.62.

DEFINE RECTANGLE RECT-72
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.62.

DEFINE VARIABLE tbOnlyTarget AS LOGICAL INITIAL no 
     LABEL "View only target language translations" 
     VIEW-AS TOGGLE-BOX
     SIZE 40.4 BY .67 NO-UNDO.

DEFINE VARIABLE tbSetDefName AS LOGICAL INITIAL no 
     LABEL "Set as default for object name" 
     VIEW-AS TOGGLE-BOX
     SIZE 36.8 BY .81 TOOLTIP "ie: Save without file-name reference" NO-UNDO.

DEFINE VARIABLE tbSetDefNameType AS LOGICAL INITIAL no 
     LABEL "Set as default for object name/type" 
     VIEW-AS TOGGLE-BOX
     SIZE 36.8 BY .81 TOOLTIP "ie: Save without file-name reference" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 3" 
     SIZE 2 BY 16.14.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 67 BY .48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnReload AT ROW 2.57 COL 129.4
     cFileName AT ROW 1.38 COL 58 COLON-ALIGNED
     cLanguage AT ROW 1.38 COL 116 COLON-ALIGNED
     cmbObjectType AT ROW 1.48 COL 12.6 COLON-ALIGNED
     cObjectName AT ROW 2.52 COL 12.6 COLON-ALIGNED
     rsTransStatus AT ROW 2.67 COL 50.2 NO-LABEL
     tbSetDefNameType AT ROW 4.1 COL 104.2
     tbSetDefName AT ROW 4.86 COL 104.2
     cLabel AT ROW 5.86 COL 83 COLON-ALIGNED
     cTitle AT ROW 6.95 COL 83 COLON-ALIGNED
     cTooltip AT ROW 8.05 COL 83 COLON-ALIGNED
     cHelpText AT ROW 9.14 COL 83 COLON-ALIGNED
     cInitValue AT ROW 10.24 COL 83 COLON-ALIGNED
     cHelpTag AT ROW 11.33 COL 83 COLON-ALIGNED
     iMsgNo AT ROW 15.81 COL 83 COLON-ALIGNED
     iMsgButtonType AT ROW 15.81 COL 108 COLON-ALIGNED
     cMessage AT ROW 17.19 COL 85 NO-LABEL
     btnTry AT ROW 2.57 COL 117
     btnClearFilter AT ROW 1.95 COL 101
     iMsgDataPos AT ROW 19.24 COL 100.2 NO-LABEL
     tbOnlyTarget AT ROW 19.33 COL 1.6
     btnDelete AT ROW 4.29 COL 95
     btnSave AT ROW 4.29 COL 99.2
     fi-cMessageLabel AT ROW 15.19 COL 73.4 COLON-ALIGNED NO-LABEL
     fi-cDataPosLabel AT ROW 19.24 COL 82 COLON-ALIGNED NO-LABEL
     BrwSource AT ROW 3.86 COL 1.6
     RECT-69 AT ROW 1.14 COL 1
     RECT-72 AT ROW 1.14 COL 107
     BrwTrans AT ROW 15 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143 BY 19.19.

DEFINE FRAME frSplitBarY
     btnSplitBarY AT ROW 2.62 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 12.81
         SIZE 67.42 BY 2.81.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1.05 COL 16.2
    WITH 1 DOWN NO-BOX NO-HIDE KEEP-TAB-ORDER 
         SIDE-LABELS THREE-D 
         AT COL 54 ROW 3.86
         SCROLLABLE SIZE 25 BY 16.19.


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
         TITLE              = "JukeBox Translation"
         HEIGHT             = 19.19
         WIDTH              = 141.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frSplitBarY:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN cHelpTag IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN cHelpText IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN cInitValue IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN cLabel IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cLanguage IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       cLanguage:DELIMITER IN FRAME DEFAULT-FRAME      = "|".

/* SETTINGS FOR EDITOR cMessage IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FILL-IN cTitle IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN cTooltip IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX iMsgButtonType IN FRAME DEFAULT-FRAME
   2                                                                    */
ASSIGN 
       iMsgButtonType:DELIMITER IN FRAME DEFAULT-FRAME      = "|".

/* SETTINGS FOR RADIO-SET iMsgDataPos IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FILL-IN iMsgNo IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FRAME frSplitBarX
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME frSplitBarX:HEIGHT           = 16.19
       FRAME frSplitBarX:WIDTH            = 25.

ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

/* SETTINGS FOR FRAME frSplitBarY
                                                                        */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frSplitBarY          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox Translation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox Translation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearFilter C-Win
ON CHOOSE OF btnClearFilter IN FRAME DEFAULT-FRAME /* S */
DO:
  RUN ClearFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* D */
DO:
  IF hMyTransBuffer:AVAIL THEN DO:
    MESSAGE "Delete translation " SKIP(1)
            hMyTransBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE bOK.
    IF bOk THEN DO:
      IF hMyTransBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE NE "" THEN DO:
        bOk = DYNAMIC-FUNCTION("DoDelete","JBoxTranslation","",
              "",
              hMyTransBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
              TRUE).
        IF bOk THEN 
          hMyTransBuffer:BUFFER-DELETE().
      END.

      IF bOk THEN RefreshTranslation().

/*       IF bOk THEN DO:                                                                         */
/*         IF VALID-HANDLE(WIDGET-HANDLE(hBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE)) THEN DO: */
/*           DYNAMIC-FUNCTION("setSourceFileName",cSourceProcFile).                              */
/*           DYNAMIC-FUNCTION("setTranManBuffer",hBuffer).                                       */
/*           DYNAMIC-FUNCTION("InitTranslation",hBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE).   */
/*         END.                                                                                  */
/*         APPLY "value-changed" TO hBrowse.                                                     */
/*         IF NOT hMyTransBuffer:AVAIL THEN DO:                                                  */
/*           hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = NO.                                   */
/*           hBrowse:REFRESH().                                                                  */
/*         END.                                                                                  */
/*       END.                                                                                    */
/*       ELSE MESSAGE "Couldn't delete translation" SKIP                                         */
/*                    DYNAMIC-FUNCTION("getTransMessage")                                        */
/*                    VIEW-AS ALERT-BOX ERROR.                                                   */
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReload
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReload C-Win
ON CHOOSE OF btnReload IN FRAME DEFAULT-FRAME /* Reload all */
DO:
  IF VALID-HANDLE(hCurrWindow) THEN DO:
    ResetTranslation().
    hBuffer:EMPTY-TEMP-TABLE().
    DYNAMIC-FUNCTION("setTranManBuffer",hBuffer).
    DYNAMIC-FUNCTION("setTranManProc",THIS-PROCEDURE).
    DYNAMIC-FUNCTION("setSourceFileName",cSourceProcFile).
    DYNAMIC-FUNCTION("InitTranslation",hCurrWindow).

    RUN StartQuery.
    cmbObjectType:LIST-ITEMS = DYNAMIC-FUNCTION("getTransObjectTypeList").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* S */
DO:
  RUN SaveRecord.
/*   IF NOT hMenuLoadDb:SENSITIVE THEN */
/*     APPLY "choose" TO btnTry.       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX /* Button 3 */
DO:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DO WITH FRAME frSplitBarY:
    btnSplitBarY:WIDTH-PIXELS = 10.
    btnSplitBarY:X = 1.
    btnSplitBarY:WIDTH-PIXELS = FRAME frSplitBarY:WIDTH-PIXELS - 2 NO-ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarY
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frSplitBarY /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frSplitBarY,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnTry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTry C-Win
ON CHOOSE OF btnTry IN FRAME DEFAULT-FRAME /* Refresh all */
DO:
  IF VALID-HANDLE(hCurrWindow) THEN DO:
    ResetTranslation().
    DYNAMIC-FUNCTION("setSourceFileName",cSourceProcFile).
    DYNAMIC-FUNCTION("InitTranslation",hCurrWindow).
  END.
/*
  DEF VAR hWidget AS HANDLE NO-UNDO.

  IF cLanguage:SCREEN-VALUE = "" OR cLanguage:SCREEN-VALUE = ? THEN DO:
    MESSAGE "You must supply a target language"
            VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  CREATE QUERY hQueryTest.
  hQueryTest:SET-BUFFERS(hBuffer).
  hQueryTest:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + 
                           " WHERE cLanguage = '" + cLanguage:SCREEN-VALUE + "'"
                         + "   AND cFileName = '" + cSourceProcFile + "'").
  hQueryTest:QUERY-OPEN().
  hQueryTest:GET-FIRST().
  IF hBuffer:AVAIL THEN DO:
    hWidget = WIDGET-HANDLE(hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE).
    IF VALID-HANDLE(hWidget) THEN DO:
      REPEAT WHILE NOT hQueryTest:QUERY-OFF-END:
        DYNAMIC-FUNCTION("setWidgetTranslation",hBuffer,WIDGET-HANDLE(hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE)).
        hQueryTest:GET-NEXT().
      END.
    
      hQueryTest:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + 
                               " WHERE cLanguage = '" + cLanguage:SCREEN-VALUE + "'").
      hQueryTest:QUERY-OPEN().
      hQueryTest:GET-FIRST().
      REPEAT WHILE NOT hQueryTest:QUERY-OFF-END:
        DYNAMIC-FUNCTION("setWidgetTranslation",hBuffer,WIDGET-HANDLE(hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE)).
        hQueryTest:GET-NEXT().
      END.
    
      DELETE OBJECT hQueryTest.
    END.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFileName C-Win
ON RETURN OF cFileName IN FRAME DEFAULT-FRAME /* Filename */
DO:
 RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbObjectType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbObjectType C-Win
ON VALUE-CHANGED OF cmbObjectType IN FRAME DEFAULT-FRAME /* Object type */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cObjectName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cObjectName C-Win
ON RETURN OF cObjectName IN FRAME DEFAULT-FRAME /* Object name */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsTransStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsTransStatus C-Win
ON VALUE-CHANGED OF rsTransStatus IN FRAME DEFAULT-FRAME
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbOnlyTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbOnlyTarget C-Win
ON VALUE-CHANGED OF tbOnlyTarget IN FRAME DEFAULT-FRAME /* View only target language translations */
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"queryfilter",
                   IF SELF:CHECKED THEN " AND cLanguage = '" + cLanguage:SCREEN-VALUE + "'" ELSE "").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSetDefName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSetDefName C-Win
ON VALUE-CHANGED OF tbSetDefName IN FRAME DEFAULT-FRAME /* Set as default for object name */
DO:
  ASSIGN tbSetDefName.
  IF tbSetDefName:CHECKED THEN
    tbSetDefNameType:CHECKED = NO.
  setFieldStatus().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSetDefNameType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSetDefNameType C-Win
ON VALUE-CHANGED OF tbSetDefNameType IN FRAME DEFAULT-FRAME /* Set as default for object name/type */
DO:
  ASSIGN tbSetDefNameType.
  IF tbSetDefNameType:CHECKED THEN
    tbSetDefName:CHECKED = NO.
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
  ResetTranslation().
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hMsgDetail) THEN APPLY "close" TO hMsgDetail.
  DYNAMIC-FUNCTION("setTranManProc",?).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN TranslationSetup.

  RUN enable_UI.

  DYNAMIC-FUNCTION("setTranManProc",THIS-PROCEDURE).
  RUN InitWindow.
  APPLY "window-resized" TO {&WINDOW-NAME}.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

ON 'ctrl-s':U OF FRAME {&FRAME-NAME} ANYWHERE 
DO:
  RUN SaveRecord.
END.

ON 'window-resized':U OF {&WINDOW-NAME}
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  APPLY "end-move" TO btnSplitBarX IN FRAME frSplitBarX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddTransTypeToDropDown C-Win 
PROCEDURE AddTransTypeToDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icObjectType AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF NOT CAN-DO(cmbObjectType:LIST-ITEMS,icObjectType) THEN
    cmbObjectType:LIST-ITEMS = cmbObjectType:LIST-ITEMS + "," + icObjectType.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildScreenObjects C-Win 
PROCEDURE BuildScreenObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWidget    AS HANDLE NO-UNDO.

IF ihWidget = THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cOperMode   = "Window"
         cmbObjectType:LIST-ITEMS = ",DBFIELD"
         cSourceProcFile = SOURCE-PROCEDURE:FILE-NAME
         tbSetDefName:SENSITIVE = TRUE
         tbSetDefNameType:SENSITIVE = TRUE
         hMenuLoadDb:SENSITIVE = NO
/*          hMenuLoadMsg:SENSITIVE = NO  */
         btnTry:SENSITIVE = YES
         btnReload:SENSITIVE = YES
         .
  
  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"getTransSourceProc") THEN DO:
    cSourceProcFile = DYNAMIC-FUNCTION("getTransSourceProc" IN SOURCE-PROCEDURE,ihWidget).
    IF cSourceProcFile = "" THEN DO:
      MESSAGE "Couldn't retrieve source file name"
              VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
  END.
  ELSE IF cSourceProcFile MATCHES "*.ab" THEN
    cSourceProcFile = ENTRY(2,SUBSTR(cSourceProcFile,1,LENGTH(cSourceProcFile) - 3),"_") + ".w".

  IF SOURCE-PROCEDURE:FILE-NAME NE "JBoxASlib.p" THEN DO:
    hBuffer:EMPTY-TEMP-TABLE().
    DYNAMIC-FUNCTION("setTranManBuffer",hBuffer).
    DYNAMIC-FUNCTION("setSourceFileName",cSourceProcFile).
    DYNAMIC-FUNCTION("InitTranslation",ihWidget).
  END.

  RUN StartQuery.
  cmbObjectType:LIST-ITEMS = DYNAMIC-FUNCTION("getTransObjectTypeList").

  hCurrWindow = ihWidget.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilter C-Win 
PROCEDURE ClearFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbObjectType:SCREEN-VALUE = " " 
         rsTransStatus:SCREEN-VALUE = "1"
         cFileName:SCREEN-VALUE     = ""
         cObjectName:SCREEN-VALUE   = ""
         .
  RUN StartQuery.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrBuffer AS HANDLE NO-UNDO.
DEF VAR cObjectName AS CHAR   NO-UNDO.
DEF VAR cModObjName AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

DO WITH FRAME {&FRAME-NAME}:
  IF hBuffer:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hBrowseTrans THEN     
    ASSIGN hCurrBuffer = IF hMyTransBuffer:AVAIL THEN hMyTransBuffer ELSE hBuffer
           btnDelete:SENSITIVE         = hMyTransBuffer:AVAIL
           cLabel:SCREEN-VALUE         = hCurrBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE 
           cTitle:SCREEN-VALUE         = hCurrBuffer:BUFFER-FIELD("cTitle"):BUFFER-VALUE 
           cTooltip:SCREEN-VALUE       = hCurrBuffer:BUFFER-FIELD("cTooltip"):BUFFER-VALUE
           cHelpText:SCREEN-VALUE      = hCurrBuffer:BUFFER-FIELD("cHelpText"):BUFFER-VALUE
           cInitValue:SCREEN-VALUE     = hCurrBuffer:BUFFER-FIELD("cInitValue"):BUFFER-VALUE 
           iMsgNo:SCREEN-VALUE         = STRING(hCurrBuffer:BUFFER-FIELD("iMsgNo"):BUFFER-VALUE)
           cMessage:SCREEN-VALUE       = hCurrBuffer:BUFFER-FIELD("cMessage"):BUFFER-VALUE 
           iMsgButtonType:SCREEN-VALUE = STRING(hCurrBuffer:BUFFER-FIELD("iMsgButtonType"):BUFFER-VALUE)
           iMsgDataPos:SCREEN-VALUE    = STRING(hCurrBuffer:BUFFER-FIELD("iMsgDataPos"):BUFFER-VALUE)
           cHelpTag:SCREEN-VALUE       = hCurrBuffer:BUFFER-FIELD("cHelpTag"):BUFFER-VALUE
           NO-ERROR.
  ELSE IF hBuffer:AVAIL THEN DO:
    cObjectName = hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE.

    IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "button" AND INDEX(cObjectName,"_btn_") > 0 THEN 
      cModObjName = SUBSTR(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,INDEX(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,"btn_")).
    ELSE IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "menu-item" AND INDEX(cObjectName,"_mitm_") > 0 THEN
      cModObjName = SUBSTR(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,INDEX(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,"mitm_")).
    ELSE cModObjName = cObjectName.

    DYNAMIC-FUNCTION("setAttribute",hBrowseTrans,"parentlink",                                             
                        "WHERE (cObjectName = '" + hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "'"
                      + (IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "button" THEN
                           " OR cObjectName = '" + cModObjName + "')"
                         ELSE IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "menu-item" THEN
                           " OR cObjectName = '" + cModObjName + "')"
                         ELSE ")")
                      + "  AND (cFileName   = '" + hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE +  "' OR cFileName = '')"
                      + "  AND (cObjectType = '" + hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE + "' OR cObjectType = '' OR cObjectType = 'dbfield')"
                        ).
    
    IF VALID-HANDLE(hCurrWidget) THEN DO:
      IF VALID-HANDLE(hCurrRect) THEN DO:
        IF iOrgRectBgColor = 0 THEN  DELETE OBJECT hCurrRect.
        ELSE hCurrRect:BGCOLOR = iOrgRectBgColor.
      END.
      IF CAN-QUERY(hCurrWidget,"label-bgcolor") THEN
        hCurrWidget:LABEL-BGCOLOR = iOrgBgColor.
      ELSE IF CAN-QUERY(hCurrWidget,"bgcolor") THEN
        hCurrWidget:BGCOLOR = iOrgBgColor.
    END.

    ASSIGN hCurrWidget = hBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE
           iOrgBgColor = IF hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE THEN 10 
                         ELSE hBuffer:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE
           iOrgRectBgColor = IF hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE THEN 10 
                         ELSE hBuffer:BUFFER-FIELD("iOrgRectBgColor"):BUFFER-VALUE
           hCurrRect   = hBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE
           .
    IF VALID-HANDLE(hCurrWidget) THEN DO:
      ASSIGN tbSetDefName:SENSITIVE     = YES
             tbSetDefNameType:SENSITIVE = YES.

      IF hCurrWidget:TYPE = "button" THEN DO:
        IF NOT VALID-HANDLE(hCurrRect) THEN DO:
          CREATE RECTANGLE hCurrRect
                 ASSIGN NAME          = "TranslationMarker_" + hCurrWidget:NAME
                        FRAME         = hCurrWidget:FRAME
                        X             = hCurrWidget:X
                        Y             = hCurrWidget:Y + hCurrWidget:HEIGHT-PIXELS
                        WIDTH-PIXELS  = hCurrWidget:WIDTH-PIXELS
                        HEIGHT-PIXELS = IF hCurrWidget:FRAME:HEIGHT-PIXELS - (hCurrWidget:Y + hCurrWidget:HEIGHT-PIXELS) < 6 THEN
                                          MAX(2,hCurrWidget:FRAME:HEIGHT-PIXELS - (hCurrWidget:Y + hCurrWidget:HEIGHT-PIXELS))
                                        ELSE 6
                        EDGE-PIXELS   = 1
                        GRAPHIC-EDGE  = YES
                        FILLED        = YES
                        BGCOLOR       = 13
                        VISIBLE       = YES
                        .
          ASSIGN hBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE     = hCurrRect
                 hBuffer:BUFFER-FIELD("iOrgRectBgColor"):BUFFER-VALUE = ?.
          DYNAMIC-FUNCTION("addTranslationMarkerRule",hCurrRect,hCurrWidget).
        END.
        ELSE ASSIGN hCurrRect:BGCOLOR = 13
                    hCurrRect:VISIBLE = YES.
                  
      END.
      ELSE IF CAN-QUERY(hCurrWidget,"label-bgcolor") THEN
        hCurrWidget:LABEL-BGCOLOR = 13.
      ELSE IF CAN-QUERY(hCurrWidget,"bgcolor") THEN
        hCurrWidget:BGCOLOR = 13.
    END.     
  END.
  
  RUN SUPER.
  
  IF hBuffer:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hBrowseTrans THEN DO WITH FRAME {&FRAME-NAME}:
    setFieldStatus().
  
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.
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
  DISPLAY cFileName cLanguage cmbObjectType cObjectName rsTransStatus 
          tbSetDefNameType tbSetDefName cLabel cTitle cTooltip cHelpText 
          cInitValue cHelpTag iMsgNo iMsgButtonType cMessage iMsgDataPos 
          tbOnlyTarget fi-cMessageLabel fi-cDataPosLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnReload BrwSource RECT-69 RECT-72 BrwTrans cFileName cmbObjectType 
         cObjectName rsTransStatus tbSetDefNameType tbSetDefName cLabel cTitle 
         cTooltip cHelpText cInitValue cHelpTag iMsgNo iMsgButtonType cMessage 
         btnTry btnClearFilter iMsgDataPos tbOnlyTarget btnDelete btnSave 
         fi-cMessageLabel fi-cDataPosLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
  ENABLE btnSplitBarY 
      WITH FRAME frSplitBarY IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarY}
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
DO WITH FRAME {&FRAME-NAME}:

  CREATE BUFFER hMyTransBuffer FOR TABLE hBuffTranslation.

/*   CREATE QUERY hQueryTranslation.                 */
/*   hQueryTranslation:SET-BUFFERS(hMyTransBuffer).  */

  ASSIGN cLanguage:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getLanguageCode") + "|" + DYNAMIC-FUNCTION("getLanguageCode")
         cLanguage:SCREEN-VALUE = DYNAMIC-FUNCTION("getLanguageCode")
         btnTry:SENSITIVE = NO
         btnReload:SENSITIVE = NO
         tbOnlyTarget:HIDDEN = YES 

         iMsgButtonType:HIDDEN = YES
         cMessage:HIDDEN = YES
         fi-cMessageLabel:HIDDEN = YES
         iMsgNo:HIDDEN = YES
         fi-cDataPosLabel:HIDDEN = YES
         iMsgDataPos:HIDDEN = YES
         .

/*   cmbLanguage:LIST-ITEM-PAIRS = cLanguage:LIST-ITEM-PAIRS + "|" + DYNAMIC-FUNCTION("getBaseLanguageCode") +    */
/*                                 "|" + DYNAMIC-FUNCTION("getBaseLanguageCode").                                 */

/*   cLanguage:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList",                                                          */
/*                                             "JBoxGenCode;cDescription;cCodeValue",                                             */
/*                                             "WHERE JBoxGenCode.iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")) + */
/*                                             "  AND JBoxGenCode.cCodeType = 'Language'" + " BY cDescription").                  */

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    BrwSource:HANDLE,
                    100,
                    "",
                    "JBoxTranslation"
                    + ";cLanguage"
                    + ";+bExist|logical|y/n|no|Trans"
                    + ";cObjectName"
                    + ";cObjectType"
                    + ";cLabel"
                    + ";cTooltip"
                    + ";cHelpText"
                    + ";cTitle"
                    + ";cInitValue"
                    + ";!cHelpTag;!iMsgDataPos;!iMsgNo;!cMessage;+!chWidget|character;!iMsgButtonType"
                    + ";cFileName"
                    + ";+hWidget|HANDLE"
                    + ";+bPopupDefined|LOGICAL"
                    + ";+!iOrgBgColor|INTEGER"
                    + ";+!hWidgetRect|LOGICAL"
                    + ";+!iOrgRectBgColor|INTEGER"
                    ,"WHERE FALSE"
                    ,"sort|cObjectName").
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE cLanguage = '" + DYNAMIC-FUNCTION("getBaseLanguageCode") + "' AND NOT cObjectName MATCHES '*_mitm_translate'").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
  DYNAMIC-FUNCTION("NewMenuBand",hBrowse,
                   "MultiSortBrowse;Sort on multiple columns"
                 + ",BrowseConfig;Browse config"
                 + ",Excel"
                   ,"").

  DYNAMIC-FUNCTION("setAttribute",BrwTrans:HANDLE,"temptablebuffer",STRING(hMyTransBuffer)).
  hBrowseTrans = DYNAMIC-FUNCTION("NewBrowse",
                    BrwTrans:HANDLE,
                    100,
                    "",
                    "JBoxTranslation"
                    + ";cLanguage"
                    + ";cObjectName"
                    + ";cFileName|File/table"
                    + ";cObjectType"
                    + ";cLabel"
                    + ";cTooltip"
                    + ";cHelpText"
                    + ";cTitle"
                    + ";cInitValue"
                    + ";!cHelpText;!cHelpTag;!iMsgDataPos;!iMsgNo;!cMessage;+!chWidget|character"
/*                     cBuffersAndFields, */
                    ,"WHERE FALSE"
                    ,"temp-table|" + STRING(hMyTransBuffer:TABLE-HANDLE)).
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseTrans,hBrowse,"parentlink").
  
  DYNAMIC-FUNCTION("NewMenuBand",hBrowseTrans,
                   "MultiSortBrowse;Sort on multiple columns"
                 + ",BrowseConfig;Browse config"
                 + ",Excel"
                   ,"").


  hMenu = DYNAMIC-FUNCTION("NewMenuBand",THIS-PROCEDURE:CURRENT-WINDOW,
/*                    "NewMsg;Create new message" */
/*                  + ",                          */
                    "ListWinTrans;List translations for window"
                 + ",ListAllTrans;List all translations"
                 + ",LoadDb;Load database fields"
/*                  + ",LoadMessages;Load messages" */
                 + ",SetNewLanguage;Select new language"
                  ,"File").
  ASSIGN hMenuLoadDb  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hMenu,"menu-itemLoadDb"))
         hMenuLoadMsg = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hMenu,"menu-itemLoadMessages"))
         hMenuListWinTrans = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hMenu,"menu-itemListWinTrans"))
         .


  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "BrwSource,rect-72," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME frSplitBarY:HANDLE, "btnSplitBarY").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "BrwSource,rect-69,rect-72," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cHelpText,cTooltip,cHelpTag,cInitValue,cLabel,cTitle").

  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,
                                      STRING(BrwSource:HANDLE) + "," +
                                      STRING(hBrowse) + "," +
                                      STRING(cLabel:HANDLE) + "," +
                                      STRING(cTitle:HANDLE) + "," +
                                      STRING(cTooltip:HANDLE) + "," +
                                      STRING(cHelpText:HANDLE) + "," +
                                      STRING(cHelpTag:HANDLE) + "," +
                                      STRING(cInitValue:HANDLE) + "," +
/*                                       STRING(iMsgNo:HANDLE) + "," +           */
/*                                       STRING(cMessage:HANDLE) + "," +         */
/*                                       STRING(fi-cMessageLabel:HANDLE) + "," + */
                                      STRING(FRAME frSplitBarY:HANDLE) + "," +
                                      STRING(BrwTrans:HANDLE) + "," +
                                      STRING(hBrowseTrans:HANDLE)
                                      ).

  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frSplitBarY,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME frSplitBarY,
                                      STRING(BrwSource:HANDLE) + "," +
                                      STRING(hBrowse) + "," +
                                      STRING(BrwTrans:HANDLE) + "," +
                                      STRING(hBrowseTrans:HANDLE)
                                      ).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,280,0,0).

  SUBSCRIBE TO "BuildScreenObjects" ANYWHERE.
  SUBSCRIBE TO "AddTransTypeToDropDown" ANYWHERE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ListAllTransRecord C-Win 
PROCEDURE ListAllTransRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",DYNAMIC-FUNCTION("getTranslationBuffer"),0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ListWinTransRecord C-Win 
PROCEDURE ListWinTransRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE("general").
cFileName = SESSION:TEMP-DIR + "translist_" + hCurrWindow:TITLE + "_" + STRING(TIME) + ".csv".
DYNAMIC-FUNCTION("setTranslationRepFile",cFileName).
DYNAMIC-FUNCTION("InitTranslation",hCurrWindow).
DYNAMIC-FUNCTION("setWebDoc","open",cFileName).
SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadDbRecord C-Win 
PROCEDURE LoadDbRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR htt_field       AS HANDLE NO-UNDO.
DEF VAR htt_fieldBuffer AS HANDLE NO-UNDO.
DEF VAR htt_fieldQuery  AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  hBuffer:EMPTY-TEMP-TABLE().
  ASSIGN tbSetDefNameType:CHECKED    = YES
         tbSetDefName:SENSITIVE      = NO
         tbSetDefNameType:SENSITIVE  = NO
         hMenuListWinTrans:SENSITIVE = NO
         .

  htt_field = DYNAMIC-FUNCTION("getTempTableJoin",10000,0,"",
                               "_field;distinct _field-name;_label;_help,_file;_file-name",
                               "WHERE TRUE, FIRST _file OF _field NO-LOCK WHERE _tbl-type = 'T'").
  
  htt_fieldBuffer = htt_field:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY htt_fieldQuery NO-ERROR.
  htt_fieldQuery:SET-BUFFERS(htt_fieldBuffer) NO-ERROR.
  htt_fieldQuery:QUERY-PREPARE("FOR EACH " + htt_fieldBuffer:NAME).
  htt_fieldQuery:QUERY-OPEN.
  
  htt_fieldQuery:GET-FIRST().
  REPEAT WHILE NOT htt_fieldQuery:QUERY-OFF-END:
  
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cToolTip"):BUFFER-VALUE    = htt_fieldBuffer:BUFFER-FIELD("_help"):BUFFER-VALUE
           hBuffer:BUFFER-FIELD("cHelpText"):BUFFER-VALUE   = htt_fieldBuffer:BUFFER-FIELD("_help"):BUFFER-VALUE
           hBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE      = htt_fieldBuffer:BUFFER-FIELD("_label"):BUFFER-VALUE
           hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE = htt_fieldBuffer:BUFFER-FIELD("_field-name"):BUFFER-VALUE
           hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "DBFIELD"
/*            hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE   = htt_fieldBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE  */
           hBuffer:BUFFER-FIELD("cLanguage"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getBaseLanguageCode")
           hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE      = NO
           .
  
    bOk = hMyTransBuffer:FIND-FIRST("WHERE cObjectName = '" + hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "'"
                                  + " AND cObjectType = 'dbfield'"
                                  + (IF tbOnlyTarget:CHECKED THEN
                                      " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'"
                                     ELSE "")
                                    ) NO-ERROR.
   IF bOk THEN
     hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.

/*     hQueryTranslation:QUERY-PREPARE("FOR EACH " + hMyTransBuffer:NAME                                                   */
/*                                   + " WHERE cObjectName = '" + hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "'"   */
/*                                   + (IF tbOnlyTarget:CHECKED THEN                                                       */
/*                                       " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'"                  */
/*                                      ELSE "")                                                                           */
/*                                     ).                                                                                  */
/*     hQueryTranslation:QUERY-OPEN.                                                                                       */
/*     IF hQueryTranslation:IS-OPEN THEN DO:                                                                               */
/*       hQueryTranslation:GET-FIRST().                                                                                    */
/*       IF hMyTransBuffer:AVAIL THEN                                                                                      */
/*         hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.                                                              */
/*       REPEAT WHILE NOT hQueryTranslation:QUERY-OFF-END:                                                                 */
/*         hBuffer:BUFFER-CREATE().                                                                                        */
/*         hBuffer:BUFFER-COPY(hMyTransBuffer).                                                                            */
/*         IF hMyTransBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "dbfield" THEN                                     */
/*           hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.                                                            */
/*         IF NOT CAN-DO(cmbObjectType:LIST-ITEMS,hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE) THEN                   */
/*           cmbObjectType:LIST-ITEMS = cmbObjectType:LIST-ITEMS + "," + hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE. */
/*         hQueryTranslation:GET-NEXT().                                                                                   */
/*       END.                                                                                                              */
/*     END.                                                                                                                */

    htt_fieldQuery:GET-NEXT().
  END.
  
  DELETE OBJECT htt_fieldQuery.
  DELETE OBJECT htt_field.

  RUN StartQuery.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadMessagesRecord C-Win 
PROCEDURE LoadMessagesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR htt_field       AS HANDLE NO-UNDO.
DEF VAR htt_fieldBuffer AS HANDLE NO-UNDO.
DEF VAR htt_fieldQuery  AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  hBuffer:EMPTY-TEMP-TABLE().
  ASSIGN tbSetDefNameType:CHECKED    = YES
         tbSetDefName:SENSITIVE      = NO
         tbSetDefNameType:SENSITIVE  = NO
         hMenuListWinTrans:SENSITIVE = NO
         .

  htt_field = DYNAMIC-FUNCTION("getTempTableJoin",10000,0,"",
                               "JBoxTranslation",
                               "WHERE iMsgNo > 0").
  
  htt_fieldBuffer = htt_field:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY htt_fieldQuery NO-ERROR.
  htt_fieldQuery:SET-BUFFERS(htt_fieldBuffer) NO-ERROR.
  htt_fieldQuery:QUERY-PREPARE("FOR EACH " + htt_fieldBuffer:NAME).
  htt_fieldQuery:QUERY-OPEN.
  
  htt_fieldQuery:GET-FIRST().
  REPEAT WHILE NOT htt_fieldQuery:QUERY-OFF-END:
  
    hBuffer:BUFFER-CREATE().
    hBuffer:BUFFER-COPY(htt_fieldBuffer).
    hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = NO.
  
    bOk = hMyTransBuffer:FIND-FIRST("WHERE cObjectName = '" + hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "'"
                                  + " AND cObjectType = 'message'"
                                  + (IF tbOnlyTarget:CHECKED THEN
                                      " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'"
                                     ELSE "")
                                    ) NO-ERROR.
   IF bOk THEN
     hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.

    htt_fieldQuery:GET-NEXT().
  END.
  
  DELETE OBJECT htt_fieldQuery.
  DELETE OBJECT htt_field.

  RUN StartQuery.
END.

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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO cmbObjectType IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewMsgRecord C-Win 
PROCEDURE NewMsgRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hMsgDetail) THEN DO:
  RUN JBoxMessageDetail.w PERSIST SET hMsgDetail.
  RUN InitializeObject IN hMsgDetail.
END.
RUN NewRecord IN hMsgDetail.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsTransStatus cmbObjectType.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter",
                              " AND " +
                              (IF cmbObjectType NE "" AND cmbObjectType NE ? THEN
                                 " cObjectType = '" + cmbObjectType + "'"
                               ELSE "TRUE") +
                              (IF cObjectName:SCREEN-VALUE NE "" THEN 
                                 " AND cObjectName " + 
                                 (IF INDEX(cObjectName:SCREEN-VALUE,"*") > 0 THEN "MATCHES " ELSE "BEGINS ") +
                                 "'" + cObjectName:SCREEN-VALUE + "'"
                               ELSE "") +
                              (IF cFileName:SCREEN-VALUE NE "" THEN
                                 " AND cFileName " +
                                (IF INDEX(cFileName:SCREEN-VALUE,"*") > 0 THEN "MATCHES " ELSE "BEGINS ") +
                                "'" + cFileName:SCREEN-VALUE + "'"
                               ELSE "") +
                              (IF rsTransStatus = 2 THEN
                                " AND bExist"
                               ELSE IF rsTransStatus = 3 THEN
                                 " AND NOT bExist"
                               ELSE "")
                               ).

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"checkmodified","no").
END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bExist        AS LOG    NO-UNDO.
DEF VAR cObjectName   AS CHAR   NO-UNDO.
DEF VAR cModObjName   AS CHAR   NO-UNDO.
DEF VAR cObjectType   AS CHAR   NO-UNDO.

IF hBuffer:AVAIL THEN DO WITH FRAME {&FRAME-NAME}:
  IF cLanguage:SCREEN-VALUE = "" OR cLanguage:SCREEN-VALUE = ? THEN DO:
    MESSAGE "You must supply a target language"
            VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  ASSIGN cObjectName = hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE
         cObjectType = hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE
         .

  bExist = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cObjectName + "'"
                                     + "  AND cLanguage = '" + cLanguage:SCREEN-VALUE + "'" 
                                     + (IF tbSetDefNameType:CHECKED THEN 
                                         " AND cObjectType = '" + cObjectType + "'"
                                       + " AND cFileName = ''"
                                        ELSE IF tbSetDefName:CHECKED THEN
                                         " AND cFileName = ''"
                                       + " AND cObjectType = ''"
                                        ELSE
                                          " AND cObjectType = '" + cObjectType + "'"
                                        + " AND cFileName = '" +  hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE + "'"
                                       )
                                       ) NO-ERROR.
  
  IF NOT bExist AND ((cObjectType = "button" AND INDEX(cObjectName,"_btn_") > 0) OR (cObjectType = "menu-item"  AND INDEX(cObjectName,"_mitm_") > 0)) AND
     (tbSetDefNameType:CHECKED OR tbSetDefName:CHECKED)
     THEN DO:
    IF cObjectType = "button" THEN
      cModObjName = SUBSTR(cObjectName,INDEX(cObjectName,"btn_")).
    ELSE
      cModObjName = SUBSTR(cObjectName,INDEX(cObjectName,"mitm_")).

    bExist = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cModObjName + "'"
                                       + "  AND cLanguage = '" + cLanguage:SCREEN-VALUE + "'" 
                                       + (IF tbSetDefNameType:CHECKED THEN 
                                           " AND cObjectType = '" + cObjectType + "'"
                                         + " AND cFileName = ''"
                                          ELSE IF tbSetDefName:CHECKED THEN
                                           " AND cFileName = ''"
                                         + " AND cObjectType = ''"
                                          ELSE
                                            " AND cObjectType = '" + cObjectType + "'"
                                          + " AND cFileName = '" +  hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE + "'"
                                         )
                                         ) NO-ERROR.
  END.
  ELSE cModObjName = cObjectName.

  IF bExist THEN DO:
    bOk = DYNAMIC-FUNCTION("DoUpdate","JBoxTranslation","",
                  "",
                  hMyTransBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                  "cLabel,cTitle,cTooltip,cHelpText,cInitValue,cHelpTag",
                   cLabel:SCREEN-VALUE + CHR(1) + 
                      cTitle:SCREEN-VALUE + CHR(1) + 
                      cTooltip:SCREEN-VALUE + CHR(1) +
                      cHelpText:SCREEN-VALUE + CHR(1) +
                      cInitValue:SCREEN-VALUE + CHR(1) + 
                      cHelpTag:SCREEN-VALUE
                 ,TRUE).
    rRepos = hBuffTranslation:ROWID.
  END.
  ELSE DO: 
    bOk = DYNAMIC-FUNCTION("DoCreate","JBoxTranslation","",
                  "cLanguage,cObjectName,cObjectType,cLabel,cTitle,cTooltip,cHelpText,cInitValue,cHelpTag,cFileName",
                  cLanguage:SCREEN-VALUE + CHR(1) +
                    cModObjName + CHR(1) +
                    (IF NOT tbSetDefName:CHECKED THEN
                       cObjectType
                     ELSE "") + CHR(1) +
                    cLabel:SCREEN-VALUE + CHR(1) + 
                    cTitle:SCREEN-VALUE + CHR(1) + 
                    cTooltip:SCREEN-VALUE + CHR(1) +
                    cHelpText:SCREEN-VALUE + CHR(1) +
                    cInitValue:SCREEN-VALUE + CHR(1) + 
                    cHelpTag:SCREEN-VALUE + CHR(1) +
                    (IF NOT tbSetDefNameType:CHECKED AND NOT tbSetDefName:CHECKED AND cOperMode = "Window" THEN
                      hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE
                     ELSE "")
                 ,TRUE).
    rRepos = ?.
  END.

  IF NOT bOk THEN DO:
    MESSAGE "Error:" SKIP
             DYNAMIC-FUNCTION("getTransMessage")
             VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  ELSE hCurrWidget = ?.

  IF NOT bExist THEN 
    hBuffTranslation:BUFFER-CREATE().

  DYNAMIC-FUNCTION("DoRefetchTrans",hBuffTranslation,"","").

  RefreshTranslation().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetNewLanguageRecord C-Win 
PROCEDURE SetNewLanguageRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLng     AS CHAR NO-UNDO.
DEF VAR iReturn  AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Set new translation language","CHARACTER|xx",INPUT-OUTPUT cLng,OUTPUT iReturn).
IF iReturn = 2 THEN DO WITH FRAME {&FRAME-NAME}:
  ResetTranslation().

  DYNAMIC-FUNCTION("setLanguageCode",cLng).
   
  DYNAMIC-FUNCTION("setLanguages",DYNAMIC-FUNCTION("getLanguages") + "|" + cLng).

  DYNAMIC-FUNCTION("getSessionData").

  ASSIGN cLanguage:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getLanguageCode") + "|" + DYNAMIC-FUNCTION("getLanguageCode")
         cLanguage:SCREEN-VALUE = DYNAMIC-FUNCTION("getLanguageCode").

  hBuffer:EMPTY-TEMP-TABLE().
  DYNAMIC-FUNCTION("setTranManBuffer",hBuffer).
  DYNAMIC-FUNCTION("setSourceFileName",cSourceProcFile).
  DYNAMIC-FUNCTION("InitTranslation",hCurrWindow).

  RUN StartQuery.
  cmbObjectType:LIST-ITEMS = DYNAMIC-FUNCTION("getTransObjectTypeList").
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
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TranslateRecord C-Win 
PROCEDURE TranslateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hMenuItem      AS HANDLE NO-UNDO.
DEF VAR hTransWidget   AS HANDLE NO-UNDO.
DEF VAR rRepos         AS ROWID  NO-UNDO.
DEF VAR iMousePos      AS INT    NO-UNDO.

ASSIGN hMenuItem    = DYNAMIC-FUNCTION("getCurrentWidget")
       hTransWidget = hMenuItem:PARENT:OWNER.

IF hTransWidget:TYPE = "browse" THEN DO:
  iMousePos = DYNAMIC-FUNCTION("getMousePosition",hTransWidget:FRAME,"x").
  hTransWidget = hTransWidget:FIRST-COLUMN.
  REPEAT WHILE VALID-HANDLE(hTransWidget):
    IF iMousePos GE hTransWidget:X AND iMousePos LE hTransWidget:X + hTransWidget:WIDTH-PIXELS THEN LEAVE.
    hTransWidget = hTransWidget:NEXT-COLUMN.
  END.
END.

IF NOT VALID-HANDLE(hTransWidget) THEN RETURN.

bOk = hBuffer:FIND-FIRST("WHERE hWidget = WIDGET-HANDLE('" + STRING(hTransWidget) + "')") NO-ERROR.
IF bOk THEN DO:
  rRepos = hBuffer:ROWID.
  bOk = hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
  IF NOT bOk THEN DO:
    RUN ClearFilter.
    bOk = hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
    IF bOk THEN
      APPLY "value-changed" TO hBrowse.
  END.
  ELSE
    APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TranslationSetup C-Win 
PROCEDURE TranslationSetup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cBaseLng AS CHAR NO-UNDO.
DEF VAR cLng     AS CHAR NO-UNDO.
DEF VAR bNoLng   AS LOG  NO-UNDO.
DEF VAR bNoBase  AS LOG  NO-UNDO.
DEF VAR iReturn  AS INT  NO-UNDO.

IF DYNAMIC-FUNCTION("getBaseLanguageCode") = "" THEN DO:
  bNoBase = YES.
  RUN JBoxAskForValue.w ("Set base (development) language","CHARACTER|xx",INPUT-OUTPUT cBaseLng,OUTPUT iReturn).
  IF iReturn = 2 THEN
    DYNAMIC-FUNCTION("setBaseLanguageCode",cBaseLng).
  ELSE
    APPLY "close" TO THIS-PROCEDURE.
END.

IF DYNAMIC-FUNCTION("getLanguageCode") = "" OR DYNAMIC-FUNCTION("getLanguageCode") = DYNAMIC-FUNCTION("getBaseLanguageCode") THEN DO:
  RUN JBoxAskForValue.w ("Set translation language","CHARACTER|xx",INPUT-OUTPUT cLng,OUTPUT iReturn).
  IF iReturn = 2 THEN DO:
    DYNAMIC-FUNCTION("setLanguageCode",cLng).
    IF bNoBase THEN
      DYNAMIC-FUNCTION("setLanguages",cBaseLng + "|" + cLng).
    ELSE 
      DYNAMIC-FUNCTION("setLanguages",DYNAMIC-FUNCTION("getLanguages") + "|" + cLng).

    DYNAMIC-FUNCTION("getSessionData").

    hBuffTranslation = DYNAMIC-FUNCTION("getTranslationBuffer").
  END.
  ELSE
    APPLY "close" TO THIS-PROCEDURE.
END.

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

IF icBrowseName BEGINS "BrwTrans" THEN DO:

  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF CAN-DO("2,3,8,9",STRING(ix)) THEN
      ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = 50.
    ELSE IF CAN-DO("5,6,7",STRING(ix)) THEN
      ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = 150.
  END.
  ihBrowse:MOVE-COLUMN(8,1).
  ihBrowse:MOVE-COLUMN(9,2).
  ihBrowse:MOVE-COLUMN(3,12).
  ihBrowse:MOVE-COLUMN(5,15).
END.


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSourceBrowse C-Win 
FUNCTION getSourceBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowseTrans.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTranManBuffer C-Win 
FUNCTION getTranManBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWidgets C-Win 
FUNCTION getWidgets RETURNS LOGICAL
  ( INPUT ihWidget  AS HANDLE, 
    INPUT iiLevel   AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  List widgets to file
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/ 
DEF VAR cWidgetHandle AS CHAR NO-UNDO.
REPEAT WHILE ihWidget NE ?:

  IF CAN-DO("BROWSE,BUTTON,COMBO-BOX,DIALOG-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX,WINDOW",ihWidget:TYPE) 
     AND (IF ihWidget:TYPE NE "window" AND ihWidget:TYPE NE "BROWSE" THEN ihWidget:NAME NE ? ELSE TRUE) THEN DO:

    IF ihWidget:PARENT NE ? AND ihWidget:PARENT:TYPE = "BROWSE" THEN 
      cWidgetType = "BROWSE-COLUMN".
    ELSE
      cWidgetType = ihWidget:TYPE.

    IF ihWidget:TYPE = "BUTTON" AND SUBSTR(ihWidget:NAME,1,4) = "btn_" AND R-INDEX(ihWidget:NAME,"_") > 4 THEN
      cWidgetName = SUBSTR(ihWidget:NAME,1,R-INDEX(ihWidget:NAME,"_") - 1).
    ELSE IF ihWidget:TYPE = "window" THEN 
      cWidgetName = cSourceProcFile.
    ELSE
      cWidgetName = ihWidget:NAME.

    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cToolTip"):BUFFER-VALUE    = IF CAN-QUERY(ihWidget,"TOOLTIP") THEN ihWidget:TOOLTIP ELSE ""
           hBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE      = IF ihWidget:TYPE = "RADIO-SET" THEN ihWidget:RADIO-BUTTONS
/*                                                               ELSE IF ihWidget:TYPE = "COMBO-BOX" AND ihWidget:SUBTYPE = "SIMPLE" THEN ihWidget:LIST-ITEMS */
/*                                                               ELSE IF ihWidget:TYPE = "COMBO-BOX" THEN ihWidget:LIST-ITEM-PAIRS                            */
                                                              ELSE IF cWidgetType = "BROWSE-COLUMN" THEN
                                                                DYNAMIC-FUNCTION("getStrippedSortLabel",ihWidget)
                                                              ELSE IF CAN-QUERY(ihWidget,"LABEL") THEN ihWidget:LABEL                                                            
                                                              ELSE ""
           hBuffer:BUFFER-FIELD("cTitle"):BUFFER-VALUE      = IF CAN-QUERY(ihWidget,"TITLE") THEN 
                                                                IF ihWidget:TITLE MATCHES "*[*" THEN SUBSTR(ihWidget:TITLE,1,INDEX(ihWidget:TITLE,"[") - 2)
                                                                ELSE ihWidget:TITLE
                                                              ELSE ""
           hBuffer:BUFFER-FIELD("cInitValue"):BUFFER-VALUE  = IF CAN-QUERY(ihWidget,"TEXT") THEN ihWidget:SCREEN-VALUE 
/*                                                               ELSE IF ihWidget:TYPE = "FILL-IN" AND ihWidget:PARENT:TYPE = "BROWSE" THEN */
/*                                                                 ihWidget:PARENT:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ihWidget:NAME):INITIAL */
                                                              ELSE ""
           hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE = cWidgetName
           hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = cWidgetType
           hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE    = STRING(ihWidget)
           hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE      = NO
           hBuffer:BUFFER-FIELD("cLanguage"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getBaseLanguageCode")
           .

    bOk = hMyTransBuffer:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
/*                                         " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" +  */
                                        " AND cFileName   = '" + cSourceProcFile + "'" +
                                        " AND cObjectType = '" + cWidgetType + "'") NO-ERROR.
/*     IF bOk THEN                                                           */
/*       hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE   = cSourceProcFile. */
/*     ELSE                                                                  */
    IF NOT bOk THEN
      bOk = hMyTransBuffer:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
/*                                           " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" +  */
                                          " AND cFileName   = ''" +
                                          " AND cObjectType = '" + cWidgetType + "'") NO-ERROR.
    IF NOT bOk THEN
      bOk = hMyTransBuffer:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
/*                                           " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" +  */
                                          " AND cFileName   = ''" +
                                          " AND cObjectType NE 'DBFIELD'") NO-ERROR.
    IF NOT bOk THEN
      bOk = hMyTransBuffer:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
/*                                           " AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'" +  */
                                          " AND cObjectType = 'DBFIELD'") NO-ERROR.

    IF bOk THEN
      ASSIGN 
/*              hBuffer:BUFFER-FIELD("cLanguage"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getLanguageCode") */
             hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES
             .
/*     ELSE                                                                                          */
/*       hBuffer:BUFFER-FIELD("cLanguage"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getBaseLanguageCode"). */

    IF NOT CAN-DO(cmbObjectType:LIST-ITEMS IN FRAME {&FRAME-NAME},cWidgetType) THEN
      cmbObjectType:LIST-ITEMS = cmbObjectType:LIST-ITEMS + "," + cWidgetType.


/*
    hQueryTranslation:QUERY-PREPARE("FOR EACH " + hMyTransBuffer:NAME + " WHERE cObjectName = '" + hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE 
                                  + "' AND cLanguage = '" + DYNAMIC-FUNCTION("getLanguageCode") + "'"
                                  + (IF bOk THEN 
                                      " AND ROWID(" + hMyTransBuffer:NAME + ") NE TO-ROWID('" + STRING(hMyTransBuffer:ROWID) + "')"
                                     ELSE "")
                                    ).
    hQueryTranslation:QUERY-OPEN.
    IF hQueryTranslation:IS-OPEN THEN DO:
      cWidgetHandle = hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE.
      hQueryTranslation:GET-FIRST().
      REPEAT WHILE NOT hQueryTranslation:QUERY-OFF-END:
        hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.
        hBuffer:BUFFER-CREATE().
        hBuffer:BUFFER-COPY(hMyTransBuffer).
        ASSIGN hBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES
               hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE = cWidgetHandle.
        IF NOT CAN-DO(cmbObjectType:LIST-ITEMS,hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE) THEN
          cmbObjectType:LIST-ITEMS = cmbObjectType:LIST-ITEMS + "," + hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE.
        hQueryTranslation:GET-NEXT().
      END.
    END.

    IF CAN-QUERY(ihWidget,'MENU-BAR') AND ihWidget:MENU-BAR NE ? THEN
      getWidgets(ihWidget:MENU-BAR:FIRST-CHILD,iiLevel + 1).
    ELSE IF CAN-QUERY(ihWidget,'FIRST-COLUMN') AND ihWidget:FIRST-COLUMN NE ? THEN 
      getWidgets(ihWidget:FIRST-COLUMN,iiLevel + 1).
    ELSE IF CAN-QUERY(ihWidget,'NEXT-COLUMN') AND ihWidget:NEXT-COLUMN NE ? THEN 
      getWidgets(ihWidget:NEXT-COLUMN,iiLevel + 1).
*/
  END.
      
  IF CAN-QUERY(ihWidget,'FIRST-CHILD') AND ihWidget:FIRST-CHILD  <> ? THEN
    getWidgets(ihWidget:FIRST-CHILD,iiLevel + 1).

  IF CAN-QUERY(ihWidget,'POPUP-MENU') AND ihWidget:POPUP-MENU NE ? THEN
    getWidgets(ihWidget:POPUP-MENU:FIRST-CHILD,iiLevel + 1).

  IF iiLevel > 0 AND CAN-QUERY(ihWidget,'NEXT-SIBLING') THEN ihWidget = ihWidget:NEXT-SIBLING.
  ELSE LEAVE.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RefreshTranslation C-Win 
FUNCTION RefreshTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQueryRefresh AS HANDLE NO-UNDO.
DEF VAR hBuffRefresh  AS HANDLE NO-UNDO.
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR cTypeList     AS CHAR   NO-UNDO.
DEF VAR cModObjName   AS CHAR   NO-UNDO.


IF hBuffer:AVAIL THEN DO:
  cTypeList = cmbObjectType:LIST-ITEMS IN FRAME {&FRAME-NAME}.


  IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "button" AND INDEX(cObjectName,"_btn_") > 0 THEN 
    cModObjName = SUBSTR(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,INDEX(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,"btn_")).
  ELSE IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "menu-item" AND INDEX(cObjectName,"_mitm_") > 0 THEN
    cModObjName = SUBSTR(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,INDEX(hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE,"mitm_")).
  ELSE cModObjName = cObjectName.

  CREATE BUFFER hBuffRefresh FOR TABLE hBuffer.
  CREATE QUERY hQueryRefresh.
  hQueryRefresh:SET-BUFFERS(hBuffRefresh).
  hQueryRefresh:QUERY-PREPARE("FOR EACH " + hBuffRefresh:NAME + " WHERE cObjectName = '" + hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "' OR cObjectName = '" + cModObjName + "'").
  hQueryRefresh:QUERY-OPEN().
  hQueryRefresh:GET-FIRST().
  REPEAT WHILE NOT hQueryRefresh:QUERY-OFF-END:
    IF cOperMode = "Window" THEN DO:
      DYNAMIC-FUNCTION("setTranManBuffer",hBuffer).
      hWidget = hBuffRefresh:BUFFER-FIELD("hWidget"):BUFFER-VALUE.
      IF VALID-HANDLE(hWidget) THEN DO:
        DYNAMIC-FUNCTION("setSourceFileName",cSourceProcFile).
        DYNAMIC-FUNCTION("InitTranslation",hWidget).
      END.
      ELSE PUBLISH "InitStringTranslation" (hCurrWindow,INPUT-OUTPUT cTypeList). 
    END.
    ELSE hBuffRefresh:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.

    hQueryRefresh:GET-NEXT().
  END.
  DELETE OBJECT hQueryRefresh.
  DELETE OBJECT hBuffRefresh.

  hBrowse:REFRESH().
  APPLY "value-changed" TO hBrowse.

  IF rRepos NE ? THEN DO:
    hBrowseTrans:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowseTrans.
  END.

  RETURN YES.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ResetTranslation C-Win 
FUNCTION ResetTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQueryRefresh AS HANDLE NO-UNDO.
DEF VAR hBuffRefresh  AS HANDLE NO-UNDO.
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR hWidgetRect   AS HANDLE NO-UNDO.

IF cOperMode = "window" THEN DO:

  CREATE BUFFER hBuffRefresh FOR TABLE hBuffer.
  CREATE QUERY hQueryRefresh.
  hQueryRefresh:SET-BUFFERS(hBuffRefresh).
  hQueryRefresh:QUERY-PREPARE("FOR EACH " + hBuffRefresh:NAME).
  hQueryRefresh:QUERY-OPEN().
  hQueryRefresh:GET-FIRST().
  REPEAT WHILE NOT hQueryRefresh:QUERY-OFF-END:
    ASSIGN hWidget     = hBuffRefresh:BUFFER-FIELD("hWidget"):BUFFER-VALUE
           hWidgetRect = hBuffRefresh:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE.
    IF VALID-HANDLE(hWidget) THEN DO:
      IF CAN-QUERY(hWidget,"label-bgcolor") THEN
        hWidget:LABEL-BGCOLOR = hBuffRefresh:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE NO-ERROR.
      ELSE IF CAN-QUERY(hWidget,"bgcolor") THEN
        hWidget:BGCOLOR = hBuffRefresh:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE NO-ERROR.
    END.
    IF VALID-HANDLE(hWidgetRect) THEN
      DELETE OBJECT hWidgetRect.
    hQueryRefresh:GET-NEXT().
  END.
  DELETE OBJECT hQueryRefresh.
  DELETE OBJECT hBuffRefresh.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldStatus C-Win 
FUNCTION setFieldStatus RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF hBuffer:AVAIL THEN DO:
    IF hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE NE "" THEN DO:
      IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "message" THEN DO:
        DISABLE {&List-1}.
        ENABLE {&List-2}.
      END.
      ELSE DO:
        DISABLE {&List-2}.
        IF cOperMode = "window" THEN DO:
          IF hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = "text" THEN DO:
            DISABLE {&List-1}.
            ENABLE cInitValue.
          END.
          ELSE DO:
            DISABLE cInitValue.
            IF CAN-DO(cToolTipList,hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE) OR tbSetDefName:CHECKED THEN
              ENABLE cToolTip cHelpText.
            ELSE DISABLE cToolTip.
            IF CAN-DO(cLabelList,hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE) OR tbSetDefName:CHECKED THEN
              ENABLE cLabel.
            ELSE DISABLE cLabel.
            IF CAN-DO(cTitleList,hBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE) THEN
              ENABLE cTitle.
            ELSE DISABLE cTitle.
          END.
        END.
        ELSE DO:
          ENABLE {&List-1}.
          DISABLE cTitle cInitValue.
        END.
      END.
    END.
    ELSE DO:
      DISABLE {&List-1}.
      DISABLE {&List-2}.
      ENABLE cLabel.
      ASSIGN tbSetDefName:CHECKED       = NO
             tbSetDefNameType:CHECKED   = NO
             tbSetDefName:SENSITIVE     = NO
             tbSetDefNameType:SENSITIVE = NO
             .
    END.
  END.
  ELSE DO:
    DISABLE {&List-1}.
    DISABLE {&List-2}.
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION swapValue C-Win 
FUNCTION swapValue RETURNS LOGICAL () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* DEF VAR hWidget           AS HANDLE NO-UNDO.                                                                         */
/* hWidget = WIDGET-HANDLE(hBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE).                                              */
/* IF hWidget:PARENT NE ? AND hWidget:PARENT:TYPE = "BROWSE" THEN                                                       */
/*   cWidgetType = "BROWSE-COLUMN".                                                                                     */
/* ELSE                                                                                                                 */
/*   cWidgetType = hWidget:TYPE.                                                                                        */
/*                                                                                                                      */
/* IF VALID-HANDLE(hWidget) THEN DO:                                                                                    */
/*   IF CAN-DO(cTitleList,cWidgetType) THEN                                                                             */
/*     hWidget:TITLE = hBuffer:BUFFER-FIELD("cTitle"):BUFFER-VALUE +                                                    */
/*                     IF hWidget:TITLE MATCHES "*[*" THEN SUBSTR(hWidget:TITLE,INDEX(hWidget:TITLE,"[") - 1) ELSE "".  */
/*   ELSE IF hWidget:TYPE = "text" THEN                                                                                 */
/*     hWidget:SCREEN-VALUE = hBuffer:BUFFER-FIELD("cInitValue"):BUFFER-VALUE.                                          */
/*   ELSE DO:                                                                                                           */
/*     IF CAN-DO(cToolTipList,cWidgetType) THEN hWidget:TOOLTIP = hBuffer:BUFFER-FIELD("cToolTip"):BUFFER-VALUE.        */
/*     IF CAN-DO(cLabelList,cWidgetType) THEN hWidget:LABEL = hBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE.              */
/*   END.                                                                                                               */
/* END.                                                                                                                 */

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

