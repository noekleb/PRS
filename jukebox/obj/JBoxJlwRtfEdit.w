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

{JukeBoxControlsGeneral.i}
{JukeBoxRichEditor.i}

DEF VAR hEditor             AS INT    NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR bSetToolbar         AS LOG    NO-UNDO INIT YES.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR iSuccess            AS INT    NO-UNDO.
DEF VAR cHelpText           AS CHAR   NO-UNDO.
DEF VAR hHelpTextWidget     AS HANDLE NO-UNDO.
DEF VAR hTabFromWidget      AS HANDLE NO-UNDO.
DEF VAR cPrepSaveFile       AS CHAR   NO-UNDO.
DEF VAR cDefaultFont        AS CHAR   NO-UNDO.
DEF VAR iDefaultFontSize    AS INT    NO-UNDO INIT 8.
DEF VAR cFontList           AS CHAR   NO-UNDO
    INIT "Arial,Arial Black,Comic Sans MS,Comic Sans MS Bold Courier,Courier New,Estrangelo Edessa,Franklin Gothic Medium,Italic,Georgia,Lucida Console,Lucida Sans Unicode,MS Sans Serif,Modern MS Sans Serif,MS Serif,Mv Boli,Palatino Linotype,Roman,Tahoma,Tahoma Bold,Times New Roman,Trebuchet MS,Tunga,Verdana". 
DEF VAR cFontSizeList       AS CHAR   NO-UNDO
    INIT "8,9,10,12,14,16,18,24,28".
DEF VAR cColorList          AS CHAR   NO-UNDO
    INIT "Aqua,Black,Blue,Cream,Darkgray,Fuchsia,Gray,Green,Limegreen,Lightgray,Maroon,Mediumgray,Mintgreen,Navyblue,Olive,Purple,Red,Silver,Skyblue,Teal,White,Yellow".
DEF VAR cColorNumList       AS CHAR   NO-UNDO
    INIT "16776960,0,16711680,15793151,8421504,16711935,8421504,32768,65280,12632256,128,10789024,12639424,8388608,32896,8388736,255,12632256,15780518,8421376,16777215,65535".

DEF STREAM strRtfLoad.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rtf-edit-frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRtfFocus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AppendRtfEditor C-Win 
FUNCTION AppendRtfEditor RETURNS LOGICAL
  ( INPUT icTextOrFile  AS CHAR,
    INPUT ibFirst       AS LOG,
    INPUT ibSetToolbar  AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearRtfEditor C-Win 
FUNCTION ClearRtfEditor RETURNS LOGICAL (
       INPUT ibSetToolbar AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CommitSaveRtfDoc C-Win 
FUNCTION CommitSaveRtfDoc RETURNS LOGICAL
  ( INPUT icContext         AS CHAR,
    INPUT icEntityId        AS CHAR,
    INPUT icDesc            AS CHAR,
    INPUT ibSavePlainAsDesc AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteRtfTextDoc C-Win 
FUNCTION DeleteRtfTextDoc RETURNS LOGICAL
  ( INPUT icContext         AS CHAR,
    INPUT icEntityId        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPlainText C-Win 
FUNCTION getPlainText RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRichText C-Win 
FUNCTION getRichText RETURNS LONGCHAR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitRtfEditor C-Win 
FUNCTION InitRtfEditor RETURNS HANDLE
  ( INPUT ihEditRect AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrepareSaveRtfDoc C-Win 
FUNCTION PrepareSaveRtfDoc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveRtfTextDoc C-Win 
FUNCTION SaveRtfTextDoc RETURNS LOGICAL
  ( INPUT icContext         AS CHAR,
    INPUT icEntityId        AS CHAR,
    INPUT icDesc            AS CHAR,
    INPUT ibSavePlainAsDesc AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveToFile C-Win 
FUNCTION SaveToFile RETURNS LOGICAL
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setColorList C-Win 
FUNCTION setColorList RETURNS LOGICAL
  ( INPUT icColorList    AS CHAR,
    INPUT icColorNumList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFontList C-Win 
FUNCTION setFontList RETURNS LOGICAL
  ( INPUT icFontList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFontSizeList C-Win 
FUNCTION setFontSizeList RETURNS LOGICAL
  ( INPUT icFontSizeList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setHelpText C-Win 
FUNCTION setHelpText RETURNS LOGICAL
  ( INPUT icHelpText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTabFromWidget C-Win 
FUNCTION setTabFromWidget RETURNS LOGICAL
  ( INPUT ihTabFromWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTBmodified C-Win 
FUNCTION setTBmodified RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setToolbarHandle C-Win 
FUNCTION setToolbarHandle RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewRtfTextDoc C-Win 
FUNCTION ViewRtfTextDoc RETURNS LOGICAL
  ( INPUT icContext  AS CHAR,
    INPUT icEntityId AS CHAR,
    INPUT icText     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnRtfFocus 
     LABEL "Just for focus" 
     SIZE 17 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rtf-edit-frame
     btnRtfFocus AT ROW 6.24 COL 21
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 39.2 BY 6.71 DROP-TARGET.


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
         HEIGHT             = 6.71
         WIDTH              = 39.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME rtf-edit-frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rtf-edit-frame:SELECTABLE       = TRUE.

ASSIGN 
       btnRtfFocus:HIDDEN IN FRAME rtf-edit-frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
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
  IF VALID-HANDLE(hParent) THEN
    APPLY "close" TO hParent.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rtf-edit-frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rtf-edit-frame C-Win
ON DROP-FILE-NOTIFY OF FRAME rtf-edit-frame
DO:
  MESSAGE 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rtf-edit-frame C-Win
ON MOUSE-SELECT-DOWN OF FRAME rtf-edit-frame
DO:
  MESSAGE "mouse-select-down"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rtf-edit-frame C-Win
ON SELECTION OF FRAME rtf-edit-frame
DO:
  MESSAGE "selection"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRtfFocus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRtfFocus C-Win
ON CHOOSE OF btnRtfFocus IN FRAME rtf-edit-frame /* Just for focus */
DO:
  MESSAGE SELF:NAME FOCUS:NAME 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
  RUN disable_UI.
  APPLY "close" TO hParent.
  DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON "any-printable" OF FRAME rtf-edit-frame ANYWHERE DO:
  setTBmodified().
  IF FOCUS:TYPE = "fill-in" THEN
    RUN SetRtfFocus.
  APPLY "entry" TO FRAME rtf-edit-frame.
  RETURN.
END.
ON "delete-character" OF FRAME rtf-edit-frame ANYWHERE DO:
  setTBmodified().
  APPLY "entry" TO FRAME rtf-edit-frame.
  RETURN.
END.
ON "backspace" OF FRAME rtf-edit-frame ANYWHERE DO:
  setTBmodified().
  APPLY "entry" TO FRAME rtf-edit-frame.
  RETURN.
END.

ON "ctrl-s" OF FRAME rtf-edit-frame ANYWHERE DO:
  IF VALID-HANDLE(hToolbar) THEN
    RUN InvokeMethod(hToolbar,"SaveRecord").
  RETURN.
END.

ON "entry" OF FRAME rtf-edit-frame ANYWHERE DO:
  IF VALID-HANDLE(hHelpTextWidget) THEN
    hHelpTextWidget:SCREEN-VALUE = cHelpText.
  RETURN.
END.
/*
ON "mouse-select-click" OF FRAME rtf-edit-frame 
   OR "mouse-select-dblclick" OF FRAME rtf-edit-frame 
   OR "mouse-select-down" OF FRAME rtf-edit-frame 
   OR "mouse-select-up" OF FRAME rtf-edit-frame 
   OR "mouse-menu-down" OF FRAME rtf-edit-frame 
   OR "MOUSE-MENU-CLICK" OF FRAME rtf-edit-frame 
   OR "MOUSE-EXTEND-DOWN" OF FRAME rtf-edit-frame 
   OR "MOUSE-MOVE-CLICK" OF FRAME rtf-edit-frame
   OR "LEFT-MOUSE-CLICK" OF FRAME rtf-edit-frame
   OR "LEFT-MOUSE-down" OF FRAME rtf-edit-frame
   OR "LEFT-MOUSE-up" OF FRAME rtf-edit-frame
  ANYWHERE DO:
  MESSAGE LAST-EVENT:LABEL
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AlignRecord C-Win 
PROCEDURE AlignRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.
DEF VAR cAlign      AS CHAR   NO-UNDO INIT "Left Justified,Centered,Right Justified".

hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF VALID-HANDLE(hCurrWidget) THEN DO:
    
  IF hCurrWidget:LABEL NE "Left Indent" THEN 
    RUN RichEditorSetParagraph(hEditor,
                          LOOKUP(hCurrWidget:LABEL,cAlign),
                          0,
                          0,
                          0,
                          0,
                          {&PAR_VAL_ALIGNMENT},OUTPUT iSuccess).
  ELSE
    RUN RichEditorSetParagraph(hEditor,
                               0,0,10,0,0,
                               {&PAR_VAL_LEFTINDENT}  ,OUTPUT iSuccess).
    
  setTBmodified().

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppendFirstRecord C-Win 
PROCEDURE AppendFirstRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cAppendFile AS CHAR NO-UNDO.

SYSTEM-DIALOG GET-FILE cAppendFile
              FILTERS "Rtf file (*.rtf)" "*.rtf",
                      "Text file (*.txt)" "*.txt"
              UPDATE bOk.

IF bOK THEN
  AppendRtfEditor(cAppendFile,YES,YES).
              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppendLastRecord C-Win 
PROCEDURE AppendLastRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cAppendFile AS CHAR NO-UNDO.

SYSTEM-DIALOG GET-FILE cAppendFile
              FILTERS "Rtf file (*.rtf)" "*.rtf",
                      "Text file (*.txt)" "*.txt"
              UPDATE bOk.

IF bOK THEN
  AppendRtfEditor(cAppendFile,NO,YES).
              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BulletsRecord C-Win 
PROCEDURE BulletsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorSetParagraph(hEditor,
                           0,0,0,0,{&PAR_BULLETS},
                           {&PAR_VAL_NUMBERING}  ,OUTPUT iSuccess).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColorRecord C-Win 
PROCEDURE ColorRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.
DEF VAR iColorNum   AS INT    NO-UNDO.
hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").

IF VALID-HANDLE(hCurrWidget) THEN DO:
  iColorNum = INT(ENTRY(LOOKUP(hCurrWidget:LABEL,cColorList),cColorNumList)) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
    RUN RichEditorSetFont(hEditor,"",
                          0,0,iColorNum,{&FN_VAL_FONTCOLOR},OUTPUT iSuccess).  
    setTBmodified().
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorCopyToClipBoard(hEditor, OUTPUT iSuccess).
setTBmodified().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CutRecord C-Win 
PROCEDURE CutRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorCutToClipBoard(hEditor, OUTPUT iSuccess).  
setTBmodified().

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
  HIDE FRAME rtf-edit-frame.
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
  ENABLE btnRtfFocus 
      WITH FRAME rtf-edit-frame.
  {&OPEN-BROWSERS-IN-QUERY-rtf-edit-frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FontRecord C-Win 
PROCEDURE FontRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.
hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF VALID-HANDLE(hCurrWidget) THEN DO:
  RUN RichEditorSetFont(hEditor,hCurrWidget:LABEL,
                         0,0,0,{&FN_VAL_FONTNAME},OUTPUT iSuccess).
  setTBmodified().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FontSizeRecord C-Win 
PROCEDURE FontSizeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.
hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF VALID-HANDLE(hCurrWidget) THEN DO:
  RUN RichEditorSetFont(hEditor,"",
                         INT(hCurrWidget:LABEL),0,0,{&FN_VAL_FONTSIZE},OUTPUT iSuccess).
  setTBmodified().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FontStyleRecord C-Win 
PROCEDURE FontStyleRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.
DEF VAR cStyles     AS CHAR   NO-UNDO.
DEF VAR iStyles     AS INT    NO-UNDO.

hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF VALID-HANDLE(hCurrWidget) THEN DO:
  cStyles = hCurrWidget:LABEL.
  DO ix = 1 TO NUM-ENTRIES(cStyles," "):
    CASE ENTRY(ix,cStyles," "):
      WHEN "Normal"    THEN iStyles = 0.
      WHEN "Bold"      THEN iStyles = iStyles + {&FS-BOLD}.
      WHEN "Underline" THEN iStyles = iStyles + {&FS-UNDERLINE}.
      WHEN "Italic"    THEN iStyles = iStyles + {&FS-ITALIC}.
      WHEN "Strikeout" THEN iStyles = iStyles + {&FS-STRIKEOUT}.
    END CASE.
  END.
  RUN RichEditorSetFont(hEditor,"",0,iStyles,0, 
                      {&FN_VAL_FONTSTYLE}, OUTPUT iSuccess).
  setTBmodified().
END.

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
DEF INPUT PARAM ihRectangle        AS HANDLE NO-UNDO.

DEF VAR cMenu     AS CHAR NO-UNDO.

DO WITH FRAME rtf-edit-frame:
  ASSIGN FRAME rtf-edit-frame:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS
         FRAME rtf-edit-frame:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
         FRAME rtf-edit-frame:VIRTUAL-WIDTH-PIXELS = ihRectangle:WIDTH-PIXELS
         FRAME rtf-edit-frame:VIRTUAL-HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
         FRAME rtf-edit-frame:X = DYNAMIC-FUNCTION("getAbsPosition",ihRectangle,"X") /* ihRectangle:X */
         FRAME rtf-edit-frame:Y = DYNAMIC-FUNCTION("getAbsPosition",ihRectangle,"Y") /* ihRectangle:Y */
         btnRtfFocus:HIDDEN = YES
         FRAME rtf-edit-frame:TAB-STOP = NO
         .

  hHelpTextWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"HelpTextWidget")) NO-ERROR.

  RUN RichEditorCreate(FRAME rtf-edit-frame:HWND, OUTPUT hEditor).
  
  DYNAMIC-FUNCTION("NewMenuBand",FRAME rtf-edit-frame:HANDLE
                  ,"Undo"
                 + ",rule"
                 + ",Cut"
                 + ",Copy"
                 + ",Paste"
                 + ",PasteSpecial;Paste special"
                 + ",rule"
                 + ",|Font"
                 + ",|Font size"
                 + ",|Font style"
                 + ",|Font Color"
                 + ",ResetFontAndStyle;Reset Font Settings"
                 + ",rule"
                 + ",|Paragraph"
                 + ",rule"
                 + ",InsertFromFile;Load file"
/*                  + ",|Append file"  */
                 + ",Print"
                  ,"").

  cMenu = "".
  DO ix = 1 TO NUM-ENTRIES(cFontList):
    cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Font;" + ENTRY(ix,cFontList).
  END.
  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder1"))
                  ,cMenu
                  ,"").

  cMenu = "".
  DO ix = 1 TO NUM-ENTRIES(cFontSizeList):
    cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "FontSize;" + ENTRY(ix,cFontSizeList).
  END.
  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder2"))
                  ,cMenu
                  ,"").

  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder3"))
                ,"FontStyle;Normal"
               + ",FontStyle;Bold"
               + ",FontStyle;Bold Italic"
               + ",FontStyle;Bold Underline"
               + ",FontStyle;Bold Strikeout"
               + ",FontStyle;Bold Italic Underline"
               + ",FontStyle;Bold Italic Strikeout"
               + ",FontStyle;Bold Underline Strikeout"
               + ",FontStyle;Bold Italic Underline Strikeout"
               + ",FontStyle;Italic"
               + ",FontStyle;Italic Underline"
               + ",FontStyle;Italic Strikeout"
               + ",FontStyle;Italic Underline Strikeout"
               + ",FontStyle;Underline"
               + ",FontStyle;Underline Strikeout"
               + ",FontStyle;Strikeout"
                ,"").

  cMenu = "".
  DO ix = 1 TO NUM-ENTRIES(cColorList):
    cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Color;" + ENTRY(ix,cColorList).
  END.
  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder4"))
                  ,cMenu
                  ,"").


  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder5"))
                ,"Align;Left Justified"
               + ",Align;Centered"
               + ",Align;Right Justified"
               + ",Align;Left Indent"
               + ",Bullets;Bullets"
               + ",NoBullets;No Bullets"
                ,"").

/*   DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder6")) */
/*                 ,"AppendFirst;First"                                                                                        */
/*                + ",AppendLast;Last"                                                                                         */
/*                 ,"").                                                                                                       */
/*                                                                                                                             */

  GET-KEY-VALUE SECTION "StartUp" KEY "DefaultFont" VALUE cDefaultFont.
  IF INDEX(cDefaultFont,"size=") > 0 THEN DO:
    iDefaultFontSize = INT(SUBSTR(cDefaultFont,INDEX(cDefaultFont,"size=") + 5)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN iDefaultFontSize = 8.
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InsertFromFileRecord C-Win 
PROCEDURE InsertFromFileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorOpenFile(hEditor,"",OUTPUT iSuccess).

setTBmodified().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoBulletsRecord C-Win 
PROCEDURE NoBulletsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorSetParagraph(hEditor,
                           0,0,0,0,{&PAR_NOBULLETS},
                           {&PAR_VAL_NUMBERING}  ,OUTPUT iSuccess).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PasteRecord C-Win 
PROCEDURE PasteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorPaste(hEditor, OUTPUT iSuccess).  
  
setTBmodified().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PasteSpecialRecord C-Win 
PROCEDURE PasteSpecialRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorPasteSpecial(hEditor, OUTPUT iSuccess).  
setTBmodified().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorPrint(hEditor, "Some Title for the print.",OUTPUT iSuccess).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResetFontAndStyleRecord C-Win 
PROCEDURE ResetFontAndStyleRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RichEditorSetFont(hEditor,cDefaultFont,0,0,0,{&FN_VAL_FONTNAME},OUTPUT iSuccess).
RUN RichEditorSetFont(hEditor,"",0,0,0,{&FN_VAL_FONTSTYLE}, OUTPUT iSuccess).
RUN RichEditorSetFont(hEditor,"",iDefaultFontSize,0,0,{&FN_VAL_FONTSIZE},OUTPUT iSuccess).
RUN RichEditorSetFont(hEditor,"",0,0,0,{&FN_VAL_FONTCOLOR},OUTPUT iSuccess).  
  
IF NOT PROGRAM-NAME(2) MATCHES "*ViewRtfTextDoc*" THEN
  setTBmodified().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetRtfFocus C-Win 
PROCEDURE SetRtfFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "entry" TO btnRtfFocus IN FRAME rtf-edit-frame.
APPLY "tab" TO btnRtfFocus.
/* APPLY "any-printable" TO FRAME rtf-edit-frame.  */

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
RUN RichEditorUndo(hEditor, OUTPUT iSuccess).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AppendRtfEditor C-Win 
FUNCTION AppendRtfEditor RETURNS LOGICAL
  ( INPUT icTextOrFile  AS CHAR,
    INPUT ibFirst       AS LOG,
    INPUT ibSetToolbar  AS LOG) :
/*------------------------------------------------------------------------------
  Purpose: Append text or file to the editor
    Notes: 
------------------------------------------------------------------------------*/
DEF VAR cAddFile       AS CHAR NO-UNDO.
DEF VAR cRtfTextFile   AS CHAR NO-UNDO.
DEF VAR bDeleteAddFile AS LOG  NO-UNDO.
DEF VAR cLine          AS CHAR NO-UNDO.

cRtfTextFile = DYNAMIC-FUNCTION("getUniqueFileName").
RUN RichEditorSaveFile(hEditor, cRtfTextFile,OUTPUT iSuccess).  

IF NOT PROGRAM-NAME(2) MATCHES("*Append*Record*") THEN DO:
  cAddFile = DYNAMIC-FUNCTION("getUniqueFileName").
  
  OUTPUT STREAM strRtfLoad TO VALUE(cAddFile) APPEND.
  PUT STREAM strRtfLoad UNFORMATTED icTextOrFile.
  OUTPUT STREAM strRtfLoad CLOSE.

  bDeleteAddFile = YES.
END.
ELSE cAddFile = icTextOrFile.

IF ibFirst THEN DO:
  OS-APPEND VALUE(cRtfTextFile) VALUE(cAddFile).
  RUN RichEditorOpenFile(hEditor,cAddFile,OUTPUT iSuccess).
END.
ELSE DO:
  INPUT FROM VALUE(cAddFile).
  OUTPUT TO VALUE(cRtfTextFile) APPEND.
  REPEAT:
    IMPORT UNFORMATTED cLine.
    PUT UNFORMATTED cLine.
  END.
  INPUT CLOSE.
  OUTPUT CLOSE.
/*   OS-APPEND VALUE(cAddFile) VALUE(cRtfTextFile). */
  MESSAGE OS-ERROR SKIP cAddFile SKIP cRtfTextFile
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RUN RichEditorOpenFile(hEditor,cRtfTextFile,OUTPUT iSuccess).

END.

IF bDeleteAddFile THEN OS-DELETE VALUE(cAddFile).

OS-DELETE VALUE(cRtfTextFile).

IF ibSetToolbar THEN setTBmodified().

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearRtfEditor C-Win 
FUNCTION ClearRtfEditor RETURNS LOGICAL (
       INPUT ibSetToolbar AS LOG) :
/*------------------------------------------------------------------------------
  Purpose: Clear content of editor
    Notes: 
------------------------------------------------------------------------------*/
DEF VAR cClearFile       AS CHAR NO-UNDO.

cClearFile = DYNAMIC-FUNCTION("getUniqueFileName").

OUTPUT STREAM strRtfLoad TO VALUE(cClearFile) APPEND.
PUT STREAM strRtfLoad UNFORMATTED "".
OUTPUT STREAM strRtfLoad CLOSE.

RUN RichEditorOpenFile(hEditor,cClearFile,OUTPUT iSuccess).

OS-DELETE VALUE(cClearFile).

IF ibSetToolbar THEN setTBmodified().

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CommitSaveRtfDoc C-Win 
FUNCTION CommitSaveRtfDoc RETURNS LOGICAL
  ( INPUT icContext         AS CHAR,
    INPUT icEntityId        AS CHAR,
    INPUT icDesc            AS CHAR,
    INPUT ibSavePlainAsDesc AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:    Save the rtf content of the already prepared file as blob document 
  Parameters: Context and identificator for document
  Notes:      Requires JBoxDocument tables and features
              Normal invocation is post successful save or adjacent record
------------------------------------------------------------------------------*/
bSetToolbar = YES.
IF SEARCH(cPrepSaveFile) NE ? THEN DO:
  RUN RichEditorOpenFile(hEditor,cPrepSaveFile,OUTPUT iSuccess).
  DYNAMIC-FUNCTION("setDocLoadParam","replacedoc").
  DYNAMIC-FUNCTION("LoadDocs",cPrepSaveFile,icContext,icEntityId,icDesc).
  OS-DELETE VALUE(cPrepSaveFile).
  RETURN YES.
END.

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteRtfTextDoc C-Win 
FUNCTION DeleteRtfTextDoc RETURNS LOGICAL
  ( INPUT icContext         AS CHAR,
    INPUT icEntityId        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:    Save the rtf content as blob document 
  Parameters: Context and identificator for document
  Notes:      Requires JBoxDocument tables and features
------------------------------------------------------------------------------*/
RETURN DYNAMIC-FUNCTION("runProc","jbdoc_delete_entity_doc.p",
                        icContext + "|" + icEntityId,?).

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

RETURN FRAME rtf-edit-frame:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPlainText C-Win 
FUNCTION getPlainText RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPlainText AS CHAR NO-UNDO.
DEF VAR cDummy     AS CHAR NO-UNDO.
/*First call determines the size.*/
RUN RichEditorGetPlainText(hEditor,OUTPUT cDummy,0,OUTPUT iSuccess).

/* DO ix = 1 TO 30000 * TRUNCATE(iSuccess / 30000,0) BY 30000: */
/*   cPlainText = cPlainText + FILL(" ",30000).                */
/* END.                                                        */
/* cPlainText = cPlainText + FILL(" ",iSuccess - ix + 1).      */

cPlainText = FILL(" ",MIN(iSuccess,30000)).


/*Second retrieves the data.*/
RUN RichEditorGetPlainText(hEditor,OUTPUT cPlainText,iSuccess,OUTPUT iSuccess).

RETURN cPlainText.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRichText C-Win 
FUNCTION getRichText RETURNS LONGCHAR
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRichText AS LONGCHAR NO-UNDO.
DEF VAR cDummy    AS CHAR NO-UNDO.
/*First call determines the size.*/
RUN RichEditorGetRichText(hEditor,OUTPUT cDummy,0,OUTPUT iSuccess).

DO ix = 1 TO 30000 * TRUNCATE(iSuccess / 30000,0) BY 30000:
  cRichText = cRichText + FILL(" ",30000).
END.
cRichText = cRichText + FILL(" ",iSuccess - ix + 1).

/*Second retrieves the data.*/
RUN RichEditorGetRichText(hEditor,OUTPUT cRichText,iSuccess,OUTPUT iSuccess).

RETURN cRichText.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitRtfEditor C-Win 
FUNCTION InitRtfEditor RETURNS HANDLE
  ( INPUT ihEditRect AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hParent) THEN hParent = SOURCE-PROCEDURE.

DO WITH FRAME rtf-edit-frame:
  ASSIGN FRAME rtf-edit-frame:X = ihEditRect:X 
         FRAME rtf-edit-frame:Y = ihEditRect:Y 
         FRAME rtf-edit-frame:HEIGHT-PIXELS = ihEditRect:HEIGHT-PIXELS
         FRAME rtf-edit-frame:WIDTH-PIXELS  = ihEditRect:WIDTH-PIXELS
         .

/*   InitTree().  */
END.

RETURN FRAME rtf-edit-frame:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrepareSaveRtfDoc C-Win 
FUNCTION PrepareSaveRtfDoc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cPrepSaveFile = REPLACE(DYNAMIC-FUNCTION("getUniqueFileName"),".tmp",".rtf").

RUN RichEditorSaveFile(hEditor,cPrepSaveFile,OUTPUT iSuccess).  

RETURN getPlainText().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveRtfTextDoc C-Win 
FUNCTION SaveRtfTextDoc RETURNS LOGICAL
  ( INPUT icContext         AS CHAR,
    INPUT icEntityId        AS CHAR,
    INPUT icDesc            AS CHAR,
    INPUT ibSavePlainAsDesc AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:    Save the rtf content as blob document 
  Parameters: Context and identificator for document
  Notes:      Requires JBoxDocument tables and features
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

cFileName = SESSION:TEMP-DIRECTORY + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_"
          + DYNAMIC-FUNCTION("getASuserId") + "_" + STRING(TIME) + ".rtf".

RUN RichEditorSaveFile(hEditor,cFileName,OUTPUT iSuccess).  

IF iSuccess = 0 THEN DO:
  IF ibSavePlainAsDesc THEN
    icDesc = getPlainText().
  DYNAMIC-FUNCTION("setDocLoadParam","replacedoc").
  DYNAMIC-FUNCTION("LoadDocs",cFileName,icContext,icEntityId,icDesc).
END.

OS-DELETE VALUE(cFileName).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveToFile C-Win 
FUNCTION SaveToFile RETURNS LOGICAL
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN RichEditorSaveFile(hEditor, icFileName,OUTPUT iSuccess).  
  
RETURN iSuccess = 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setColorList C-Win 
FUNCTION setColorList RETURNS LOGICAL
  ( INPUT icColorList    AS CHAR,
    INPUT icColorNumList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Invoke before InitializeObject
------------------------------------------------------------------------------*/
ASSIGN cColorList    = icColorList
       cColorNumList = icColorNumList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFontList C-Win 
FUNCTION setFontList RETURNS LOGICAL
  ( INPUT icFontList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Invoke before InitializeObject
------------------------------------------------------------------------------*/
cFontList = icFontList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFontSizeList C-Win 
FUNCTION setFontSizeList RETURNS LOGICAL
  ( INPUT icFontSizeList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Invoke before InitializeObject
------------------------------------------------------------------------------*/
cFontSizeList = icFontSizeList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setHelpText C-Win 
FUNCTION setHelpText RETURNS LOGICAL
  ( INPUT icHelpText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cHelpText = icHelpText.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTabFromWidget C-Win 
FUNCTION setTabFromWidget RETURNS LOGICAL
  ( INPUT ihTabFromWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hTabFromWidget = ihTabFromWidget.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTBmodified C-Win 
FUNCTION setTBmodified RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF bSetToolbar AND VALID-HANDLE(hToolbar) THEN DO:
  DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
  bSetToolbar = NO.
  RETURN YES.
END.
  
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setToolbarHandle C-Win 
FUNCTION setToolbarHandle RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hToolbar = ihToolbar.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewRtfTextDoc C-Win 
FUNCTION ViewRtfTextDoc RETURNS LOGICAL
  ( INPUT icContext  AS CHAR,
    INPUT icEntityId AS CHAR,
    INPUT icText     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Download document from database and view in rtf editor 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName  AS CHAR NO-UNDO.
DEF VAR bBlankLoad AS LOG  NO-UNDO.
DEF VAR bResetFont AS LOG  NO-UNDO.

IF SEARCH(cPrepSaveFile) NE ? THEN RETURN NO.

IF icText NE "" AND DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxDocRel",
                                     "WHERE cContext = '" + icContext + "' AND cEntityId = '" + icEntityId + "'"
                                   ,"iJBoxDocumentId") NE ? THEN DO:

  DYNAMIC-FUNCTION("ViewDocs",icContext,icEntityId,NO,"").
  
  cFileName = RIGHT-TRIM(DYNAMIC-FUNCTION("getTmpDocFileNames"),".gz").
END.

IF cFileName = "" THEN DO:
  ASSIGN cFileName  = DYNAMIC-FUNCTION("getUniqueFileName")
         bResetFont = YES.
  IF icText = "" THEN 
    ASSIGN icText     = " "
           bBlankLoad = YES.

  OUTPUT STREAM strRtfLoad TO VALUE(cFileName).
  PUT STREAM strRtfLoad UNFORMATTED icText.
  OUTPUT STREAM strRtfLoad CLOSE.
END.

bSetToolbar = YES.

IF SEARCH(cFileName) NE ? THEN DO:
  RUN RichEditorOpenFile(hEditor,cFileName,OUTPUT iSuccess).
  IF bBlankLoad THEN DO:
    OUTPUT STREAM strRtfLoad TO VALUE(cFileName).
    PUT STREAM strRtfLoad UNFORMATTED "".
    OUTPUT STREAM strRtfLoad CLOSE.
    RUN RichEditorOpenFile(hEditor,cFileName,OUTPUT iSuccess).
  END.
  IF bResetFont THEN
    RUN ResetFontAndStyleRecord.

  OS-DELETE VALUE(cFileName).
  RETURN YES.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

