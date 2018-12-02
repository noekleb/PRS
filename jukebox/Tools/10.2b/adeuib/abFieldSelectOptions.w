&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocUsage     AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnNavigateJukeBox rsDbFieldPrefix ~
fiFieldPrefix tbRemoveDuplicates rsFieldHelp tbStandardPrefix ~
tbStandardHelp tbClipBoard rsListFormat fiOtherDelimiter tbStandardList ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS rsDbFieldPrefix fiFieldPrefix ~
tbRemoveDuplicates rsFieldHelp tbStandardPrefix tbStandardHelp tbClipBoard ~
rsListFormat fiOtherDelimiter tbStandardList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadDefaults Dialog-Frame 
FUNCTION LoadDefaults RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveDefault Dialog-Frame 
FUNCTION SaveDefault RETURNS LOGICAL
  ( INPUT icKey   AS CHAR,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnNavigateJukeBox  NO-FOCUS FLAT-BUTTON
     LABEL "Find out about JukeBox" 
     SIZE 29 BY .91.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiFieldPrefix AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prefix" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiOtherDelimiter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE rsDbFieldPrefix AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "No <db.table> prefix", "noprefix",
"AppBuilder default", "abdefault",
"My prefix", "myprefix",
"Clipboard only", "clipboard"
     SIZE 77 BY .95 NO-UNDO.

DEFINE VARIABLE rsFieldHelp AS CHARACTER INITIAL "fh_t" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Use help text as tooltip", "fh_t",
"Use help text as help", "fh_h",
"Use help text as both", "fh_ht",
"Use help text as help and field description as tooltip", "fh_h-de_t",
"Don't use 'em (they are rubbish)", "none"
     SIZE 53 BY 5.95 NO-UNDO.

DEFINE VARIABLE rsListFormat AS CHARACTER INITIAL "comma" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Comma delimited list", "comma",
"Space delimited", "space",
"Semicolon", "semicolon",
"Other", "other",
"JukeBox browse/query definition", "jukebox"
     SIZE 35.4 BY 5.95 NO-UNDO.

DEFINE VARIABLE tbClipBoard AS LOGICAL INITIAL no 
     LABEL "Clipboard" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbRemoveDuplicates AS LOGICAL INITIAL no 
     LABEL "Remove duplicates" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tbStandardHelp AS LOGICAL INITIAL no 
     LABEL "Use as default for help and tooltip" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbStandardList AS LOGICAL INITIAL no 
     LABEL "Use as default for clipboard format" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbStandardPrefix AS LOGICAL INITIAL no 
     LABEL "Use as default" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnNavigateJukeBox AT ROW 16.57 COL 47.2
     rsDbFieldPrefix AT ROW 1.81 COL 2.6 NO-LABEL
     fiFieldPrefix AT ROW 2.71 COL 46 COLON-ALIGNED
     tbRemoveDuplicates AT ROW 2.76 COL 5.2
     rsFieldHelp AT ROW 3.76 COL 10.8 NO-LABEL
     tbStandardPrefix AT ROW 3.91 COL 48
     tbStandardHelp AT ROW 9.86 COL 23.6
     tbClipBoard AT ROW 10.67 COL 5.2
     rsListFormat AT ROW 11.62 COL 10.8 NO-LABEL
     fiOtherDelimiter AT ROW 15.24 COL 19.8 COLON-ALIGNED NO-LABEL
     tbStandardList AT ROW 17.81 COL 23.2
     Btn_OK AT ROW 19.1 COL 46
     Btn_Cancel AT ROW 19.1 COL 62
     SPACE(2.99) SKIP(0.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Field Select Options"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Field Select Options */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNavigateJukeBox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNavigateJukeBox Dialog-Frame
ON CHOOSE OF btnNavigateJukeBox IN FRAME Dialog-Frame /* Find out about JukeBox */
DO:
  OS-COMMAND NO-WAIT VALUE("start www.chemistry.no").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ocUsage = rsDbFieldPrefix:SCREEN-VALUE.
  IF CAN-DO("noprefix,myprefix",rsDbFieldPrefix:SCREEN-VALUE) THEN DO:
    DYNAMIC-FUNCTION("setFieldSelectFieldHelp",rsFieldHelp:SCREEN-VALUE).
    IF tbStandardHelp:CHECKED THEN
      SaveDefault("rsFieldHelp",rsFieldHelp:SCREEN-VALUE).

    IF rsDbFieldPrefix:SCREEN-VALUE = "myprefix" THEN DO:
      DYNAMIC-FUNCTION("setFieldSelectFieldPrefix",fiFieldPrefix:SCREEN-VALUE).
      IF tbStandardPrefix:CHECKED THEN           
        SaveDefault("fiFieldPrefix",fiFieldPrefix:SCREEN-VALUE).
    END.

    DYNAMIC-FUNCTION("setFieldSelectRemoveDuplicates",tbRemoveDuplicates:CHECKED).
  END.
  ELSE IF rsDbFieldPrefix:SCREEN-VALUE = "clipboard" THEN ocUsage = "".

  IF NOT tbClipboard:HIDDEN AND tbClipboard:CHECKED THEN DO:
    IF NOT tbStandardList:HIDDEN AND tbStandardList:CHECKED THEN DO:         
      SaveDefault("rsListFormat",rsListFormat:SCREEN-VALUE).
      IF rsListFormat:SCREEN-VALUE = "other" THEN
        SaveDefault("fiOtherDelimiter",fiOtherDelimiter:SCREEN-VALUE).
    END.
    DYNAMIC-FUNCTION("setFieldSelectClipBoardFormat",rsListFormat:SCREEN-VALUE).
    IF rsListFormat:SCREEN-VALUE = "other" THEN
      DYNAMIC-FUNCTION("setFieldSelectClipBoardDelimiter",fiOtherDelimiter:SCREEN-VALUE).

    /* Use the same function as called from _drwflds.p to create the code */
    IF rsDbFieldPrefix:SCREEN-VALUE = "clipboard" THEN
      DYNAMIC-FUNCTION("creFillInDefFromDb",icFields).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDbFieldPrefix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDbFieldPrefix Dialog-Frame
ON VALUE-CHANGED OF rsDbFieldPrefix IN FRAME Dialog-Frame
DO:
  ASSIGN rsDbFieldPrefix
         rsFieldHelp:HIDDEN         = CAN-DO("abdefault,clipboard",rsDbFieldPrefix)
         tbRemoveDuplicates:HIDDEN  = CAN-DO("abdefault,clipboard",rsDbFieldPrefix)
         tbStandardHelp:HIDDEN      = rsFieldHelp:HIDDEN
         fiFieldPrefix:HIDDEN       = rsDbFieldPrefix NE "myprefix"
         tbStandardPrefix:HIDDEN    = fiFieldPrefix:HIDDEN 
         tbClipboard:HIDDEN         = rsDbFieldPrefix = "abdefault"
         .

  IF NOT tbClipboard:HIDDEN THEN DO:
    IF rsDbFieldPrefix = "clipboard" THEN
      tbClipboard:CHECKED = YES.
    APPLY "value-changed" TO tbClipboard.
  END.
  ELSE ASSIGN rsListFormat:HIDDEN       = YES
              fiOtherDelimiter:HIDDEN   = YES
              btnNavigateJukeBox:HIDDEN = YES
              tbStandardList:HIDDEN     = YES.

  IF NOT tbStandardList:HIDDEN THEN
    APPLY "value-changed" TO tbStandardList.
  ELSE ASSIGN btnNavigateJukeBox:HIDDEN = YES
              fiOtherDelimiter:HIDDEN   = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsListFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsListFormat Dialog-Frame
ON VALUE-CHANGED OF rsListFormat IN FRAME Dialog-Frame
DO:
  ASSIGN rsListFormat
         fiOtherDelimiter:HIDDEN   = rsListFormat NE "other"
         btnNavigateJukeBox:HIDDEN = rsListFormat NE "JukeBox"
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbClipBoard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbClipBoard Dialog-Frame
ON VALUE-CHANGED OF tbClipBoard IN FRAME Dialog-Frame /* Clipboard */
DO:
  ASSIGN rsListFormat:HIDDEN     = NOT tbClipboard:CHECKED
         tbStandardList:HIDDEN   = rsListFormat:HIDDEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitBox.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY rsDbFieldPrefix fiFieldPrefix tbRemoveDuplicates rsFieldHelp 
          tbStandardPrefix tbStandardHelp tbClipBoard rsListFormat 
          fiOtherDelimiter tbStandardList 
      WITH FRAME Dialog-Frame.
  ENABLE btnNavigateJukeBox rsDbFieldPrefix fiFieldPrefix tbRemoveDuplicates 
         rsFieldHelp tbStandardPrefix tbStandardHelp tbClipBoard rsListFormat 
         fiOtherDelimiter tbStandardList Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitBox Dialog-Frame 
PROCEDURE InitBox :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  APPLY "value-changed" TO rsDbFieldPrefix.
  LoadDefaults().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadDefaults Dialog-Frame 
FUNCTION LoadDefaults RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cValue AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  GET-KEY-VALUE SECTION "abFieldSelectOptions" KEY "rsFieldHelp" VALUE cValue. 
  IF cValue NE ? THEN
    ASSIGN rsFieldHelp:SCREEN-VALUE = cValue
           tbStandardHelp:CHECKED   = YES. 

  GET-KEY-VALUE SECTION "abFieldSelectOptions" KEY "fiFieldPrefix" VALUE cValue. 
  IF cValue NE ? THEN
    ASSIGN fiFieldPrefix:SCREEN-VALUE = cValue
           tbStandardPrefix:CHECKED   = YES. 

  GET-KEY-VALUE SECTION "abFieldSelectOptions" KEY "rsListFormat" VALUE cValue. 
  IF cValue NE ? THEN DO:      
    ASSIGN rsListFormat:SCREEN-VALUE = cValue
           tbStandardList:CHECKED    = YES. 
    IF cValue = "other" THEN DO:        
      GET-KEY-VALUE SECTION "abFieldSelectOptions" KEY "fiOtherDelimiter" VALUE cValue. 
      IF cValue NE ? THEN 
        fiOtherDelimiter:SCREEN-VALUE = cValue.
    END.
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveDefault Dialog-Frame 
FUNCTION SaveDefault RETURNS LOGICAL
  ( INPUT icKey   AS CHAR,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
PUT-KEY-VALUE SECTION "abFieldSelectOptions" 
              KEY  icKey
              VALUE icValue
              .
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

