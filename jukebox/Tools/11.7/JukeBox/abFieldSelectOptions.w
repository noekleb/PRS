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

DEF VAR bJukeBox AS LOG NO-UNDO.
DEF VAR ix       AS INT NO-UNDO.
DEF VAR cCalcFld AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCreateJboxCalcFld rsDbFieldPrefix ~
cmbFieldMap fiFieldPrefix tbRemoveDuplicates rsFieldHelp tbStandardPrefix ~
tbStandardHelp cmbWidgetType cmbApplyToQry tbClipBoard rsListFormat ~
fiOtherDelimiter tbStandardList Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS rsDbFieldPrefix cmbFieldMap fiFieldPrefix ~
tbRemoveDuplicates rsFieldHelp tbStandardPrefix tbStandardHelp ~
cmbWidgetType cmbApplyToQry tbClipBoard rsListFormat fiOtherDelimiter ~
tbStandardList 

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
DEFINE BUTTON btnCreateJboxCalcFld  NO-FOCUS FLAT-BUTTON
     LABEL "Learn more about Jukebox.." 
     SIZE 29 BY .91.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cmbApplyToQry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assign qry or brw" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE cmbFieldMap AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE cmbWidgetType AS CHARACTER FORMAT "X(256)":U INITIAL "FILL-IN" 
     LABEL "View-as" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "COMBO-BOX List-items","COMBO-BOX List-item-pairs","EDITOR","RADIO-SET","TOGGLE-BOX","FILL-IN","FILL-IN TEXT" 
     DROP-DOWN-LIST
     SIZE 44 BY 1 TOOLTIP "Use cut (ctrl-x) when converting fields" NO-UNDO.

DEFINE VARIABLE fiFieldPrefix AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prefix" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiOtherDelimiter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE rsDbFieldPrefix AS CHARACTER INITIAL "noprefix" 
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
"Other", "other",
"JukeBox buffers and fields def", "jukebox",
"JukeBox calc.field definition", "CalcFld"
     SIZE 37.2 BY 5.29 NO-UNDO.

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
     btnCreateJboxCalcFld AT ROW 19.1 COL 49
     rsDbFieldPrefix AT ROW 1.48 COL 3 NO-LABEL
     cmbFieldMap AT ROW 1.48 COL 21.8 COLON-ALIGNED WIDGET-ID 6
     fiFieldPrefix AT ROW 2.71 COL 46 COLON-ALIGNED
     tbRemoveDuplicates AT ROW 2.76 COL 5.2
     rsFieldHelp AT ROW 3.76 COL 10.8 NO-LABEL
     tbStandardPrefix AT ROW 3.91 COL 48
     tbStandardHelp AT ROW 9.86 COL 23.6
     cmbWidgetType AT ROW 11 COL 21.8 COLON-ALIGNED WIDGET-ID 10
     cmbApplyToQry AT ROW 12.1 COL 21.8 COLON-ALIGNED WIDGET-ID 4
     tbClipBoard AT ROW 13.57 COL 5.2
     rsListFormat AT ROW 14.52 COL 10.8 NO-LABEL
     fiOtherDelimiter AT ROW 16.62 COL 20 COLON-ALIGNED NO-LABEL
     tbStandardList AT ROW 20.38 COL 23.2
     Btn_OK AT ROW 21.48 COL 48
     Btn_Cancel AT ROW 21.48 COL 64
     SPACE(1.00) SKIP(0.28)
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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

ASSIGN 
       cmbWidgetType:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "FILL-IN".

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


&Scoped-define SELF-NAME btnCreateJboxCalcFld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCreateJboxCalcFld Dialog-Frame
ON CHOOSE OF btnCreateJboxCalcFld IN FRAME Dialog-Frame /* Learn more about Jukebox.. */
DO:
  OS-COMMAND NO-WAIT VALUE("start www.chemistry.no"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR bForgot AS LOG NO-UNDO INIT YES.
  IF (bJukeBox OR rsListFormat:SCREEN-VALUE = "JBoxFieldMap") AND (cmbApplyToQry:SCREEN-VALUE = ? OR cmbApplyToQry:LIST-ITEMS = "") THEN DO:
    MESSAGE "Did you forget to assign the browse or query?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bForgot.
    IF bForgot THEN RETURN NO-APPLY.
  END.
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

  DYNAMIC-FUNCTION("setCreFieldViewAs",IF NOT cmbWidgetType:HIDDEN THEN cmbWidgetType:SCREEN-VALUE ELSE "").

  IF bJukeBox OR rsListFormat:SCREEN-VALUE = "JBoxFieldMap" THEN 
    DYNAMIC-FUNCTION("setFieldMapNameAndQry",cmbFieldMap:SCREEN-VALUE,
                     IF cmbApplyToQry:SCREEN-VALUE NE ? THEN cmbApplyToQry:SCREEN-VALUE ELSE "").

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
    IF rsDbFieldPrefix:SCREEN-VALUE = "clipboard" THEN DO:
      IF rsListFormat NE "CalcFld" THEN
        DYNAMIC-FUNCTION("creFillInDefFromDb",icFields).
      ELSE CLIPBOARD:VALUE = cCalcFld.
    END.
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
              btnCreateJboxCalcFld:HIDDEN = YES
              tbStandardList:HIDDEN     = YES.

  IF NOT tbStandardList:HIDDEN THEN
    APPLY "value-changed" TO tbStandardList.
  ELSE ASSIGN btnCreateJboxCalcFld:HIDDEN = YES
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
/*          btnCreateJboxCalcFld:HIDDEN = rsListFormat NE "JukeBox" */
         .
  IF rsListFormat = "CalcFld" THEN DO:
    IF NUM-ENTRIES(icFields) = 1 THEN
      RUN createCalcFieldDef.
    ELSE
      MESSAGE "Calc.field definition cannot be created for multiple fields"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbClipBoard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbClipBoard Dialog-Frame
ON VALUE-CHANGED OF tbClipBoard IN FRAME Dialog-Frame /* Clipboard */
DO:
  ASSIGN rsListFormat:HIDDEN         = NOT tbClipboard:CHECKED
         tbStandardList:HIDDEN       = rsListFormat:HIDDEN
         btnCreateJboxCalcFld:HIDDEN = NUM-ENTRIES(icFields) > 1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createCalcFieldDef Dialog-Frame 
PROCEDURE createCalcFieldDef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cTable      AS CHAR NO-UNDO.
  DEF VAR cField      AS CHAR NO-UNDO.
  DEF VAR cCalcParam  AS CHAR NO-UNDO.
  DEF VAR cDataType   AS CHAR NO-UNDO.
  DEF VAR cFormat     AS CHAR NO-UNDO.
  DEF VAR cLabel      AS CHAR NO-UNDO.
  DEF VAR cCalcCont   AS CHAR NO-UNDO.
  DEF VAR cCalcProc   AS CHAR NO-UNDO.
  DEF VAR bOk         AS LOG  NO-UNDO.

  ASSIGN cTable = ENTRY(1,icFields,".")
         cField = ENTRY(2,icFields,".")
         .

  RUN jukebox\defineCalcField.w (cTable
                        ,INPUT-OUTPUT cTable
                        ,INPUT-OUTPUT cCalcParam
                        ,INPUT-OUTPUT cField 
                        ,INPUT-OUTPUT cDataType
                        ,INPUT-OUTPUT cFormat
                        ,INPUT-OUTPUT cLabel
                        ,INPUT-OUTPUT cCalcCont
                        ,INPUT-OUTPUT cCalcProc
                        ,OUTPUT bOK).

  cCalcFld = "+ ';+" + cField + "|" + cDataType + "|" + REPLACE(cFormat,",","<") + cCalcCont + "|" + cCalcProc + cCalcParam + "|" + cLabel + "'".

  IF cCalcCont NE "" THEN
    MESSAGE "Remember to add" cCalcCont SKIP
            "to the query/browse object using the calcFieldProc property"
            VIEW-AS ALERT-BOX.

/*   MESSAGE cTable SKIP cCalcParam SKIP cField SKIP cDataType SKIP cFormat SKIP cLabel SKIP cCalcCont SKIP cCalcProc SKIP(1) */
/*           cCalcCont + "|" + cCalcProc + cCalcParam SKIP(1)                                                                 */
/*           cCalcFld                                                                                                         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY rsDbFieldPrefix cmbFieldMap fiFieldPrefix tbRemoveDuplicates 
          rsFieldHelp tbStandardPrefix tbStandardHelp cmbWidgetType 
          cmbApplyToQry tbClipBoard rsListFormat fiOtherDelimiter tbStandardList 
      WITH FRAME Dialog-Frame.
  ENABLE btnCreateJboxCalcFld rsDbFieldPrefix cmbFieldMap fiFieldPrefix 
         tbRemoveDuplicates rsFieldHelp tbStandardPrefix tbStandardHelp 
         cmbWidgetType cmbApplyToQry tbClipBoard rsListFormat fiOtherDelimiter 
         tbStandardList Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
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
DEF VAR cFirstTableList AS CHAR NO-UNDO.
DEF VAR cFmNameList     AS CHAR NO-UNDO.
DEF VAR iReturn         AS INT  NO-UNDO.

DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
IF cDefaultFrameFont NE "" THEN
  FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  bJukeBox = DYNAMIC-FUNCTION("getNewJukeBoxObject") NO-ERROR.
  IF ERROR-STATUS:ERROR OR DYNAMIC-FUNCTION("getIsNotJukeBoxObject") THEN bJukeBox = NO.

  IF bJukeBox THEN DO:
    APPLY "value-changed" TO rsDbFieldPrefix.

    ASSIGN cmbApplyToQry:LIST-ITEMS  = DYNAMIC-FUNCTION("getQryObjectsInUse")
           rsDbFieldPrefix:HIDDEN    = YES
           cFirstTableList           = DYNAMIC-FUNCTION("getFirstTableList")
           FRAME {&FRAME-NAME}:TITLE = "Create FieldMap"
           .
    DO ix = 1 TO NUM-ENTRIES(cFirstTableList):
      cFmNameList = "oFm" + ENTRY(ix,cFirstTableList).
    END.
    IF cFmNameList = "" THEN DO:
      RUN JBoxAskForValue.w ("FieldMap name","CHARACTER|x(20)|oFm",INPUT-OUTPUT cFmNameList,OUTPUT iReturn).
      IF iReturn NE 2 OR cFmNameList = "" THEN cFmNameList = "oFm1". 
    END.
    IF cFmNameList NE "" THEN DO:
      cmbFieldMap:LIST-ITEMS = cFmNameList.
      cmbFieldMap:SCREEN-VALUE = cmbFieldMap:ENTRY(1).
    END. 
    ELSE DO:
      MESSAGE "Invalid FieldMap name"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.  
      APPLY "window-close" TO FRAME {&FRAME-NAME}.
      RETURN.
    END.
  END.
  ELSE DO:
    ASSIGN cmbFieldMap:HIDDEN = YES
           cmbApplyToQry:HIDDEN = YES
           .  
    APPLY "value-changed" TO rsDbFieldPrefix.
  END.

  cmbWidgetType:HIDDEN = NUM-ENTRIES(icFields) > 1.  

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

