&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEF INPUT  PARAM ihBrowse AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.

DEF VAR cSortMap          AS CHAR NO-UNDO.
DEF VAR cLabelList        AS CHAR NO-UNDO.
DEF VAR cSortedLabelList  AS CHAR NO-UNDO.
DEF VAR cCalcFields       AS CHAR NO-UNDO.

DEF TEMP-TABLE ttSort
    FIELD cColumn   AS CHAR 
    FIELD cDbColumn AS CHAR 
    FIELD cLabel    AS CHAR
    FIELD iSelected AS INT
    FIELD iIndex    AS INT
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-68 RECT-69 RECT-70 RECT-71 rs1stAsc ~
cmb1stColumn rs2ndAsc cmb2ndColumn rs3rdAsc cmb3rdColumn rs4thAsc ~
cmb4thColumn tbAlfaSort Btn_OK Btn_Cancel fiSortBy1label fiSortBy2label ~
fiSortBy3label fiSortBy4label 
&Scoped-Define DISPLAYED-OBJECTS rs1stAsc cmb1stColumn rs2ndAsc ~
cmb2ndColumn rs3rdAsc cmb3rdColumn rs4thAsc cmb4thColumn tbAlfaSort ~
fiSortBy1label fiSortBy2label fiSortBy3label fiSortBy4label 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD realDbSortField Dialog-Frame 
FUNCTION realDbSortField RETURNS CHARACTER
  ( INPUT icColumnLabel AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD realSortField Dialog-Frame 
FUNCTION realSortField RETURNS CHARACTER
  ( INPUT icColumnLabel AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setListItems Dialog-Frame 
FUNCTION setListItems RETURNS LOGICAL
    (INPUT iiField AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sortField Dialog-Frame 
FUNCTION sortField RETURNS CHARACTER
  ( INPUT icSortColumn AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cmb1stColumn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE cmb2ndColumn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE cmb3rdColumn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE cmb4thColumn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE fiSortBy1label AS CHARACTER FORMAT "X(256)":U INITIAL "Sorter på:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fiSortBy2label AS CHARACTER FORMAT "X(256)":U INITIAL "deretter på:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE fiSortBy3label AS CHARACTER FORMAT "X(256)":U INITIAL "deretter på:" 
      VIEW-AS TEXT 
     SIZE 11.2 BY .62 NO-UNDO.

DEFINE VARIABLE fiSortBy4label AS CHARACTER FORMAT "X(256)":U INITIAL "deretter på:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE rs1stAsc AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Stigende", yes,
"Synkende", no
     SIZE 16 BY 1.95 NO-UNDO.

DEFINE VARIABLE rs2ndAsc AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Stigende", yes,
"Synkende", no
     SIZE 16 BY 1.95 NO-UNDO.

DEFINE VARIABLE rs3rdAsc AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Stigende", yes,
"Synkende", no
     SIZE 16 BY 1.95 NO-UNDO.

DEFINE VARIABLE rs4thAsc AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Stigende", yes,
"Synkende", no
     SIZE 16 BY 1.95 NO-UNDO.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.48.

DEFINE RECTANGLE RECT-69
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.48.

DEFINE RECTANGLE RECT-70
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.48.

DEFINE RECTANGLE RECT-71
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.48.

DEFINE VARIABLE tbAlfaSort AS LOGICAL INITIAL no 
     LABEL "Vis sorteringskolonner alfabetisk" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rs1stAsc AT ROW 1.81 COL 42 NO-LABEL
     cmb1stColumn AT ROW 2.29 COL 1.6 COLON-ALIGNED NO-LABEL
     rs2ndAsc AT ROW 4.19 COL 42 NO-LABEL
     cmb2ndColumn AT ROW 4.57 COL 1.6 COLON-ALIGNED NO-LABEL
     rs3rdAsc AT ROW 6.57 COL 42 NO-LABEL
     cmb3rdColumn AT ROW 7 COL 1.6 COLON-ALIGNED NO-LABEL
     rs4thAsc AT ROW 8.95 COL 42 NO-LABEL
     cmb4thColumn AT ROW 9.38 COL 1.6 COLON-ALIGNED NO-LABEL
     tbAlfaSort AT ROW 11.29 COL 1.8
     Btn_OK AT ROW 12.33 COL 28.2
     Btn_Cancel AT ROW 12.33 COL 43.6
     fiSortBy1label AT ROW 1.19 COL 2 COLON-ALIGNED NO-LABEL
     fiSortBy2label AT ROW 3.57 COL 1 COLON-ALIGNED NO-LABEL
     fiSortBy3label AT ROW 5.95 COL 1.2 COLON-ALIGNED NO-LABEL
     fiSortBy4label AT ROW 8.33 COL 1 COLON-ALIGNED NO-LABEL
     RECT-68 AT ROW 1.52 COL 1.4
     RECT-69 AT ROW 3.91 COL 1.4
     RECT-70 AT ROW 8.67 COL 1.4
     RECT-71 AT ROW 6.29 COL 1.4
     SPACE(0.19) SKIP(4.93)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg sortering"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg sortering */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN rs2ndAsc rs3rdAsc rs1stAsc rs4thAsc.

  IF cmb1stColumn:SCREEN-VALUE NE ? AND cmb1stColumn:SCREEN-VALUE NE "0" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stSortColumn",realSortField(cmb1stColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stDbSortColumn",realDbSortField(cmb1stColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stSortColumnDesc",IF rs1stAsc THEN "" ELSE "desc").
    obOk  = TRUE.
  END.
  ELSE DO: 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stDbSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stSortColumnDesc","").
  END.

  IF cmb2ndColumn:SCREEN-VALUE NE ? AND cmb2ndColumn:SCREEN-VALUE NE "0" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"2ndSortColumn",realSortField(cmb2ndColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"2ndDbSortColumn",realDbSortField(cmb2ndColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"2ndSortColumnDesc",IF rs2ndAsc THEN "" ELSE "desc").
    obOk  = TRUE.
  END.
  ELSE DO: 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"2ndSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"2ndDbSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"2ndSortColumnDesc","").
  END.

  IF cmb3rdColumn:SCREEN-VALUE NE ? AND cmb3rdColumn:SCREEN-VALUE NE "0" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"3rdSortColumn",realSortField(cmb3rdColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"3rdDbSortColumn",realDbSortField(cmb3rdColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"3rdSortColumnDesc",IF rs3rdAsc THEN "" ELSE "desc").
    obOk  = TRUE.
  END.
  ELSE DO: 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"3rdSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"3rdDbSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"3rdSortColumnDesc","").
  END.

  IF cmb4thColumn:SCREEN-VALUE NE ? AND cmb4thColumn:SCREEN-VALUE NE "0" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"4thSortColumn",realSortField(cmb4thColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"4thDbSortColumn",realDbSortField(cmb4thColumn:SCREEN-VALUE)).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"4thSortColumnDesc",IF rs4thAsc THEN "" ELSE "desc").
    obOk  = TRUE.
  END.
  ELSE DO: 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"4thSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"4thDbSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"4thSortColumnDesc","").
  END.

  IF tbAlfaSort:CHECKED THEN
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"sortcolumnviewpref","alfa").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"sortcolumnviewpref","").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmb1stColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmb1stColumn Dialog-Frame
ON VALUE-CHANGED OF cmb1stColumn IN FRAME Dialog-Frame
DO:
  setListItems(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmb2ndColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmb2ndColumn Dialog-Frame
ON VALUE-CHANGED OF cmb2ndColumn IN FRAME Dialog-Frame
DO:
  setListItems(3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmb3rdColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmb3rdColumn Dialog-Frame
ON VALUE-CHANGED OF cmb3rdColumn IN FRAME Dialog-Frame
DO:
  setListItems(4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAlfaSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAlfaSort Dialog-Frame
ON VALUE-CHANGED OF tbAlfaSort IN FRAME Dialog-Frame /* Vis sorteringskolonner alfabetisk */
DO:
  IF cmb3rdColumn:SCREEN-VALUE NE ? THEN
    setListItems(4).
  ELSE IF cmb2ndColumn:SCREEN-VALUE NE ? THEN
    setListItems(3).
  ELSE IF cmb1stColumn:SCREEN-VALUE NE ? THEN
    setListItems(2).
  ELSE setListItems(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitializeObject.
  FRAME {&FRAME-NAME}:HIDDEN = FALSE.
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
  DISPLAY rs1stAsc cmb1stColumn rs2ndAsc cmb2ndColumn rs3rdAsc cmb3rdColumn 
          rs4thAsc cmb4thColumn tbAlfaSort fiSortBy1label fiSortBy2label 
          fiSortBy3label fiSortBy4label 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-68 RECT-69 RECT-70 RECT-71 rs1stAsc cmb1stColumn rs2ndAsc 
         cmb2ndColumn rs3rdAsc cmb3rdColumn rs4thAsc cmb4thColumn tbAlfaSort 
         Btn_OK Btn_Cancel fiSortBy1label fiSortBy2label fiSortBy3label 
         fiSortBy4label 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Dialog-Frame 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cNoColumnSortList AS CHAR NO-UNDO.
DEF VAR iy                AS INT NO-UNDO.
DEF VAR cExtentFlds       AS CHAR   NO-UNDO.
DEF VAR cAltExtFlds       AS CHAR   NO-UNDO.
DEF VAR cAltExtentField   AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  
  IF ihBrowse:type = "menu" THEN
    ihBrowse = DYNAMIC-FUNCTION("getLinkedObject",ihBrowse,"browse","from").
  
  ASSIGN cmb1stColumn:DELIMITER = "|"
         cmb2ndColumn:DELIMITER = "|"
         cmb3rdColumn:DELIMITER = "|"
         cmb4thColumn:DELIMITER = "|"
         cNoColumnSortList      = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"nocolumnsort")
         cSortMap               = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"SortMap")
         cCalcFields            = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allcalcfields")
         cExtentFlds            = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"extentFields")
         cAltExtFlds            = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"altExtentFields") 
         .
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE AND NOT CAN-DO(cNoColumnSortList,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN DO:
      cLabelList = cLabelList + DYNAMIC-FUNCTION("getStrippedSortLabel",ihBrowse:GET-BROWSE-COLUMN(ix)) + "|".
      CREATE ttSort.
      ASSIGN ttSort.cLabel    = DYNAMIC-FUNCTION("getStrippedSortLabel",ihBrowse:GET-BROWSE-COLUMN(ix))
             ttSort.cColumn   = ihBrowse:GET-BROWSE-COLUMN(ix):NAME
             ttSort.cDbColumn = (IF NOT CAN-DO(cCalcFields,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN 
                                   DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldbuffer" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) + "."
                                 + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgdbfield" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) 
                                 ELSE ihBrowse:GET-BROWSE-COLUMN(ix):NAME)
             iy               = iy + 1
             ttSort.iIndex    = iy    
             ttSort.iSelected = 999
             .

      IF CAN-DO(cExtentFlds,ttSort.cColumn) THEN DO:
        cAltExtentField =  ENTRY(LOOKUP(ttSort.cColumn,cExtentFlds),cAltExtFlds).
        IF LOOKUP(ttSort.cColumn + ";" + cAltExtentField,cSortMap) = 0 AND SUBSTR(ttSort.cColumn,LENGTH(ttSort.cColumn)) NE "]" THEN DO:
          cSortMap = cSortMap + (IF cSortMap NE "" THEN "," ELSE "") 
                   + ttSort.cColumn + ";" 
                   + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgdbfield"
                               + SUBSTR(ttSort.cColumn,1,R-INDEX(ttSort.cColumn,"_") - 1)
                               + "[" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"extent_" + ttSort.cColumn) + "]")
                  .
          DYNAMIC-FUNCTION("setAttribute",ihBrowse,"sortMap",cSortMap).
        END.
      END.
    END.
  END.
  cSortMap = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortmap").

  ASSIGN cLabelList = SUBSTR(cLabelList,1,LENGTH(cLabelList) - 1)
         cmb1stColumn:LIST-ITEMS = "|" + cLabelList
         cmb2ndColumn:LIST-ITEMS = "|" + cLabelList
         cmb3rdColumn:LIST-ITEMS = "|" + cLabelList
         cmb4thColumn:LIST-ITEMS = "|" + cLabelList
         cmb1stColumn:SCREEN-VALUE    = sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn")) 
         cmb2ndColumn:SCREEN-VALUE    = sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumn"))
         cmb3rdColumn:SCREEN-VALUE    = sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumn"))
         cmb4thColumn:SCREEN-VALUE    = sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumn"))
         rs1stAsc:SCREEN-VALUE        = IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumnDesc") = "desc" THEN STRING(NO) ELSE STRING(YES)
         rs2ndAsc:SCREEN-VALUE        = IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumnDesc") = "desc" THEN STRING(NO) ELSE STRING(YES)
         rs3rdAsc:SCREEN-VALUE        = IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumnDesc") = "desc" THEN STRING(NO) ELSE STRING(YES)
         rs4thAsc:SCREEN-VALUE        = IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumnDesc") = "desc" THEN STRING(NO) ELSE STRING(YES)
         .

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortcolumnviewpref") = "alfa" THEN tbAlfaSort:CHECKED = TRUE.

  setListItems(IF sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumn")) NE "" THEN 4
               ELSE IF sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumn")) NE "" THEN 3
               ELSE IF sortField(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumn")) NE "" THEN 2
               ELSE 1).

  LocalTranslation().
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cScreenValues AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE DO:
    ASSIGN cScreenValues               = rs1stAsc:SCREEN-VALUE + "," + rs2ndAsc:SCREEN-VALUE + "," + rs3rdAsc:SCREEN-VALUE + "," + rs4thAsc:SCREEN-VALUE
           Btn_Cancel:LABEL            = "Cancel" 
           fiSortBy1label:SCREEN-VALUE = "Sort by" 
           fiSortBy2label:SCREEN-VALUE = "Then by" 
           fiSortBy3label:SCREEN-VALUE = "Then by" 
           fiSortBy4label:SCREEN-VALUE = "Then by" 
           rs1stAsc:RADIO-BUTTONS      = "Ascending,yes,Descending,no"       
           rs2ndAsc:RADIO-BUTTONS      = "Ascending,yes,Descending,no"       
           rs3rdAsc:RADIO-BUTTONS      = "Ascending,yes,Descending,no"       
           rs4thAsc:RADIO-BUTTONS      = "Ascending,yes,Descending,no"       
           tbAlfaSort:LABEL            = "Columns in alphabetic order"
           rs1stAsc:SCREEN-VALUE       = ENTRY(1,cScreenValues)
           rs2ndAsc:SCREEN-VALUE       = ENTRY(2,cScreenValues)       
           rs3rdAsc:SCREEN-VALUE       = ENTRY(3,cScreenValues)       
           rs4thAsc:SCREEN-VALUE       = ENTRY(4,cScreenValues)       
           FRAME Dialog-Frame:TITLE    = "Sort"
           .
  END.
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION realDbSortField Dialog-Frame 
FUNCTION realDbSortField RETURNS CHARACTER
  ( INPUT icColumnLabel AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttSort
     WHERE ttSort.cLabel = icColumnLabel.

DO ix = 1 TO NUM-ENTRIES(cSortMap):
  IF ENTRY(1,ENTRY(ix,cSortMap),";") = ttSort.cColumn THEN
    RETURN ENTRY(2,ENTRY(ix,cSortMap),";").
END.

RETURN ttSort.cDbColumn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION realSortField Dialog-Frame 
FUNCTION realSortField RETURNS CHARACTER
  ( INPUT icColumnLabel AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttSort
     WHERE ttSort.cLabel = icColumnLabel.

DO ix = 1 TO NUM-ENTRIES(cSortMap):
  IF ENTRY(1,ENTRY(ix,cSortMap),";") = ttSort.cColumn THEN
    RETURN ENTRY(2,ENTRY(ix,cSortMap),";").
END.

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"useLocalData") = "yes" AND SUBSTR(ttSort.cColumn,LENGTH(ttSort.cColumn)) = "]" THEN
  RETURN "jbextent_" + RIGHT-TRIM(SUBSTR(ttSort.cColumn,R-INDEX(ttSort.cColumn,"[") + 1),"]") + "_" + SUBSTR(ttSort.cColumn,1,R-INDEX(ttSort.cColumn,"[") - 1). 

RETURN ttSort.cColumn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setListItems Dialog-Frame 
FUNCTION setListItems RETURNS LOGICAL
    (INPUT iiField AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: (Temporary) out of order. All fields are available for all drop-downs 
------------------------------------------------------------------------------*/
DEF VAR cScreenValue AS CHAR NO-UNDO EXTENT 4.

DO WITH FRAME {&FRAME-NAME}:

  FOR EACH ttSort:
    ttSort.iSelected = 999.
  END.

  ASSIGN cScreenValue[1] = cmb1stColumn:SCREEN-VALUE
         cScreenValue[2] = cmb2ndColumn:SCREEN-VALUE
         cScreenValue[3] = cmb3rdColumn:SCREEN-VALUE
         cScreenValue[4] = cmb4thColumn:SCREEN-VALUE
         .

  DO ix = 1 TO iiField:
    FIND FIRST ttSort WHERE ttSort.cLabel = cScreenValue[ix] NO-ERROR.
    IF AVAIL ttSort THEN ttSort.iSelected = ix.

    cLabelList = "|".
    IF tbAlfaSort:CHECKED THEN
      FOR EACH ttSort BY ttSort.cLabel:         
        IF ttSort.iSelected < ix THEN NEXT.
        ELSE cLabelList = cLabelList + ttSort.cLabel + "|".
      END.
    ELSE
      FOR EACH ttSort BY ttSort.iIndex:
        IF ttSort.iSelected < ix THEN NEXT.
        ELSE cLabelList = cLabelList + ttSort.cLabel + "|".
      END.
    CASE ix:
      WHEN 1 THEN 
        ASSIGN cmb1stColumn:LIST-ITEMS   = TRIM(cLabelList,"|")
               cmb1stColumn:SCREEN-VALUE = cScreenValue[ix].
      WHEN 2 THEN 
        ASSIGN cmb2ndColumn:LIST-ITEMS   = TRIM(cLabelList,"|")
               cmb2ndColumn:SCREEN-VALUE = cScreenValue[ix].
      WHEN 3 THEN 
        ASSIGN cmb3rdColumn:LIST-ITEMS   = TRIM(cLabelList,"|")
               cmb3rdColumn:SCREEN-VALUE = cScreenValue[ix].
      WHEN 4 THEN 
        ASSIGN cmb4thColumn:LIST-ITEMS   = TRIM(cLabelList,"|")
               cmb4thColumn:SCREEN-VALUE = cScreenValue[ix].
    END CASE.
  END.
  cmb2ndColumn:ADD-FIRST(" ").
  cmb3rdColumn:ADD-FIRST(" ").
  cmb4thColumn:ADD-FIRST(" ").
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sortField Dialog-Frame 
FUNCTION sortField RETURNS CHARACTER
  ( INPUT icSortColumn AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(cSortMap):
  IF ENTRY(2,ENTRY(ix,cSortMap),";") = icSortColumn THEN
    RETURN ENTRY(1,ENTRY(ix,cSortMap),";").
END.

IF icSortColumn BEGINS "jbextent_" THEN
  icSortColumn = SUBSTR(icSortColumn,INDEX(icSortColumn,"_",10) + 1) + "[" + SUBSTR(icSortColumn,INDEX(icSortColumn,"_") + 1,INDEX(icSortColumn,"_",10) - INDEX(icSortColumn,"_") - 1) + "]".

FIND FIRST ttSort
     WHERE ttSort.cColumn = icSortColumn NO-ERROR.
IF AVAIL ttSort THEN
  RETURN ttSort.cLabel.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

