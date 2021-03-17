&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
/*          This .W file was created with the Progress UIB.             */
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

def stream O.
DEF STREAM I.

DEF VAR stdFont AS CHAR INITIAL "Arial".

DEF VAR backsequence    AS CHAR NO-UNDO.                /* by names */
DEF VAR pRes        AS INT NO-UNDO.

&scop Inc   3

/* { xPrint.i } */

RUN getPrinterRes(OUTPUT pRes).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME bF1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES _File _Field

/* Definitions for BROWSE bF1                                           */
&Scoped-define FIELDS-IN-QUERY-bF1 _File._File-name _File._Desc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bF1   
&Scoped-define SELF-NAME bF1
&Scoped-define QUERY-STRING-bF1 FOR EACH _File where not _hidden no-lock
&Scoped-define OPEN-QUERY-bF1 OPEN QUERY {&SELF-NAME} FOR EACH _File where not _hidden no-lock.
&Scoped-define TABLES-IN-QUERY-bF1 _File
&Scoped-define FIRST-TABLE-IN-QUERY-bF1 _File


/* Definitions for BROWSE bField                                        */
&Scoped-define FIELDS-IN-QUERY-bField _Field-name _Field._Data-Type _Field._Format   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bField   
&Scoped-define SELF-NAME bField
&Scoped-define QUERY-STRING-bField FOR EACH _Field of _file no-lock
&Scoped-define OPEN-QUERY-bField OPEN QUERY {&SELF-NAME} FOR EACH _Field of _file no-lock.
&Scoped-define TABLES-IN-QUERY-bField _Field
&Scoped-define FIRST-TABLE-IN-QUERY-bField _Field


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-bF1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-4 RECT-5 RECT-6 bF1 bField ~
SelectBack rOrder topBorder EDITOR-1 rightBorder leftBorder bOne ~
bottomBorder bFull 
&Scoped-Define DISPLAYED-OBJECTS SelectBack rOrder topBorder EDITOR-1 ~
rightBorder leftBorder bottomBorder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD autoBottomBorder C-Win 
FUNCTION autoBottomBorder RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD autoLeftBorder C-Win 
FUNCTION autoLeftBorder RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD autoRightBorder C-Win 
FUNCTION autoRightBorder RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD autoTopBorder C-Win 
FUNCTION autoTopBorder RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD drawRectangle C-Win 
FUNCTION drawRectangle RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bFull 
     LABEL "Print full report" 
     SIZE 32 BY 1.14
     FONT 6.

DEFINE BUTTON bOne 
     LABEL "" 
     SIZE 32 BY 1.14
     FONT 6.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "cust-num name ------ ------ 000001 Lift skiing" 
     VIEW-AS EDITOR
     SIZE 21.8 BY 2.76
     BGCOLOR 8 FGCOLOR 5 FONT 3 NO-UNDO.

DEFINE VARIABLE infoMes AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 38 BY .86
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE rOrder AS CHARACTER INITIAL "N" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Field names", "N",
"Field number", "O"
     SIZE 24 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 0    
     SIZE 29 BY 1.19
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.4 BY 3.24
     FGCOLOR 5 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.6 BY 9.14.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 9.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 2.86.

DEFINE RECTANGLE RECT-Bottom
     EDGE-PIXELS 0    
     SIZE 24.8 BY .33
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-Left
     EDGE-PIXELS 0    
     SIZE 1.4 BY 3.52
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-Right
     EDGE-PIXELS 0    
     SIZE 1.4 BY 3.48
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-Top
     EDGE-PIXELS 0    
     SIZE 24.4 BY .33
     BGCOLOR 0 .

DEFINE VARIABLE SelectBack AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 28.4 BY 7.86 TOOLTIP "Select a background."
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE bottomBorder AS LOGICAL INITIAL no 
     LABEL "Bottom" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE leftBorder AS LOGICAL INITIAL no 
     LABEL "Top" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE rightBorder AS LOGICAL INITIAL no 
     LABEL "Right" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.6 BY .81 NO-UNDO.

DEFINE VARIABLE topBorder AS LOGICAL INITIAL no 
     LABEL "Top" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bF1 FOR 
      _File SCROLLING.

DEFINE QUERY bField FOR 
      _Field SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bF1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bF1 C-Win _FREEFORM
  QUERY bF1 DISPLAY
      _File._File-name
_File._Desc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 74 BY 10.71 TOOLTIP "Select one or more file(s).".

DEFINE BROWSE bField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bField C-Win _FREEFORM
  QUERY bField DISPLAY
      _Field-name
_Field._Data-Type
_Field._Format
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 10.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bF1 AT ROW 2.91 COL 1
     bField AT ROW 2.91 COL 76
     SelectBack AT ROW 16 COL 16.8 NO-LABEL
     rOrder AT ROW 16 COL 114 NO-LABEL
     topBorder AT ROW 16.19 COL 63.2
     EDITOR-1 AT ROW 17.57 COL 61.4 NO-LABEL
     rightBorder AT ROW 18.43 COL 88.2
     leftBorder AT ROW 18.52 COL 55.2
     bOne AT ROW 20.29 COL 116
     bottomBorder AT ROW 20.91 COL 63.6
     bFull AT ROW 21.95 COL 116
     infoMes AT ROW 23.38 COL 114 COLON-ALIGNED NO-LABEL
     " Order by..." VIEW-AS TEXT
          SIZE 15 BY .71 AT ROW 15.05 COL 114
          BGCOLOR 7 FGCOLOR 15 FONT 6
     "Left" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 18.52 COL 50.8
     " Background image" VIEW-AS TEXT
          SIZE 23.8 BY .71 AT ROW 14.81 COL 17
          BGCOLOR 7 FGCOLOR 15 FONT 6
     " Borders" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 14.86 COL 50.8
          BGCOLOR 7 FGCOLOR 15 FONT 6
     " xPrint dictionary report" VIEW-AS TEXT
          SIZE 29 BY 1.19 AT ROW 1 COL 2
          BGCOLOR 3 FGCOLOR 10 FONT 6
     RECT-Left AT ROW 17.19 COL 59.8
     RECT-Top AT ROW 17.19 COL 60.2
     RECT-Bottom AT ROW 20.38 COL 60
     RECT-Right AT ROW 17.24 COL 83.4
     RECT-1 AT ROW 1.24 COL 3
     RECT-2 AT ROW 17.38 COL 60.6
     RECT-4 AT ROW 15.1 COL 15
     RECT-5 AT ROW 15.1 COL 47.6
     RECT-6 AT ROW 15.29 COL 113
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155.6 BY 23.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "xprint Dictionary"
         HEIGHT             = 23.95
         WIDTH              = 155.6
         MAX-HEIGHT         = 25.67
         MAX-WIDTH          = 157.6
         VIRTUAL-HEIGHT     = 25.67
         VIRTUAL-WIDTH      = 157.6
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB bF1 RECT-6 DEFAULT-FRAME */
/* BROWSE-TAB bField bF1 DEFAULT-FRAME */
ASSIGN 
       bFull:AUTO-RESIZE IN FRAME DEFAULT-FRAME      = TRUE.

ASSIGN 
       bOne:AUTO-RESIZE IN FRAME DEFAULT-FRAME      = TRUE.

ASSIGN 
       EDITOR-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN infoMes IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       infoMes:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-Bottom IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       RECT-Bottom:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-Left IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       RECT-Left:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-Right IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       RECT-Right:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-Top IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       RECT-Top:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bF1
/* Query rebuild information for BROWSE bF1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH _File where not _hidden no-lock.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bF1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bField
/* Query rebuild information for BROWSE bField
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH _Field of _file no-lock.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE bField */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* xprint Dictionary */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* xprint Dictionary */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bF1
&Scoped-define SELF-NAME bF1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bF1 C-Win
ON VALUE-CHANGED OF bF1 IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-bField}
  
    bone:sensitive = bF1:num-selected-rows <> 0.
  
  If bF1:num-selected-rows = 1 then  
          bone:LABEL = 'Print ' + _File._File-Name + ' description.'.
  else
          bone:LABEL = 'Print selected files description.'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bFull
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bFull C-Win
ON CHOOSE OF bFull IN FRAME DEFAULT-FRAME /* Print full report */
DO:

  RUN drawBackground.  
  ASSIGN rOrder.

  Run loadxPrint.        /* We have an extensive usage of the DLL, then load it first */
  output stream O to 'Full.xpr' paged page-size 200.
  put stream o control '<PREVIEW=zoomToWidth>'.
  
  infoMes:HIDDEN = FALSE.
  
  GET FIRST bF1.
  DO WHILE AVAILABLE _File :
            infoMes:SCREEN-VALUE = "Working on " + _File-Name + "...".
            PROCESS EVENTS. 
            RUN printFileDesc.
            PAGE STREAM O.
            GET NEXT bF1.
            END.
  
  output stream O close.
  
  file-info:file-name = 'Full.xpr'.
  
  Run unloadxPrint.
/*--------------------------------------------*/  

            infoMes:SCREEN-VALUE = "Result sent to xPrint...".
            PROCESS EVENTS. 
   
   run printFile(file-info:FULL-PATHNAME).                        
   
   infoMes:HIDDEN = TRUE.

   APPLY "value-changed" TO bF1.
   APPLY "entry" TO bF1.
   RETURN NO-APPLY.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bOne
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOne C-Win
ON CHOOSE OF bOne IN FRAME DEFAULT-FRAME
DO:

def var I           AS INT NO-UNDO.

ASSIGN rOrder.

  if bF1:num-selected-Rows = 0 then do :
        message "No file found."
                view-as alert-box info.
        return no-apply.
        end.
        
  RUN drawBackGround.
  
  Run loadxPrint.        /* We have an extensive usage of the DLL, then load it first */

      
  output stream O to value('xDict.xpr') paged page-size 200.
  put stream o control '<PREVIEW=zoomToWidth>'.
  
  infomes:Hidden = FALSE.
  
  do i = 1 to bF1:num-selected-Rows :              
      bF1:fetch-Selected-Row(I).
            infoMes:SCREEN-VALUE = "Working on " + _File._File-Name + "...".
            PROCESS EVENTS.
            
      RUN printFileDesc.
      page stream O.
      end.
      
  output stream O close.
  
  file-info:file-name = 'xDict.xpr'.
  
  Run unloadxPrint.
/*---------------*/  

            infoMes:SCREEN-VALUE = "Result sent to xPrint...".
            PROCESS EVENTS. 
            
   run printFile(file-info:FULL-PATHNAME).
   
   infoMes:Hidden = TRUE.
                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bottomBorder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bottomBorder C-Win
ON VALUE-CHANGED OF bottomBorder IN FRAME DEFAULT-FRAME /* Bottom */
DO:
    Rect-Bottom:HIDDEN = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME leftBorder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL leftBorder C-Win
ON VALUE-CHANGED OF leftBorder IN FRAME DEFAULT-FRAME /* Top */
DO:
      Rect-Left:HIDDEN = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-Bottom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-Bottom C-Win
ON MOUSE-SELECT-CLICK OF RECT-Bottom IN FRAME DEFAULT-FRAME
DO:
    Rect-Bottom:HIDDEN = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rightBorder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rightBorder C-Win
ON VALUE-CHANGED OF rightBorder IN FRAME DEFAULT-FRAME /* Right */
DO:
    Rect-Right:HIDDEN = NOT SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME topBorder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL topBorder C-Win
ON VALUE-CHANGED OF topBorder IN FRAME DEFAULT-FRAME /* Top */
DO:
  Rect-Top:HIDDEN = NOT SELF:CHECKED.
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
RETURN.

RUN loadBack.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */


{ gradient.i }

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  APPLY "value-changed" TO bF1.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE drawBackGround C-Win 
PROCEDURE drawBackGround :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  backSequence = ''.
  IF TopBorder:CHECKED IN FRAME {&FRAME-NAME} THEN
        backSequence = autoTopBorder().
  IF rightBorder:CHECKED IN FRAME {&FRAME-NAME} THEN
        backSequence = backSequence + autorightBorder().
  IF bottomBorder:CHECKED IN FRAME {&FRAME-NAME} THEN
        backSequence = backSequence + autobottomBorder().
  IF LeftBorder:CHECKED IN FRAME {&FRAME-NAME} THEN
        backSequence = backSequence + autoLeftBorder().        
          
  IF selectBack:SCREEN-VALUE IN FRAME {&FRAME-NAME}  <> "" 
  AND backSequence = "" THEN DO :
        FILE-INFO:FILE-NAME = selectBack:SCREEN-VALUE.
        backSequence = "<#50><R1><C1><#1><R80><C78.5><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + "><=#50>".
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
  DISPLAY SelectBack rOrder topBorder EDITOR-1 rightBorder leftBorder 
          bottomBorder 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-4 RECT-5 RECT-6 bF1 bField SelectBack rOrder topBorder 
         EDITOR-1 rightBorder leftBorder bOne bottomBorder bFull 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadBack C-Win 
PROCEDURE loadBack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR A AS CHAR NO-UNDO.
DEF VAR basename    AS CHAR NO-UNDO.
DEF VAR absname     AS CHAR NO-UNDO.

INPUT STREAM I FROM OS-DIR("cliparts").
REPEAT:
    IMPORT STREAM i baseName absName.
    IF NOT baseName MATCHES "back*.jpg"
    AND NOT baseName MATCHES "corner*.jpg" THEN NEXT.
    
    A = A + ',cliparts/' + basename.
    
    END.
    
INPUT STREAM I CLOSE.
    
selectBack:LIST-ITEMS IN FRAME {&FRAME-NAME} = A.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printFileDesc C-Win 
PROCEDURE printFileDesc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
def var viewAs          as char  no-undo.
DEF VAR I               AS INT NO-UNDO.                        
DEF VAR firstTime       AS LOGICAL NO-UNDO.

DEF VAR seqfirstLine AS CHAR     NO-UNDO FORMAT "x(8)".
DEF VAR currentLine AS INT     NO-UNDO.
DEF VAR maxLineNumber       AS INT NO-UNDO INITIAL 57.
&Scop progressColor         97,77,240
                        
  PUT stream o control "<||2><ALIGN=BASE><UNITS=INCHES>".       /* you can use <TRANSPARENT=TRUE>  */
          
  FIND _index WHERE RECID(_index) = _File._prime-index NO-LOCK NO-ERROR. 
  
  /* get the file description height in lines
     ======================================*/
  IF _File._Desc = "" 
  OR _File._Desc = ? THEN 
        I = 0.
  ELSE 
        RUN getTextHeight(replace(_File._Desc, CHR(10), " "), stdFont + ',10,B,I', INT(3.6 * pRes), OUTPUT I).   

  
  IF backSequence = ? THEN
                        backSequence = "".
        
  /* display the file header with description
     ======================================*/        
  PUT STREAM O CONTROL backSequence "<#1>"         /* for returning to the starting point */
                                             "<R2><C40><#2><AT=,+3.6><R+" 5 + I "><FRAME#2><BGCOLOR=225,241,233><FILLRECT#2><USE#2>"
                                             "<FCourier New></B>"
                                             "Primary index : <B>" (IF AVAILABLE _index THEN _index-name ELSE "") "</B>~015~012"                                              
                                             "# keys        : <B>" _Numkey "</B>~015~012" 
                                             "# fields      : <B>" _Numfld - 1 "</B>~015~012" 
                                             "dump name     : <B>" _dump-Name "</B>~015~012~015~012"
                                             "<I><B><F" stdFont "><P10>" replace(_File._Desc, CHR(10), " ")
                                             "</I></USE=PROGRESS><R1><C1><F" stdFont "></B>".
                                             
  /*   what's the first line for printing ?  (depending on the file description height)
       ----------------------------------*/
       
  seqFirstLine = "<R" + string(3 + 5 + I) + ">".
  currentLine = 2 + 5 + I + 2.
      
  /* Define the header          (note that we use a PROGRESS display simulation)
     ===============*/
           
  form header              
             '<FArial><P10>' '<C5>' + pdbname( "DictDb" ) format "x(50)"
                     skip(3)
             "<FGCOLOR=109,190,100><C10><P24><B>" "<C3>" + _File._File-NAME FORMAT "x(50)" '<P10></B><FGCOLOR=BLACK>' SKIP(2)
             "<F" + stdFont + ">" FORMAT "x(50)" SKIP 
             "<R65><C1><I>Date" TODAY FORMAT "99/99/9999" STRING(TIME, 'HH:MM')
             "<C32>Powered by <FGCOLOR={&progressColor}><B>xPrint</B>   <U>www.4gl.fr</U><FGCOLOR=BLACK>"
             "<C65>Page" STRING(PAGE-NUMBER(O)) + ' / <#PAGES>' FORMAT "x(20)" "</I>"
             seqFirstline AT 1
             "<C4><B>Name<C24>Type<C32>Format<C44>Label<C65>I<C67>M<C69>C<C75>T</B>"
             "  ------------------- -------- ----------- -------------------- - - - --- -" at 1 
        with page-top
             frame myTopbyOrder
             stream-io
             width 255.
             
  /* display each field
     ================*/
  ASSIGN FRAME {&FRAME-NAME} rOrder.

  for each _Field of _file no-lock  BREAK by (if rOrder = "O" then string(_Order, "99999") else _Field-name ):
             view stream O frame myTopbyOrder.
             
             /* Field name
                ========*/
             PUT STREAM O unformatted
                 "<C4><FGCOLOR=BLUE><B>" _Field._Field-name "</B>"
                 "<C23><FGCOLOR=BLACK><I>" _data-Type format "x(4)" "</I>".
             /* Field Type
                ========*/   
             If _Data-Type = "DECIMAL" then
                 put stream O unformatted '-' _Field._Decimals.
                 
             If _Extent > 0 then
                 Put stream O unformatted "[" _Field._Extent "]".
                 
             /* Field format
                ==========*/
            put stream O UNFORMATTED
                 "<C32>" reduce(lc(_Field._Format), stdFont + ',10', 1.1).
            
            /* Field label
                ========*/
                     
            PUT STREAM o UNFORMATTED
                 "<C44>" reduce(_Field._label, stdFont + ',10', 2.0).       /* reduce the label to fit in the column */
               
            /* Index Field
               =========*/
            
            FOR EACH _index OF _File NO-LOCK,
                FIRST _index-Field OF _Index NO-LOCK
                      WHERE _index-Field._field-recid = RECID( _Field ):
                      PUT STREAM O UNFORMATTED
                                "<C65><BGCOLOR=FUCHSIA>" drawRectangle().
                      LEAVE.
                END.
                      
            /* Field mandatory
                ============*/     
            If _Field._mandatory then
                 put stream o unformatted     "<C67><BGCOLOR=RED>" drawRectangle().

            /* Field case-sensitive
                =================*/                 
            if _Field._Fld-case then
                 put stream O unformatted     "<C69><BGCOLOR=RED>"  drawRectangle().
                 
            PUT stream o unformatted "<F" stdFont "><FGCOLOR=BLACK>".
            
            /* View-as phrase                   display a graphical icon
               ============*/
            If _View-as > "" then do :
                  viewAs = trim(entry(2, _View-as, " ")).       /* V9 version padding with spaces */
                  IF SEARCH("cliparts/" + viewas + ".bmp") <> ? THEN DO :                         /* try to find a valid icon */
                        FILE-INFO:FILE-NAME = "cliparts/" + viewas + ".bmp".
                        put stream o unformatted "<C71>"
                                "<#3><R+1><C+3><IMAGE#3="
                                FILE-INFO:FULL-PATHNAME
                                "><R-1><F" stdFont ">".
                        END.
                  end.

           /* Field Trigger
              ===========*/
              
           IF CAN-FIND(FIRST _Field-Trig WHERE _Field-Trig._Field-RECID = RECID(_Field)) THEN
                    PUT STREAM O UNFORMATTED
                        "<C75><BGCOLOR=GREEN>" drawrectangle().
                        
           /* Field Desc an column-label are one line lower then the first line desc.
              ---------------------------------------------------------------------*/
           If _Field._Desc > "" 
           or _Col-label <> ? THEN DO :
                put stream O skip.           /* Note the current line */
                currentLine = currentLine + 1.
                END.
                
           /* Column-label
              ==========*/
           if _Col-label <> ? THEN DO :
                  put stream o unformatted
                        "<C44><BGCOLOR=GRAY>" drawrectangle() "<FGCOLOR=GRAY><I><P8><C+1>"
                                reduce(_Col-Label, stdfont + '8,I', 1.9)
                                "</I></USE=progress><FGCOLOR=BLACK><P10>".
                  END.
                                  
           /* Field description
              ===============*/                     
           If _Field._Desc > "" then   DO :        /* Description */
                  put stream O unformatted
                        "<c5><#1><AT=,+1.7><R+6><FRAME#1><USE#1><I><P8>" _Field._Desc "</I></USE=PROGRESS><P10>".
                        
                  RUN getTextHeight(_Field._Desc, stdFont + ',8,I', INT( 1.7 * pRes), OUTPUT I). 
                  currentLine = currentLine + I - 1.
                  END.
                    
                                      .
            
          
            PUT STREAM O SKIP.
                  
            currentLine = currentline + 1.
            
            IF currentLine > maxLineNumber             /* max lines per page */
            AND NOT last((if rOrder = "O" then string(_Order, "99999") else _Field-name )) THEN DO :
                PAGE STREAM O.
                PUT STREAM O CONTROL backSequence "<R1><C1>".
                seqFirstLine = "<R9>".
                currentLine = 9.
                END.
                          
            END.
             
/*          Index description on the next page
            ================================*/
            
            
                PAGE STREAM O.
                HIDE STREAM o FRAME myTopbyOrder.
                PUT STREAM O CONTROL backSequence.
                seqFirstLine = "<R10>".
                currentLine = 9.

  form header              
             '<FArial><R2><P10>' '<C5>' + pdbname( "dictDB" ) format "x(50)"
                     skip(3)
             "<FGCOLOR=109,190,100><C10><P24><B>" "<C3>" + _File._File-NAME FORMAT "x(50)" '<P10></B><FGCOLOR=BLACK>' SKIP(2)
             "<F" + stdFont + ">" FORMAT "x(50)" SKIP 
                          "<R65><C1><I>Date" TODAY FORMAT "99/99/9999" STRING(TIME, 'HH:MM')
                          "<C32>Powered by <FGCOLOR={&progressColor}><B>xPrint</B>   <U>www.4gl.fr</U><FGCOLOR=BLACK>"
                          "<C65>Page" STRING(PAGE-NUMBER(O)) + ' / <#PAGES>' FORMAT "x(20)" "</I>"
             seqFirstline AT 1
             
             "<B><C3.4>I<C5.2>U<C7>W<C9>Name<C40><C43>Field</B>" SKIP
             "  - - - ------------------------------ -- ----------------------------" 
        with page-top
             frame myTopIndex
             stream-io
             width 255.
                
VIEW STREAM o FRAME myTopIndex.                

PUT STREAM o CONTROL
            "<R2><C40><#2><AT=,+3.6><R+7><FRAME#2><BGCOLOR=225,241,233><FILLRECT#2><USE#2>~015~012"                                             
                                             "<P20><B><U>INDEX SUMMARY" "</B></U><P10>~015~012"                 
                                             " <B>I</B> : inactive~015~012"
                                             " <B>U</B> : unique index~015~012"
                                             " <B>W</B> : word index~015~012" 
                                             "<FGCOLOR=GREEN><B> Primary index<FGCOLOR=BLACK></B>"
                                             "</USE=PROGRESS><R1><C1><F" stdFont "></B><R10>".

firstTime = TRUE.
             
FOR EACH _index OF _File NO-LOCK
                   BY (IF recid(_index) = _File._Prime-Index THEN 1 ELSE 2)
                   BY _index._Index-name:
      IF NOT firstTime THEN  DO :
        PUT STREAM o UNFORMATTED
                "<C3><R+0.2><FROM><C71><||4><LINECOLOR=GREEN><LINE><R+0.2><||2><LINECOLOR=BLACK>".
        currentLine = currentLine + 1.
        END.
    
    firstTime = FALSE.
                            
    IF NOT _active THEN
            PUT STREAM o UNFORMATTED
                "<C3><BGCOLOR=RED>" drawrectangle().
                
    IF _Unique THEN 
            PUT STREAM o UNFORMATTED
                "<C5><BGCOLOR=FUCHSIA>" drawrectangle().
    IF _Wordidx <> ? THEN
            PUT STREAM o UNFORMATTED
                "<C7><BGCOLOR=GREEN>" drawrectangle().
                    
    IF RECID( _Index ) = _File._Prime-Index THEN
            PUT STREAM o UNFORMATTED
                    "<FGCOLOR=GREEN>".
                                    
    PUT STREAM o UNFORMATTED
            "<C9><B>" _Index-Name.

    PUT STREAM o UNFORMATTED '<FGCOLOR=BLACK></B>'.
    
DEF BUFFER iField FOR _Field.
    FOR EACH _Index-Field OF _index NO-LOCK BY _Index-Field._Index-Seq  :
        FIND iField WHERE RECID( iField ) = _index-Field._Field-recid NO-LOCK.
        PUT STREAM O UNFORMATTED
                "<C40>" _index-seq
                "<C43.5>" _Ascending FORMAT "+/-"
                " " iField._Field-Name
                SKIP.
    currentLine = currentLine + 1.
    IF currentLine > maxLineNumber THEN DO :
            PAGE STREAM O.
            currentLine = 9.
            END.
    END.
  
    if not can-find(_Index-Field OF _index) then        /* in case of no field in this index */
            put stream O skip. 

END.
    
          
    PUT STREAM o UNFORMATTED "<R+0.2>".                                    
                                           
                                       
*/  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION autoBottomBorder C-Win 
FUNCTION autoBottomBorder RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR A AS CHAR NO-UNDO.
DEF VAR currentColor    AS INT NO-UNDO INITIAL 200.


DO I = 1 to 78 BY {&INC} :
               A = A +
                   "<R64><AT=-0.1><C" + string(I) + "><FROM><C+{&INC}><AT=+0.1><BGCOLOR=0," + STRING(currentColor)
                   + ",0><FILLRECT>".
                    
                   IF I < 26 THEN
                        currentColor = currentColor - 5.
                   ELSE IF I < 52 THEN
                        currentColor = currentColor + 5.
                   ELSE
                        currentColor = currentColor - 5.

                   end.     
                   
RETURN A.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION autoLeftBorder C-Win 
FUNCTION autoLeftBorder RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR A AS CHAR NO-UNDO.
DEF VAR currentColor    AS INT NO-UNDO INITIAL 200.


DO I = 1 to 63 BY {&Inc} :
               A = A +
                   "<R" + string(I) + "><C1><FROM><R+{&Inc}><AT=,+0.1><BGCOLOR=0," + STRING(currentColor)
                   + ",0><FILLRECT>".
                    
                   IF I < 20 THEN
                        currentColor = currentColor - 10.
                   ELSE IF I < 42 THEN
                        currentColor = currentColor + 10.
                   ELSE
                        currentColor = currentColor - 10.

                   end.     
                   
RETURN A.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION autoRightBorder C-Win 
FUNCTION autoRightBorder RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR A AS CHAR NO-UNDO.
DEF VAR currentColor    AS INT NO-UNDO INITIAL 200.


DO I = 1 to 63 BY {&Inc} :
               A = A +
                   "<R" + string(I) + "><C78><FROM><R+{&Inc}><AT=,+0.1><BGCOLOR=0," + STRING(currentColor)
                   + ",0><FILLRECT>".
                    
                   IF I < 20 THEN
                        currentColor = currentColor - 10.
                   ELSE IF I < 42 THEN
                        currentColor = currentColor + 10.
                   ELSE
                        currentColor = currentColor - 10.

                   end.     
                   
RETURN A.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION autoTopBorder C-Win 
FUNCTION autoTopBorder RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR A AS CHAR NO-UNDO.
DEF VAR currentColor    AS INT NO-UNDO INITIAL 200.


DO I = 1 to 78 BY {&INC} :
               A = A +
                   "<R1><C" + string(I) + "><FROM><C+{&INC}><AT=+0.1><BGCOLOR=0," + STRING(currentColor)
                   + ",0><FILLRECT>".
                    
                   IF I < 26 THEN
                        currentColor = currentColor - 5.
                   ELSE IF I < 52 THEN
                        currentColor = currentColor + 5.
                   ELSE
                        currentColor = currentColor - 5.

                   end.     
                   
RETURN A.
        
       

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION drawRectangle C-Win 
FUNCTION drawRectangle RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 RETURN "<AT=+0.05><FROM><AT=+0.1,+0.1><FILLRECT><AT=-0.15,-0.1>".      /* rectangle of 0.1 inch */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

