&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              analyse4GLTrace.W

  Description:       Identifies performancebottlenecks in your application by
                     analysing the logfile you create with the log-manager
                     
                     Create the logmanagerfile by adding this code at the startpoint
                     for measuring performance in your application: 
                     
                     LOG-MANAGER:LOGFILE-NAME = "<Your temp-dir?>/4GLTrace.log".
                     LOG-MANAGER:LOGGING-LEVEL = 4.                    
                     LOG-MANAGER:LOG-ENTRY-TYPES = "4GLTRACE".        

                     Where you want to close the log add 

                     LOG-MANAGER:LOGGING-LEVEL = 0.
                     LOG-MANAGER:CLOSE-LOG().
                     
                     Read the logfile with this program
                      
                     Further info about dynamic object logging can be found on
                     http://www.progress.com/progress/products/documentation/docs/dvdbg/dvdbg.pdf 

  Input Parameters:  
                     
 
  Output Parameters:
      <none>
 
  Author:            Stefan Houtzager 
 
  Created:           January 15 2010 
                     Houtzager ICT consultancy & development
                     
  Notes:             Written with 10.2A02. 
                     
                     comments/bugreports welcome, send them to agent_008_nl@yahoo.com   
  
  Revisions:                   
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
  ROUTINE-LEVEL ON ERROR UNDO, THROW. 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEFINE TEMP-TABLE ttLine NO-UNDO
    FIELD Line          AS CHARACTER
    FIELD EventType     AS CHARACTER
    FIELD CallerName    AS CHARACTER
    FIELD CodeBlock     AS CHARACTER
    FIELD Params        AS CHARACTER
    FIELD CallerLineNo  AS CHARACTER
    FIELD EventTime     AS DATETIME
    FIELD EventDuration AS DECIMAL
    INDEX I_CodeBlockEventType CodeBlock EventType.

DEFINE TEMP-TABLE ttCallee NO-UNDO
    FIELD CodeBlock   AS CHARACTER
    FIELD CallsTo     AS INTEGER
    FIELD TotTime     AS DECIMAL
    FIELD AvgTime     AS DECIMAL
    FIELD EventType   AS CHARACTER
    FIELD SessionPerc AS DECIMAL
    INDEX PU_Callee AS PRIMARY UNIQUE CodeBlock EventType
    INDEX I_SessionPerc SessionPerc
    INDEX I_CodeBlock   CodeBlock
    INDEX I_EventType   EventType
    INDEX I_AvgTime     AvgTime
    INDEX I_TotTime     TotTime
    INDEX I_CallsTo     CallsTo.

DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hCurCol      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSessionPerc AS HANDLE    NO-UNDO.
DEFINE VARIABLE rTtCallee    AS ROWID     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-Callee

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCallee ttLine

/* Definitions for BROWSE BR-Callee                                     */
&Scoped-define FIELDS-IN-QUERY-BR-Callee ttCallee.CodeBlock ttCallee.EventType ttCallee.CallsTo ttCallee.TotTime ttCallee.AvgTime ttCallee.SessionPerc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Callee ttCallee.CodeBlock ttCallee.SessionPerc   
&Scoped-define ENABLED-TABLES-IN-QUERY-BR-Callee ttCallee
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BR-Callee ttCallee
&Scoped-define SELF-NAME BR-Callee
&Scoped-define OPEN-QUERY-BR-Callee /* OPEN QUERY {&SELF-NAME} FOR EACH ttCallee BY SessionPerc DESCENDING. */.
&Scoped-define TABLES-IN-QUERY-BR-Callee ttCallee
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Callee ttCallee


/* Definitions for BROWSE BR-Lines                                      */
&Scoped-define FIELDS-IN-QUERY-BR-Lines ttLine.CallerName ttLine.CallerLineNo ttLine.EventDuration ttLine.Params   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Lines   
&Scoped-define SELF-NAME BR-Lines
&Scoped-define QUERY-STRING-BR-Lines FOR EACH ttLine                                WHERE ttLine.CodeBlock = ttCallee.CodeBlock                                  AND ttLine.EventType = ttCallee.EventType
&Scoped-define OPEN-QUERY-BR-Lines OPEN QUERY {&SELF-NAME} FOR EACH ttLine                                WHERE ttLine.CodeBlock = ttCallee.CodeBlock                                  AND ttLine.EventType = ttCallee.EventType.
&Scoped-define TABLES-IN-QUERY-BR-Lines ttLine
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Lines ttLine


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BU-chooseLog FI-search BR-Callee BR-Lines 
&Scoped-Define DISPLAYED-OBJECTS FI-log FI-RunTime FI-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD repositionBR-Callee C-Win 
FUNCTION repositionBR-Callee RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BU-analysis 
     LABEL "Start analysis" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BU-chooseLog 
     LABEL "..." 
     SIZE 5 BY 1.14.

DEFINE BUTTON BU-showlog 
     LABEL "Show logfile (notepad)" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE FI-log AS CHARACTER FORMAT "X(256)":U 
     LABEL "4GLtrace logfile to analyze" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-RunTime AS CHARACTER FORMAT "XXXXXXXX":U 
     LABEL "Total runtime" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-search AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 TOOLTIP "Press return to search" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-Callee FOR 
      ttCallee SCROLLING.

DEFINE QUERY BR-Lines FOR 
      ttLine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-Callee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Callee C-Win _FREEFORM
  QUERY BR-Callee DISPLAY
      ttCallee.CodeBlock   FORMAT "X(90)"     COLUMN-LABEL "Codeblock"
ttCallee.EventType   FORMAT "X(20)"       COLUMN-LABEL "Event"
ttCallee.CallsTo     FORMAT ">>>>>>9"     COLUMN-LABEL "Calls"
ttCallee.TotTime     FORMAT ">>>>>>9.999" COLUMN-LABEL "Total time"
ttCallee.AvgTime     FORMAT ">>>>9.999"   COLUMN-LABEL "Avg time"
ttCallee.SessionPerc FORMAT ">>>>9.999"   COLUMN-LABEL "% Session"
ENABLE ttCallee.CodeBlock ttCallee.SessionPerc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 166 BY 12.38 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.

DEFINE BROWSE BR-Lines
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Lines C-Win _FREEFORM
  QUERY BR-Lines DISPLAY
      ttLine.CallerName      FORMAT "X(90)"     COLUMN-LABEL "Calling block"
ttLine.CallerLineNo    FORMAT "XXXXXXXX"  COLUMN-LABEL "LineNo"
ttLine.EventDuration   FORMAT ">>>>>9"    COLUMN-LABEL "Event duration (ms)"
ttLine.Params          FORMAT "X(200)"    COLUMN-LABEL "Parameters"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 166 BY 13.1 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN TOOLTIP "CTRL-C = copy the to the focused row corresponding logfileline to the clipboard".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-log AT ROW 1.24 COL 29 COLON-ALIGNED WIDGET-ID 2
     BU-chooseLog AT ROW 1.24 COL 75 WIDGET-ID 4
     BU-analysis AT ROW 1.24 COL 81 WIDGET-ID 6
     BU-showlog AT ROW 1.24 COL 105 WIDGET-ID 14
     FI-RunTime AT ROW 1.24 COL 153 COLON-ALIGNED WIDGET-ID 8
     FI-search AT ROW 3.14 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     BR-Callee AT ROW 4.33 COL 5 WIDGET-ID 200
     BR-Lines AT ROW 17.67 COL 5 WIDGET-ID 300
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 173 BY 30.05 WIDGET-ID 100.


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
         TITLE              = "Profile viewer"
         HEIGHT             = 30.05
         WIDTH              = 173
         MAX-HEIGHT         = 30.05
         MAX-WIDTH          = 173
         VIRTUAL-HEIGHT     = 30.05
         VIRTUAL-WIDTH      = 173
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
/* BROWSE-TAB BR-Callee FI-search DEFAULT-FRAME */
/* BROWSE-TAB BR-Lines BR-Callee DEFAULT-FRAME */
ASSIGN 
       BR-Callee:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BR-Callee:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BR-Lines:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR BUTTON BU-analysis IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BU-showlog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-log IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-log:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-RunTime IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Callee
/* Query rebuild information for BROWSE BR-Callee
     _START_FREEFORM
/* OPEN QUERY {&SELF-NAME} FOR EACH ttCallee BY SessionPerc DESCENDING. */
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BR-Callee */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Lines
/* Query rebuild information for BROWSE BR-Lines
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttLine
                               WHERE ttLine.CodeBlock = ttCallee.CodeBlock
                                 AND ttLine.EventType = ttCallee.EventType
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BR-Lines */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Profile viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Profile viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Callee
&Scoped-define SELF-NAME BR-Callee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Callee C-Win
ON ITERATION-CHANGED OF BR-Callee IN FRAME DEFAULT-FRAME
DO: 
  RUN openQueryLines.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Callee C-Win
ON START-SEARCH OF BR-Callee IN FRAME DEFAULT-FRAME
DO:
  IF hCurCol NE BR-Callee:CURRENT-COLUMN THEN BR-Callee:CLEAR-SORT-ARROWS().
  
  rTtCallee = IF AVAILABLE ttCallee THEN ROWID(ttCallee) ELSE ?.

  hCurCol = BR-Callee:CURRENT-COLUMN. 

  BR-Callee:SET-SORT-ARROW(LOOKUP(hCurCol:NAME,cColumns),hCurCol:SORT-ASCENDING = FALSE).
  
  CASE hCurCol:NAME:
      WHEN 'CodeBlock' THEN DO:
          IF hCurCol:SORT-ASCENDING THEN OPEN QUERY BR-Callee FOR EACH ttCallee BY CodeBlock.
          ELSE OPEN QUERY BR-Callee FOR EACH ttCallee BY CodeBlock  DESCENDING.
      END.
      WHEN 'SessionPerc' THEN DO:
          IF hCurCol:SORT-ASCENDING THEN OPEN QUERY BR-Callee FOR EACH ttCallee BY SessionPerc.
          ELSE OPEN QUERY BR-Callee FOR EACH ttCallee BY SessionPerc DESCENDING.
      END.   
      WHEN 'CallsTo' THEN DO:
          IF hCurCol:SORT-ASCENDING THEN OPEN QUERY BR-Callee FOR EACH ttCallee BY CallsTo.
          ELSE OPEN QUERY BR-Callee FOR EACH ttCallee BY CallsTo DESCENDING.
      END.   
      WHEN 'EventType' THEN DO:
          IF hCurCol:SORT-ASCENDING THEN OPEN QUERY BR-Callee FOR EACH ttCallee BY EventType.
          ELSE OPEN QUERY BR-Callee FOR EACH ttCallee BY EventType DESCENDING.
      END.   
      WHEN 'TotTime' THEN DO:
          IF hCurCol:SORT-ASCENDING THEN OPEN QUERY BR-Callee FOR EACH ttCallee BY TotTime.
          ELSE OPEN QUERY BR-Callee FOR EACH ttCallee BY TotTime DESCENDING.
      END.   
      WHEN 'AvgTime' THEN DO:
          IF hCurCol:SORT-ASCENDING THEN OPEN QUERY BR-Callee FOR EACH ttCallee BY AvgTime.
          ELSE OPEN QUERY BR-Callee FOR EACH ttCallee BY AvgTime DESCENDING.
      END.   
  END CASE.

  repositionBR-Callee().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Lines
&Scoped-define SELF-NAME BR-Lines
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Lines C-Win
ON CTRL-C OF BR-Lines IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE ttLine THEN CLIPBOARD:VALUE = ttLine.Line.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BU-analysis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BU-analysis C-Win
ON CHOOSE OF BU-analysis IN FRAME DEFAULT-FRAME /* Start analysis */
DO:
  RUN analyse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BU-chooseLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BU-chooseLog C-Win
ON CHOOSE OF BU-chooseLog IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEFINE VARIABLE cLogFile AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lOK      AS LOGICAL     NO-UNDO.

  SYSTEM-DIALOG GET-FILE cLogFile    
      TITLE   "Choose 4GLTracefile to analyse ..."    
      FILTERS "Logfiles (*.log)"   "*.log"
      INITIAL-DIR SESSION:TEMP-DIR 
      MUST-EXIST    
      USE-FILENAME    
      UPDATE lOK.   
  
  IF lOK = TRUE THEN FI-log:SCREEN-VALUE = cLogFile. 

  ASSIGN BU-showlog:SENSITIVE  = FI-log:SCREEN-VALUE NE '' 
         BU-analysis:SENSITIVE = BU-showlog:SENSITIVE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BU-showlog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BU-showlog C-Win
ON CHOOSE OF BU-showlog IN FRAME DEFAULT-FRAME /* Show logfile (notepad) */
DO:
  OS-COMMAND NO-WAIT VALUE('notepad.exe ' + FI-log:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-search C-Win
ON RETURN OF FI-search IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE hColumn AS HANDLE      NO-UNDO.
  DEFINE BUFFER ttCallee FOR ttCallee.

  IF BR-Callee:CURRENT-COLUMN NE BR-Callee:FIRST-COLUMN THEN DO:
    hColumn = BR-Callee:FIRST-COLUMN.
    hColumn:READ-ONLY = FALSE.
    APPLY 'ENTRY':u TO hColumn. /* set current-column to CodeBlock */
    APPLY 'start-search':U TO BR-Callee.
    hColumn:READ-ONLY = TRUE.
    APPLY "ENTRY":U TO FI-search.
  END.

/*   APPLY LASTKEY.  */

  FIND FIRST ttCallee WHERE ttCallee.CodeBlock BEGINS SELF:SCREEN-VALUE NO-ERROR.
  rTtCallee = IF AVAILABLE ttCallee THEN ROWID(ttCallee) ELSE rTtCallee.

  repositionBR-Callee().

/*   RETURN NO-APPLY.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Callee
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
  RUN initialize.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE analyse C-Win 
PROCEDURE analyse PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER ttLine             FOR ttLine.
  DEFINE BUFFER ttCallee           FOR ttCallee.
  DEFINE BUFFER bufPreviousLine    FOR ttLine.

  DEFINE VARIABLE cLine                   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLineWithoutExtraGaps   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSep                    AS CHARACTER  NO-UNDO INITIAL " @ ".
  DEFINE VARIABLE cSep2                   AS CHARACTER  NO-UNDO INITIAL " - ".
  DEFINE VARIABLE iLineNr                 AS INTEGER    NO-UNDO. 
  DEFINE VARIABLE cCaller                 AS CHARACTER  NO-UNDO. 
  DEFINE VARIABLE cCodeBlock              AS CHARACTER  NO-UNDO. 
  DEFINE VARIABLE cDateFormat             AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE deDurationTotal         AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDurationTotal          AS CHARACTER  NO-UNDO. 
  DEFINE VARIABLE iHours                  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE deSeconds               AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iMinutes                AS INTEGER    NO-UNDO.
  DEFINE VARIABLE rPreviousTtLine         AS ROWID      NO-UNDO.

  SESSION:SET-WAIT-STATE("GENERAL").

  EMPTY TEMP-TABLE ttLine.
  EMPTY TEMP-TABLE ttCallee.


  ASSIGN cDateFormat         = SESSION:DATE-FORMAT /* datetime calculations, format has to be ymd temporarily */
         SESSION:DATE-FORMAT = 'ymd'.

  INPUT FROM VALUE(INPUT FRAME {&FRAME-NAME} FI-log).

  REPEAT ON ERROR UNDO, THROW:

    IMPORT UNFORMATTED cLine.

    IF ENTRY(6,cLine,' ') <> "4GLTRACE" THEN NEXT.
    
    IF rPreviousTtLine NE ? THEN FIND bufPreviousLine WHERE ROWID(bufPreviousLine) = rPreviousTtLine.

    cLineWithoutExtraGaps = cLine. 

    DO WHILE INDEX(cLineWithoutExtraGaps,'  ') > 0:
       cLineWithoutExtraGaps = REPLACE(cLineWithoutExtraGaps,'  ',' ').
    END.

    CREATE ttLine.                   
    ASSIGN ttLine.EventType         = IF ENTRY(7,cLineWithoutExtraGaps,' ') = 'RETURN' THEN 'RETURN FROM' 
                                      ELSE IF ENTRY(7,cLineWithoutExtraGaps,' ') = 'RUN' AND ENTRY(9,cLineWithoutExtraGaps,' ') = 'PERSIST' THEN 'RUN PERSISTENT' 
                                      ELSE CAPS(ENTRY(7,cLineWithoutExtraGaps,' '))
           cCaller                  = IF ttLine.EventType = 'RETURN FROM' THEN '' 
                                      ELSE REPLACE(SUBSTRING(cLine,R-INDEX(cLine,'[') + 1),']','') /* get the last part of the line, between [] */
           ttLine.CallerLineNo      = IF INDEX(cCaller,cSep) NE 0 
                                      THEN SUBSTRING(cCaller,
                                                     INDEX(cCaller,cSep) + LENGTH(cSep),
                                                     LENGTH(cCaller) - INDEX(cCaller,cSep2) - LENGTH(cSep)) 
                                      ELSE '' 
           cCaller                  = IF ttLine.EventType = 'RETURN FROM' THEN ''
                                      ELSE IF INDEX(cCaller,cSep) NE 0 THEN SUBSTRING(cCaller,1,INDEX(cCaller,cSep)) 
                                      ELSE cCaller
           cCodeBlock               = IF ttLine.EventType = 'RETURN FROM' 
                                      THEN (
                                            ENTRY(9,cLineWithoutExtraGaps,' ') + (IF ENTRY(9,cLineWithoutExtraGaps,' ') = 'main' 
                                                                                  AND ENTRY(10,cLineWithoutExtraGaps,' ') = 'block' 
                                                                                  THEN ' block' 
                                                                                  ELSE IF ENTRY(9,cLineWithoutExtraGaps,' ') = ' PUBLISH'
                                                                                  THEN ENTRY(10,cLineWithoutExtraGaps,' ')
                                                                                  ELSE '')
                                            + ' - ' 
                                            + TRIM(ENTRY(NUM-ENTRIES(cLineWithoutExtraGaps,' '),cLineWithoutExtraGaps,' '),'[]')
                                           )
                                      ELSE /* get de last part of the line after the eventtype*/         
                                           SUBSTRING(cLine,
                                                     INDEX(cLine,
                                                           ' ' + ttLine.EventType + ' '
                                                           ) 
                                                     + LENGTH(ttLine.EventType) + 2
                                                     )
           cCodeBlock               = IF ttLine.EventType = 'RETURN FROM' THEN cCodeBlock
                                      ELSE IF ttLine.EventType = 'RUN PERSISTENT' THEN ENTRY(8,cLineWithoutExtraGaps,' ') 
                                      /* extract the callee from cCodeBlock */
                                      ELSE IF ENTRY(9,cLineWithoutExtraGaps,' ') = 'in'
                                      THEN (ENTRY(8,cLineWithoutExtraGaps,' ') + cSep2 + ENTRY(10,cLineWithoutExtraGaps,' '))
                                      ELSE IF ttLine.EventType = 'NEW' THEN ENTRY(8,cLineWithoutExtraGaps,' ')
                                      ELSE IF INDEX(cCaller,cSep2) NE 0 
                                          THEN (ENTRY(8,cLineWithoutExtraGaps,' ') 
                                                + cSep2 
                                                + SUBSTRING(cCaller,
                                                            INDEX(cCaller,cSep2) + LENGTH(cSep2), 
                                                            LENGTH(cCaller) - INDEX(cCaller,cSep2) - LENGTH(cSep2)
                                                            )
                                               ) 
                                      ELSE IF INDEX(cCodeBlock,'[') > 0 THEN SUBSTRING(cCodeBlock,1,INDEX(cCodeBlock,'[') - 2)  
                                      ELSE '** error interpreting line! **' + cCodeBlock
           ttLine.Params            = IF INDEX(cLine,'"') NE 0 THEN SUBSTRING(cLine,INDEX(cLine,'"'),R-INDEX(cLine,'~"') - INDEX(cLine,'"') + 1 ) ELSE ''
           ttLine.Line              = cLine
           ttLine.CodeBlock         = cCodeBlock
           ttLine.CallerName        = cCaller 
           
           ttLine.EventTime         = DATETIME(SUBSTRING(cLine,2,8) + ' ' + SUBSTRING(cLine,11,12))
           ttLine.EventDuration     = IF AVAILABLE bufPreviousLine THEN INTERVAL(ttLine.EventTime,bufPreviousLine.EventTime,'milliseconds') ELSE 0
           deDurationTotal  /* s */ = deDurationTotal + (ttLine.EventDuration / 1000)
           rPreviousTtLine          = ROWID(ttLine). 

  END. /* REPEAT */

  ASSIGN iHours         = TRUNCATE(deDurationTotal / 3600, 0)
         iMinutes       = TRUNCATE((deDurationTotal - (iHours * 3600)) / 60, 0)
         deSeconds      = (deDurationTotal - (iHours * 3600) - (iMinutes * 60)) 
         cDurationTotal = STRING(iHours,'99') + ':' + STRING(iMinutes,'99') + ':' + STRING(deSeconds,'99.999').

  FOR EACH ttLine 
      BREAK BY ttLine.CodeBlock  
            BY ttLine.EventType
      ON ERROR UNDO, THROW:

    IF FIRST-OF(ttLine.EventType) THEN DO:
        CREATE ttCallee.
        ASSIGN ttCallee.CodeBlock = ttLine.CodeBlock
               ttCallee.EventType = ttLine.EventType.
    END.
    ELSE FIND ttCallee
           WHERE ttCallee.CodeBlock = ttLine.CodeBlock
             AND ttCallee.EventType = ttLine.EventType. 

    ASSIGN ttCallee.CallsTo = ttCallee.CallsTo + 1
           ttCallee.TotTime = ttCallee.TotTime + (ttLine.EventDuration / 1000).


    IF LAST-OF(ttLine.EventType)
    THEN ASSIGN ttCallee.AvgTime     = ttCallee.TotTime / ttCallee.CallsTo
                ttCallee.SessionPerc = 100 * (ttCallee.TotTime / deDurationTotal).
  END.

  FI-RunTime:SCREEN-VALUE = cDurationTotal.

  APPLY 'start-search':U TO BR-Callee.

  IF AVAILABLE ttCallee THEN RUN openQueryLines.

  CATCH ProError AS Progress.Lang.ProError: 
    MESSAGE 'Error, cannot proceed analysing ' + '~n' +  ProError:GetMessage(1) VIEW-AS ALERT-BOX ERROR.
    DELETE OBJECT ProError. /* cannot afford to create leaks myself ;-) */
  END CATCH.

  FINALLY: 
    SESSION:DATE-FORMAT = cDateFormat.
    INPUT CLOSE.
    SESSION:SET-WAIT-STATE("").
  END FINALLY.

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
  DISPLAY FI-log FI-RunTime FI-search 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BU-chooseLog FI-search BR-Callee BR-Lines 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize C-Win 
PROCEDURE initialize PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     fill cColumns with columnnames, used in start-search trigger BR-Callee
               set current-column BR-Callee to sessionperc
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE hColumn AS HANDLE      NO-UNDO.

   ASSIGN hColumn           = BR-Callee:FIRST-COLUMN IN FRAME {&FRAME-NAME}.
          hColumn:READ-ONLY = TRUE. /* undo enable in display trigger */

   DO WHILE VALID-HANDLE(hColumn):
      
      IF hColumn:NAME = 'sessionperc' THEN DO: 
        APPLY 'ENTRY':U TO hColumn. /* set CURRENT-COLUMN to sessionperc */
        hColumn:READ-ONLY = TRUE. /* undo enable in display trigger */
      END.

      ASSIGN cColumns = (IF cColumns NE '' THEN cColumns + ',' ELSE '') + hColumn:NAME
             hColumn  = hColumn:NEXT-COLUMN.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryLines C-Win 
PROCEDURE openQueryLines PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&OPEN-QUERY-BR-Lines}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION repositionBR-Callee C-Win 
FUNCTION repositionBR-Callee RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF rTtCallee NE ? THEN DO WITH FRAME {&FRAME-NAME}:
      BR-Callee:SET-REPOSITIONED-ROW(BR-Callee:FOCUSED-ROW,"CONDITIONAL").
      REPOSITION BR-Callee TO ROWID rTtCallee.
  END.

  RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

