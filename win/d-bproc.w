&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:   SJ, 19.04.99
  Beskrivelse: Programinformasjon - Viser persistente prosedyrer m.m.
  Parametere:  
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */

DEFINE TEMP-TABLE tPrg NO-UNDO
   FIELD tNummer     AS INTE 
   FIELD th          AS HANDLE
   FIELD twh         AS WIDGET
   FIELD tProgram    AS CHAR
   FIELD tWinTit     AS CHAR
   FIELD tWinStatus  AS LOGI 
   INDEX tNummer IS PRIMARY UNIQUE
             tNummer ASCENDING.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Prg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tPrg

/* Definitions for BROWSE BROWSE-Prg                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Prg tPrg.tNummer SEARCH(tPrg.tProgram) tPrg.tWinTit tPrg.tWinStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Prg   
&Scoped-define SELF-NAME BROWSE-Prg
&Scoped-define QUERY-STRING-BROWSE-Prg FOR EACH tPrg NO-LOCK BY tPrg.tNummer DESCENDING
&Scoped-define OPEN-QUERY-BROWSE-Prg OPEN QUERY {&SELF-NAME} FOR EACH tPrg NO-LOCK BY tPrg.tNummer DESCENDING.
&Scoped-define TABLES-IN-QUERY-BROWSE-Prg tPrg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Prg tPrg


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Prg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 BROWSE-Prg SELECT-IntProc ~
SELECT-Para Btn_OK Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Type FILL-IN-Returns SELECT-Para 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CommandLine Dialog-Frame 
FUNCTION CommandLine RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 14 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Lukk" 
     SIZE 14 BY 1.1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-Returns AS CHARACTER FORMAT "X(256)":U 
     LABEL "Returns" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120 BY 10.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120 BY 13.1.

DEFINE VARIABLE SELECT-IntProc AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 59 BY 11.91 NO-UNDO.

DEFINE VARIABLE SELECT-Para AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 55 BY 8.57 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Prg FOR 
      tPrg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Prg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Prg Dialog-Frame _FREEFORM
  QUERY BROWSE-Prg DISPLAY
      tPrg.tNummer    FORMAT "zzzz-"          LABEL "Nr "
      SEARCH(tPrg.tProgram)   FORMAT "X(40)"          LABEL " Navn"
      tPrg.tWinTit    FORMAT "X(56)"          LABEL "Tittel"
      tPrg.tWinStatus FORMAT "Hidden/Visible" LABEL "Status"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 115.8 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-Prg AT ROW 1.95 COL 4
     SELECT-IntProc AT ROW 12.67 COL 4 NO-LABEL
     FILL-IN-Type AT ROW 12.67 COL 72 COLON-ALIGNED
     FILL-IN-Returns AT ROW 13.86 COL 72 COLON-ALIGNED
     SELECT-Para AT ROW 16 COL 65 NO-LABEL
     Btn_OK AT ROW 25.29 COL 92
     Btn_Help AT ROW 25.29 COL 108
     "Programmer" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.24 COL 3
     "Internprosedyrer i ~"merket~" program" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.71 COL 3
     "Parametere" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 15.29 COL 65
     RECT-3 AT ROW 1.48 COL 2
     RECT-4 AT ROW 11.95 COL 2
     SPACE(0.00) SKIP(1.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "VS90 - Persistente programmer"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-Prg RECT-4 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-Returns IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Type IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST SELECT-IntProc IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Prg
/* Query rebuild information for BROWSE BROWSE-Prg
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tPrg NO-LOCK BY tPrg.tNummer DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Prg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* VS90 - Persistente programmer */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Prg
&Scoped-define SELF-NAME BROWSE-Prg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Prg Dialog-Frame
ON ROW-DISPLAY OF BROWSE-Prg IN FRAME Dialog-Frame
DO:
  ASSIGN tPrg.tWinTit:FONT IN BROWSE BROWSE-Prg = INT(STRING(tPrg.twh = CURRENT-WINDOW,"6/1")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Prg Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Prg IN FRAME Dialog-Frame
DO:
  ASSIGN SELECT-IntProc:LIST-ITEMS   = tPrg.th:INTERNAL-ENTRIES.         
  DISPL SELECT-IntProc WITH FRAME {&FRAME-NAME}.
  ASSIGN SELECT-IntProc:SCREEN-VALUE = ENTRY(1,tPrg.th:INTERNAL-ENTRIES).
  APPLY "VALUE-CHANGED" TO SELECT-IntProc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-IntProc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-IntProc Dialog-Frame
ON VALUE-CHANGED OF SELECT-IntProc IN FRAME Dialog-Frame
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-Type:SCREEN-VALUE    =      ENTRY(1,tPrg.th:GET-SIGNATURE(SELF:SCREEN-VALUE))
            FILL-IN-Returns:SCREEN-VALUE = CAPS(ENTRY(2,tPrg.th:GET-SIGNATURE(SELF:SCREEN-VALUE))).
     SELECT-PARA:LIST-ITEMS = SUBSTR(tPrg.th:GET-SIGNATURE(SELF:SCREEN-VALUE),LENGTH(ENTRY(1,tPrg.th:GET-SIGNATURE(SELF:SCREEN-VALUE))) 
                                              + LENGTH(ENTRY(2,tPrg.th:GET-SIGNATURE(SELF:SCREEN-VALUE))) + 3).       
  END.          
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
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN Initiering.
  {lng.i} RUN enable_UI.
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Prg.
  WAIT-FOR GO, END-ERROR, ENDKEY OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN NO-APPLY.

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
  DISPLAY FILL-IN-Type FILL-IN-Returns SELECT-Para 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-4 BROWSE-Prg SELECT-IntProc SELECT-Para Btn_OK Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Dialog-Frame 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     Initierer skjermbildet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR i  AS INTE NO-UNDO.
   DEF VAR h  AS HANDLE NO-UNDO.
   DEF VAR wh AS WIDGET NO-UNDO.     
   
   ASSIGN h = SESSION:FIRST-PROCEDURE.
   DO WHILE VALID-HANDLE(h):
      CREATE tPrg.
      ASSIGN i = i + 1
             tPrg.tNummer    = i
             tPrg.th         = h
             tPrg.twh        = h:CURRENT-WINDOW
             tPrg.tProgram   = h:FILE-NAME
             wh              = h:CURRENT-WINDOW
             tPrg.tWinTit    = IF VALID-HANDLE(wh) THEN wh:TITLE ELSE ""
             tPrg.tWinStatus = IF VALID-HANDLE(wh) THEN wh:HIDDEN ELSE ?
             h               = h:NEXT-SIBLING.
   END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CommandLine Dialog-Frame 
FUNCTION CommandLine RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR mCmdLine AS MEMPTR NO-UNDO.
  RUN GetCommandLineA (OUTPUT mCmdLine).
  RETURN GET-STRING(mCmdLine,1).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

