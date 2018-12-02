&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_StrKonv NO-UNDO LIKE StrKonv.


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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-StrKonv

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_StrKonv

/* Definitions for BROWSE BROWSE-StrKonv                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-StrKonv TT_StrKonv.StrKode ~
TT_StrKonv.Storl TT_StrKonv.Merknad 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-StrKonv 
&Scoped-define OPEN-QUERY-BROWSE-StrKonv OPEN QUERY BROWSE-StrKonv FOR EACH TT_StrKonv NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-StrKonv TT_StrKonv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-StrKonv TT_StrKonv


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-StrKonv}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-StrKonv Btn_Cancel FI-Info 
&Scoped-Define DISPLAYED-OBJECTS FI-Merknad FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Temp 
     LABEL "Temporärt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Lagra" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U INITIAL "Välj Temporärt sedan Lagra" 
      VIEW-AS TEXT 
     SIZE 36 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Merknad AS CHARACTER FORMAT "X(256)":U INITIAL "Skapad av fixrutin" 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-StrKonv FOR 
      TT_StrKonv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-StrKonv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-StrKonv Dialog-Frame _STRUCTURED
  QUERY BROWSE-StrKonv NO-LOCK DISPLAY
      TT_StrKonv.StrKode FORMAT "999":U
      TT_StrKonv.Storl FORMAT "X(4)":U WIDTH 14.6
      TT_StrKonv.Merknad FORMAT "X(60)":U WIDTH 24.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 15.48 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Merknad AT ROW 3.14 COL 12 COLON-ALIGNED
     BROWSE-StrKonv AT ROW 5.05 COL 1
     Btn_OK AT ROW 5.05 COL 57
     B-Temp AT ROW 6.95 COL 57
     Btn_Cancel AT ROW 8.86 COL 57
     FI-Info AT ROW 1.48 COL 3 NO-LABEL
     SPACE(32.99) SKIP(18.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Skapa StrKonv"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_StrKonv T "?" NO-UNDO skotex StrKonv
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-StrKonv FI-Merknad Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Temp IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_OK IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Merknad IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-StrKonv
/* Query rebuild information for BROWSE BROWSE-StrKonv
     _TblList          = "Temp-Tables.TT_StrKonv"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_StrKonv.StrKode
     _FldNameList[2]   > Temp-Tables.TT_StrKonv.Storl
"Storl" ? ? "character" ? ? ? ? ? ? no ? no no "14.6" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.TT_StrKonv.Merknad
"Merknad" ? ? "character" ? ? ? ? ? ? no ? no no "24.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-StrKonv */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Skapa StrKonv */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Temp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Temp Dialog-Frame
ON CHOOSE OF B-Temp IN FRAME Dialog-Frame /* Temporärt */
DO:
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  FOR EACH strtstr BREAK BY StrTStr.SoStorl.
    IF FIRST-OF(sostorl) THEN DO:
        ASSIGN iCount = iCount + 1.
        CREATE TT_StrKonv.
        ASSIGN TT_StrKonv.StrKode = iCount
               TT_StrKonv.Storl   = StrTstr.SoStorl
               TT_StrKonv.Merknad = FI-Merknad:SCREEN-VALUE.
        RELEASE TT_StrKonv.
    END.
  END.
  ASSIGN SELF:SENSITIVE = FALSE
         Btn_OK:SENSITIVE = TRUE.

  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Lagra */
DO:
  FOR EACH TT_StrKonv.
      BUFFER-COPY TT_StrKonv TO StrKonv.
      RELEASE StrKonv.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-StrKonv
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
    IF CAN-FIND(FIRST StrKonv) THEN
        RUN InitTT_StrKonv. /* visa redan registrerade StrKonv */
                            /* Fixrutin redan körd */
  RUN enable_UI.
  IF NOT CAN-FIND(FIRST TT_StrKonv) THEN
      ASSIGN B-Temp:SENSITIVE = TRUE
             FI-Merknad:SENSITIVE = TRUE.
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
  DISPLAY FI-Merknad FI-Info 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-StrKonv Btn_Cancel FI-Info 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTT_StrKonv Dialog-Frame 
PROCEDURE InitTT_StrKonv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH StrKonv NO-LOCK:
        BUFFER-COPY StrKonv TO TT_StrKonv.
        RELEASE TT_StrKonv.
    END.
    ASSIGN FI-Info = "Fixrutin redan körd"
           FI-Merknad = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

