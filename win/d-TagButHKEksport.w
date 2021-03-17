&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-Butiker NO-UNDO LIKE Butiker
       FIELD NyElValgt AS CHAR.
DEFINE TEMP-TABLE tt-Valgte NO-UNDO LIKE Butiker
       FIELD NyElValgt AS CHAR.


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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  define VAR      cButListe as char init "1,51,260" no-undo.
  DEFINE VARIABLE cValgteListe AS CHARACTER  NO-UNDO.
&ELSE
  define INPUT  parameter cButListe as char  no-undo.
  define OUTPUT parameter cValgteListe as char  no-undo.
&ENDIF

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
DEFINE BUFFER btt-butiker FOR tt-butiker.
DEFINE BUFFER btt-valgte FOR tt-valgte.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Butiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-Butiker tt-Valgte

/* Definitions for BROWSE BROWSE-Butiker                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Butiker tt-Butiker.Butik tt-Butiker.ButNamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Butiker   
&Scoped-define SELF-NAME BROWSE-Butiker
&Scoped-define QUERY-STRING-BROWSE-Butiker FOR EACH tt-Butiker
&Scoped-define OPEN-QUERY-BROWSE-Butiker OPEN QUERY BROWSE-Butiker FOR EACH tt-Butiker.
&Scoped-define TABLES-IN-QUERY-BROWSE-Butiker tt-Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Butiker tt-Butiker


/* Definitions for BROWSE BROWSE-Valgte                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Valgte tt-Valgte.Butik tt-Valgte.ButNamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Valgte   
&Scoped-define SELF-NAME BROWSE-Valgte
&Scoped-define QUERY-STRING-BROWSE-Valgte FOR EACH tt-Valgte
&Scoped-define OPEN-QUERY-BROWSE-Valgte OPEN QUERY BROWSE-Valgte FOR EACH tt-Valgte.
&Scoped-define TABLES-IN-QUERY-BROWSE-Valgte tt-Valgte
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Valgte tt-Valgte


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Butiker}~
    ~{&OPEN-QUERY-BROWSE-Valgte}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Fra BUTTON-Til BROWSE-Butiker ~
BROWSE-Valgte Btn_OK Btn_Cancel Btn_Help RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Fra  NO-FOCUS
     LABEL " <" 
     SIZE 4 BY 1.14.

DEFINE BUTTON BUTTON-Til  NO-FOCUS
     LABEL " >" 
     SIZE 4 BY 1.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 21.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Butiker FOR 
      tt-Butiker SCROLLING.

DEFINE QUERY BROWSE-Valgte FOR 
      tt-Valgte SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Butiker Dialog-Frame _FREEFORM
  QUERY BROWSE-Butiker DISPLAY
      tt-Butiker.Butik
tt-Butiker.ButNamn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 30 BY 19.24 ROW-HEIGHT-CHARS .61 EXPANDABLE.

DEFINE BROWSE BROWSE-Valgte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Valgte Dialog-Frame _FREEFORM
  QUERY BROWSE-Valgte DISPLAY
      tt-Valgte.Butik
tt-Valgte.ButNamn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 30 BY 19.24 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-Fra AT ROW 7.76 COL 36.8 NO-TAB-STOP 
     BUTTON-Til AT ROW 5.86 COL 36.8 NO-TAB-STOP 
     BROWSE-Butiker AT ROW 2.71 COL 4
     BROWSE-Valgte AT ROW 2.71 COL 43
     Btn_OK AT ROW 23.14 COL 2
     Btn_Cancel AT ROW 23.14 COL 17.8
     Btn_Help AT ROW 23.14 COL 60
     RECT-2 AT ROW 1.48 COL 2
     "Tilgjenglige butikker" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.86 COL 6
          FONT 6
     "Valgte butikker" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 1.86 COL 46
          FONT 6
     SPACE(5.00) SKIP(21.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg butikker"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-Butiker T "?" NO-UNDO SkoTex Butiker
      ADDITIONAL-FIELDS:
          FIELD NyElValgt AS CHAR
      END-FIELDS.
      TABLE: tt-Valgte T "?" NO-UNDO SkoTex Butiker
      ADDITIONAL-FIELDS:
          FIELD NyElValgt AS CHAR
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
/* BROWSE-TAB BROWSE-Butiker BUTTON-Til Dialog-Frame */
/* BROWSE-TAB BROWSE-Valgte BROWSE-Butiker Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Butiker
/* Query rebuild information for BROWSE BROWSE-Butiker
     _START_FREEFORM
OPEN QUERY BROWSE-Butiker FOR EACH tt-Butiker.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Butiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Valgte
/* Query rebuild information for BROWSE BROWSE-Valgte
     _START_FREEFORM
OPEN QUERY BROWSE-Valgte FOR EACH tt-Valgte.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Valgte */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg butikker */
DO:
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  run AssignValgte.
  if return-value = "AVBRYT" OR cValgteListe = "" then
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Fra Dialog-Frame
ON CHOOSE OF BUTTON-Fra IN FRAME Dialog-Frame /*  < */
DO:
  BUFFER-COPY tt-Valgte USING Butik ButNamn TO btt-Butiker.
  RELEASE btt-Butiker.
  {&OPEN-QUERY-BROWSE-Butiker}
  DELETE tt-Valgte.
  BROWSE-Valgte:DELETE-SELECTED-ROWS().
  ASSIGN SELF:SENSITIVE = BROWSE-Valgte:FOCUSED-ROW <> ?
         BUTTON-Til:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Til
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Til Dialog-Frame
ON CHOOSE OF BUTTON-Til IN FRAME Dialog-Frame /*  > */
DO:
  BUFFER-COPY tt-Butiker USING Butik ButNamn NyElValgt TO btt-Valgte.
  RELEASE btt-Valgte.
  {&OPEN-QUERY-BROWSE-Valgte}
  DELETE tt-Butiker.
  BROWSE-Butiker:DELETE-SELECTED-ROWS().
  ASSIGN SELF:SENSITIVE = BROWSE-Butiker:FOCUSED-ROW <> ?
         BUTTON-Fra:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Butiker
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
  RUN InitTT.
  {lng.i} /* Oversettelse */
  RUN enable_UI.
  ASSIGN BUTTON-Til:SENSITIVE = BROWSE-Butiker:FOCUSED-ROW <> ?
         BUTTON-Fra:SENSITIVE = BROWSE-Valgte:FOCUSED-ROW <> ?.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  wRetur-Verdi = "OK".
END.
RUN disable_UI.
&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return wretur-verdi.
&else
 message wretur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignValgte Dialog-Frame 
PROCEDURE AssignValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tt-Valgte BY tt-Valgte.butik:
      ASSIGN cValgteListe = cValgteListe + (IF cValgteListe = "" THEN "" ELSE ",") + 
                          STRING(tt-Valgte.butik).
  END.
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
  ENABLE BUTTON-Fra BUTTON-Til BROWSE-Butiker BROWSE-Valgte Btn_OK Btn_Cancel 
         Btn_Help RECT-2 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTT Dialog-Frame 
PROCEDURE InitTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCount,cButListe)) NO-LOCK NO-ERROR.
      IF AVAIL Butiker THEN DO:
          BUFFER-COPY Butiker USING Butik ButNamn TO tt-Butiker.
          RELEASE tt-Butiker.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

