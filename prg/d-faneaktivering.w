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
DEFINE INPUT         PARAMETER cTexter  AS CHARACTER  NO-UNDO.
DEFINE INPUT         PARAMETER cDefault AS CHARACTER  NO-UNDO.
DEFINE INPUT         PARAMETER cSport1Default AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER cEnaDis  AS CHARACTER  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lLagreSysPara AS LOGICAL    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-62 Btn_OK TG-1 B-OkLagre TG-2 TG-3 ~
B-Alle TG-4 TG-5 B-Default TG-6 Btn_Cancel TG-7 TG-8 TG-9 TG-10 TG-11 TG-12 
&Scoped-Define DISPLAYED-OBJECTS TG-1 TG-2 TG-3 TG-4 TG-5 TG-6 TG-7 TG-8 ~
TG-9 TG-10 TG-11 TG-12 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Alle 
     LABEL "Marker alle" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Default 
     LABEL "Standard" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-OkLagre 
     LABEL "OK/Lagre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 13.1.

DEFINE VARIABLE TG-1 AS LOGICAL INITIAL no 
     LABEL "Tg 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-10 AS LOGICAL INITIAL no 
     LABEL "Tg 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-11 AS LOGICAL INITIAL no 
     LABEL "Tg 11" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-12 AS LOGICAL INITIAL no 
     LABEL "Tg 12" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-2 AS LOGICAL INITIAL no 
     LABEL "Tg 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-3 AS LOGICAL INITIAL no 
     LABEL "Tg 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-4 AS LOGICAL INITIAL no 
     LABEL "Tg 4" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-5 AS LOGICAL INITIAL no 
     LABEL "Tg 5" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-6 AS LOGICAL INITIAL no 
     LABEL "Tg 6" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-7 AS LOGICAL INITIAL no 
     LABEL "Tg 7" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-8 AS LOGICAL INITIAL no 
     LABEL "Tg 8" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-9 AS LOGICAL INITIAL no 
     LABEL "Tg 9" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.48 COL 38.6
     TG-1 AT ROW 2.24 COL 5.6
     B-OkLagre AT ROW 3.1 COL 38.6
     TG-2 AT ROW 3.24 COL 5.6
     TG-3 AT ROW 4.24 COL 5.6
     B-Alle AT ROW 4.81 COL 38.6
     TG-4 AT ROW 5.24 COL 5.6
     TG-5 AT ROW 6.24 COL 5.6
     B-Default AT ROW 6.43 COL 38.6
     TG-6 AT ROW 7.24 COL 5.6
     Btn_Cancel AT ROW 7.91 COL 38.6
     TG-7 AT ROW 8.24 COL 5.6
     TG-8 AT ROW 9.24 COL 5.6
     TG-9 AT ROW 10.24 COL 5.6
     TG-10 AT ROW 11.24 COL 5.6
     TG-11 AT ROW 12.24 COL 5.6
     TG-12 AT ROW 13.24 COL 5.6
     RECT-62 AT ROW 1.48 COL 2.4
     SPACE(17.39) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vis/skjul faner"
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
ON GO OF FRAME Dialog-Frame /* Vis/skjul faner */
DO:
  RUN OppdaterEnaDis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vis/skjul faner */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Alle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Alle Dialog-Frame
ON CHOOSE OF B-Alle IN FRAME Dialog-Frame /* Marker alle */
DO:
    ASSIGN TG-1:CHECKED = TRUE
           TG-2:CHECKED = TRUE
           TG-3:CHECKED = TRUE
           TG-4:CHECKED = TRUE
           TG-5:CHECKED = TRUE
           TG-6:CHECKED = TRUE
           TG-7:CHECKED = TRUE
           TG-8:CHECKED = TRUE
           TG-9:CHECKED = TRUE
           TG-10:CHECKED = TRUE
           TG-11:CHECKED = TRUE
           TG-12:CHECKED = TRUE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Default
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Default Dialog-Frame
ON CHOOSE OF B-Default IN FRAME Dialog-Frame /* Standard */
DO:
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DO ii = 4 TO NUM-ENTRIES(cSport1Default):
        CASE ii:
            WHEN 4 THEN
                TG-4:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 5 THEN
                TG-5:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 6 THEN
                TG-6:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 7 THEN
                TG-7:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 8 THEN
                TG-8:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 9 THEN
                TG-9:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 10 THEN
                TG-10:CHECKED = ENTRY(ii,cSport1Default) = "E".
            WHEN 11 THEN
                TG-11:CHECKED = ENTRY(ii,cSport1Default) = "E".
        END CASE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OkLagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OkLagre Dialog-Frame
ON CHOOSE OF B-OkLagre IN FRAME Dialog-Frame /* OK/Lagre */
DO:
    IF NOT TRANSACTION THEN
        lLagreSysPara = TRUE.
    APPLY "CHOOSE" TO Btn_OK.
    RETURN NO-APPLY.
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
  RUN InitToggle.
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
  DISPLAY TG-1 TG-2 TG-3 TG-4 TG-5 TG-6 TG-7 TG-8 TG-9 TG-10 TG-11 TG-12 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-62 Btn_OK TG-1 B-OkLagre TG-2 TG-3 B-Alle TG-4 TG-5 B-Default 
         TG-6 Btn_Cancel TG-7 TG-8 TG-9 TG-10 TG-11 TG-12 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitToggle Dialog-Frame 
PROCEDURE InitToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    DO ii = 1 TO NUM-ENTRIES(cTexter):
        CASE ii:
            WHEN 1  THEN ASSIGN TG-1:LABEL = ENTRY(ii,cTexter)
                                TG-1:CHECKED = ENTRY(ii,cEnaDis) = "E"
                                TG-1:SENSITIVE = FALSE.
            WHEN 2  THEN ASSIGN TG-2:LABEL = ENTRY(ii,cTexter)
                                TG-2:CHECKED = ENTRY(ii,cEnaDis) = "E"
                                TG-2:SENSITIVE = FALSE.
            WHEN 3  THEN ASSIGN TG-3:LABEL = ENTRY(ii,cTexter)
                                TG-3:CHECKED = ENTRY(ii,cEnaDis) = "E"
                                TG-3:SENSITIVE = FALSE.
            WHEN 4  THEN ASSIGN TG-4:LABEL = ENTRY(ii,cTexter)
                                TG-4:CHECKED = ENTRY(ii,cEnaDis) = "E"
                                TG-3:SENSITIVE = FALSE.
            WHEN 5  THEN ASSIGN TG-5:LABEL = ENTRY(ii,cTexter)
                                TG-5:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 6  THEN ASSIGN TG-6:LABEL = ENTRY(ii,cTexter)
                                TG-6:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 7  THEN ASSIGN TG-7:LABEL = ENTRY(ii,cTexter)
                                TG-7:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 8  THEN ASSIGN TG-8:LABEL = ENTRY(ii,cTexter)
                                TG-8:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 9  THEN ASSIGN TG-9:LABEL = ENTRY(ii,cTexter)
                                TG-9:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 10 THEN ASSIGN TG-10:LABEL = ENTRY(ii,cTexter)
                                TG-10:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 11 THEN ASSIGN TG-11:LABEL = ENTRY(ii,cTexter)
                                TG-11:CHECKED = ENTRY(ii,cEnaDis) = "E".
            WHEN 12 THEN ASSIGN TG-12:LABEL = ENTRY(ii,cTexter)
                                TG-12:CHECKED = ENTRY(ii,cEnaDis) = "E".
        END CASE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterEnaDis Dialog-Frame 
PROCEDURE OppdaterEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    DO ii = 1 TO NUM-ENTRIES(cEnadis):
        CASE ii:
            WHEN 1  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-1:CHECKED,"E/D").
            WHEN 2  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-2:CHECKED,"E/D").
            WHEN 3  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-3:CHECKED,"E/D").
            WHEN 4  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-4:CHECKED,"E/D").
            WHEN 5  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-5:CHECKED,"E/D").
            WHEN 6  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-6:CHECKED,"E/D").
            WHEN 7  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-7:CHECKED,"E/D").
            WHEN 8  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-8:CHECKED,"E/D").
            WHEN 9  THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-9:CHECKED,"E/D").
            WHEN 10 THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-10:CHECKED,"E/D").
            WHEN 11 THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-11:CHECKED,"E/D").
            WHEN 12 THEN ASSIGN ENTRY(ii,cEnaDis) = STRING(TG-11:CHECKED,"E/D").
        END CASE.
    END.
    IF lLagreSysPara = TRUE THEN DO:
        FIND SysPara WHERE SysPara.SysHId = 2 and
                           SysPara.SysGr  = 5 AND
                           SysPara.ParaNr = 101 NO-ERROR.
        IF AVAIL SysPara THEN
            SysPara.Parameter2 = cEnadis.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

