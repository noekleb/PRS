&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR wAntal AS INTE INIT 1234 NO-UNDO.
    DEFINE VAR             wFraBut  AS CHAR    NO-UNDO.
    DEFINE VAR             wFraStrl AS CHAR    NO-UNDO.
    DEFINE VAR             wTilBut  AS CHAR    NO-UNDO.
    DEFINE VAR             wTilStrl AS CHAR    NO-UNDO.
&ELSE 
    DEFINE INPUT-OUTPUT PARAMETER wAntal   AS INTE NO-UNDO.
    DEFINE INPUT        PARAMETER wFraBut  AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER wFraStrl AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER wTilBut  AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER wTilStrl AS CHAR    NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */
DEFINE VAR ch_Spin   AS COM-HANDLE NO-UNDO.
DEFINE VAR Ret-Value AS CHAR INIT "<Avbryt>" NO-UNDO.
define var wStrBytte as log  no-undo.
define var wTekst    as char no-undo.
DEFINE VARIABLE iMaxAntal AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-Antal Btn_OK Btn_Cancel Btn_Help ~
FILL-IN-FRB FILL-IN-TIB FILL-IN-FRS TOGGLE-Byt FILL-IN-TIS B-SpinUp ~
B-SpinDown 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Antal FILL-IN-FRB FILL-IN-TIB ~
FILL-IN-FRS TOGGLE-Byt FILL-IN-TIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SpinDown 
     LABEL "-" 
     SIZE 4.4 BY .71
     FONT 6.

DEFINE BUTTON B-SpinUp 
     LABEL "+" 
     SIZE 4.4 BY .71
     FONT 6.

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

DEFINE VARIABLE FILL-IN-Antal AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra størrelse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til størrelse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE TOGGLE-Byt AS LOGICAL INITIAL no 
     LABEL "Bytte størrelse" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-Antal AT ROW 2.19 COL 15 COLON-ALIGNED
     Btn_OK AT ROW 1.48 COL 42.2
     Btn_Cancel AT ROW 2.67 COL 42.2
     Btn_Help AT ROW 4.1 COL 42.2
     FILL-IN-FRB AT ROW 4.33 COL 12 COLON-ALIGNED
     FILL-IN-TIB AT ROW 5.52 COL 12 COLON-ALIGNED
     FILL-IN-FRS AT ROW 6.71 COL 12 COLON-ALIGNED
     TOGGLE-Byt AT ROW 6.71 COL 32
     FILL-IN-TIS AT ROW 7.91 COL 12 COLON-ALIGNED
     B-SpinUp AT ROW 1.95 COL 28.8
     B-SpinDown AT ROW 2.67 COL 28.8
     SPACE(25.59) SKIP(6.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Angi/bekreft antall"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-FRB:MANUAL-HIGHLIGHT IN FRAME Dialog-Frame = TRUE
       FILL-IN-FRB:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       FILL-IN-FRS:MANUAL-HIGHLIGHT IN FRAME Dialog-Frame = TRUE
       FILL-IN-FRS:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       FILL-IN-TIB:MANUAL-HIGHLIGHT IN FRAME Dialog-Frame = TRUE
       FILL-IN-TIB:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       FILL-IN-TIS:MANUAL-HIGHLIGHT IN FRAME Dialog-Frame = TRUE
       FILL-IN-TIS:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       TOGGLE-Byt:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Angi/bekreft antall */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinDown Dialog-Frame
ON CHOOSE OF B-SpinDown IN FRAME Dialog-Frame /* - */
DO:
    IF INPUT FILL-IN-Antal > 0 THEN
        ASSIGN FILL-in-Antal:SCREEN-VALUE = STRING(INPUT FILL-IN-Antal - 1)
               wAntal = INPUT FILL-IN-Antal.
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinUp Dialog-Frame
ON CHOOSE OF B-SpinUp IN FRAME Dialog-Frame /* + */
DO:
    IF INPUT FILL-IN-Antal < iMaxAntal THEN
        ASSIGN FILL-IN-Antal:SCREEN-VALUE = STRING(INPUT FILL-IN-Antal + 1)
               wAntal = INPUT FILL-IN-Antal.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    IF NOT TOGGLE-Byt:HIDDEN AND NOT TOGGLE-Byt:CHECKED AND 
           INPUT FILL-IN-Antal > 0 THEN DO:
        MESSAGE "Markera för storleksbyte." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ASSIGN Ret-Value = "<OK>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Antal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Antal Dialog-Frame
ON LEAVE OF FILL-IN-Antal IN FRAME Dialog-Frame /* Antall */
DO:
    IF INPUT FILL-IN-Antal > iMaxantal THEN DO:
        MESSAGE "För stort värde, max = " STRING(iMaxantal,">,>>9")
                               VIEW-AS ALERT-BOX ERROR TITLE "".
        ASSIGN FILL-IN-Antal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iMaxantal).
        RETURN NO-APPLY.
    END.
    ASSIGN wAntal = INT(FILL-IN-Antal:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 3 3 1 wTekst}
if can-do("Ja, yes,true, J,Yep",trim(wTekst)) then
  wStrBytte = true.
else
  wStrBytte = false.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  if wStrBytte then
    TOGGLE-Byt = true.
  iMaxAntal = wAntal.
  RUN enable_UI.
  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-FRB:BGCOLOR = 15
            FILL-IN-TIB:BGCOLOR = 15
            FILL-IN-FRS:BGCOLOR = 15
            FILL-IN-TIS:BGCOLOR = 15.
      ASSIGN FILL-IN-Antal:SCREEN-VALUE = STRING(wAntal)
             FILL-IN-FRB:SCREEN-VALUE = wFraBut
             FILL-IN-TIB:SCREEN-VALUE = wTilBut
             FILL-IN-FRS:SCREEN-VALUE = wFraStrl
             FILL-IN-TIS:SCREEN-VALUE = wTilStrl
             TOGGLE-Byt:HIDDEN = NOT wFraBut = wTilBut.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    MESSAGE Ret-Value SKIP 
            wAntal VIEW-AS ALERT-BOX.
&ELSE 
    RETURN Ret-Value.
&ENDIF

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
  DISPLAY FILL-IN-Antal FILL-IN-FRB FILL-IN-TIB FILL-IN-FRS TOGGLE-Byt 
          FILL-IN-TIS 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-Antal Btn_OK Btn_Cancel Btn_Help FILL-IN-FRB FILL-IN-TIB 
         FILL-IN-FRS TOGGLE-Byt FILL-IN-TIS B-SpinUp B-SpinDown 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

