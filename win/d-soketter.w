&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Syntax:
  Forfatter: 
  Programtype: 
  Beskrivelse:
  Parametere:
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

DEF INPUT        PARAMETER wh-Editor  AS WIDGET NO-UNDO.
DEF INPUT-OUTPUT PARAMETER wSokeTekst AS CHAR   NO-UNDO.
DEF OUTPUT       PARAMETER wSokeFlagg AS INTE   NO-UNDO.
/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.

define var m-ja              as logical no-undo.
define var m-i               as integer no-undo.
define var m-x               as character no-undo.
define var m-handle          as handle no-undo.
define var m-wh              as widget-handle no-undo.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 FILL-IN-1 Btn_Sok TOGGLE-1 ~
RADIO-SET-1 Btn_Avbryt Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 TOGGLE-1 RADIO-SET-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 18 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Hjelp" 
     SIZE 18 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Sok DEFAULT 
     LABEL "Søk &etter neste" 
     SIZE 18 BY 1.1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Søk etter" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&Opp", 2,
"&Ned", 1
     SIZE 9 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 2.62.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Skill &mellom store og små bokstaver" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-1 AT ROW 1.48 COL 10 COLON-ALIGNED
     Btn_Sok AT ROW 1.48 COL 55
     TOGGLE-1 AT ROW 4.57 COL 2
     RADIO-SET-1 AT ROW 3.38 COL 43 NO-LABEL
     Btn_Avbryt AT ROW 2.91 COL 55
     Btn_Help AT ROW 4.33 COL 55
     RECT-1 AT ROW 2.91 COL 41
     "Retning" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.67 COL 42
     SPACE(23.59) SKIP(2.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søk etter"
         CANCEL-BUTTON Btn_Avbryt.


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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON F10 OF FRAME Dialog-Frame /* Søk etter */
DO:
  APPLY "CHOOSE" TO Btn_Sok IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Søk etter */
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


&Scoped-define SELF-NAME Btn_Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sok Dialog-Frame
ON CHOOSE OF Btn_Sok IN FRAME Dialog-Frame /* Søk etter neste */
DO:
  ASSIGN 
     wSokeFlagg = INT(RADIO-SET-1:SCREEN-VALUE) + FIND-WRAP-AROUND + FIND-SELECT + INT(INPUT TOGGLE-1:CHECKED) * 4
     wSokeTekst = FILL-IN-1:SCREEN-VALUE.
  IF NOT wh-Editor:SEARCH(wSokeTekst,wSokeFlagg ) 
  THEN MESSAGE 'Finner ikke "' wSokeTekst '".' VIEW-AS ALERT-BOX INFORMATION TITLE "Søk".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{defaultbtn.i Btn_Sok} /* Setter evt. default-knapp */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-1 = wSokeTekst.
  END.   
  
  {lng.i} RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

  /* All kode som skal utføres når Btn_OK er trykket skal ligge her.
     Avbryt-knappen og window-close hopper ut av denne blokka fordi de
     er definert som endkey-hendelser. */   
  /*run sd-start.*/
  retur-verdi = "jeg er en retur-verdi".

END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

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
  DISPLAY FILL-IN-1 TOGGLE-1 RADIO-SET-1 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 FILL-IN-1 Btn_Sok TOGGLE-1 RADIO-SET-1 Btn_Avbryt Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

