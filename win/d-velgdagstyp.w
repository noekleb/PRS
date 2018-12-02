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
DEFINE INPUT-OUTPUT PARAMETER dDato AS DATE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE dJmfDato AS DATE  NO-UNDO.
DEFINE VARIABLE cReturVerdi AS CHARACTER INIT "AVBRYT" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-53 Btn_OK RS-Velg Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS RS-Velg FI-StartRapport FI-Dato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD veckodag Dialog-Frame 
FUNCTION veckodag RETURNS CHARACTER
  ( INPUT dDato AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Startdato sammenligning" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StartRapport AS CHARACTER FORMAT "X(256)":U 
     LABEL "Startdato rapport" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Velg AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Omsetningsrapport", 1,
"Periodesammenligning", 2
     SIZE 30 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SokDato AT ROW 5.29 COL 41
     Btn_OK AT ROW 1.48 COL 60
     RS-Velg AT ROW 2.19 COL 6 NO-LABEL
     Btn_Cancel AT ROW 2.71 COL 60
     FI-StartRapport AT ROW 4.1 COL 26 COLON-ALIGNED
     Btn_Help AT ROW 4.71 COL 60
     FI-Dato AT ROW 5.29 COL 26 COLON-ALIGNED
     RECT-53 AT ROW 1.48 COL 2
     SPACE(19.79) SKIP(0.17)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg rapporttype"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokDato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Dato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StartRapport IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg rapporttype */
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
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    IF RS-Velg:SCREEN-VALUE = "1" THEN
        ASSIGN dDato = ?.
    ELSE DO:
        ASSIGN INPUT FI-Dato.
        IF FI-Dato = ? THEN DO:
            MESSAGE "Ange dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-Dato.
            ASSIGN FI-Dato:CURSOR-OFFSET = 1.
            RETURN NO-APPLY.
        END.
        ELSE IF dDato = dJmfDato THEN DO:
            MESSAGE "Rapportens startdato = dato jämförelse."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-Dato.
            RETURN NO-APPLY.
        END.
/*         ELSE IF WEEKDAY(FI-Dato) <> WEEKDAY(dJmfDato) THEN DO:                       */
/*             MESSAGE "Veckodag för rapportens startdato <> veckodag jämförelse." SKIP */
/*                 "(" veckodag(dJmfDato) " - " veckodag(FI-Dato) ")"                   */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
/*             APPLY "ENTRY" TO FI-Dato.                                                */
/*             RETURN NO-APPLY.                                                         */
/*         END.                                                                         */
    END.
    ASSIGN dDato       = FI-Dato
           cReturVerdi = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato Dialog-Frame
ON CHOOSE OF BUTTON-SokDato IN FRAME Dialog-Frame /* ... */
or F10 of FI-Dato
DO:
  DEFINE VARIABLE dDato AS DATE NO-UNDO.
  ASSIGN INPUT FI-Dato
         dDato = IF FI-Dato = ? THEN dJmfDato ELSE FI-Dato.
/*       date(FI-FraDat:screen-value in frame {&FRAME-NAME}). */

  do with frame {&FRAME-NAME}:  

  /* Start søkeprogram */
      RUN kalender.w (INPUT-OUTPUT dDato, "Datosøk").
      IF RETURN-VALUE = "<avbryt>" THEN
          RETURN NO-APPLY.
      IF dDato = dJmfDato THEN DO:
          MESSAGE "Rapportens startdato = dato jämförelse."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE IF WEEKDAY(dDato) <> WEEKDAY(dJmfDato) THEN DO:
          MESSAGE "Veckodag för rapportens startdato <> veckodag jämförelse." SKIP
              "(" veckodag(dJmfDato) " - " veckodag(dDato) ")"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ASSIGN FI-Dato:SCREEN-VALUE = STRING(dDato).
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Velg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Velg Dialog-Frame
ON VALUE-CHANGED OF RS-Velg IN FRAME Dialog-Frame
DO:
  ASSIGN FI-Dato:SENSITIVE = SELF:SCREEN-VALUE = "2"
         BUTTON-SokDato:SENSITIVE = FI-Dato:SENSITIVE.
  IF SELF:SCREEN-VALUE = "2" THEN DO:
      APPLY "ENTRY" TO FI-Dato.
      ASSIGN FI-Dato:CURSOR-OFFSET = 1.
  END.
  ELSE DO:
      ASSIGN FI-Dato = ?.
      DISPLAY FI-Dato WITH FRAME {&FRAME-NAME}.
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
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    ASSIGN dJmfDato = dDato
           dDato = ?
           FI-StartRapport = STRING(dJmfDato) + " " + veckodag(dJmfDato).
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cReturVerdi.

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
  DISPLAY RS-Velg FI-StartRapport FI-Dato 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-53 Btn_OK RS-Velg Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION veckodag Dialog-Frame 
FUNCTION veckodag RETURNS CHARACTER
  ( INPUT dDato AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN ENTRY(WEEKDAY(dDato),"söndag,måndag,tisdag,onsdag,torsdag,fredag,lördag").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

