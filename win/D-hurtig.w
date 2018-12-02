&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:              d-hurtigtast.w
  Description:       Angivelse av hurtigtast for meny     
  Input Parameters:  INPUT CHAR wHurtigtast
  Output Parameters: (Returnerer ny tast)
  Author:            Sturla Johnsen
  Created:           juni 1998
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN  
    DEF VAR wHurtigTast AS CHAR NO-UNDO.
&ELSE
    DEF INPUT PARAMETER wHurtigTast AS CHAR NO-UNDO.
&ENDIF    
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-Hurtigtast BUTTON-OK ~
BUTTON-Nullstill BUTTON-Avbryt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 13 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Nullstill 
     LABEL "Nullstill" 
     SIZE 12 BY 1.1.

DEFINE BUTTON BUTTON-OK AUTO-GO 
     LABEL "OK" 
     SIZE 13 BY 1.1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-Hurtigtast AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-Hurtigtast AT ROW 2.43 COL 4 COLON-ALIGNED NO-LABEL
     BUTTON-OK AT ROW 3.86 COL 3
     BUTTON-Nullstill AT ROW 3.86 COL 17
     BUTTON-Avbryt AT ROW 3.86 COL 30
     "Trykk en hurtigtast (tastkombinasjon)" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 1.48 COL 5
     SPACE(2.59) SKIP(2.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Hurtigtast"
         CANCEL-BUTTON BUTTON-Avbryt.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-Hurtigtast IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Hurtigtast */
DO:
  ASSIGN wHurtigTast = FILL-IN-Hurtigtast:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Hurtigtast */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Nullstill Dialog-Frame
ON CHOOSE OF BUTTON-Nullstill IN FRAME Dialog-Frame /* Nullstill */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-Hurtigtast:SCREEN-VALUE = "".
     APPLY "ENTRY" TO FILL-IN-Hurtigtast.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Hurtigtast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Hurtigtast Dialog-Frame
ON ANY-KEY OF FILL-IN-Hurtigtast IN FRAME Dialog-Frame
DO:
   ASSIGN FILL-IN-Hurtigtast:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   IF CAN-DO("ENTER,RETURN,TAB",LAST-EVENT:FUNCTION )
        OR CAN-DO("ENTER,RETURN,CTRL-TAB,TAB,SHIFT-TAB",LAST-EVENT:LABEL)
   THEN DO:
      MESSAGE LAST-EVENT:LABEL SKIP(1)
              "Kan ikke brukes som hurtigtast."
         VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      APPLY "ENTRY" TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN FILL-IN-Hurtigtast:SCREEN-VALUE IN FRAME {&FRAME-NAME} = LAST-EVENT:LABEL.
   APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Hurtigtast Dialog-Frame
ON ANY-PRINTABLE OF FILL-IN-Hurtigtast IN FRAME Dialog-Frame
DO:
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
  ASSIGN FILL-IN-Hurtigtast:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wHurtigtast
         wHurtigtast = "Avbryt".
  {lng.i} RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN wHurtigtast.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  ENABLE FILL-IN-Hurtigtast BUTTON-OK BUTTON-Nullstill BUTTON-Avbryt 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


