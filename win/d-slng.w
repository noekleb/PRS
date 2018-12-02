&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:              d-slng.w
  Description:       Lagre som...
  Input Parameters:  INPUT HANDLE whCaller
  Output Parameters: <none>
  Author:            Sturla Johnsen
  Created:           18.12.98 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER whCaller AS HANDLE NO-UNDO.
/* Local Variable Definitions ---                                       */

DEF VAR wLI AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-Lng BUTTON-OK SELECT-Lng ~
BUTTON-Avbryt 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Lng SELECT-Lng 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 11 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-OK AUTO-GO 
     LABEL "OK" 
     SIZE 11 BY 1.1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-Lng AS CHARACTER FORMAT "X(7)":U 
     LABEL "&Språk" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE SELECT-Lng AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 29 BY 6.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-Lng AT ROW 1.48 COL 12 COLON-ALIGNED
     BUTTON-OK AT ROW 1.48 COL 44
     SELECT-Lng AT ROW 2.67 COL 14 NO-LABEL
     BUTTON-Avbryt AT ROW 2.67 COL 44
     SPACE(0.00) SKIP(5.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Lagre som..."
         DEFAULT-BUTTON BUTTON-OK CANCEL-BUTTON BUTTON-Avbryt.


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Lagre som... */
DO:
  IF FILL-IN-Lng:SCREEN-VALUE = "" OR FILL-IN-Lng:SCREEN-VALUE = "Design" THEN DO:
     MESSAGE "Ugyldig språkkode." 
       VIEW-AS ALERT-BOX ERROR TITLE "Feil".
     APPLY "ENTRY" TO FILL-IN-Lng.
     RETURN NO-APPLY.
  END.
  
  RUN Save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Lagre som... */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Lng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Lng Dialog-Frame
ON ANY-PRINTABLE OF FILL-IN-Lng IN FRAME Dialog-Frame /* Språk */
DO:
  /* Komma er ikke tillat */
  IF LAST-EVENT:FUNCTION = "," THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-Lng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-Lng Dialog-Frame
ON DEFAULT-ACTION OF SELECT-Lng IN FRAME Dialog-Frame
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-Lng Dialog-Frame
ON VALUE-CHANGED OF SELECT-Lng IN FRAME Dialog-Frame
DO:
  ASSIGN FILL-IN-Lng:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND Lng WHERE Lng.Lng = "@@@" AND Lng.PrgNavn = "@@@@@@@" NO-LOCK NO-ERROR.
IF AVAIL Lng THEN ASSIGN wLI = wLI + (IF wLI <> "" AND Lng.Tekster <> "" THEN "," ELSE "") + Lng.Tekster.

ASSIGN SELECT-Lng:LIST-ITEMS = wLI.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  {lng.i} 
 
  ON "ALT->" ANYWHERE BELL.
   
  ASSIGN FILL-IN-Lng = ENTRY(1,wLi).
  RUN enable_UI.
  ASSIGN SELECT-Lng:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wCurrLng NO-ERROR.
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
  DISPLAY FILL-IN-Lng SELECT-Lng 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-Lng BUTTON-OK SELECT-Lng BUTTON-Avbryt 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save Dialog-Frame 
PROCEDURE Save :
/*------------------------------------------------------------------------------
  Purpose:     Lagrer 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN wCurrLng = FILL-IN-Lng:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  
  RUN Save IN whCaller.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

