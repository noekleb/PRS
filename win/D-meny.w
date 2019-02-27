&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:              d-meny.w
  Description:       Dialog for å oppgi menynavn (Nytt, endre, kopier til)
  Input Parameters:  Inp CHAR wNavn
  Output Parameters: ingen
  Author:            Sturla Johnsen
  Created:           23.05.98
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN  
    DEF VAR wNavn AS CHAR NO-UNDO.
&ELSE
    DEF INPUT PARAMETER wNavn AS CHAR NO-UNDO.
   
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
&Scoped-Define ENABLED-OBJECTS FILL-IN-Navn BUTTON-OK BUTTON-Avbryt ~
FILL-IN-Tekst 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Navn FILL-IN-Tekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-Navn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Tekst AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 51 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-Navn AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL
     BUTTON-OK AT ROW 3.62 COL 3
     BUTTON-Avbryt AT ROW 3.62 COL 38
     FILL-IN-Tekst AT ROW 1.48 COL 1 COLON-ALIGNED NO-LABEL
     SPACE(0.00) SKIP(2.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Menyeditor"
         DEFAULT-BUTTON BUTTON-OK CANCEL-BUTTON BUTTON-Avbryt.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Menyeditor */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     IF FILL-IN-Navn:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Et navn må oppgis."
           VIEW-AS ALERT-BOX ERROR TITLE "Feil".
        APPLY "ENTRY" TO FILL-IN-Navn.
        RETURN NO-APPLY.
     END.
     
     IF wNavn = "" THEN DO:
        IF CAN-FIND(Meny WHERE Meny.Navn = wNavn) THEN DO:
           MESSAGE "Det finnes allerede en meny med det angitte navnet." SKIP
                   "Du må oppgi et nytt navn."
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           APPLY "ENTRY" TO FILL-IN-Navn.   
           RETURN NO-APPLY.
        END.
        DO TRANSACTION:
          CREATE Meny.
          ASSIGN Meny.Navn = FILL-IN-Navn:SCREEN-VALUE.
        END.     
     END.           
     
     ASSIGN wNavn = FILL-IN-Navn:SCREEN-VALUE.
  END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Menyeditor */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Navn Dialog-Frame
ON ANY-PRINTABLE OF FILL-IN-Navn IN FRAME Dialog-Frame
DO:
  /* Sørger for at ingen skilletegn kommer inn */
  IF KEYFUNCT(LASTKEY) = "|" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ASSIGN FILL-IN-Navn = wNavn.
IF wNavn = "" THEN
      ASSIGN FILL-IN-Tekst = "Skriv inn navnet på menyen du vil lage"
             FRAME {&FRAME-NAME}:TITLE = "Menyeditor - Ny meny".
ELSE  ASSIGN FILL-IN-Tekst = "Skriv inn et navn du vil lagre menyen som"
             FRAME {&FRAME-NAME}:TITLE = "Menyeditor - Lagre som".           

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i} RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN wNavn.

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
  DISPLAY FILL-IN-Navn FILL-IN-Tekst 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-Navn BUTTON-OK BUTTON-Avbryt FILL-IN-Tekst 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


