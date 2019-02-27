&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEFINE OUTPUT PARAMETER cDTfra AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cDTtil AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE retur_verdi AS CHARACTER INIT "AVBRYT"  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK FI-DFRA FI-TFRA Btn_Cancel FI-DTIL ~
FI-TTIL 
&Scoped-Define DISPLAYED-OBJECTS FI-DFRA FI-TFRA FI-DTIL FI-TTIL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-DFRA AS DATE FORMAT "99/99/99":U 
     LABEL "Dato-tid fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DTIL AS DATE FORMAT "99/99/99":U 
     LABEL "Dato-tid til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TFRA AS CHARACTER FORMAT "xx:xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TTIL AS CHARACTER FORMAT "xx:xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.52 COL 49
     FI-DFRA AT ROW 1.62 COL 13 COLON-ALIGNED
     FI-TFRA AT ROW 1.62 COL 27.4 COLON-ALIGNED NO-LABEL
     Btn_Cancel AT ROW 2.76 COL 49 NO-TAB-STOP 
     FI-DTIL AT ROW 2.81 COL 13 COLON-ALIGNED
     FI-TTIL AT ROW 2.81 COL 27.4 COLON-ALIGNED NO-LABEL
     SPACE(21.79) SKIP(0.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg periode"
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
ON GO OF FRAME Dialog-Frame /* Velg periode */
DO:
  cDTfra = INPUT FI-DFRA + " " + FI-TFRA:SCREEN-VALUE.
  cDTtil = INPUT FI-DTIL + " " + FI-TTIL:SCREEN-VALUE.
  retur_verdi = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg periode */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cH AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cM AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cS AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iTidfra AS INTEGER     NO-UNDO.
   DEFINE VARIABLE iTidtil AS INTEGER     NO-UNDO.
   ASSIGN FI-DFRA 
          FI-DTIL 
          FI-TFRA 
          FI-TTIL.

   IF FI-DFRA = ? OR FI-DFRA = ? THEN DO:
       MESSAGE "Fyll i dato fra/til"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-DFRA.
       RETURN NO-APPLY.
   END.
   IF FI-DTIL < FI-DFRA THEN DO:
       MESSAGE "Dato til < dato fra"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-DFRA.
       RETURN NO-APPLY.
   END.
   IF LENGTH(FI-TFRA) <> 6 THEN DO:
       MESSAGE "FEIL"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-TFRA.
       RETURN NO-APPLY.
   END.
   IF LENGTH(FI-TTIL) <> 6 THEN DO:
       MESSAGE "FEIL"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-TTIL.
       RETURN NO-APPLY.
   END.
   ASSIGN cH = SUBSTR(FI-TFRA,1,2)
          cM = SUBSTR(FI-TFRA,3,2)
          cS = SUBSTR(FI-TFRA,5,2).
   IF INT(cH) > 23 OR INT(cM) > 59 OR INT(cS) > 59 THEN DO:
       MESSAGE "Feil tid. Gyldig tid = 00:00:00 - 23:59:59"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-TFRA.
       RETURN NO-APPLY.
   END.
   iTidFra = 3600 * INT(cH) + 60 * INT(cM) + INT(cS).

   ASSIGN cH = SUBSTR(FI-TTIL,1,2)
          cM = SUBSTR(FI-TTIL,3,2)
          cS = SUBSTR(FI-TTIL,5,2).
   IF INT(cH) > 23 OR INT(cM) > 59 OR INT(cS) > 59 THEN DO:
       MESSAGE "Feil tid. Gyldig tid = 00:00:00 - 23:59:59"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-TTIL.
       RETURN NO-APPLY.
   END.
   iTidTil = 3600 * INT(cH) + 60 * INT(cM) + INT(cS).
   IF FI-DFRA = FI-DTIL AND iTidTil < iTidFra THEN DO:
       MESSAGE "Sluttid < starttid"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO FI-TFRA.
       RETURN NO-APPLY.
   END.
/*                                                         */
/*     ASSIGN fi-dtfra                                     */
/*            fi-dttil.                                    */
/*     IF fi-dtfra = ? OR fi-dttil = ? THEN DO:            */
/*         MESSAGE "Fyll i fra/til"                        */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
/*         APPLY "ENTRY" TO fi-dtfra.                      */
/*         RETURN NO-APPLY.                                */
/*     END.                                                */
/*     ELSE IF fi-dttil < fi-dtfra THEN DO:                */
/*         MESSAGE FI-DTtil:LABEL + " < " + FI-DTfra:LABEL */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
/*         APPLY "ENTRY" TO fi-dtfra.                      */
/*         RETURN NO-APPLY.                                */
/*     END.                                                */

/* MESSAGE fi-dtfra SKIP                  */
/*         fi-dttil                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TFRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TFRA Dialog-Frame
ON ANY-PRINTABLE OF FI-TFRA IN FRAME Dialog-Frame
DO:
  IF LASTKEY < 48 OR LASTKEY > 57 THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TTIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TTIL Dialog-Frame
ON ANY-PRINTABLE OF FI-TTIL IN FRAME Dialog-Frame
DO:
  IF LASTKEY < 48 OR LASTKEY > 57 THEN
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
  APPLY "ENTRY" TO FI-DFRA.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN retur_verdi.

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
  DISPLAY FI-DFRA FI-TFRA FI-DTIL FI-TTIL 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK FI-DFRA FI-TFRA Btn_Cancel FI-DTIL FI-TTIL 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

