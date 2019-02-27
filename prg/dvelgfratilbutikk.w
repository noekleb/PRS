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
DEF INPUT-OUTPUT PARAMETER i1ButNr AS INT FORMAT ">>>>>9" NO-UNDO.
DEF INPUT-OUTPUT PARAMETER i2ButNr AS INT FORMAT ">>>>>9" NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR cTekst AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SokButikkFra ButikkNrFra ButikkNrTil ~
Btn_OK Btn_Cancel B-SokButikkTil 
&Scoped-Define DISPLAYED-OBJECTS ButikkNrFra fFraButikk ButikkNrTil ~
fTilbutikk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokButikkFra 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 4.6 BY 1.14.

DEFINE BUTTON B-SokButikkTil 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "SokTil" 
     SIZE 4.6 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ButikkNrFra AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Fra" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Alt-A for å addere en ny post.".

DEFINE VARIABLE ButikkNrTil AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE fFraButikk AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE fTilbutikk AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-SokButikkFra AT ROW 1.48 COL 20.2 WIDGET-ID 2 NO-TAB-STOP 
     ButikkNrFra AT ROW 1.52 COL 7.8 COLON-ALIGNED HELP
          "Alt-A for å addere en post." WIDGET-ID 6
     fFraButikk AT ROW 1.52 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     ButikkNrTil AT ROW 2.52 COL 7.8 COLON-ALIGNED HELP
          "Butikk det overføres til" WIDGET-ID 8
     fTilbutikk AT ROW 2.52 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     Btn_OK AT ROW 3.86 COL 2
     Btn_Cancel AT ROW 3.86 COL 51
     B-SokButikkTil AT ROW 2.48 COL 20.2 WIDGET-ID 4 NO-TAB-STOP 
     SPACE(42.19) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg fra til butikk for overføring"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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

/* SETTINGS FOR FILL-IN fFraButikk IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fFraButikk:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fTilbutikk IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fTilbutikk:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg fra til butikk for overføring */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikkFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikkFra Dialog-Frame
ON CHOOSE OF B-SokButikkFra IN FRAME Dialog-Frame /* Button 1 */
OR F10 OF ButikkNrFra
DO:
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gButiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    ButikkNrFra:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        ButikkNrFra:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        fFraButikk:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
        fFraButikk:MODIFIED      = FALSE
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO ButikkNrFra.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikkTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikkTil Dialog-Frame
ON CHOOSE OF B-SokButikkTil IN FRAME Dialog-Frame /* SokTil */
OR F10 OF ButikkNrTil
DO:
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gButiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    ButikkNrFra:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        ButikkNrTil:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        fTilButikk:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
        fTilButikk:MODIFIED      = FALSE
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO ButikkNrTil.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        i1ButNr = 0  
        i2ButNr = 0  
        .
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        i1ButNr = INT(ButikkNrFra:SCREEN-VALUE)  
        i2ButNr = INT(ButikkNrTil:SCREEN-VALUE)  
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNrFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNrFra Dialog-Frame
ON TAB OF ButikkNrFra IN FRAME Dialog-Frame /* Fra */
OR "RETURN":U OF ButikkNrFra /*OR "LEAVE":U OF RowObject.ButikkNrFra */
DO:
   FIND Butiker NO-LOCK WHERE
       Butiker.Butik = INT(ButikkNrFra:SCREEN-VALUE) NO-ERROR.
   IF NOT AVAILABLE Butiker THEN
   DO:
       ASSIGN
           fFraButikk:SCREEN-VALUE = ""
           fFraButikk:MODIFIED     = FALSE
           .
       MESSAGE "Ugyldig butikknummer!"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
   END.
   ELSE DO:
       ASSIGN
           fFraButikk:SCREEN-VALUE = Butiker.ButNamn
           fFraButikk:MODIFIED     = FALSE
           .
       APPLY "ENTRY" TO ButikkNrTil.
       RETURN NO-APPLY.
/*        APPLY LASTKEY. */

   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNrTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNrTil Dialog-Frame
ON TAB OF ButikkNrTil IN FRAME Dialog-Frame /* Til */
OR "RETURN":U OF ButikkNrTil /*OR "LEAVE":U OF RowObject.ButikkNrTil*/
DO:

   FIND Butiker NO-LOCK WHERE
       Butiker.Butik = INT(ButikkNrTil:SCREEN-VALUE) NO-ERROR.
   IF NOT AVAILABLE Butiker THEN
   DO:
       MESSAGE "Ugyldig butikknummer!"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
   END.    
  
   IF INPUT ButikkNrTil <> 0 AND
      INPUT ButikkNrFra <> 0 THEN
   DO:
       IF INPUT ButikkNrTil = INPUT ButikkNrFra THEN
       DO:
           MESSAGE "Fra og til butikk kan ikke være like."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
       END.
   END.

   ASSIGN
     fTilButikk:SCREEN-VALUE = Butiker.ButNamn
     fTilButikk:MODIFIED     = FALSE
     .

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

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          ButikkNrFra:SCREEN-VALUE = STRING(i1ButNr)
          ButikkNrTil:SCREEN-VALUE = STRING(i2ButNr)
          .
  END.

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
  DISPLAY ButikkNrFra fFraButikk ButikkNrTil fTilbutikk 
      WITH FRAME Dialog-Frame.
  ENABLE B-SokButikkFra ButikkNrFra ButikkNrTil Btn_OK Btn_Cancel 
         B-SokButikkTil 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

