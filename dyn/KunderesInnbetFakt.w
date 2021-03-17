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
DEF INPUT  PARAM icIdListDebet  AS CHAR NO-UNDO.
DEF INPUT  PARAM ifBelopDebet   AS DEC  NO-UNDO.
DEF INPUT  PARAM ifBelopKredit  AS DEC  NO-UNDO.
DEF OUTPUT PARAM ofBelopKredit  AS DEC  NO-UNDO.
DEF INPUT-OUTPUT PARAM iodDato  AS DATE NO-UNDO.
DEF OUTPUT PARAM ocNotat        AS CHAR NO-UNDO.
DEF OUTPUT PARAM lKID           AS DEC  NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fiBelopKredit fiDato fiKid edNotat Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiBelopDebet fiBelopKredit fiAntDebet ~
fiDato fiKid edNotat fiNotatTekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE edNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 58 BY 3.1 NO-UNDO.

DEFINE VARIABLE fiAntDebet AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall valgte debetposter til dekning" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiBelopDebet AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Beløp debet" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiBelopKredit AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Beløp kredit" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiKid AS DECIMAL FORMAT "->>>>>>>>>>>>9":U INITIAL 0 
     LABEL "KID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiMsg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kredit beløp dekker eldste post(er), evt føres a konto" 
      VIEW-AS TEXT 
     SIZE 2.8 BY .62 NO-UNDO.

DEFINE VARIABLE fiNotatTekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Notat" 
      VIEW-AS TEXT 
     SIZE 5.8 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiBelopDebet AT ROW 1.38 COL 23.2 COLON-ALIGNED
     fiBelopKredit AT ROW 2.57 COL 23.2 COLON-ALIGNED
     fiAntDebet AT ROW 3.86 COL 35.2 COLON-ALIGNED
     fiDato AT ROW 5.05 COL 23.4 COLON-ALIGNED
     fiKid AT ROW 6.29 COL 23.2 COLON-ALIGNED
     edNotat AT ROW 9.33 COL 2 NO-LABEL
     Btn_OK AT ROW 12.71 COL 29.2
     Btn_Cancel AT ROW 12.71 COL 45.2
     fiMsg AT ROW 7.52 COL 54.4 COLON-ALIGNED
     fiNotatTekst AT ROW 8.52 COL 2.4
     SPACE(46.59) SKIP(4.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrer innbetaling"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiAntDebet IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBelopDebet IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMsg IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiMsg:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN fiNotatTekst IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrer innbetaling */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME Dialog-Frame:
      IF edNotat:SCREEN-VALUE MATCHES "*|*" THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig tegn i notatfelt: |","","").
        RETURN NO-APPLY.
      END.

      IF DEC(fiKID:SCREEN-VALUE) > 0 THEN DO:
          cTekst = DYNAMIC-FUNCTION("getFieldList","FakturaHode;KID","WHERE KID = '" + fiKID:SCREEN-VALUE + "'").
          IF TRIM(cTekst) = '' OR cTekst = ? THEN
          DO:
              MESSAGE 'Ukjent eller ugyldig KID nr.'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN NO-APPLY.
              APPLY 'ENTRY' TO fiKID.
          END.
      END.

      ASSIGN fiBelopKredit fiDato fiKID
             ofBelopKredit = fiBelopKredit
             iodDato       = fiDato   
             lKID          = DEC(fiKID)
             ocNotat       = edNotat:SCREEN-VALUE
             .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
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
  DISPLAY fiBelopDebet fiBelopKredit fiAntDebet fiDato fiKid edNotat 
          fiNotatTekst 
      WITH FRAME Dialog-Frame.
  ENABLE fiBelopKredit fiDato fiKid edNotat Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fiBelopDebet  = ifBelopDebet
         fiBelopKredit = IF ifBelopKredit NE 0 THEN ifBelopKredit ELSE ifBelopDebet
         fiAntDebet    = NUM-ENTRIES(icIdListDebet)
         fiDato        = IF ifBelopKredit = 0 THEN TODAY ELSE iodDato
         .
  DISP fiBelopDebet
       fiBelopKredit
       fiDato
       fiAntDebet
       .
  IF ifBelopKredit NE 0 THEN 
    ASSIGN fiBelopKredit:SENSITIVE = FALSE
           fiDato:SENSITIVE        = FALSE
           .
  IF fiAntDebet = 0 THEN
    fiMsg:HIDDEN = FALSE.
  
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

