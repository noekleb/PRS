&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iButikknr AS INTEGER    NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE TT_fel NO-UNDO
    FIELD kode AS CHAR
    FIELD antal AS DECI
    INDEX kode IS UNIQUE kode.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Butik FI-TellenrFra FI-TellenrTil ~
FI-SisteFra FI-SisteTil B-Rapport 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik FI-TellenrFra FI-TellenrTil ~
FI-SisteFra FI-SisteTil 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Butik AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SisteFra AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Siste rapport fra/til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SisteTil AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TellenrFra AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Tellenr fra/til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TellenrTil AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Butik AT ROW 2.29 COL 20.6 COLON-ALIGNED NO-TAB-STOP 
     FI-TellenrFra AT ROW 3.76 COL 20.6 COLON-ALIGNED
     FI-TellenrTil AT ROW 3.76 COL 36.2 COLON-ALIGNED NO-LABEL
     FI-SisteFra AT ROW 5.19 COL 20.6 COLON-ALIGNED NO-TAB-STOP 
     FI-SisteTil AT ROW 5.19 COL 36.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     B-Rapport AT ROW 6.91 COL 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kontroll tellefiler"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FI-Butik:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-SisteFra:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-SisteTil:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kontroll tellefiler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kontroll tellefiler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport C-Win
ON CHOOSE OF B-Rapport IN FRAME DEFAULT-FRAME /* Rapport */
DO:
  RUN kontroller.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN.
  RUN Rapport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    ASSIGN FI-Butik = iButikknr.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FI-Butik FI-TellenrFra FI-TellenrTil FI-SisteFra FI-SisteTil 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Butik FI-TellenrFra FI-TellenrTil FI-SisteFra FI-SisteTil B-Rapport 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontroller C-Win 
PROCEDURE kontroller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF INPUT FI-TellenrFra = 0 THEN DO:
          MESSAGE "Feil fra. = '0'"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO FI-TellenrFra.
          RETURN NO-APPLY "AVBRYT".
      END.
      IF INPUT FI-TellenrTil > 0 AND INPUT FI-TellenrTil < INPUT FI-TellenrFra THEN DO:
          MESSAGE "Feil til, < fra"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO FI-TellenrFra.
          RETURN NO-APPLY "AVBRYT".
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport C-Win 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cKod    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKodTmp AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iFra AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTil AS INTEGER    NO-UNDO.

EMPTY TEMP-TABLE TT_fel.

DO WITH FRAME {&FRAME-NAME}:
    iFra = INPUT FI-TellenrFra.
    iTil = IF INPUT FI-TellenrTil = 0 THEN iFra ELSE INPUT FI-TellenrTil.
    FI-SisteFra:SCREEN-VALUE = FI-TellenrFra:SCREEN-VALUE.
    FI-SisteTil:SCREEN-VALUE = FI-TellenrTil:SCREEN-VALUE.
    FI-TellenrFra = 0. 
    FI-TellenrTil = 0.
    FI-TellenrFra:SCREEN-VALUE = "0".
    FI-TellenrTil:SCREEN-VALUE = "0".

    FOR EACH tellehode WHERE tellehode.butikkliste = FI-Butik:SCREEN-VALUE AND
                             tellehode.tellenr >= iFra AND tellehode.tellenr <= iTil NO-LOCK.
        FOR EACH ht-filhode WHERE ht-filhode.tellenr = tellehode.tellenr NO-LOCK:
            FOR EACH ht-fillinje OF ht-filhode NO-LOCK.
                cKod = ENTRY(2,HT-FilLinje.LinjeData,CHR(1)).
                FIND strekkode WHERE strekkode.kode = cKod NO-LOCK NO-ERROR.
                IF NOT AVAIL strekkode THEN DO:
                    cKodTmp = FILL("0",13 - LENGTH(cKod)) + cKod.
                    FIND strekkode WHERE strekkode.kode = cKodTmp NO-LOCK NO-ERROR.
                END.
                IF NOT AVAIL strekkode THEN DO:
                    FIND TT_fel WHERE TT_Fel.kode = cKod NO-ERROR.
                    IF NOT AVAIL TT_fel THEN DO:
                        CREATE TT_Fel.
                        ASSIGN tt_fel.kode  = cKod.
                    END.
                    tt_fel.antal = tt_fel.antal + INT(ENTRY(9,HT-FilLinje.LinjeData,CHR(1))).
                END.
            END.
        END.
    END.
END.
IF NOT CAN-FIND(FIRST tt_fel) THEN DO:
    MESSAGE "Ingen ukjente poster"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
OUTPUT TO "CLIPBOARD".

/* PUT UNFORMATTED "Tellenr: " Tellehode.tellenr chr(13) chr(10). */
/* PUT UNFORMATTED "Strekkode" CHR(9) "Antal" chr(13) chr(10).    */
FOR EACH TT_fel:
    PUT UNFORMATTED TT_fel.kode CHR(9) tt_fel.antal SKIP.
END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

