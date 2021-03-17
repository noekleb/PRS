&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt_saknade NO-UNDO
    FIELD butik AS INTE
    FIELD kassenr AS INTE
    FIELD datum AS DATE
    FIELD kvittonr AS INTE
    INDEX bkdk IS PRIMARY UNIQUE butik kassenr datum kvittonr.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_saknade

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 butik kasse datum kvittonr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt_saknade
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tt_saknade.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt_saknade
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt_saknade


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Butiker CB-Kasse BUTTON-SokDatoTil ~
FI-Dato FI-DatoTil B-Sok BROWSE-3 BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS CB-Butiker CB-Kasse FI-Dato FI-DatoTil 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Sok 
     LABEL "Sök saknade" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Butiker AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Butik" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",         0
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kasse AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Kassa" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Datum från/till" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tt_saknade SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _FREEFORM
  QUERY BROWSE-3 DISPLAY
      butik
kasse
datum
kvittonr FORMAT ">>>>>>>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 11.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB-Butiker AT ROW 2.86 COL 21 COLON-ALIGNED
     CB-Kasse AT ROW 4.05 COL 21 COLON-ALIGNED
     BUTTON-SokDatoTil AT ROW 5.52 COL 54.6
     FI-Dato AT ROW 5.52 COL 21.2 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 5.52 COL 39.4 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     B-Sok AT ROW 5.52 COL 63
     BROWSE-3 AT ROW 7.67 COL 10
     BUTTON-SokDato AT ROW 5.52 COL 36.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 132 BY 19.81.


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
         TITLE              = "Saknade kvitton"
         HEIGHT             = 19.81
         WIDTH              = 132
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 132
         VIRTUAL-HEIGHT     = 19.81
         VIRTUAL-WIDTH      = 132
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
/* BROWSE-TAB BROWSE-3 B-Sok DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_saknade.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Saknade kvitton */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Saknade kvitton */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok C-Win
ON CHOOSE OF B-Sok IN FRAME DEFAULT-FRAME /* Sök saknade */
DO:
    DEFINE VARIABLE dDatoLoop     AS DATE        NO-UNDO.
    DEFINE VARIABLE iLastBongnr   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER     NO-UNDO.
    IF INPUT FI-Dato = ? THEN DO:
        MESSAGE "Datum från saknas"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    FOR LAST bonghode WHERE bonghode.butik = 473 AND bonghode.gruppe = 1
                      AND bonghode.kassenr = 4 AND bonghode.dato = INPUT FI-Dato - 1 NO-LOCK.
        ASSIGN iLastBongnr = bonghode.bongnr.
    END.
    DO dDatoLoop = INPUT FI-Dato TO (IF INPUT FI-DatoTil = ? THEN INPUT FI-Dato ELSE INPUT FI-DatoTil):
        FOR EACH bonghode WHERE bonghode.butikknr = INPUT CB-Butiker AND
                                bonghode.gruppenr = 1 AND
                                bonghode.kassenr  = INPUT CB-Kasse AND
                                bonghode.dato     = dDatoLoop NO-LOCK.
            IF bonghode.bongnr - iLastBongnr > 1 THEN DO:
                DO iLoop = iLastBongnr + 1 TO bonghode.bongnr - 1:
                    CREATE tt_saknade.
                    ASSIGN tt_saknade.butik    = bonghode.butikknr
                           tt_saknade.kassenr  = bonghode.kassenr
                           tt_saknade.datum    = bonghode.dato
                           tt_saknade.kvittonr = iLoop NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        DELETE tt_saknade.
                END.
            END.
            iLastBongNr = bonghode.bongnr.
        END.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
/*     butik                                             */
/*     kassenr                                           */
/*     datum                                             */
/*     kvittonr                                          */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Från kvittodatum" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Dato.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato > INPUT FI-DatoTil THEN DO:
            MESSAGE "Fel datumo, > Till datum"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-Dato:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoTil C-Win
ON CHOOSE OF BUTTON-SokDatoTil IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-DatoTil
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Till kvittodatum" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    IF INPUT FI-Dato = ? THEN
        RETURN NO-APPLY.
    ASSIGN dDato = IF INPUT FI-DatoTil = ? THEN INPUT FI-Dato ELSE INPUT FI-DatoTil.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato < INPUT FI-Dato THEN DO:
            MESSAGE "Fel datum, < Från datum"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-DatoTil:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butiker C-Win
ON VALUE-CHANGED OF CB-Butiker IN FRAME DEFAULT-FRAME /* Butik */
DO:
  DEFINE VARIABLE cListItemPairs AS CHARACTER   NO-UNDO.

  ASSIGN INPUT CB-Butiker.
  RUN InitCBKasse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Kasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Kasse C-Win
ON VALUE-CHANGED OF CB-Kasse IN FRAME DEFAULT-FRAME /* Kassa */
DO:
    ASSIGN INPUT CB-Kasse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
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
    RUN InitCBButiker.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO CB-Butiker.
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
  DISPLAY CB-Butiker CB-Kasse FI-Dato FI-DatoTil 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB-Butiker CB-Kasse BUTTON-SokDatoTil FI-Dato FI-DatoTil B-Sok 
         BROWSE-3 BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCBButiker C-Win 
PROCEDURE InitCBButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cListItemPairs AS CHARACTER   NO-UNDO.

FOR EACH Butiker NO-LOCK:
    IF CAN-FIND(FIRST kasse WHERE kasse.butik = butiker.butik AND kasse.kassenr < 99 AND kasse.aktiv = TRUE) THEN DO:
        cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + Butiker.butnamn + "," + STRING(butiker.butik).
        IF CB-Butiker = 0 THEN
            CB-Butiker = butiker.butik.
    END.
END.
CB-Butiker:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs.
CB-Butiker:SCREEN-VALUE = STRING(CB-Butiker).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCBKasse C-Win 
PROCEDURE InitCBKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEFINE VARIABLE cListItemPairs AS CHARACTER   NO-UNDO.
/*  DEFINE VARIABLE iInitValue AS INTEGER     NO-UNDO. */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN INPUT CB-Butiker.
      CB-Kasse = 0.
      FOR EACH kasse WHERE kasse.butik = CB-Butiker AND kasse.kassenr < 99 AND kasse.aktiv = TRUE NO-LOCK:
          cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + "Kassa " + STRING(kasse.kassenr) + "," + STRING(kasse.kassenr).
          IF CB-Kasse = 0 THEN
              CB-Kasse = kasse.kassenr.
      END.
      IF cListItemPairs = "" THEN
          ASSIGN cListItemPairs = "SAKNAS,0".
      CB-Kasse:LIST-ITEM-PAIRS = cListItemPairs.
      CB-Kasse:SCREEN-VALUE = STRING(CB-Kasse).
  END.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

