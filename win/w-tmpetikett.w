&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_Etikett NO-UNDO LIKE etikett
       FIELD individnr LIKE Individ.IndividNr
       .



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
DEFINE INPUT  PARAMETER hParentWin AS HANDLE     NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iButik        AS INTEGER    NO-UNDO.
DEFINE VARIABLE hFrameHandle AS HANDLE     NO-UNDO.
DEFINE VARIABLE cSkjul AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lSkjul AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iPksdlbutNr AS INTEGER NO-UNDO.
DEF VAR cReturnValues   AS CHAR NO-UNDO.
DEF VAR bOk             AS LOG  NO-UNDO.

DEF VAR iCL AS INT NO-UNDO.

DEFINE VARIABLE lInlev AS LOGICAL    NO-UNDO.
{etikettlogg.i &NEW=NEW}

DEF BUFFER clButiker FOR Butiker.

DEFINE VARIABLE lValgt AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Etiketter

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Etikett

/* Definitions for BROWSE BROWSE-Etiketter                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Etiketter TT_Etikett.Kode ~
TT_Etikett.texten TT_Etikett.storlek TT_Etikett.individnr TT_Etikett.antal ~
TT_Etikett.pris TT_Etikett.Pris2 TT_Etikett.vg TT_Etikett.lopnr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Etiketter 
&Scoped-define QUERY-STRING-BROWSE-Etiketter FOR EACH TT_Etikett NO-LOCK ~
    BY TT_Etikett.rad INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Etiketter OPEN QUERY BROWSE-Etiketter FOR EACH TT_Etikett NO-LOCK ~
    BY TT_Etikett.rad INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Etiketter TT_Etikett
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Etiketter TT_Etikett


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Etiketter}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS TT_Etikett.Antal 
&Scoped-define ENABLED-TABLES TT_Etikett
&Scoped-define FIRST-ENABLED-TABLE TT_Etikett
&Scoped-Define ENABLED-OBJECTS B-Minimer BROWSE-Etiketter B-Skriv B-Valgt ~
B-Slett TG-Tilbud2 
&Scoped-Define DISPLAYED-FIELDS TT_Etikett.Antal 
&Scoped-define DISPLAYED-TABLES TT_Etikett
&Scoped-define FIRST-DISPLAYED-TABLE TT_Etikett
&Scoped-Define DISPLAYED-OBJECTS TG-Tilbud2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Minimer  NO-FOCUS
     LABEL "Minimer" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Skriv 
     LABEL "Skriv ut" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Slett 
     LABEL "Slett" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Valgt 
     LABEL "Skriv valgt" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE TG-Tilbud2 AS LOGICAL INITIAL NO 
     LABEL "Skriv tilbudspris" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.2 BY .81 NO-UNDO.

DEFINE VARIABLE FI-Antall AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icon/ean13.jpg":U
     SIZE 27.6 BY 3.

DEFINE VARIABLE TG-Tilbud1 AS LOGICAL INITIAL NO 
     LABEL "Skriv tilbudspris" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Etiketter FOR 
      TT_Etikett SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Etiketter C-Win _STRUCTURED
  QUERY BROWSE-Etiketter NO-LOCK DISPLAY
      TT_Etikett.Kode COLUMN-LABEL "EAN-kode"
      TT_Etikett.texten COLUMN-LABEL "Bongtekst" FORMAT "x(15)":U
      TT_Etikett.storlek COLUMN-LABEL "Størrelse" FORMAT "x(10)":U
            WIDTH 9.4
      TT_Etikett.individnr COLUMN-LABEL "Individ"
      TT_Etikett.antal COLUMN-LABEL "Antall" FORMAT "ZZZ9":U
      TT_Etikett.pris COLUMN-LABEL "Pris" FORMAT "-ZZZZZ9.99":U
            WIDTH 13.4
      TT_Etikett.Pris2 FORMAT "->>,>>9.99":U
      TT_Etikett.vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U
      TT_Etikett.lopnr COLUMN-LABEL "Løpnr" FORMAT "ZZZZZ9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 102.2 BY 9.91 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Minimer AT ROW 5.43 COL 106.4
     BROWSE-Etiketter AT ROW 1.33 COL 1.8
     B-Skriv AT ROW 1.48 COL 106.4
     B-Valgt AT ROW 2.81 COL 106.4
     B-Slett AT ROW 4.19 COL 106.4
     TT_Etikett.Antal AT ROW 6.67 COL 109.8 COLON-ALIGNED HELP
          ""
          LABEL "Antall" FORMAT "ZZZZ9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     TG-Tilbud2 AT ROW 8.62 COL 105.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.2 BY 10.33.

DEFINE FRAME FRAME-EAN
     FI-Antall AT ROW 4.14 COL 7.8 COLON-ALIGNED
     TG-Tilbud1 AT ROW 5.38 COL 4
     IMAGE-1 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 28 BY 5.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Etikett T "NEW SHARED" NO-UNDO skotex etikett
      ADDITIONAL-FIELDS:
          FIELD individnr LIKE Individ.IndividNr
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Etiketter"
         HEIGHT             = 10.33
         WIDTH              = 126.2
         MAX-HEIGHT         = 12.95
         MAX-WIDTH          = 126.2
         VIRTUAL-HEIGHT     = 12.95
         VIRTUAL-WIDTH      = 126.2
         SHOW-IN-TASKBAR    = NO
         MIN-BUTTON         = NO
         MAX-BUTTON         = NO
         ALWAYS-ON-TOP      = YES
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-EAN:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-EAN:MOVE-BEFORE-TAB-ITEM (BROWSE-Etiketter:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BROWSE-Etiketter FRAME-EAN DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN TT_Etikett.Antal IN FRAME DEFAULT-FRAME
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FRAME FRAME-EAN
                                                                        */
/* SETTINGS FOR FILL-IN FI-Antall IN FRAME FRAME-EAN
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Etiketter
/* Query rebuild information for BROWSE BROWSE-Etiketter
     _TblList          = "Temp-Tables.TT_Etikett"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.TT_Etikett.rad|yes"
     _FldNameList[1]   > "_<CALC>"
"TT_Etikett.Kode" "EAN-kode" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.TT_Etikett.texten
"TT_Etikett.texten" "Bongtekst" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.TT_Etikett.storlek
"TT_Etikett.storlek" "Størrelse" ? "character" ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"TT_Etikett.individnr" "Individ" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.TT_Etikett.antal
"TT_Etikett.antal" "Antall" "ZZZ9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.TT_Etikett.pris
"TT_Etikett.pris" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = Temp-Tables.TT_Etikett.Pris2
     _FldNameList[8]   > Temp-Tables.TT_Etikett.vg
"TT_Etikett.vg" "Vg" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.TT_Etikett.lopnr
"TT_Etikett.lopnr" "Løpnr" "ZZZZZ9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Etiketter */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Etiketter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON PARENT-WINDOW-CLOSE OF C-Win /* Etiketter */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Etiketter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESTORED OF C-Win /* Etiketter */
DO:
  MESSAGE "window-restored"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TT_Etikett.Antal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TT_Etikett.Antal C-Win
ON RETURN OF TT_Etikett.Antal IN FRAME DEFAULT-FRAME /* Antall */
DO:
  IF INPUT TT_Etikett.antal = 0 THEN
      APPLY "CHOOSE" TO B-Slett.
  ELSE DO:
      FI-Antall  = FI-Antall + INPUT TT_Etikett.antal - TT_Etikett.antal.
      ASSIGN INPUT TT_Etikett.antal.
  END.
  DISPLAY FI-Antall WITH FRAME FRAME-EAN.
  BROWSE BROWSE-Etiketter:REFRESH().
  APPLY "ENTRY" TO BROWSE-Etiketter.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Minimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Minimer C-Win
ON CHOOSE OF B-Minimer IN FRAME DEFAULT-FRAME /* Minimer */
DO:
  RUN EndreSize ("LITEN").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Skriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Skriv C-Win
ON CHOOSE OF B-Skriv IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
    RUN SkrivUt IN THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett C-Win
ON CHOOSE OF B-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
      ASSIGN FI-Antall = FI-Antall - TT_Etikett.Antal.
      DISPLAY FI-Antall WITH FRAME FRAME-EAN.
      DELETE TT_etikett.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Valgt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Valgt C-Win
ON CHOOSE OF B-Valgt IN FRAME DEFAULT-FRAME /* Skriv valgt */
DO:
    lValgt = TRUE.
    RUN SkrivUt IN THIS-PROCEDURE.
    lValgt =FALSE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Etiketter
&Scoped-define SELF-NAME BROWSE-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Etiketter C-Win
ON VALUE-CHANGED OF BROWSE-Etiketter IN FRAME DEFAULT-FRAME
DO:
  ASSIGN TT_Etikett.antal:SCREEN-VALUE = STRING(TT_Etikett.antal,">>>>9").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-EAN
&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 C-Win
ON MOUSE-SELECT-CLICK OF IMAGE-1 IN FRAME FRAME-EAN
DO:
  RUN EndreSize ("STOR").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Tilbud1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Tilbud1 C-Win
ON VALUE-CHANGED OF TG-Tilbud1 IN FRAME FRAME-EAN /* Skriv tilbudspris */
DO:
  TG-Tilbud2:CHECKED  IN FRAME DEFAULT-FRAME = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TG-Tilbud2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Tilbud2 C-Win
ON VALUE-CHANGED OF TG-Tilbud2 IN FRAME DEFAULT-FRAME /* Skriv tilbudspris */
DO:
    TG-Tilbud1:CHECKED  IN FRAME FRAME-EAN = SELF:CHECKED.
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
  {syspara.i 5 1 1 iCL INT} 
  {syspara.i 5 4 14 cSkjul}
  ASSIGN lSkjul = cSkjul = "1".
  RUN EndreSize ("LITEN").
  RUN enable_UI.
  ASSIGN TG-Tilbud1:HIDDEN = TRUE
         TG-Tilbud2:HIDDEN = TRUE.
  {lng.i}
  
  /* Skrives det ut fra en pakkseddel, skal butikknr hentes fra pakkseddelen. */
  PUBLISH 'getPkSdlButNr' (OUTPUT iPkSdlbutNr).
  
  FRAME FRAME-EAN:MOVE-TO-TOP().
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avslutt C-Win 
PROCEDURE Avslutt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE "Etikettvindu avsluttes"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
    IF choice THEN
        APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY TG-Tilbud2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE TT_Etikett THEN 
    DISPLAY TT_Etikett.Antal 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Minimer BROWSE-Etiketter B-Skriv B-Valgt B-Slett TT_Etikett.Antal 
         TG-Tilbud2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Antall TG-Tilbud1 
      WITH FRAME FRAME-EAN IN WINDOW C-Win.
  ENABLE IMAGE-1 TG-Tilbud1 
      WITH FRAME FRAME-EAN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-EAN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreSize C-Win 
PROCEDURE EndreSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cStorlek AS CHARACTER  NO-UNDO.
/*     DEFINE VARIABLE hFrameHandle AS HANDLE     NO-UNDO. */
    IF cStorlek = "LITEN" THEN DO:
        ASSIGN hFrameHandle = FRAME FRAME-EAN:HANDLE
               hFrameHandle:HIDDEN = FALSE.
/*         FRAME FRAME-EAN:MOVE-TO-TOP(). */
    END.
    ELSE DO:
        ASSIGN hFrameHandle = FRAME DEFAULT-FRAME:HANDLE
               FRAME FRAME-EAN:HIDDEN = TRUE.
    END.
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = hFrameHandle:WIDTH-PIXELS
     {&WINDOW-NAME}:HEIGHT-PIXELS = hFrameHandle:HEIGHT-PIXELS.
    RUN WinPosition.
    APPLY 'ENTRY' TO B-Skriv IN FRAME Default-Frame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyEtikett C-Win 
PROCEDURE NyEtikett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStrekKode LIKE StrekKode.Kode NO-UNDO.
  DEFINE INPUT  PARAMETER iAntal     AS INTEGER          NO-UNDO.
  DEFINE INPUT  PARAMETER dIndividNr LIKE Individ.individnr    NO-UNDO.

  FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL StrekKode THEN
      RETURN.
  IF StrekKode.StrKode > 0 THEN
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR. 
  FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.
      
  FIND ArtPris OF ArtBas NO-LOCK WHERE
    ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN     
    FIND FIRST artpris OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAIL artpris THEN
      RETURN.
  IF iAntal = 0 THEN
      RETURN.
  FIND TT_etikett WHERE TT_etikett.kode = cStrekKode AND
                        TT_etikett.individnr = dIndividnr NO-ERROR.
  IF AVAIL TT_etikett AND dIndividnr > 0 THEN
      RETURN.
  ELSE IF AVAIL TT_etikett THEN DO:
      ASSIGN TT_etikett.antal = TT_etikett.antal + iAntal
             FI-Antall        = FI-Antall + iAntal.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  ELSE DO:
     
      CREATE TT_Etikett.
      ASSIGN iButik                = iButik +  1
             TT_etikett.antal      = iAntal
             TT_etikett.butik      = iButik
             TT_etikett.levinnr    = 1
             TT_etikett.lopnr      = ArtBas.lopnr
             TT_etikett.pris       = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
/*              TT_etikett.rad */
             TT_etikett.storlek    = IF AVAIL StrKonv THEN StrKonv.Storl ELSE ""
             TT_etikett.texten     = ArtBas.BongTekst
             TT_etikett.vg         = ArtBas.Vg
             TT_etikett.kode       = cStrekKode
             TT_etikett.artikkelnr = ArtBas.Artikkelnr
             TT_etikett.IndividNr  = dIndividNr
             FI-Antall             = FI-Antall + iAntal.
  END.
  APPLY "VALUE-CHANGED" TO BROWSE-Etiketter IN FRAME DEFAULT-FRAME.
  DISPLAY FI-Antall WITH FRAME FRAME-EAN.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyEtikettInlev C-Win 
PROCEDURE NyEtikettInlev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStrekKode LIKE StrekKode.Kode NO-UNDO.
  DEFINE INPUT  PARAMETER iAntal     AS INTEGER          NO-UNDO.
  DEFINE INPUT  PARAMETER dIndividNr LIKE Individ.individnr    NO-UNDO.
  ASSIGN lInlev = TRUE.
/*   IF TG-Tilbud1:HIDDEN IN FRAME FRAME-EAN = TRUE THEN           */
/*       ASSIGN TG-Tilbud1:HIDDEN IN FRAME FRAME-EAN = FALSE       */
/*              TG-Tilbud2:HIDDEN IN FRAME DEFAULT-FRAME = FALSE.  */
 
  FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL StrekKode THEN
      RETURN.
  IF StrekKode.StrKode > 0 THEN
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR. 
  FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.
      
  FIND ArtPris OF ArtBas NO-LOCK WHERE
    ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN     
    FIND FIRST artpris OF ArtBas NO-LOCK NO-ERROR.
    
  IF NOT AVAIL artpris THEN
      RETURN.
  IF iAntal = 0 THEN
      RETURN.
  FIND TT_etikett WHERE TT_etikett.kode = cStrekKode AND
                        TT_etikett.individnr = dIndividnr NO-ERROR.
  IF AVAIL TT_etikett AND dIndividnr > 0 THEN
      RETURN.
  ELSE IF AVAIL TT_etikett THEN DO:
      ASSIGN TT_etikett.antal = TT_etikett.antal + iAntal
             FI-Antall        = FI-Antall + iAntal.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  ELSE DO:
     
      CREATE TT_Etikett.
      ASSIGN iButik                = iButik + 1
             TT_etikett.antal      = iAntal
             TT_etikett.butik      = iButik
             TT_etikett.levinnr    = 1
             TT_etikett.lopnr      = ArtBas.lopnr
             TT_etikett.pris       = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
             TT_etikett.pris2      = ArtPris.Pris[1]  /* Speciell hantering av tilbud vid inleveranseartikel */
/*              TT_etikett.rad */
             TT_etikett.storlek    = IF AVAIL StrKonv THEN StrKonv.Storl ELSE ""
             TT_etikett.texten     = ArtBas.BongTekst
             TT_etikett.vg         = ArtBas.Vg
             TT_etikett.kode       = cStrekKode
             TT_etikett.artikkelnr = ArtBas.Artikkelnr
             TT_etikett.IndividNr  = dIndividNr
             FI-Antall             = FI-Antall + iAntal.
  END.
  APPLY "VALUE-CHANGED" TO BROWSE-Etiketter IN FRAME DEFAULT-FRAME.
  DISPLAY FI-Antall WITH FRAME FRAME-EAN.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyEtikettPakkseddel C-Win 
PROCEDURE NyEtikettPakkseddel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStrekKode LIKE StrekKode.Kode NO-UNDO.
  DEFINE INPUT  PARAMETER iAntal     AS INTEGER          NO-UNDO.
  DEFINE INPUT  PARAMETER dIndividNr LIKE Individ.individnr    NO-UNDO.
  DEFINE INPUT  PARAMETER dPris      LIKE PrisKo.Pris    NO-UNDO.


  FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
  
  IF NOT AVAIL StrekKode THEN
      RETURN.
  IF StrekKode.StrKode > 0 THEN
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR. 
  FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.

  FIND ArtPris OF ArtBas NO-LOCK WHERE
    ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN     
    FIND FIRST artpris OF ArtBas NO-LOCK NO-ERROR.

  IF NOT AVAIL artpris THEN
      RETURN.
  IF iAntal = 0 THEN
      RETURN.
  FIND TT_etikett WHERE TT_etikett.kode = cStrekKode AND
                        TT_etikett.individnr = dIndividnr NO-ERROR.
  IF AVAIL TT_etikett AND dIndividnr > 0 THEN
      RETURN.
  ELSE IF AVAIL TT_etikett THEN DO:
      ASSIGN TT_etikett.antal = TT_etikett.antal + iAntal
             FI-Antall        = FI-Antall + iAntal.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  ELSE DO:
     
      CREATE TT_Etikett.
      ASSIGN iButik                = iButik + 1
             TT_etikett.antal      = iAntal
             TT_etikett.butik      = iButik
             TT_etikett.levinnr    = 1
             TT_etikett.lopnr      = ArtBas.lopnr
             TT_etikett.pris       = dPris
             TT_etikett.pris2      = ArtBas.AnbefaltPris
/*              TT_etikett.rad */
             TT_etikett.storlek    = IF AVAIL StrKonv THEN StrKonv.Storl ELSE ""
             TT_etikett.texten     = ArtBas.BongTekst
             TT_etikett.vg         = ArtBas.Vg
             TT_etikett.kode       = cStrekKode
             TT_etikett.artikkelnr = ArtBas.Artikkelnr
             TT_etikett.IndividNr  = dIndividNr
             FI-Antall             = FI-Antall + iAntal.
  END.
  APPLY "VALUE-CHANGED" TO BROWSE-Etiketter IN FRAME DEFAULT-FRAME.
  DISPLAY FI-Antall WITH FRAME FRAME-EAN.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyEtikettTelling C-Win 
PROCEDURE NyEtikettTelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iTelleNr   LIKE TelleHode.TelleNr NO-UNDO.
  DEFINE INPUT  PARAMETER cStrekKode LIKE StrekKode.Kode NO-UNDO.
  DEFINE INPUT  PARAMETER iAntal     AS INTEGER          NO-UNDO.
  DEFINE INPUT  PARAMETER dIndividNr LIKE Individ.individnr    NO-UNDO.

  DEF BUFFER telleButiker FOR Butiker.

  FIND TelleHode NO-LOCK WHERE
      TelleHode.TelleNr = iTelleNr NO-ERROR.
  IF NOT AVAILABLE TelleHode THEN
      RETURN.
  FIND telleButiker NO-LOCK WHERE
        telleButiker.Butik = INT(TelleHode.ButikkListe) NO-ERROR.
  IF NOT AVAILABLE tellebutiker THEN
      RETURN.

  FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL StrekKode THEN
      RETURN.
  IF StrekKode.StrKode > 0 THEN
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR. 
  FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.
      
  FIND ArtPris OF ArtBas NO-LOCK WHERE
    ArtPris.ProfilNr = telleButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN     
    FIND FIRST artpris OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAIL artpris THEN
      RETURN.

  IF iAntal = 0 THEN
      RETURN.
  FIND TT_etikett WHERE TT_etikett.kode = cStrekKode AND
                        TT_etikett.individnr = dIndividnr NO-ERROR.
  IF AVAIL TT_etikett AND dIndividnr > 0 THEN
      RETURN.
  ELSE IF AVAIL TT_etikett THEN DO:
      ASSIGN TT_etikett.antal = TT_etikett.antal + iAntal
             FI-Antall        = FI-Antall + iAntal.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  ELSE DO:
     
      CREATE TT_Etikett.
      ASSIGN iButik                = iButik +  1
             TT_etikett.antal      = iAntal
             TT_etikett.butik      = iButik
             TT_etikett.levinnr    = 1
             TT_etikett.lopnr      = ArtBas.lopnr
             TT_etikett.pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
             TT_etikett.pris2       = ArtPris.Pris[1]  /* Speciell hantering av tilbud vid inleveranseartikel */
             /*              TT_etikett.rad */
             TT_etikett.storlek    = IF AVAIL StrKonv THEN StrKonv.Storl ELSE ""
             TT_etikett.texten     = ArtBas.BongTekst
             TT_etikett.vg         = ArtBas.Vg
             TT_etikett.kode       = cStrekKode
             TT_etikett.artikkelnr = ArtBas.Artikkelnr
             TT_etikett.IndividNr  = dIndividNr
             FI-Antall             = FI-Antall + iAntal.
  END.
  APPLY "VALUE-CHANGED" TO BROWSE-Etiketter IN FRAME DEFAULT-FRAME.
  DISPLAY FI-Antall WITH FRAME FRAME-EAN.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivUt C-Win 
PROCEDURE SkrivUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iStartEtikett AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPrinterValg  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lIgjen        AS LOGICAL    NO-UNDO.
  
  DEFINE BUFFER bTT_Etikett FOR TT_Etikett.
  
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
      MESSAGE "Ingen etiketter registrert"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  RUN d-skrivervalg.w (OUTPUT iPrinterValg,OUTPUT iStartEtikett).
  IF iPrinterValg = 0 THEN
      RETURN.
      
  /* Henter butikk for RFID skrivere. */
  FIND SysPara WHERE 
    SysPara.SysHId = 5 AND 
    SysPara.SysGr = 21 AND 
    SysPara.ParaNr =  iPrinterValg NO-LOCK NO-ERROR.
  IF AVAILABLE SysPara AND SysPara.Beskrivelse MATCHES '*RFID*' THEN
  HENT_BUTIKKNR: 
  DO:
      FIND Bruker NO-LOCK WHERE 
        Bruker.BrukerID = USERID('Skotex') NO-ERROR.
      IF AVAILABLE Bruker AND Bruker.ButikkNr <> 0 THEN 
      DO:
         ASSIGN 
            iPkSdlbutNr = Bruker.ButikkNr
            .
         LEAVE HENT_BUTIKKNR.
      END.
      ELSE DO:
          THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
          RUN JBoxLookup.w (THIS-PROCEDURE,200,
                            "Butiker"
                            + ";Butik"
                            + ";Butnamn"
                           ,"WHERE true"
                            ,""                                                  
                            ,"Butik,Butnamn",   /* <- return values for these fields */
                            OUTPUT cReturnValues,
                            OUTPUT bOK).
          THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
        
          IF bOk AND cReturnValues NE "" THEN DO:
            ASSIGN iPkSdlbutNr = INT(ENTRY(1,cReturnValues,"|"))
                   .
        
          END.
          THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
      END.
  END. /* HENT_BUTIKKNR - RFID skriver håndtering slutt. */ 
   
  IF iStartEtikett > 1 THEN DO:
    CREATE EtikettLogg.
    ASSIGN EtikettLogg.Butik = 0
           EtikettLogg.SeqNr = 0
           EtikettLogg.Storl  = "STARTETIKETT"
           EtikettLogg.Ant = iStartEtikett.
    /* Ant avänds för att ange startetikett */
  END.
  
  IF lValgt = FALSE THEN DO:
      FOR EACH bTT_Etikett BY bTT_Etikett.ArtikkelNr BY bTT_Etikett.Kode:
          CREATE EtikettLogg.
          ASSIGN iCount = iCount + 1
/*                  EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */ */
                 EtikettLogg.butik     = iPkSdlbutNr
                 EtikettLogg.Vg        = bTT_Etikett.Vg
                 EtikettLogg.LopNr     = bTT_Etikett.Lopnr
                 EtikettLogg.Ant       = bTT_Etikett.Antal
                 EtikettLogg.Storl     = bTT_Etikett.Kode
                 EtikettLogg.Bongtekst = bTT_Etikett.texten
                 EtikettLogg.Pris      = IF (lInlev = TRUE AND NOT TG-Tilbud1:CHECKED IN FRAME FRAME-EAN) 
                                           THEN bTT_Etikett.Pris2 
                                           ELSE bTT_Etikett.Pris
                 EtikettLogg.Pris2     = bTT_Etikett.Pris2
                 EtikettLogg.Individ   = bTT_Etikett.Individnr
                 EtikettLogg.SeqNr     = iCount.
      END.
  END.
  ELSE DO:
      CREATE EtikettLogg.
      ASSIGN iCount = iCount + 1
/*              EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */ */
             EtikettLogg.butik     = iPkSdlbutNr
             EtikettLogg.Vg        = TT_Etikett.Vg
             EtikettLogg.LopNr     = TT_Etikett.Lopnr
             EtikettLogg.Ant       = TT_Etikett.Antal
             EtikettLogg.Storl     = TT_Etikett.Kode
             EtikettLogg.Bongtekst = TT_Etikett.texten
             EtikettLogg.Pris      = IF (lInlev = TRUE AND NOT TG-Tilbud1:CHECKED IN FRAME FRAME-EAN) 
                                       THEN TT_Etikett.Pris2 
                                       ELSE TT_Etikett.Pris
             EtikettLogg.Pris2     = TT_Etikett.Pris2
             EtikettLogg.Individ   = TT_Etikett.Individnr
             EtikettLogg.SeqNr     = iCount.
      DELETE TT_Etikett.
      BROWSE {&BROWSE-NAME}:DELETE-SELECTED-ROWS().
  END.
  C-Win:ALWAYS-ON-TOP = FALSE.
  REPEAT:
      RUN x-etikettsend.w (iPrinterValg).
      IF NOT lSkjul THEN DO:
          MESSAGE "Skrive ut igjen?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lIgjen.
          IF NOT lIgjen THEN
              LEAVE.
      END.
      ELSE
          LEAVE.
  END.
  IF lValgt = FALSE THEN 
      APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE DO:
      EMPTY TEMP-TABLE Etikettlogg.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinPosition C-Win 
PROCEDURE WinPosition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iX AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iY AS INTEGER    NO-UNDO.
  ASSIGN iX = hParentWin:X + hParentWin:WIDTH-PIXELS - 
                                     hFrameHandle:WIDTH-PIXELS
         iX = IF iX + hFrameHandle:WIDTH-PIXELS > SESSION:WIDTH-PIXELS THEN 
                        SESSION:WIDTH-PIXELS - hFrameHandle:WIDTH-PIXELS ELSE iX
         iX = IF iX < 1 THEN 1 ELSE iX
         iY = hParentWin:Y
         iY = IF hParentWin:Y < 1 THEN 1 ELSE iY
         iY = IF iY + hFrameHandle:HEIGHT-PIXELS > SESSION:HEIGHT-PIXELS THEN 
                        SESSION:HEIGHT-PIXELS - hFrameHandle:HEIGHT-PIXELS ELSE iY.

  ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:X = iX
         THIS-PROCEDURE:CURRENT-WINDOW:Y = iY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

