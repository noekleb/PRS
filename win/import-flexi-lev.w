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
DEF INPUT PARAMETER iBatchNr AS INT  NO-UNDO.
DEF INPUT PARAMETER cTabell  AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR lLevNye        AS LOG NO-UNDO.
DEF VAR lLevOverskriv  AS LOG NO-UNDO.
DEF VAR cEDB-System    LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell     LIKE ImpKonv.Tabell     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Info AT ROW 1.48 COL 2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.4 BY 2.14.


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
         TITLE              = "Import av leverandører"
         HEIGHT             = 2.14
         WIDTH              = 74.4
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
/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import av leverandører */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import av leverandører */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

/* Henter parametre for konvertering. */
{syspara.i 1 2 1000 cEDB-System}
{syspar2.i 1 2 1000 cImpTabell}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}
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
  DISPLAY FI-Info 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importer C-Win 
PROCEDURE Importer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  #KOD *
  #NAMN FF
  #FÖRETAG Fem Fackmän AB
  #REFERENS 
  #ADRESS1 
  #ADRESS2 
  #POSTNR 
  #POSTORT 
  #LAND SVERIGE
  #TELEFON 
  #TELEFAX 
  #EMAIL 
  #WWW 
  #VALUTA SEK
  #LEVVILLKOR 
  #KUNDKOD 
  #BETVILLKOR 
  #RABPROC 0
  #MARG 0
  #KOSTPROC 0
  #KOSTKR 0
  #TULLPROC 0
  #FRAKT1 0
  #FRAKT2 0
  #GROSS 
  #TYP 
  
------------------------------------------------------------------------------*/
  DEF VAR piLoop  AS INT NO-UNDO.
  DEF VAR piLevNr AS INT NO-UNDO.
  
  MAINLOOP:
  FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
      ImportLinje.BatchNr = iBatchNr AND
      ImportLinje.Tabell  = cTabell:

      ASSIGN
          piLoop  = piLoop + 1
          FI-Info = "Behandler post " + STRING(piLoop) + "."
          .
      DISPLAY
          FI-Info
      WITH FRAME Default-Frame.

      ASSIGN
        piLevNr = int(ImportLinje.Felt[1])
        .

      /* Utfører konvertering. */
      FIND FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = cImpTabell AND 
          ImpKonv.EksterntId = trim(ImportLinje.Felt[ 1]) NO-ERROR.
      IF AVAILABLE ImpKonv THEN
        ASSIGN
          piLevNr = int(ImpKonv.InterntId)
          .
      /* Sjekker om leverandøren er opprettet fra før. */
      FIND LevBas EXCLUSIVE-LOCK WHERE
          LevBas.LevNr = piLevNr NO-ERROR.
      
      /* Er det ny leverandør, sjekk om den skal opprettes ? */
      IF lLevNye = FALSE AND NOT AVAILABLE LevBas THEN
          NEXT MAINLOOP.

      /* Oppretter ny post. */
      IF NOT AVAILABLE LevBas THEN
      DO:
          CREATE LevBas .
          ASSIGN
              /* '#KOD' */ LevBas.LevNr      = piLevNr
              .
      END.

      /* Hvis det ikke er en ny leverandør, skal data overskrives ? */
      IF lLevOverskriv = FALSE AND NOT NEW LevBas THEN
          NEXT MAINLOOP.

      /* fikser postnummer/adresse */
      IF NOT CAN-FIND(Post WHERE
                      Post.PostNr = TRIM(ImportLinje.Felt[ 7])) THEN
      DO:
          CREATE Post.
          ASSIGN
              Post.PostNr      = TRIM(ImportLinje.Felt[ 7])
              Post.Beskrivelse = TRIM(ImportLinje.Felt[ 8])
              Post.KommNr      = "1"
              Post.fylkesNr    = "1"
              .
      END.

      ASSIGN
        /*#NAMN      */ 
        /*#FÖRETAG   */ LevBas.LevNamn       = ImportLinje.Felt[ 3]
        /*#REFERENS  */ LevBas.LevKon        = ImportLinje.Felt[ 4]
        /*#ADRESS1   */ LevBas.LevAdr        = ImportLinje.Felt[ 5]
        /*#ADRESS2   */
        /*#POSTNR    */ LevBas.LevPoNr       = ImportLinje.Felt[ 7]
        /*#POSTORT   */ LevBas.LevPAdr       = ImportLinje.Felt[ 8]
        /*#LAND      */ LevBas.LevLand       = ImportLinje.Felt[ 9]
        /*#TELEFON   */ LevBas.LevTel        = ImportLinje.Felt[10]
        /*#TELEFAX   */ LevBas.TeleFax       = ImportLinje.Felt[11]
        /*#EMAIL     */ LevBas.Kommentar[1]  = "EMAIL: " + ImportLinje.Felt[12]
        /*#WWW       */ LevBas.Kommentar[2]  = "WWW: "   + ImportLinje.Felt[13]
        /*#VALUTA    */ LevBas.ValKod        = ImportLinje.Felt[14]
        /*#LEVVILLKOR*/ LevBas.Kommentar[3]  = "Lev.Vilk: "  + ImportLinje.Felt[15]
        /*#KUNDKOD   */ LevBas.Kommentar[4]  = "Kundekode: " + ImportLinje.Felt[16]
        /*#BETVILLKOR*/ LevBas.Notat         = "Betalvilkår: " + ImportLinje.Felt[17] + CHR(10) +
        /*#RABPROC   */                        "RabProc "  + ImportLinje.Felt[18] + CHR(10) +
        /*#MARG      */                        "Marginal " + ImportLinje.Felt[19] + CHR(10) +
        /*#KOSTPROC  */                        "KostProc " + ImportLinje.Felt[20] + CHR(10) +
        /*#KOSTKR    */                        "KostKr "   + ImportLinje.Felt[21] + CHR(10) +
        /*#TULLPROC  */                        "TollProc " + ImportLinje.Felt[22] + CHR(10) +
        /*#FRAKT1    */                        "Frakt1 "   + ImportLinje.Felt[23] + CHR(10) +
        /*#FRAKT2    */                        "Faakt2 "   + ImportLinje.Felt[24] + CHR(10) +
        /*#GROSS     */                        "Gross "    + ImportLinje.Felt[25] + CHR(10) +
        /*#TYP       */                        "Type "     + ImportLinje.Felt[26]
        .

  END. /* MAINLOOP */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettPara C-Win 
PROCEDURE SettPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plLevNye       AS LOG NO-UNDO.
  DEF INPUT PARAMETER plLevOverskriv AS LOG NO-UNDO.
  
  ASSIGN
    lLevNye       = plLevNye      
    lLevOverskriv = plLevOverskriv
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartImport C-Win 
PROCEDURE StartImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Importer.
  RUN StoppImport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StoppImport C-Win 
PROCEDURE StoppImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

