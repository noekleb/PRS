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
DEF VAR lStrTypeNye        AS   LOG                NO-UNDO.
DEF VAR lStrTypeOverskriv  AS   LOG                NO-UNDO.
DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

{runlib.i}

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
         TITLE              = "Import av størrelsestype"
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
ON END-ERROR OF C-Win /* Import av størrelsestype */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import av størrelsestype */
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
{syspara.i 1 2 1008 cEDB-System}
{syspar2.i 1 2 1008 cImpTabell}

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
  
  #KOD 01
  #BESK Franska helnummer
  #STLK1 16
  #STLK2 17
  #STLK3 18
  #STLK4 19
  #STLK5 20
  #STLK6 21
  #STLK7 22
  #STLK8 23
  #STLK9 24
  #STLK10 25
  #STLK11 26
  #STLK12 27
  #STLK13 28
  #STLK14 29
  #STLK15 30
  #STLK16 31
  #STLK17 32
  #STLK18 33
  #STLK19 34
  #STLK20 35
  #STLK21 36
  #STLK22 37
  #STLK23 38
  #STLK24 39
  #STLK25 40
  #STLK26 41
  #STLK27 42
  #STLK28 43
  #STLK29 44
  #STLK30 45
  #STLK31 46
  #STLK32 47
  #STLK33 48
  #STLK34 49
  #STLK35 50
  #STLK36 51
  #STLK37 52
  #STLK38 53
  #STLK39 54
  #STLK40 55
------------------------------------------------------------------------------*/
  DEF VAR piLoop      AS INT  NO-UNDO.
  DEF VAR pi2Loop     AS INT  NO-UNDO.
  DEF VAR pcStorl     AS char NO-UNDO.
  DEF VAR piOfset     AS INT  NO-UNDO.
  DEF VAR piStrType   AS INT  NO-UNDO.
  
  MAINLOOP:
  FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
      ImportLinje.BatchNr = iBatchNr AND
      ImportLinje.Tabell  = cTabell:

      ASSIGN
          piOfset = 0
          piLoop  = piLoop + 1
          FI-Info = "Behandler post " + STRING(piLoop) + "."
          .
      DISPLAY
          FI-Info
      WITH FRAME Default-Frame.
      /*
          MESSAGE "Nøkkel:" skip
                  ImportLinje.Felt[1] SKIP
                  ImportLinje.Felt[2] SKIP
                  ImportLinje.Felt[3]
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */

      ASSIGN
        piStrType = int(ImportLinje.Felt[1])
        .

      /* Utfører konvertering. */
      FIND FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = cImpTabell AND 
          ImpKonv.EksterntId = string(ImportLinje.Felt[1]) NO-ERROR.
      IF AVAILABLE ImpKonv THEN
        ASSIGN
          piStrType = int(ImpKonv.InterntId)
          .

      FIND FIRST StrType EXCLUSIVE-LOCK WHERE
          StrType.StrTypeId = piStrType NO-ERROR.
      
      /* Er det ny post, sjekk om den skal opprettes ? */
      IF lStrTypeNye = FALSE AND NOT AVAILABLE StrType THEN
          NEXT MAINLOOP.

      IF NOT AVAILABLE StrType THEN
      DO:
          CREATE StrType.
          ASSIGN
              /* '#KOD' */ StrType.StrTypeId = piStrType
              .
      END.
      ASSIGN
        /* '#KOD'      */  StrType.KortNavn    = ImportLinje.Felt[1]
        /* '#BESK'     */  StrType.Beskrivelse = ImportLinje.Felt[2]
                           StrType.Intervall   = ""
                           StrType.Fordeling   = ""
        .

      /* Hvis det ikke er en ny varegrupper, skal data overskrives ? */
      IF lStrTypeOverskriv = FALSE AND NOT NEW StrType THEN
          NEXT MAINLOOP.

      /* Plukker bort de gamle størrelsestypen. */
      FOR EACH StrTStr OF StrType EXCLUSIVE-LOCK:
          DELETE StrTStr.
      END.

      /* Legger opp de nye størrelsene */
      STRLOOP:
      DO pi2Loop = 1 TO 40:
        IF ImportLinje.Felt[pi2Loop + 2] = "" THEN
        DO:
            ASSIGN
                piOfset = piOfset + 1
                .
            NEXT STRLOOP.
        END.
        
        ASSIGN
          pcStorl = ImportLinje.Felt[pi2Loop + 2]
          .

        /* Konverterer størrelsen. */
        IF VALID-HANDLE(wLibHandle) THEN
            RUN FiksStorl IN wLibHandle (INPUT-OUTPUT pcStorl).

        /* Legger opp størrelsen */
        FIND StrTStr EXCLUSIVE-LOCK WHERE
            StrTStr.StrTypeId = StrType.StrTypeId AND
            StrTStr.SeqNr     = pi2Loop - piOfset NO-ERROR.
        IF NOT AVAILABLE StrTStr THEN
        DO:
            CREATE StrTStr.
            ASSIGN
                StrTStr.StrTypeId = StrType.StrTypeId
                StrTStr.SeqNr     = pi2Loop
                .
        END.
        ASSIGN
            StrTStr.SoStorl = pcStorl
            .
      END. /* STRLOOP */
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
  DEF INPUT PARAMETER plStrTypeNye       AS LOG NO-UNDO.
  DEF INPUT PARAMETER plStrTypeOverskriv AS LOG NO-UNDO.
  
  ASSIGN
    lStrTypeNye       = plStrTypeNye      
    lStrTypeOverskriv = plStrTypeOverskriv
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

