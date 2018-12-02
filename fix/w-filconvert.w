&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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
DEF STREAM InnFil.

DEFINE TEMP-TABLE TT_Filer
    FIELD Radnr     AS INTEGER 
    FIELD FilNavn   AS CHARACTER FORMAT "x(15)" LABEL "Filnavn"
    FIELD Katalog   AS CHARACTER LABEL "Katalog"
    FIELD Storrelse AS INTEGER   FORMAT ">>>,>>>,>>9" LABEL "Storlek"
    FIELD Konvtid   AS CHAR      LABEL "Konverteringstid"
    INDEX Radnr IS PRIMARY Radnr ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Filer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Filer

/* Definitions for BROWSE BROWSE-Filer                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Filer FilNavn /* Katalog */ Storrelse Konvtid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Filer   
&Scoped-define SELF-NAME BROWSE-Filer
&Scoped-define OPEN-QUERY-BROWSE-Filer OPEN QUERY {&SELF-NAME} FOR EACH TT_Filer.
&Scoped-define TABLES-IN-QUERY-BROWSE-Filer TT_Filer
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Filer TT_Filer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Filer}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Dir BROWSE-Filer B-LesInn B-Konvertera 
&Scoped-Define DISPLAYED-OBJECTS FI-Dir FI-Konv FI-Antal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Konvertera 
     LABEL "Konvertera" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-LesInn 
     LABEL "Läs in filer" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Antal AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antal inlästa filer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Dir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sourcedirectory" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Konv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Targetdirectory" 
     VIEW-AS FILL-IN 
     SIZE 52.2 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Filer FOR 
      TT_Filer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Filer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Filer C-Win _FREEFORM
  QUERY BROWSE-Filer DISPLAY
      FilNavn  
/* Katalog */
Storrelse
Konvtid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 48 BY 11.57 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Dir AT ROW 1.48 COL 17 COLON-ALIGNED
     FI-Konv AT ROW 2.67 COL 16.8 COLON-ALIGNED
     FI-Antal AT ROW 3.86 COL 17 COLON-ALIGNED
     BROWSE-Filer AT ROW 5.29 COL 7
     B-LesInn AT ROW 5.29 COL 56
     B-Konvertera AT ROW 6.71 COL 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71 BY 16.14.


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
         TITLE              = "Filkonvertering"
         HEIGHT             = 16.14
         WIDTH              = 71
         MAX-HEIGHT         = 16.24
         MAX-WIDTH          = 116.6
         VIRTUAL-HEIGHT     = 16.24
         VIRTUAL-WIDTH      = 116.6
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
/* BROWSE-TAB BROWSE-Filer FI-Antal DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Antal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Konv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Filer
/* Query rebuild information for BROWSE BROWSE-Filer
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Filer.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Filer */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Filkonvertering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Filkonvertering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Konvertera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Konvertera C-Win
ON CHOOSE OF B-Konvertera IN FRAME DEFAULT-FRAME /* Konvertera */
DO:
    DEFINE VARIABLE stat AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTidsforbruk AS INTEGER    NO-UNDO.
    DEFINE BUFFER bTT_Filer FOR TT_Filer.
    SESSION:SET-WAIT-STATE("GENERAL").
    DO WITH FRAME {&FRAME-NAME}:
        OS-CREATE-DIR VALUE(FI-Dir:SCREEN-VALUE + "\konv").
        ASSIGN stat = OS-ERROR.
        IF stat NE 0 THEN DO:
            MESSAGE "Directory " FI-Dir:SCREEN-VALUE  "\konv" " kann ikke skapes. "
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END. 
    {&BROWSE-NAME}:SET-REPOSITIONED-ROW(5,"CONDITIONAL") .
    FOR EACH bTT_Filer:
        REPOSITION {&BROWSE-NAME} TO ROWID ROWID(bTT_Filer).
        PROCESS EVENTS.
        RUN BytUt13 (FI-Dir:SCREEN-VALUE + "\" + bTT_Filer.FilNavn,
                     FI-Dir:SCREEN-VALUE + "\konv\" + bTT_Filer.FilNavn,
                     OUTPUT iTidsforbruk).
        ASSIGN TT_Filer.Konvtid = STRING(iTidsforbruk,"HH:MM:SS").
        BROWSE {&BROWSE-NAME}:REFRESH().
        PROCESS EVENTS.
    END.
    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LesInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LesInn C-Win
ON CHOOSE OF B-LesInn IN FRAME DEFAULT-FRAME /* Läs in filer */
DO:
  RUN LesInnFilnavn.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Filer
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
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytUt13 C-Win 
PROCEDURE BytUt13 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cSourcefil   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cKonvfil     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER iTidsForbruk AS INTEGER    NO-UNDO.
   DEFINE VARIABLE         iStartTid    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE         iCount       AS INTEGER    NO-UNDO.

   ASSIGN iStartTid = TIME.
   INPUT FROM VALUE(cSourcefil) UNBUFFERED.
   OUTPUT TO VALUE(cKonvfil) NO-ECHO.
   FILE-INFO:FILENAME = cSourcefil.
   REPEAT WHILE iCount < FILE-INFO:FILE-SIZE:
       READKEY.
       IF LASTKEY = 13 THEN
           PUT UNFORMATTED CHR(10).
       ELSE
           PUT UNFORMATTED CHR(LASTKEY).
       iCount = iCount + 1.
   END.
   INPUT CLOSE.
   OUTPUT CLOSE.
   ASSIGN iTidsforbruk = TIME - iStartTid.
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
  DISPLAY FI-Dir FI-Konv FI-Antal 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Dir BROWSE-Filer B-LesInn B-Konvertera 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFilnavn C-Win 
PROCEDURE LesInnFilnavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntFil AS INTEGER    NO-UNDO.
  DEF VAR pcFileName    AS CHAR FORMAT "x(50)" NO-UNDO.
  DEF VAR pcFilePath    AS CHAR FORMAT "x(100)" NO-UNDO.
  DEF VAR pcFileAttrib  AS CHAR FORMAT "x(15)" NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN FI-Dir.
    INPUT STREAM InnFil FROM OS-DIR (FI-Dir) NO-ECHO.
    FILINPUT:
    REPEAT:
      SET STREAM InnFil
        pcFileName  
        pcFilePath  
        pcFileAttrib
        WITH WIDTH 248.
      /* Bare filer skal opprettes */
      IF LOOKUP("F",pcFileAttrib) <> 0 THEN
      DO:
        /* Åpner for filinformasjonen */
        assign
          FILE-INFO:FILE-NAME = pcFilePath
          . 
  
        DO:
          /* Finner FilId */
          CREATE TT_Filer.
          ASSIGN
            iAntFil            = iAntFil + 1
            TT_Filer.Radnr     = iAntFil
            TT_Filer.FilNavn   = pcFileName
            TT_Filer.Katalog   = pcFilePath
            TT_Filer.Storrelse = FILE-INFO:FILE-SIZE
            .
        END.
      END.
  
    END. /* FILINPUT */
    INPUT STREAM InnFil CLOSE.
    ASSIGN FI-Dir:SENSITIVE = FALSE 
           FI-Konv:SCREEN-VALUE = FI-Dir:SCREEN-VALUE + "\konv"
           FI-Antal:SCREEN-VALUE = STRING(iAntFil).
  END.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

