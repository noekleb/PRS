&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
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

DEFINE VARIABLE cType AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Analyse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Analyse

/* Definitions for BROWSE BROWSE-Analyse                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Analyse Analyse.AnalyseId getType() ~
Analyse.AnalyseType Analyse.Aktiv Analyse.StartDato Analyse.SluttDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Analyse 
&Scoped-define QUERY-STRING-BROWSE-Analyse FOR EACH Analyse NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Analyse OPEN QUERY BROWSE-Analyse FOR EACH Analyse NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Analyse Analyse
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Analyse Analyse


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Analyse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-Analyse FI-PLU-Strek B-Starta 
&Scoped-Define DISPLAYED-OBJECTS FI-PLU-Strek FI-Behandlas FI-count ~
FI-TotAnt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getType C-Win 
FUNCTION getType RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Starta 
     LABEL "Starta" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Behandlas AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nu behandlas" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-count AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Nummer i process" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-PLU-Strek AS CHARACTER FORMAT "X(2000)":U 
     LABEL "PLUer/Streckkoder" 
     VIEW-AS FILL-IN 
     SIZE 106 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TotAnt AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Totalt antal" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Analyse FOR 
      Analyse SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Analyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Analyse C-Win _STRUCTURED
  QUERY BROWSE-Analyse NO-LOCK DISPLAY
      Analyse.AnalyseId FORMAT ">>>>>9":U
      getType() COLUMN-LABEL "Analystyp" FORMAT "x(10)":U
      Analyse.AnalyseType FORMAT ">9":U
      Analyse.Aktiv FORMAT "yes/no":U
      Analyse.StartDato FORMAT "99/99/99":U
      Analyse.SluttDato FORMAT "99/99/99":U WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 4.29 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-Analyse AT ROW 1.24 COL 22
     FI-PLU-Strek AT ROW 5.76 COL 20 COLON-ALIGNED
     FI-Behandlas AT ROW 6.95 COL 20 COLON-ALIGNED
     FI-count AT ROW 6.95 COL 59 COLON-ALIGNED
     FI-TotAnt AT ROW 6.95 COL 81.4 COLON-ALIGNED
     B-Starta AT ROW 8.38 COL 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129 BY 9.67.


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
         TITLE              = "Uppdatering av SL-kortbongar"
         HEIGHT             = 9.67
         WIDTH              = 129
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 139.8
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 139.8
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
/* BROWSE-TAB BROWSE-Analyse 1 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Behandlas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-count IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-PLU-Strek:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-TotAnt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Analyse
/* Query rebuild information for BROWSE BROWSE-Analyse
     _TblList          = "data.Analyse"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = data.Analyse.AnalyseId
     _FldNameList[2]   > "_<CALC>"
"getType()" "Analystyp" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = data.Analyse.AnalyseType
     _FldNameList[4]   = data.Analyse.Aktiv
     _FldNameList[5]   = data.Analyse.StartDato
     _FldNameList[6]   > data.Analyse.SluttDato
"SluttDato" ? ? "date" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-Analyse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Uppdatering av SL-kortbongar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Uppdatering av SL-kortbongar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Starta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Starta C-Win
ON CHOOSE OF B-Starta IN FRAME DEFAULT-FRAME /* Starta */
DO:
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    IF Analyse.Aktiv = FALSE THEN DO:
        MESSAGE "Analysen er ikke aktiv"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    IF (Analyse.StartDato = ? OR AnaLyse.SluttDato = ?) OR 
        (Analyse.StartDato > AnaLyse.SluttDato)         THEN DO:
        MESSAGE "Feilaktigt dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    IF Analyse.AnalyseType = 1 THEN DO: /* SL-Kort */
        FOR EACH AnalyseArtikkel OF Analyse WHERE AnalyseArtikkel.Aktiv = TRUE NO-LOCK:
            ASSIGN iCount = iCount + 1
                   FI-count:SCREEN-VALUE = STRING(iCount)
                   FI-Behandlas:SCREEN-VALUE = STRING(AnaLyseArtikkel.Artikkelnr).
            FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = AnalyseArtikkel.ArtikkelNr NO-LOCK:
                RUN FinnSLBonger(StrekKode.Kode).
            END.
            ASSIGN FI-PLU-Strek = FI-PLU-Strek + (IF FI-PLU-Strek <> "" THEN "," ELSE "") + FI-Behandlas:SCREEN-VALUE
                   FI-PLU-Strek:SCREEN-VALUE = FI-PLU-Strek.
            PROCESS EVENTS.
        END.
    END.
    ELSE DO:
        MESSAGE "Inte SL-Analys"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Analyse
&Scoped-define SELF-NAME BROWSE-Analyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Analyse C-Win
ON VALUE-CHANGED OF BROWSE-Analyse IN FRAME DEFAULT-FRAME /* Browse 3 */
DO:
  ASSIGN FI-TotAnt = 0.
  FOR EACH AnalyseArtikkel OF Analyse WHERE AnalyseArtikkel.Aktiv = TRUE NO-LOCK:
      ASSIGN FI-TotAnt = FI-TotAnt + 1.
  END.
  ASSIGN FI-TotAnt:SCREEN-VALUE = STRING(FI-TotAnt).
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
/*     ASSIGN FI-PLU-Strek = "27,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,94,95,288,1496,1497,1498,1499,1500,1501,1502,1503,1504,1505,1506,1507,1508,1509,1510,1512,1514,1515,1516,1517,1518,1519,1520,2727,2929,3030,3131,3232,7393528210017,7393528210024,7393528210031,7393528210048,7393528210055,7393528210062,7393528210079,7393528210086,7393528210093,7393528210109,7393528210116,7393528210123,7393528210130" */
/*            FI-Totalt    = NUM-ENTRIES(FI-PLU-Strek).                                                                                                                                                                                                                                                                                                                                                                                                     */
    
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO BROWSE-Analyse.
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
  DISPLAY FI-PLU-Strek FI-Behandlas FI-count FI-TotAnt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BROWSE-Analyse FI-PLU-Strek B-Starta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnSLBonger C-Win 
PROCEDURE FinnSLBonger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStrekKode AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      FOR EACH BongLinje WHERE BongLinje.StrekKode = cStrekKode AND BongLinje.Dato >= Analyse.StartDato AND 
                               BongLinje.Dato <= Analyse.SluttDato USE-INDEX StrekKode NO-LOCK.
          DO TRANSACTION:
              FIND BongHode WHERE BongHode.b_id = Bonglinje.b_id NO-ERROR.
              IF AVAIL BongHode THEN DO:
                  CREATE AnalyseLogg.
                  ASSIGN AnalyseLogg.AnalyseId  = AnalyseArtikkel.AnalyseId
                         AnalyseLogg.ArtikkelNr = 0 /* AnalyseArtikkel.ArtikkelNr */
                         AnalyseLogg.BongNr     = BongLinje.BongNr
                         AnalyseLogg.ButikkNr   = BongLinje.ButikkNr
                         AnalyseLogg.b_id       = BongLinje.b_id    
                         AnalyseLogg.Dato       = BongLinje.Dato    
                         AnalyseLogg.GruppeNr   = BongLinje.GruppeNr
                         AnalyseLogg.KasseNr    = BongLinje.KasseNr 
                         AnalyseLogg.LinjeNr    = 0 /* BongLinje.LinjeNr */ NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN
                      DELETE AnalyseLogg.
                  ELSE
                      ASSIGN BongHode.flSlKort = 1.
              END.
          END.
      END.
      /* 
                       FOR EACH Analyse WHERE Analyse.Aktiv = TRUE AND
                                       Analyse.StartDato <= BongLinje.Dato AND 
                                       Analyse.SluttDato >= BongLinje.Dato NO-LOCK.
                    FOR EACH  AnalyseArtikkel WHERE
                              AnalyseArtikkel.AnalyseId  = Analyse.AnalyseId AND
                              AnalyseArtikkel.ArtikkelNr = DEC(BongLinje.ArtikkelNr) AND
                              AnalyseArtikkel.Aktiv      = TRUE NO-LOCK.
/*                               AnalyseArtikkel.StartDato <= BongLinje.Dato AND      */
/*                               AnalyseArtikkel.SluttDato >= BongLinje.Dato NO-LOCK. */
                        CREATE AnalyseLogg.
                        ASSIGN AnalyseLogg.AnalyseId  = AnalyseArtikkel.AnalyseId
                               AnalyseLogg.ArtikkelNr = IF Analyse.AnalyseType = 1 THEN 0 ELSE AnalyseArtikkel.ArtikkelNr
                               AnalyseLogg.BongNr     = BongLinje.BongNr
                               AnalyseLogg.ButikkNr   = BongLinje.ButikkNr
                               AnalyseLogg.b_id       = BongLinje.b_id    
                               AnalyseLogg.Dato       = BongLinje.Dato    
                               AnalyseLogg.GruppeNr   = BongLinje.GruppeNr
                               AnalyseLogg.KasseNr    = BongLinje.KasseNr 
                               AnalyseLogg.LinjeNr    = IF Analyse.AnalyseType = 1 THEN 0 ELSE BongLinje.LinjeNr NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN
                            DELETE AnalyseLogg.
                        ELSE
                            ASSIGN BongHode.flSlKort = IF Analyse.AnalyseType = 1 THEN 1 ELSE 0.
                    END.
                END.

       
       
       
       */




  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getType C-Win 
FUNCTION getType RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF Analyse.AnalyseType = 1 THEN "SL-Kort" ELSE "?okänd?".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

