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
DEFINE TEMP-TABLE TT_RapHodePB
    FIELD b_id      LIKE BongHode.b_id
    FIELD betalkort AS LOGICAL 
    FIELD BongType  AS INTE /* 1 = bara SL 2 = båda */
    FIELD antall    AS DECI EXTENT 12 LABEL "Antal" /*                      */
    FIELD Belop     AS DECI EXTENT 12 LABEL "Belopp" /*                      */
    FIELD sumFsgSL  AS DECI EXTENT 12 LABEL "sumSL" /*                      */
    FIELD sumOvr    AS DECI EXTENT 12 LABEL "sumOvr" /*                      */
    INDEX BID IS PRIMARY UNIQUE b_id.


DEFINE TEMP-TABLE TT_RapLinjePB
    FIELD b_id      LIKE BongHode.b_id
    FIELD grupp     AS INTE
    FIELD StrekKode LIKE Bonglinje.StrekKode
    FIELD antall    AS DECI EXTENT 12  /*                      */
    FIELD sumFsg    AS DECI EXTENT 12  /*                      */
    INDEX BID IS PRIMARY UNIQUE b_id StrekKode
    INDEX grupp grupp.

DEFINE TEMP-TABLE TmpTT_RapLinjePB LIKE TT_RapLinjePB.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TT_RapHodePB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_RapHodePB TT_RapLinjePB

/* Definitions for BROWSE BROWSE-TT_RapHodePB                           */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_RapHodePB TT_RapHodePB.b_id TT_RapHodePB.betalkort TT_RapHodePB.BongType TT_RapHodePB.antall TT_RapHodePB.belop TT_RapHodePB.sumFsgSL TT_RapHodePB.sumOvr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_RapHodePB   
&Scoped-define SELF-NAME BROWSE-TT_RapHodePB
&Scoped-define QUERY-STRING-BROWSE-TT_RapHodePB FOR EACH TT_RapHodePB
&Scoped-define OPEN-QUERY-BROWSE-TT_RapHodePB OPEN QUERY {&SELF-NAME} FOR EACH TT_RapHodePB.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_RapHodePB TT_RapHodePB
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_RapHodePB TT_RapHodePB


/* Definitions for BROWSE BROWSE-TT_RapLinjePB                          */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_RapLinjePB TT_RapLinjePB.b_id TT_RapLinjePB.grupp TT_RapLinjePB.StrekKode TT_RapLinjePB.antall TT_RapLinjePB.sumFsg   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_RapLinjePB   
&Scoped-define SELF-NAME BROWSE-TT_RapLinjePB
&Scoped-define QUERY-STRING-BROWSE-TT_RapLinjePB FOR EACH TT_RapLinjePB OF TT_RapHodePB
&Scoped-define OPEN-QUERY-BROWSE-TT_RapLinjePB OPEN QUERY {&SELF-NAME} FOR EACH TT_RapLinjePB OF TT_RapHodePB.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_RapLinjePB TT_RapLinjePB
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_RapLinjePB TT_RapLinjePB


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TT_RapHodePB}~
    ~{&OPEN-QUERY-BROWSE-TT_RapLinjePB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Summera BROWSE-TT_RapHodePB FI-Butik ~
B-Export BROWSE-TT_RapLinjePB 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Export 
     LABEL "EXPORT" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Summera 
     LABEL "Summera" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Butik AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butik" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TT_RapHodePB FOR 
      TT_RapHodePB SCROLLING.

DEFINE QUERY BROWSE-TT_RapLinjePB FOR 
      TT_RapLinjePB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TT_RapHodePB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_RapHodePB C-Win _FREEFORM
  QUERY BROWSE-TT_RapHodePB DISPLAY
      TT_RapHodePB.b_id     
TT_RapHodePB.betalkort
TT_RapHodePB.BongType 
TT_RapHodePB.antall FORMAT "->>>,>>>,>>9.99"  
TT_RapHodePB.belop  FORMAT "->>>,>>>,>>9.99"
TT_RapHodePB.sumFsgSL FORMAT "->>>,>>>,>>9.99"
TT_RapHodePB.sumOvr  FORMAT "->>>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 56 BY 6.67 EXPANDABLE.

DEFINE BROWSE BROWSE-TT_RapLinjePB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_RapLinjePB C-Win _FREEFORM
  QUERY BROWSE-TT_RapLinjePB DISPLAY
      TT_RapLinjePB.b_id     
TT_RapLinjePB.grupp    
TT_RapLinjePB.StrekKode
TT_RapLinjePB.antall  FORMAT "->>>,>>>,>>9.99" 
TT_RapLinjePB.sumFsg FORMAT "->>>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112 BY 14.29 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Summera AT ROW 1.24 COL 100
     BROWSE-TT_RapHodePB AT ROW 1.48 COL 3
     FI-Butik AT ROW 1.71 COL 64 COLON-ALIGNED
     B-Export AT ROW 2.91 COL 100
     BROWSE-TT_RapLinjePB AT ROW 8.62 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.2 BY 22.24.


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
         TITLE              = "Summera PB-SL"
         HEIGHT             = 22.24
         WIDTH              = 119.2
         MAX-HEIGHT         = 22.29
         MAX-WIDTH          = 147
         VIRTUAL-HEIGHT     = 22.29
         VIRTUAL-WIDTH      = 147
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
/* BROWSE-TAB BROWSE-TT_RapHodePB B-Summera DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-TT_RapLinjePB B-Export DEFAULT-FRAME */
ASSIGN 
       FI-Butik:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_RapHodePB
/* Query rebuild information for BROWSE BROWSE-TT_RapHodePB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_RapHodePB.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_RapHodePB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_RapLinjePB
/* Query rebuild information for BROWSE BROWSE-TT_RapLinjePB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_RapLinjePB OF TT_RapHodePB.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_RapLinjePB */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Summera PB-SL */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Summera PB-SL */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Export C-Win
ON CHOOSE OF B-Export IN FRAME DEFAULT-FRAME /* EXPORT */
DO:
    OUTPUT TO VALUE(SESSION:TEMP-DIR + "NySum2.skv").
  FOR EACH TT_RapHodePB:
      EXPORT DELIMITER ";" TT_RapHodePB.
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE(SESSION:TEMP-DIR + "NyLinje2.skv").
/*   OUTPUT TO "c:\work\tt_raplinjepb.d". */
  FOR EACH TT_RapLinjePB:
      EXPORT DELIMITER ";" TT_RapLinjePB.
  END.
  OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Summera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Summera C-Win
ON CHOOSE OF B-Summera IN FRAME DEFAULT-FRAME /* Summera */
DO:
  DEFINE VARIABLE l_BetKort AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE l_Ovr     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE i_b_id    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTmpSum   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dFraDat   AS DATE       NO-UNDO.
    DEFINE VARIABLE dTilDat   AS DATE       NO-UNDO.
    DEFINE VARIABLE dDatoLoop AS DATE       NO-UNDO.
    DEFINE VARIABLE cSLPluEan AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dSumTstTmp AS DECIMAL    NO-UNDO.
  IF SESSION:SET-WAIT-STATE("GENERAL") THEN.
/*   FOR EACH bonghode /* WHERE flslkort = 1 */ NO-LOCK: */
  FOR EACH AnalyseArtikkel WHERE AnalyseId = 1 NO-LOCK:
      FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = AnalyseArtikkel.ArtikkelNr NO-LOCK:
          ASSIGN cSLPluEan = cSLPluEan + (IF cSLPluEan <> "" THEN "," ELSE "") + TRIM(StrekKode.Kode).
      END.
  END.
  ASSIGN dFraDat = DATE(01,01,2003)
         dTilDat = DATE(08,31,2003).
  FOR EACH Butiker WHERE Butiker.butik /*= 8119 */ < 10000 OR Butiker.butik = 81001 NO-LOCK:
      ASSIGN FI-Butik:SCREEN-VALUE = STRING(Butiker.Butik,"99999").
      PROCESS EVENTS.
      FIND FIRST kasse WHERE kasse.butik = Butiker.butik NO-LOCK NO-ERROR.
      IF NOT AVAIL kasse THEN DO:
          OUTPUT TO VALUE(SESSION:TEMP-DIR + STRING(Butiker.butik,"99999") + "NoKasse.txt").
          OUTPUT CLOSE.
          NEXT.
      END.
      DO dDatoLoop = dFraDat TO dTilDat:
          FOR EACH Kasse WHERE Kasse.butik = Butiker.butik NO-LOCK:
              FOR EACH BongHode WHERE BongHode.ButikkNr = Kasse.Butik    AND
                                      BongHode.GruppeNr = Kasse.GruppeNr AND
                                      BongHode.KasseNr  = Kasse.KasseNr  AND
                                      BongHode.Dato     = dDatoLoop USE-INDEX Bong NO-LOCK:
                  FOR EACH tmpTT_RapLinjePB: 
                      DELETE tmpTT_RapLinjePB.
                  END.
                  ASSIGN l_BetKort = FALSE
                         l_Ovr     = FALSE
                         dTmpSum   = 0.
                  IF BongHode.flBankkort      = TRUE OR 
                     BongHode.flBetalingskort = TRUE OR 
                     BongHode.flKreditkort    = TRUE THEN
                      ASSIGN l_BetKort = TRUE.
                  FOR EACH bonglinje WHERE bonglinje.b_id  = bonghode.b_id AND
                                bonglinje.TTId < 13 AND
                                bonglinje.strekkode <> "".
                      IF CAN-DO(cSLPluEan,TRIM(BongLinje.StrekKode)) THEN DO:
                          FIND tmpTT_RapLinjePB WHERE tmpTT_RapLinjePB.StrekKode = bonglinje.strekkode NO-ERROR.
                          IF NOT AVAIL tmpTT_RapLinjePB THEN DO:
                              CREATE tmpTT_RapLinjePB.
                              ASSIGN tmpTT_RapLinjePB.StrekKode = bonglinje.strekkode
                                     tmpTT_RapLinjePB.grupp = 1.
                          END.
                          IF Bonglinje.Antall = ? OR 
                             bonglinje.linjesum = ? 
                               THEN  
                              ASSIGN Bonglinje.Antall = 0
                                     BongLinje.LinjeSum = 0.
                          ASSIGN dSumTstTmp = (((bonglinje.linjesum / ABSOLUTE(bonglinje.antall)) * bonglinje.antall)).
                           IF dSumTstTmp = ? THEN
                              dSumTstTmp = 0.
                          ASSIGN tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] = 
                                     tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] + bonglinje.antall
                                 tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = 
                                     tmpTT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + dSumTstTmp.
                          ASSIGN dTmpSum = dTmpSum + dSumTstTmp.
                      END.
                      ELSE DO:
                          ASSIGN l_Ovr = TRUE.
                          FIND tmpTT_RapLinjePB WHERE tmpTT_RapLinjePB.StrekKode = "Övrigt" NO-ERROR.
                          IF NOT AVAIL tmpTT_RapLinjePB THEN DO:
                              CREATE tmpTT_RapLinjePB.
                              ASSIGN tmpTT_RapLinjePB.StrekKode = "Övrigt"
                                     tmpTT_RapLinjePB.grupp     = 9.
                          END.
                          IF Bonglinje.Antall = ? OR 
                             bonglinje.linjesum = ? 
                               THEN  
                              ASSIGN Bonglinje.Antall = 0
                                     BongLinje.LinjeSum = 0.
                          ASSIGN dSumTstTmp = (((bonglinje.linjesum / ABSOLUTE(bonglinje.antall)) * bonglinje.antall)).
                           IF dSumTstTmp = ? THEN
                              dSumTstTmp = 0.
                          ASSIGN tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] = 
                                     tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] + bonglinje.antall
                                 tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = 
                                     tmpTT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + dSumTstTmp.
                      END.
                  END.
                  /* här hoppar vi av om SL=0 */
                  IF dTmpSum = 0 THEN
                      NEXT.
                  ASSIGN i_b_id = IF l_BetKort = FALSE AND l_Ovr = FALSE THEN 1 ELSE
                                  IF l_BetKort = FALSE AND l_Ovr = TRUE THEN 2 ELSE
                                  IF l_BetKort = TRUE  AND l_Ovr = FALSE THEN 3 ELSE 4.
                  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = i_b_id NO-ERROR.
                  IF NOT AVAIL TT_RapHodePB THEN DO:
                      CREATE TT_RapHodePB.
                      ASSIGN TT_RapHodePB.b_id = i_b_id
                             TT_RapHodePB.betalkort = l_BetKort
                             TT_RapHodePB.BongType = IF l_Ovr = TRUE THEN 2 ELSE 1.
                  END.
                  ASSIGN TT_RapHodePB.antall[MONTH(bonghode.dato)] = TT_RapHodePB.antall[MONTH(bonghode.dato)] + 1
                         TT_RapHodePB.belop[MONTH(bonghode.dato)] = TT_RapHodePB.belop[MONTH(bonghode.dato)] + BongHode.Belop.
                  FOR EACH tmpTT_RapLinjePB:
                      FIND TT_RapLinjePB WHERE TT_RapLinjePB.b_id = i_b_id /* tmpTT_RapLinjePB.b_id */ AND
                           TT_RapLinjePB.StrekKode = tmpTT_RapLinjePB.StrekKode NO-ERROR.
                      IF NOT AVAIL TT_RapLinjePB THEN DO:
                          BUFFER-COPY tmpTT_RapLinjePB EXCEPT b_id TO TT_RapLinjePB
                              ASSIGN TT_RapLinjePB.b_id = TT_RapHodePB.b_id.
                          RELEASE TT_RapLinjePB.
                      END.
                      ELSE DO:
                          ASSIGN TT_RapLinjePB.antall[MONTH(bonghode.dato)] = 
                                     TT_RapLinjePB.antall[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)]
                                 TT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = 
                                     TT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)].
                      END.
                      IF tmpTT_RapLinjePB.grupp = 1 THEN
                          ASSIGN TT_RapHodePB.sumFsgSL[MONTH(bonghode.dato)] = TT_RapHodePB.sumFsgSL[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)].
                      ELSE
                          ASSIGN TT_RapHodePB.sumOvr[MONTH(bonghode.dato)] = TT_RapHodePB.sumOvr[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)].
                  END.
              END.
          END.
      END.
  END.
  {&OPEN-QUERY-BROWSE-TT_RapHodePB}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-TT_RapHodePB.
  IF SESSION:SET-WAIT-STATE("") THEN.
  APPLY "CHOOSE" TO B-Export.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_RapHodePB
&Scoped-define SELF-NAME BROWSE-TT_RapHodePB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_RapHodePB C-Win
ON VALUE-CHANGED OF BROWSE-TT_RapHodePB IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-TT_RapLinjePB}
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
  DISPLAY FI-Butik 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Summera BROWSE-TT_RapHodePB FI-Butik B-Export BROWSE-TT_RapLinjePB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

