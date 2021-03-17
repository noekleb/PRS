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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Butik BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "Butikanalys" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Butik AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butik" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Butik AT ROW 2.67 COL 16 COLON-ALIGNED
     BUTTON-2 AT ROW 2.67 COL 47
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 16.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 126
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 126
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 126
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Butikanalys */
DO:
    DEFINE VARIABLE dFraDat   AS DATE       NO-UNDO.
    DEFINE VARIABLE dTilDat   AS DATE       NO-UNDO.
    DEFINE VARIABLE dDatoLoop AS DATE       NO-UNDO.
    DEFINE VARIABLE cSLPluEan AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dRapBongAnt   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapBongSum   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapBongMSalg AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapBongMPay  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapAntBaraSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapAntSLOvr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapSumBaraSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapSumSLOvr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRapSumOvr    AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButBongAnt   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButBongSum   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButBongMSalg AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButBongMPay  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButAntBaraSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButAntSLOvr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButSumBaraSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButSumSLOvr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dButSumOvr    AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dBongAnt   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dBongSum   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dBongMSalg AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dBongMPay  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dAntBaraSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dAntSLOvr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSumBaraSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSumSLOvr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSumOvr    AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iTmp1_12  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dtmpSLSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dTmpOvr   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iPayRec   AS INTEGER    NO-UNDO.
                        
    ASSIGN dFraDat = DATE(01,01,2003)
           dTilDat = DATE(08,31,2003).
    FOR EACH Butiker WHERE Butiker.butik < 10000 OR Butiker.butik = 81001 NO-LOCK:
        ASSIGN FI-Butik:SCREEN-VALUE = STRING(Butiker.Butik,"99999").
        PROCESS EVENTS.
        FIND FIRST kasse WHERE kasse.butik = Butiker.butik NO-LOCK NO-ERROR.
        IF NOT AVAIL kasse OR kasse.modellNr <> 30 THEN
            NEXT.
        ASSIGN dButBongAnt   = 0
               dButBongSum   = 0
               dButBongMSalg = 0
               dButBongMPay  = 0
               dButAntBaraSL = 0
               dButAntSLOvr  = 0
               dButSumBaraSL = 0
               dButSumSLOvr  = 0
               dButSumOvr    = 0.
        OUTPUT TO VALUE(SESSION:TEMP-DIR + string(Kasse.Modellnr) + "_" + STRING(Butiker.butik,"99999") + ".txt").
        EXPORT DELIMITER ";" " " "BongAnt" "BongSum" "BongMSalg" "BongMPay" "AntBaraSL" "AntSLOvr" "SumBaraSL" "SumSLOvr" "SumOvr".
        DO dDatoLoop = dFraDat TO dTilDat:
            ASSIGN dBongant = 0
                   dBongSum = 0
                   dBongMSalg = 0
                   dBongMPay  = 0
                   dAntBaraSL = 0
                   dAntSLOvr  = 0
                   dSumBaraSl = 0
                   dSumSLOvr  = 0
                   dSumOvr    = 0.
            FOR EACH Kasse WHERE Kasse.butik = Butiker.butik NO-LOCK:
                FOR EACH BongHode WHERE BongHode.ButikkNr = Kasse.Butik    AND
                                        BongHode.GruppeNr = Kasse.GruppeNr AND
                                        BongHode.KasseNr  = Kasse.KasseNr  AND
                                        BongHode.Dato     = dDatoLoop USE-INDEX Bong NO-LOCK:
                    ASSIGN dBongAnt  = dBongAnt + 1
                           dBongSum  = dBongSum + BongHode.Belop
                           iTmp1_12  = 0
                           dtmpSLSum = 0
                           dTmpOvr   = 0
                           iPayRec   = 0.
                    FOR EACH Bonglinje WHERE Bonglinje.b_id = BongHode.b_id NO-LOCK.
                        IF BongLinje.TTId < 13 THEN DO:
                            ASSIGN iTmp1_12 = 1.
                            IF CAN-DO(cSLPluEan,TRIM(BongLinje.StrekKode)) THEN
                                ASSIGN dtmpSLSum     = dtmpSLSum + (IF BongLinje.Antal > 0 THEN BongLinje.LinjeSum ELSE BongLinje.LinjeSum * -1).
                            ELSE
                                ASSIGN dTmpOvr    = dTmpOvr + (IF BongLinje.Antal > 0 THEN BongLinje.LinjeSum ELSE BongLinje.LinjeSum * -1).
                        END.
                        ELSE
                            ASSIGN iPayRec = 1.
                    END.
                    ASSIGN dBongMSalg = dBongMSalg + iTmp1_12
                           dBongMPay  = dBongMPay   + iPayRec
                           dAntBaraSL = dAntBaraSL + IF dtmpSLSum <> 0 AND dTmpOvr = 0 THEN 1 ELSE 0
                           dAntSLOvr  = dAntSLOvr  + IF dtmpSLSum <> 0 AND dTmpOvr <> 0 THEN 1 ELSE 0
                           dSumBaraSl = dSumBaraSl + IF dtmpSLSum <> 0 AND dTmpOvr = 0 THEN dtmpSLSum ELSE 0
                           dSumSLOvr  = dSumSLOvr  + IF dtmpSLSum <> 0 AND dTmpOvr <> 0 THEN dtmpSLSum ELSE 0
                           dSumOvr    = dSumOvr    + IF dtmpSLSum <> 0 AND dTmpOvr <> 0 THEN dTmpOvr ELSE 0.
                END.
            END.
            ASSIGN dButBongAnt   = dButBongAnt   + dBongAnt  
                   dButBongSum   = dButBongSum   + dBongSum  
                   dButBongMSalg = dButBongMSalg + dBongMSalg
                   dButBongMPay  = dButBongMPay  + dBongMPay  
                   dButAntBaraSL = dButAntBaraSL + dAntBaraSL
                   dButAntSLOvr  = dButAntSLOvr  + dAntSLOvr 
                   dButSumBaraSL = dButSumBaraSL + dSumBaraSL
                   dButSumSLOvr  = dButSumSLOvr  + dSumSLOvr 
                   dButSumOvr    = dButSumOvr    + dSumOvr.  
            EXPORT DELIMITER ";"
                dDatoLoop dBongAnt dBongSum dBongMSalg dBongMPay dAntBaraSL dAntSLOvr dSumBaraSL dSumSLOvr dSumOvr.
        END.
        ASSIGN dRapBongAnt   = dRapBongAnt   + dButBongAnt  
               dRapBongSum   = dRapBongSum   + dButBongSum  
               dRapBongMSalg = dRapBongMSalg + dButBongMSalg
               dRapBongMPay  = dRapBongMPay  + dButBongMPay 
               dRapAntBaraSL = dRapAntBaraSL + dButAntBaraSL
               dRapAntSLOvr  = dRapAntSLOvr  + dButAntSLOvr 
               dRapSumBaraSL = dRapSumBaraSL + dButSumBaraSL
               dRapSumSLOvr  = dRapSumSLOvr  + dButSumSLOvr 
               dRapSumOvr    = dRapSumOvr    + dButSumOvr.   
        EXPORT "Butiktotal".
        EXPORT DELIMITER ";" " " dButBongAnt dButBongSum dButBongMSalg dButBongMPay dButAntBaraSL dButAntSLOvr dButSumBaraSL dButSumSLOvr dButSumOvr.
        OUTPUT CLOSE.
    END.
    OUTPUT TO VALUE(SESSION:TEMP-DIR + string(00) + "_HelaRapp.txt").
    EXPORT DELIMITER ";" " " "BongAnt" "BongSum" "BongMSalg" "BongMPay" "AntBaraSL" "AntSLOvr" "SumBaraSL" "SumSLOvr" "SumOvr".
    EXPORT DELIMITER ";" " " dRapBongAnt dRapBongSum dRapBongMSalg dRapBongMPay dRapAntBaraSL dRapAntSLOvr dRapSumBaraSL dRapSumSLOvr dRapSumOvr.
    OUTPUT CLOSE.
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
  ENABLE FI-Butik BUTTON-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

