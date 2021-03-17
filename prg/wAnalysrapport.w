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

DEFINE TEMP-TABLE TT_prBMdata NO-UNDO LIKE prBMdata.
DEFINE TEMP-TABLE TT_prPGdata NO-UNDO LIKE prPGdata
    FIELD Hg AS INTE.
DEFINE VARIABLE hParent AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 TG-Obalans FI-Butik TG-AllaBut ~
FI-Dato-0 B-dagrapp FI-Dato-1 FI-Dato-2 B-intervallrapp TG-Hg 
&Scoped-Define DISPLAYED-OBJECTS TG-Obalans FI-Butik TG-AllaBut FI-Dato-0 ~
FI-Dato-1 FI-Dato-2 TG-Hg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-dagrapp 
     LABEL "Rapport dag" 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-intervallrapp 
     LABEL "Rapport intervall" 
     SIZE 20 BY 1.14.

DEFINE VARIABLE FI-Butik AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Butik" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato-0 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato-1 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum från/till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato-2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 2.86.

DEFINE VARIABLE TG-AllaBut AS LOGICAL INITIAL no 
     LABEL "Alla butiker" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Hg AS LOGICAL INITIAL no 
     LABEL "Pg-data som hgrapport" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Obalans AS LOGICAL INITIAL no 
     LABEL "Obalans" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TG-Obalans AT ROW 1.48 COL 54
     FI-Butik AT ROW 2.19 COL 16 COLON-ALIGNED
     TG-AllaBut AT ROW 2.43 COL 54
     FI-Dato-0 AT ROW 4.1 COL 16 COLON-ALIGNED
     B-dagrapp AT ROW 4.1 COL 54
     FI-Dato-1 AT ROW 5.48 COL 16 COLON-ALIGNED
     FI-Dato-2 AT ROW 5.52 COL 32 COLON-ALIGNED NO-LABEL
     B-intervallrapp AT ROW 5.52 COL 54
     TG-Hg AT ROW 7.43 COL 34
     RECT-1 AT ROW 1.19 COL 51
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 8.05.


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
         TITLE              = "Analysrapport till clipboard"
         HEIGHT             = 8.05
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         ALWAYS-ON-TOP      = yes
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Analysrapport till clipboard */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Analysrapport till clipboard */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-dagrapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-dagrapp C-Win
ON CHOOSE OF B-dagrapp IN FRAME DEFAULT-FRAME /* Rapport dag */
DO:
  RUN Rapport (INPUT INPUT FI-Butik, INPUT INPUT FI-Dato-0,INPUT INPUT FI-Dato-0).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-intervallrapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-intervallrapp C-Win
ON CHOOSE OF B-intervallrapp IN FRAME DEFAULT-FRAME /* Rapport intervall */
DO:
    RUN Rapport (INPUT INPUT FI-Butik, INPUT INPUT FI-Dato-1,INPUT INPUT FI-Dato-2).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Obalans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Obalans C-Win
ON VALUE-CHANGED OF TG-Obalans IN FRAME DEFAULT-FRAME /* Obalans */
DO:
  TG-AllaBut:SENSITIVE = TG-Obalans:CHECKED.
  TG-AllaBut:CHECKED = FALSE.
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
    IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"send_but_dato") THEN DO:
        hParent = SOURCE-PROCEDURE.
        RUN GET_butdato.
    END.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO TG-Obalans.
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
  DISPLAY TG-Obalans FI-Butik TG-AllaBut FI-Dato-0 FI-Dato-1 FI-Dato-2 TG-Hg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 TG-Obalans FI-Butik TG-AllaBut FI-Dato-0 B-dagrapp FI-Dato-1 
         FI-Dato-2 B-intervallrapp TG-Hg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GET_butdato C-Win 
PROCEDURE GET_butdato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN send_but_dato IN hParent (OUTPUT FI-Butik,OUTPUT FI-Dato-0).
    IF NOT FI-Butik > 0 OR FI-Dato-0 = ? THEN
        RETURN.
    FI-Dato-1 = DATE(MONTH(FI-Dato-0),1,YEAR(FI-Dato-0)).
    FI-Dato-2 = DATE(MONTH(FI-Dato-0) + 1,1,YEAR(FI-Dato-0)) - 1.
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
DEFINE INPUT  PARAMETER iButik AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER dDatoFra AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER dDatoTil AS DATE       NO-UNDO.

DEFINE VARIABLE dBokfdatO AS DATE       NO-UNDO.
DEFINE VARIABLE dBMbelop AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dBMbelop1 AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPGbelop AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHg AS INTEGER    NO-UNDO.
DEFINE VARIABLE dInbetKred AS DECIMAL    NO-UNDO.
ii = 1.
OUTPUT TO "CLIPBOARD".
PUT UNFORMATTED " " CHR(9) "Butik" CHR(9) "Betmedel" CHR(9) "FSG" CHR(9) "Inbet kred" chr(9) "Diff" SKIP.
FOR EACH butiker WHERE (IF NOT TG-AllaBut:CHECKED IN FRAME {&FRAME-NAME} THEN butiker.butik = iButik ELSE TRUE):
    DO dBokfdatO = dDatoFra TO dDatoTil:
        dBMbelop = 0.
        dBMbelop1 = 0.
        dPGbelop = 0.
        dInbetKred = 0.
        FOR EACH prbmdata WHERE prBMData.ButikkNr = butiker.butik AND prBMData.Dato = dBokfDato /* AND betalingstype <> 5 */ NO-LOCK:
            IF prBMData.betalingstype = 6 AND prBMData.SubType = 1 AND prBMData.SubSubType = 0 THEN
                dInbetKred = dInbetKred + prBMData.belop.
    /*         ELSE IF prBMData.betalingstype = 5 THEN     */
    /*             dBmbelop1 = dBmbelop1 + prBMData.belop. */
            ELSE
                dBmbelop = dBmbelop + prBMData.belop.
    
            FIND TT_prBMdata WHERE TT_prBMdata.konto = (IF prBMdata.konto = ? THEN 0 ELSE prBMdata.konto) AND 
                                   TT_prBMdata.SUBTYPE = prBMdata.SUBTYPE AND TT_prBMdata.subsubtype = prBMdata.subsubtype NO-ERROR.
            IF NOT AVAIL TT_prBMdata THEN DO:
                CREATE TT_prBMdata.
                ASSIGN TT_prBMdata.konto = (IF prBMdata.konto = ? THEN 0 ELSE prBMdata.konto)
                       TT_prBMdata.SUBTYPE = prBMdata.SUBTYPE
                       TT_prBMdata.subSUBTYPE = prBMdata.subSUBTYPE
                       TT_prBMdata.betalingstype = prBMdata.betalingstype.
            END.
            TT_prBMdata.belop = TT_prBMdata.belop + prBMdata.belop.
        END.
        FOR EACH prPGData WHERE prPGData.Butikknr = butiker.butik AND prPGData.Dato = dBokfDato NO-LOCK:
            dPGbelop = dPGbelop + prPGData.sumvaresalg.
            IF NOT TG-HG:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
                FIND TT_prPGdata WHERE TT_prPGdata.konto = prPGdata.konto NO-ERROR.
                IF NOT AVAIL TT_prPGdata THEN DO:
                    CREATE TT_prPGdata.
                    ASSIGN TT_prPGdata.konto = prPGdata.konto.
                END.
                ASSIGN TT_prPGdata.SumVolumAntall = TT_prPGdata.SumVolumAntall + prPGdata.SumVolumAntall
                       TT_prPGData.SumVaresalg    = TT_prPGData.SumVaresalg    + prPGData.SumVaresalg
                       TT_prPGData.MvaKr          = TT_prPGData.MvaKr          + prPGData.MvaKr.
            END.
            ELSE DO:
                FIND VarGr WHERE vargr.vg = prPGdata.varegr NO-LOCK NO-ERROR.
                iHg = IF AVAIL vargr THEN vargr.hg ELSE 0.
                FIND TT_prPGdata WHERE TT_prPGdata.hg = iHg NO-ERROR.
                IF NOT AVAIL TT_prPGdata THEN DO:
                    CREATE TT_prPGdata.
                    ASSIGN TT_prPGdata.hg = iHg.
                END.
                ASSIGN TT_prPGdata.SumVolumAntall = TT_prPGdata.SumVolumAntall + prPGdata.SumVolumAntall
                       TT_prPGData.SumVaresalg    = TT_prPGData.SumVaresalg    + prPGData.SumVaresalg
                       TT_prPGData.MvaKr          = TT_prPGData.MvaKr          + prPGData.MvaKr.
            END.
        END.
        IF TG-Obalans:CHECKED AND dInbetKred = 0 THEN
            NEXT.
        ii = ii + 1.
        PUT UNFORMATTED "'" STRING(dBokfDato) chr(9) butiker.butik CHR(9) dBmbelop CHR(9) dPGbelop chr(9) dInbetKred chr(9) "=C" string(ii) "-D" string(ii) "-E" STRING(ii) SKIP.
    END.
END.
IF NOT TG-Obalans:CHECKED THEN DO:
    PUT UNFORMATTED " " SKIP.
    FOR EACH TT_prBMdata BY TT_prBMdata.betalingstype BY TT_prBMdata.SUBTYPE BY TT_prBMdata.subsubtype:
        PUT UNFORMATTED TT_prBMdata.konto CHR(9) TT_prBMdata.betalingstype chr(9) TT_prBMdata.SUBTYPE CHR(9) TT_prBMdata.subSUBTYPE CHR(9) TT_prBMdata.belop SKIP.
    END.
    PUT UNFORMATTED " " SKIP.
    IF NOT TG-HG:CHECKED THEN DO:
        FOR EACH TT_prPGData BY TT_prPGdata.konto:
            PUT UNFORMATTED TT_prPGdata.konto CHR(9)
                            TT_prPGdata.SumVolumAntall CHR(9)
                            TT_prPGData.SumVaresalg CHR(9)
                            TT_prPGData.MvaKr SKIP.

        END.
    END.
    ELSE DO:
        FOR EACH TT_prPGData BY TT_prPGdata.hg:
            FIND HuvGr WHERE huvgr.hg = TT_prPGdata.hg NO-LOCK NO-ERROR.
            PUT UNFORMATTED TT_prPGdata.hg CHR(9)
                            (IF AVAIL huvgr THEN huvgr.hgbeskr ELSE "NA") CHR(9)
                            TT_prPGdata.SumVolumAntall CHR(9)
                            TT_prPGData.SumVaresalg CHR(9)
                            TT_prPGData.MvaKr SKIP.

        END.
    END.
END.

OUTPUT CLOSE.
MESSAGE "Klar"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportOrg C-Win 
PROCEDURE RapportOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iButik AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER dDatoFra AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER dDatoTil AS DATE       NO-UNDO.

DEFINE VARIABLE dBokfdatO AS DATE       NO-UNDO.
DEFINE VARIABLE dBMbelop AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dBMbelop1 AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPGbelop AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHg AS INTEGER    NO-UNDO.
DEFINE VARIABLE dInbetKred AS DECIMAL    NO-UNDO.
ii = 1.
OUTPUT TO "CLIPBOARD".
DO dBokfdatO = dDatoFra TO dDatoTil:
    dBMbelop = 0.
    dBMbelop1 = 0.
    dPGbelop = 0.
    dInbetKred = 0.
    ii = ii + 1.
    PUT UNFORMATTED " " CHR(9) "Betmedel" CHR(9) "FSG" CHR(9) "Inbet kred" SKIP.
    FOR EACH prbmdata WHERE prBMData.ButikkNr = iButik AND prBMData.Dato = dBokfDato /* AND betalingstype <> 5 */ NO-LOCK:
        IF prBMData.betalingstype = 6 AND prBMData.SubType = 1 AND prBMData.SubSubType = 0 THEN
            dInbetKred = dInbetKred + prBMData.belop.
/*         ELSE IF prBMData.betalingstype = 5 THEN     */
/*             dBmbelop1 = dBmbelop1 + prBMData.belop. */
        ELSE
            dBmbelop = dBmbelop + prBMData.belop.

        FIND TT_prBMdata WHERE TT_prBMdata.konto = (IF prBMdata.konto = ? THEN 0 ELSE prBMdata.konto) AND 
                               TT_prBMdata.SUBTYPE = prBMdata.SUBTYPE AND TT_prBMdata.subsubtype = prBMdata.subsubtype NO-ERROR.
        IF NOT AVAIL TT_prBMdata THEN DO:
            CREATE TT_prBMdata.
            ASSIGN TT_prBMdata.konto = (IF prBMdata.konto = ? THEN 0 ELSE prBMdata.konto)
                   TT_prBMdata.SUBTYPE = prBMdata.SUBTYPE
                   TT_prBMdata.subSUBTYPE = prBMdata.subSUBTYPE
                   TT_prBMdata.betalingstype = prBMdata.betalingstype.
        END.
        TT_prBMdata.belop = TT_prBMdata.belop + prBMdata.belop.
    END.
    FOR EACH prPGData WHERE prPGData.Butikknr = iButik AND prPGData.Dato = dBokfDato NO-LOCK:
        dPGbelop = dPGbelop + prPGData.sumvaresalg.
        IF NOT TG-HG:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            FIND TT_prPGdata WHERE TT_prPGdata.konto = prPGdata.konto NO-ERROR.
            IF NOT AVAIL TT_prPGdata THEN DO:
                CREATE TT_prPGdata.
                ASSIGN TT_prPGdata.konto = prPGdata.konto.
            END.
            ASSIGN TT_prPGdata.SumVolumAntall = TT_prPGdata.SumVolumAntall + prPGdata.SumVolumAntall
                   TT_prPGData.SumVaresalg    = TT_prPGData.SumVaresalg    + prPGData.SumVaresalg
                   TT_prPGData.MvaKr          = TT_prPGData.MvaKr          + prPGData.MvaKr.
        END.
        ELSE DO:
            FIND VarGr WHERE vargr.vg = prPGdata.varegr NO-LOCK NO-ERROR.
            iHg = IF AVAIL vargr THEN vargr.hg ELSE 0.
            FIND TT_prPGdata WHERE TT_prPGdata.hg = iHg NO-ERROR.
            IF NOT AVAIL TT_prPGdata THEN DO:
                CREATE TT_prPGdata.
                ASSIGN TT_prPGdata.hg = iHg.
            END.
            ASSIGN TT_prPGdata.SumVolumAntall = TT_prPGdata.SumVolumAntall + prPGdata.SumVolumAntall
                   TT_prPGData.SumVaresalg    = TT_prPGData.SumVaresalg    + prPGData.SumVaresalg
                   TT_prPGData.MvaKr          = TT_prPGData.MvaKr          + prPGData.MvaKr.
        END.
    END.
    PUT UNFORMATTED "'" STRING(dBokfDato) CHR(9) dBmbelop CHR(9) dPGbelop chr(9) dInbetKred SKIP.
END.
PUT UNFORMATTED " " SKIP.
FOR EACH TT_prBMdata BY TT_prBMdata.betalingstype BY TT_prBMdata.SUBTYPE BY TT_prBMdata.subsubtype:
    PUT UNFORMATTED TT_prBMdata.konto CHR(9) TT_prBMdata.betalingstype chr(9) TT_prBMdata.SUBTYPE CHR(9) TT_prBMdata.subSUBTYPE CHR(9) TT_prBMdata.belop SKIP.
END.
PUT UNFORMATTED " " SKIP.
IF NOT TG-HG:CHECKED THEN DO:
    FOR EACH TT_prPGData BY TT_prPGdata.konto:
        PUT UNFORMATTED TT_prPGdata.konto CHR(9)
                        TT_prPGdata.SumVolumAntall CHR(9)
                        TT_prPGData.SumVaresalg CHR(9)
                        TT_prPGData.MvaKr SKIP.

    END.
END.
ELSE DO:
    FOR EACH TT_prPGData BY TT_prPGdata.hg:
        FIND HuvGr WHERE huvgr.hg = TT_prPGdata.hg NO-LOCK NO-ERROR.
        PUT UNFORMATTED TT_prPGdata.hg CHR(9)
                        (IF AVAIL huvgr THEN huvgr.hgbeskr ELSE "NA") CHR(9)
                        TT_prPGdata.SumVolumAntall CHR(9)
                        TT_prPGData.SumVaresalg CHR(9)
                        TT_prPGData.MvaKr SKIP.

    END.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

