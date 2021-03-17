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
DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.

DEFINE BUFFER bufBongLinje FOR BongLinje.
DEFINE BUFFER bufBongHode FOR BongHode.
DEFINE BUFFER bufDatasett FOR Datasett.

DEFINE TEMP-TABLE TT_Dsett NO-UNDO
    FIELD id LIKE Datasett.DataSettId
    FIELD cTxt AS CHAR
    INDEX id IS UNIQUE id.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-23 FILL-IN-24 B-Starta FI-Beh 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-23 FILL-IN-24 FI-Beh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Starta 
     LABEL "Starta" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Beh AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nu behandlas" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS CHARACTER FORMAT "X(256)":U INITIAL "Datasett med 1 i pfflagg - finn bongh" 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1.67
     FONT 8 NO-UNDO.

DEFINE VARIABLE FILL-IN-24 AS CHARACTER FORMAT "X(256)":U INITIAL "Sätt bongh.pfflagg = 1" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1.67
     FONT 8 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-23 AT ROW 1.71 COL 1.4 NO-LABEL
     FILL-IN-24 AT ROW 4.81 COL 11 COLON-ALIGNED NO-LABEL
     B-Starta AT ROW 7.19 COL 27
     FI-Beh AT ROW 9.57 COL 17 COLON-ALIGNED
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
         TITLE              = "<insert window title>"
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
/* SETTINGS FOR FILL-IN FILL-IN-23 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
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


&Scoped-define SELF-NAME B-Starta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Starta C-Win
ON CHOOSE OF B-Starta IN FRAME DEFAULT-FRAME /* Starta */
DO:
  RUN Starta.
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
    RUN InitButiker.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Behandla C-Win 
PROCEDURE Behandla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipDatasettID LIKE datasett.datasettid NO-UNDO.
    FOR EACH bonghode WHERE bonghode.datasettid = ipDatasettID:
        bonghode.pfflagg = 1.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Behandla2 C-Win 
PROCEDURE Behandla2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik      AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER dDato       AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER dDataSettId AS DECIMAL NO-UNDO.
    DEFINE        VARIABLE  dPrStyck    AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  dVVK        AS DECIMAL    NO-UNDO.
    FOR EACH bonghode WHERE Bonghode.ButikkNr = iButik AND
                         Bonghode.GruppeNr = 1     AND
                         Bonghode.KasseNr  = 11    AND
                         Bonghode.Dato     = dDato NO-LOCK:
/*         IF Bonghode.pfFlagg = 1 THEN */
/*             NEXT.                    */
        ASSIGN dDataSettId = 0.
        FOR EACH bonglinje WHERE BongLinje.b_id = BongHode.b_id NO-LOCK:
            IF NOT CAN-DO("1020,1021,1022,30211,30507,30801,31001,31002,31201,31302,31304",bonglinje.strekkode) THEN
                NEXT.
            IF (Bonglinje.linjesum / abs(Bonglinje.antall) < Bonglinje.vvarekost) THEN DO:
                /* kan alltid utföras */
                IF Bonglinje.linjesum > 0 AND BongLinje.Antall <> 0 AND BongLinje.Antall <> ? THEN DO:
                    IF bonglinje.strekkode = "1020" THEN
                        dVVK = IF bonglinje.linjesum / abs(bonglinje.antall) = 500 then 493 else bonglinje.vvarekost.
                    ELSE IF bonglinje.strekkode = "1021" THEN
                        dVVK = IF bonglinje.linjesum / abs(bonglinje.antall) = 300 then 294 else 30.
                    ELSE IF bonglinje.strekkode = "1022" THEN
                        dVVK = IF bonglinje.linjesum / abs(bonglinje.antall) = 145 then 140 else bonglinje.vvarekost.
                    ELSE IF bonglinje.strekkode = "30211" THEN 
                        dVVK = IF bonglinje.bongtekst BEGINS "DELI" THEN 3.5 ELSE Bonglinje.vvarekost.
                    ELSE IF bonglinje.strekkode = "30507" THEN
                        dVVK = 6.5.
                    ELSE IF bonglinje.strekkode = "30801" THEN
                        dVVK = IF bonglinje.bongtekst BEGINS "DELI" THEN 1.5 ELSE bonglinje.vvarekost.
                    ELSE IF bonglinje.strekkode = "31201" THEN
                        dVVK = 4.6.
                    ELSE IF bonglinje.strekkode = "31302" THEN
                        dVVK = 36.
                    ELSE IF bonglinje.strekkode = "31304" THEN
                        dVVK = 53.
                    ELSE
                        NEXT.
                    /* testa om detta redan är sant innan det utförs, och om det redan är gjort - inget datasett */
                    IF Bonglinje.vvarekost <> dVVK THEN DO:
                        FIND bufBonglinje WHERE ROWID(bufBongLinje) = ROWID(BongLinje).
                        ASSIGN bufBonglinje.vvarekost = dVVK.
                        RELEASE bufBonglinje.
                        IF dDatasettId = 0 THEN DO:
                                ASSIGN dDatasettId = BongHode.DatasettId.
                        END.
                    END.
                END.
            END.
        END.
    END.

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
  DISPLAY FILL-IN-23 FILL-IN-24 FI-Beh 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-23 FILL-IN-24 B-Starta FI-Beh 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker C-Win 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Butiker NO-LOCK:
        cButiker = cButiker + (IF cButiker <> "" THEN "," ELSE "") + STRING(Butiker.butik).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFlaggSlettPF C-Win 
PROCEDURE SettFlaggSlettPF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        DEFINE INPUT  PARAMETER iButik      AS INTEGER    NO-UNDO.
        DEFINE INPUT  PARAMETER dDataSettId AS DECIMAL    NO-UNDO.
        DEFINE INPUT  PARAMETER dBongDato   AS DATE       NO-UNDO.
        FOR EACH bufBongHode WHERE bufBongHode.datasettid = dDatasettId.
            ASSIGN bufBongHode.pfFlagg = 1.
        END.
        FOR EACH pfTenderRevenue WHERE pfTenderRevenue.Store_No = iButik AND pftenderrevenue.DATE = dBongDato: 
            DELETE pfTenderRevenue. 
        END.
        
        FOR EACH pfStatisticsRevenue WHERE pfStatisticsRevenue.Store_No = iButik AND pfStatisticsrevenue.DATE = dBongDato:  
            DELETE pfStatisticsRevenue. 
        END.
        
        FOR EACH pfDaySales_HourExt WHERE pfDaysales_Hourext.Store_No = iButik AND pfDaysales_Hourext.DATE = dBongDato: 
            DELETE pfDaySales_HourExt. 
        END.
        
        FOR EACH pfDaySales WHERE pfDaysales.Store_No = iButik AND pfDaysales.DATE = dBongDato:
            DELETE pfDaySales. 
        END.
        FIND bufDatasett WHERE bufDatasett.datasettid = dDatasettId.
        bufDatasett.pfFlagg = 1.
        RELEASE bufDatasett.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Starta C-Win 
PROCEDURE Starta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dDato       AS DATE       NO-UNDO.
    DEFINE VARIABLE dDataSettid AS DECIMAL    NO-UNDO.
   
    FOR EACH datasett NO-LOCK WHERE pfflagg = 1:
        icount = icount + 1.
        FI-Beh:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iCount) + " - 37703".
        PROCESS EVENTS.
        RUN Behandla (datasett.datasettid).
    END.
    MESSAGE PROGRAM-NAME(1) SKIP
    "VI är klara - !!!! BARA 1 butik"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

