&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Bilderapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Bilderapport 
/*------------------------------------------------------------------------

  File: b-biled.w

  Description: Bestillingsprogram for bestilling av billedrapport.
               Programmet henter kriterier fra bruker, sender disse
               videre til rutine som oppretter datasettet.
               Når datasettet er opprettet, startes rutine som 
               legger data ut til fil og Excel aktiveres.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Tom Nøkleby, T.N.Consult

  Created: 24/5-98 - Stokholm.

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEFINE VARIABLE dDatumLoop AS DATE       NO-UNDO.
DEFINE VARIABLE dFraDag    AS DATE       NO-UNDO.
DEFINE VARIABLE dTilDag    AS DATE       NO-UNDO.

DEFINE TEMP-TABLE tt_prov NO-UNDO
    FIELD butik        AS INTEGER
    FIELD datum        AS DATE
    FIELD fsg          AS DECIMAL
    FIELD webplock     AS DECIMAL
    FIELD reklam       AS DECIMAL
    FIELD ret_fr_andra AS DECIMAL
    FIELD ret_i_andra  AS DECIMAL
    INDEX dabu IS PRIMARY datum butik.

DEFINE BUFFER buftt_prov FOR tt_prov.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SokDato-2 FI-Dato1 FI-Dato2 ~
BUTTON-SokDato Btn_OK Btn_Cancel Btn_Help RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato1 FI-Dato2 FILL-IN-3 FILL-IN-4 ~
FILL-IN-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Bilderapport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avsluta" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjälp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Starta utskrift" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Kriterier" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Fr.o.m" 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "T.o.m." 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.2 BY 5.91.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 5.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokDato-2 AT ROW 3.86 COL 54.6
     FI-Dato1 AT ROW 3.86 COL 17 COLON-ALIGNED
     FI-Dato2 AT ROW 3.86 COL 38 COLON-ALIGNED NO-LABEL
     BUTTON-SokDato AT ROW 3.86 COL 34
     Btn_OK AT ROW 1.67 COL 62.4
     Btn_Cancel AT ROW 4.48 COL 62.4
     Btn_Help AT ROW 5.86 COL 62.6
     FILL-IN-3 AT ROW 1.24 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 3.1 COL 17.4 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 3.1 COL 38 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 5.19 COL 3 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 1.48 COL 61.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 6.81.


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
  CREATE WINDOW C-Bilderapport ASSIGN
         HIDDEN             = YES
         TITLE              = "Beställning av provisionsrapport"
         HEIGHT             = 6.81
         WIDTH              = 80
         MAX-HEIGHT         = 27.67
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 27.67
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW C-Bilderapport
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Bilderapport)
THEN C-Bilderapport:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Bilderapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Bilderapport C-Bilderapport
ON END-ERROR OF C-Bilderapport /* Beställning av provisionsrapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Bilderapport C-Bilderapport
ON WINDOW-CLOSE OF C-Bilderapport /* Beställning av provisionsrapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Bilderapport
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  APPLY "CLOSE":U to THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Bilderapport
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjälp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Bilderapport
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Starta utskrift */
DO:
  RUN Utskrift.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Bilderapport
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FI-Dato1
DO:

  DEFINE VARIABLE wTittel AS CHARACTER NO-UNDO.
  ASSIGN FI-Dato1 = DATE(FI-Dato1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  DO WITH FRAME {&FRAME-NAME}:  

  wTittel = "Datum Fr.o.m.".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato1
    &Program     = kalender.w
    &FRAME       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  END. /* FRAME */
  ASSIGN INPUT FI-Dato1.
  ASSIGN dFraDag = FI-Dato1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Bilderapport
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FI-Dato2
DO:

  DEFINE VARIABLE wTittel AS CHARACTER NO-UNDO.
  ASSIGN FI-Dato2 = DATE(FI-Dato1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  DO WITH FRAME {&FRAME-NAME}:  

  wTittel = "Datum T.o.m.".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato2
    &Program     = kalender.w
    &FRAME       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  END. /* FRAME */
  ASSIGN INPUT FI-Dato2.
  ASSIGN dTilDag = FI-Dato2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato1 C-Bilderapport
ON TAB OF FI-Dato1 IN FRAME DEFAULT-FRAME /* Datum */
OR RETURN OF FI-Dato1
DO:
  DISPLAY FI-Dato1:SCREEN-VALUE @ FI-Dato2 WITH FRAME {&FRAME-NAME}.  
  ASSIGN INPUT FI-Dato1.
  ASSIGN dFraDag = FI-Dato1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato2 C-Bilderapport
ON LEAVE OF FI-Dato2 IN FRAME DEFAULT-FRAME
DO:
    ASSIGN INPUT FI-Dato2.
    ASSIGN dTilDag = FI-Dato2.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Bilderapport 


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

/* Statusmelding */
STATUS INPUT "Ange datum Fr.o.m - T.o.m. för utskrift av Provsionsrapport".
ASSIGN 
  FI-Dato1 = TODAY 
  FI-Dato2 = TODAY.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
/*  {lng.i}*/ 

  APPLY "ENTRY":U to FI-Dato1.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Bilderapport  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Bilderapport)
  THEN DELETE WIDGET C-Bilderapport.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Bilderapport  _DEFAULT-ENABLE
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
  DISPLAY FI-Dato1 FI-Dato2 FILL-IN-3 FILL-IN-4 FILL-IN-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Bilderapport.
  ENABLE BUTTON-SokDato-2 FI-Dato1 FI-Dato2 BUTTON-SokDato Btn_OK Btn_Cancel 
         Btn_Help RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Bilderapport.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Bilderapport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Bilderapport 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
DEFINE VARIABLE cFilNamn  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCommando AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dNtoFsg AS DECIMAL     NO-UNDO.
FOR EACH tt_prov:
    DELETE tt_prov.
END.

/*RUN kalender.w (INPUT-OUTPUT dFraDag,"Provision Fr.o.m"). 

RUN kalender.w (INPUT-OUTPUT dTilDag,"Provision T.o.m"). */
/*MESSAGE "dFraDag " dFraDag SKIP
        "dTilDag " dTilDag
    VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*ASSIGN dFraDag = DATE(3,1,2012)
       dTilDag = DATE(3,31,2012).*/
/*ASSIGN dFraDag = FI-Dato1
       DTilDag = FI-Dato2.*/

IF dTilDag < dFraDag THEN
DO:
    MESSAGE "Till-Datum är mindre än Från-Datum"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik /*AND kasse.aktiv = TRUE*/ ):
    DO dDatumLoop = dFraDag TO dTilDag:
        CREATE tt_prov.
        ASSIGN tt_prov.butik = butiker.butik
               tt_prov.datum = dDatumLoop.
        FIND Dags_Rap WHERE dags_rap.butikk = butiker.butik AND dags_rap.dato = dDatumLoop NO-LOCK NO-ERROR.
        IF AVAIL dags_rap THEN DO:
            ASSIGN tt_prov.fsg   = hg1_oms + hg2_oms + hg3_oms + hg4_oms + hg5_oms + hg6_oms + hg7_oms + hg8_oms + hg9_oms.
            
        END.
    END.
END.
FOR EACH butiker NO-LOCK:
    FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK:
        DO dDatumLoop = dFraDag TO dTilDag:
            FOR EACH bonghode WHERE bonghode.butikknr = butiker.butik  AND
                                    bonghode.gruppenr = kasse.gruppenr AND
                                    bonghode.kassenr  = kasse.kassenr  AND
                                    bonghode.dato     = dDatumLoop     AND
                                    bonghode.makulert <> 2             NO-LOCK:
                FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK.
                    IF bonglinje.makulert THEN
                        NEXT.
                    IF bonglinje.butikknr = 24 AND bonglinje.ttid = 1 AND bonghode.kordre_id > 0 THEN DO:
                        IF bonglinje.bongtekst MATCHES "*FRAKT*" THEN
                            NEXT.
                        IF bonglinje.varegr = 151 THEN
                            NEXT.
                        find kordrehode where kordrehode.KOrdre_Id = bonghode.KOrdre_Id no-lock no-error.
                        if avail kordrehode then do:
                            find kordrelinje of kordrehode where KOrdreLinje.KOrdreLinjeNr = bonglinje.linjenr no-lock no-error.
                            if avail kordrelinje AND kordrelinje.plukkbutikk <> 20 AND kordrelinje.plukkbutikk <> 21 AND kordrelinje.plukkbutikk <> 0 THEN DO:
                                FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                                   tt_prov.datum = dDatumLoop NO-ERROR.
                                FIND buftt_prov WHERE buftt_prov.butik = kordrelinje.plukkbutikk AND
                                                      buftt_prov.datum = dDatumLoop NO-ERROR.
                                IF AVAIL tt_prov AND AVAIL buftt_prov THEN DO:
                                    dNtoFsg = BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                                    tt_prov.Webplock = tt_prov.Webplock - dNtoFsg.
                                    buftt_prov.Webplock = buftt_prov.Webplock + dNtoFsg.
/*                                     tt_prov.fsg = tt_prov.fsg - dNtoFsg.       */
/*                                     buftt_prov.fsg = buftt_prov.fsg + dNtoFsg. */
                                END.
                            END.
                        end.
                    END.
                    /* kundreklamation */
                    ELSE IF bonglinje.ttid = 3 THEN DO:
                        FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                           tt_prov.datum = dDatumLoop.
                        ASSIGN tt_prov.reklam = tt_prov.reklam + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                               tt_prov.fsg    = tt_prov.fsg - (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab).
                    END.
                    /* returer */
                    ELSE IF bonglinje.ttid = 10 THEN DO:
                        IF bonglinje.ReturButikk > 0 AND bonglinje.ReturButikk <> bonglinje.butikknr THEN DO:
                            FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                               tt_prov.datum = dDatumLoop.
                            ASSIGN tt_prov.ret_fr_andra = tt_prov.ret_fr_andra + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                            FIND tt_prov WHERE tt_prov.butik = bonglinje.ReturButikk AND
                                               tt_prov.datum = dDatumLoop NO-ERROR.
                            IF NOT AVAIL tt_prov THEN DO:
                                CREATE tt_prov.
                                ASSIGN tt_prov.butik = bonglinje.ReturButikk
                                       tt_prov.datum = dDatumLoop.
                            END.
                            ASSIGN tt_prov.ret_i_andra = tt_prov.ret_i_andra + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.
OUTPUT TO VALUE(SESSION:TEMP-DIR + "prov.xls").
PUT UNFORMATTED "Butik" + CHR(9) +
                "Datum" + CHR(9) +
                "Fsg" + CHR(9) +
                "Webplock" + CHR(9) +
                "Reklamation" + CHR(9) +
                "Retur fr andra" + CHR(9) +
                "Retur i andra"                      
                SKIP.
FOR EACH tt_Prov BY tt_prov.butik BY tt_prov.datum:
    PUT UNFORMATTED STRING(tt_prov.butik) + CHR(9) +
                    STRING(tt_prov.datum) + CHR(9) +
                    STRING(tt_prov.fsg) + CHR(9) +
                    STRING(tt_prov.webplock) + CHR(9) +
                    STRING(tt_prov.reklam) + CHR(9) +
                    STRING(tt_prov.ret_fr_andra) + CHR(9) +
                    STRING(tt_prov.ret_i_andra) SKIP.
END.
OUTPUT CLOSE.

ASSIGN cFilNamn = SESSION:TEMP-DIR + "prov.xls".

MESSAGE "Klart!" SKIP "Fil: " cFilNamn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF VALID-HANDLE(wLibHandle) THEN
    RUN OpenExcelDocument IN wLibHandle (cFilNamn," ").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

