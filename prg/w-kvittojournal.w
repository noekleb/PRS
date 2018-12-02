&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_BongLinje NO-UNDO LIKE BongLinje
       FIELD cTranstype AS CHARACTER
       FIELD cTid AS CHARACTER
       FIELD KassererNr LIKE BongHode.KassererNr.



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

DEFINE VARIABLE iFraTid AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTilTid AS INTEGER    NO-UNDO.
DEFINE VARIABLE cTransTypeTekster AS CHARACTER  NO-UNDO.
DEFINE BUFFER bTT_BongLinje FOR TT_Bonglinje.

DEF VAR cTekst AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-5

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_BongLinje

/* Definitions for BROWSE BROWSE-5                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-5 TT_BongLinje.ButikkNr ~
TT_BongLinje.KasseNr TT_BongLinje.Dato TT_BongLinje.cTid ~
TT_BongLinje.BongNr TT_BongLinje.LinjeNr TT_Bonglinje.Kasserernr ~
TT_BongLinje.cTranstype TT_BongLinje.Strekkode TT_BongLinje.BongTekst ~
TT_BongLinje.Antall TT_BongLinje.LinjeSum TT_BongLinje.LinjeRab 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-5 
&Scoped-define QUERY-STRING-BROWSE-5 FOR EACH TT_BongLinje NO-LOCK ~
    BY TT_BongLinje.TransTid ~
       BY TT_BongLinje.BongNr ~
        BY TT_BongLinje.LinjeNr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-5 OPEN QUERY BROWSE-5 FOR EACH TT_BongLinje NO-LOCK ~
    BY TT_BongLinje.TransTid ~
       BY TT_BongLinje.BongNr ~
        BY TT_BongLinje.LinjeNr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-5 TT_BongLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-5 TT_BongLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-5}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-62 RECT-27 RECT-28 B-VisTrans ~
B-SokButikk B-Excel FI-Butik FI-Dato FI-FraTid FI-TilTid BSok BROWSE-5 ~
Btn_Help BUTTON-Ok BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik FI-Butnamn FI-Dato FI-FraTid ~
FI-TilTid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TransTypeTeks C-Win 
FUNCTION TransTypeTeks RETURNS CHARACTER
  ( INPUT piTTId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1 TOOLTIP "Detaljer".

DEFINE BUTTON B-SokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1 TOOLTIP "Detaljer".

DEFINE BUTTON BSok 
     LABEL "Sök" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI-Butnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraTid AS INTEGER FORMAT "99,99":U INITIAL 0 
     LABEL "Från/Till kl." 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilTid AS INTEGER FORMAT "99,99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188.8 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188.8 BY .1.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-5 FOR 
      TT_BongLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-5 C-Win _STRUCTURED
  QUERY BROWSE-5 NO-LOCK DISPLAY
      TT_BongLinje.ButikkNr FORMAT ">>>>>9":U
      TT_BongLinje.KasseNr FORMAT ">>9":U
      TT_BongLinje.Dato FORMAT "99/99/99":U
      TT_BongLinje.cTid COLUMN-LABEL "Klockan" FORMAT "x(12)":U
            WIDTH 8.4
      TT_BongLinje.BongNr FORMAT ">>>>>>>>>>>>9":U
      TT_BongLinje.LinjeNr FORMAT ">>>>9":U
      TT_Bonglinje.Kasserernr COLUMN-LABEL "Kassör" FORMAT ">>>>>>>>>9":U
      TT_BongLinje.cTranstype COLUMN-LABEL "Transtype" FORMAT "x(30)":U
            WIDTH 15
      TT_BongLinje.Strekkode FORMAT "X(20)":U
      TT_BongLinje.BongTekst FORMAT "X(30)":U
      TT_BongLinje.Antall FORMAT "->>>,>>9.999":U
      TT_BongLinje.LinjeSum FORMAT "->,>>>,>>>,>>9.99":U
      TT_BongLinje.LinjeRab FORMAT "->,>>>,>>9.99":U WIDTH 12.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 187 BY 21.91 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-VisTrans AT ROW 1.24 COL 2
     B-SokButikk AT ROW 3.52 COL 29
     B-Excel AT ROW 1.24 COL 7
     FI-Butik AT ROW 3.52 COL 16.4 COLON-ALIGNED HELP
          "Butikknummer"
     FI-Butnamn AT ROW 3.52 COL 31.6 COLON-ALIGNED NO-LABEL
     FI-Dato AT ROW 4.57 COL 16.4 COLON-ALIGNED
     FI-FraTid AT ROW 5.67 COL 16.4 COLON-ALIGNED
     FI-TilTid AT ROW 5.67 COL 27.4 COLON-ALIGNED NO-LABEL
     BSok AT ROW 5.67 COL 39.4
     BROWSE-5 AT ROW 7.67 COL 2
     Btn_Help AT ROW 1.29 COL 180 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.29 COL 184.4 NO-TAB-STOP 
     BUTTON-SokDato AT ROW 4.62 COL 32.6
     RECT-62 AT ROW 2.62 COL 2
     RECT-27 AT ROW 1.1 COL 1
     RECT-28 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 188.8 BY 28.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_BongLinje T "?" NO-UNDO data BongLinje
      ADDITIONAL-FIELDS:
          FIELD cTranstype AS CHARACTER
          FIELD cTid AS CHARACTER
          FIELD KassererNr LIKE BongHode.KassererNr
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kvittojournal"
         HEIGHT             = 28.91
         WIDTH              = 188.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 203
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 203
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-5 BSok DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Butnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-5
/* Query rebuild information for BROWSE BROWSE-5
     _TblList          = "Temp-Tables.TT_BongLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.TT_BongLinje.TransTid|yes,Temp-Tables.TT_BongLinje.BongNr|yes,Temp-Tables.TT_BongLinje.LinjeNr|yes"
     _FldNameList[1]   = Temp-Tables.TT_BongLinje.ButikkNr
     _FldNameList[2]   = Temp-Tables.TT_BongLinje.KasseNr
     _FldNameList[3]   = Temp-Tables.TT_BongLinje.Dato
     _FldNameList[4]   > "_<CALC>"
"TT_BongLinje.cTid" "Klockan" "x(12)" ? ? ? ? ? ? ? no ? no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.TT_BongLinje.BongNr
     _FldNameList[6]   = Temp-Tables.TT_BongLinje.LinjeNr
     _FldNameList[7]   > "_<CALC>"
"TT_Bonglinje.Kasserernr" "Kassör" ">>>>>>>>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"TT_BongLinje.cTranstype" "Transtype" "x(30)" ? ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = Temp-Tables.TT_BongLinje.Strekkode
     _FldNameList[10]   = Temp-Tables.TT_BongLinje.BongTekst
     _FldNameList[11]   = Temp-Tables.TT_BongLinje.Antall
     _FldNameList[12]   = Temp-Tables.TT_BongLinje.LinjeSum
     _FldNameList[13]   > Temp-Tables.TT_BongLinje.LinjeRab
"TT_BongLinje.LinjeRab" ? ? "decimal" ? ? ? ? ? ? no ? no no "12.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-5 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kvittojournal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kvittojournal */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
    IF NOT CAN-FIND(FIRST bTT_BongLinje) THEN
        RETURN NO-APPLY.
    OUTPUT TO "CLIPBOARD".
    PUT UNFORMATTED
    "Butik" CHR(9) "Kassa" chr(9) "Datum" CHR(9) "Klockan" CHR(9) "Kvittonr" chr(9) "Rad" chr(9) 
    "Kassör" chr(9) "Transtyp" chr(9) "EAN/Plu" chr(9) "Text" chr(9) "Antal" chr(9) "Linjesum" chr(9) "Linjerab" SKIP.

    FOR EACH bTT_BongLinje NO-LOCK BY bTT_BongLinje.TransTid 
                                   BY bTT_BongLinje.BongNr
                                   BY bTT_BongLinje.LinjeNr:
        PUT UNFORMATTED
                bTT_BongLinje.ButikkNr CHR(9)
                bTT_BongLinje.KasseNr CHR(9)
                bTT_BongLinje.Dato CHR(9)
                bTT_BongLinje.cTid CHR(9)
                bTT_BongLinje.BongNr CHR(9)
                bTT_BongLinje.LinjeNr CHR(9)
                bTT_BongLinje.Kasserernr CHR(9)
                SUBSTR(bTT_BongLinje.cTranstype,1,15) CHR(9)
                bTT_BongLinje.Strekkode CHR(9)
                SUBSTR(bTT_BongLinje.BongTekst,1,20) CHR(9)
                bTT_BongLinje.Antall CHR(9)
                bTT_BongLinje.LinjeSum CHR(9)
                bTT_BongLinje.LinjeRab SKIP.
    END.
    OUTPUT CLOSE.
    RUN StartExcel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk C-Win
ON CHOOSE OF B-SokButikk IN FRAME DEFAULT-FRAME /* ... */
or "F10" of FI-Butik IN FRAME {&FRAME-NAME}
DO:
  /*
  {soek.i
    &Felt        = FI-Butik
    &Program     = d-bbutiker.w
    &Frame       = DEFAULT-FRAME
    &PostRun     = "find Butiker no-lock where recid(Butiker) = int(return-value) no-error."
    &OptDisp     = "Butiker.ButNamn when available Butiker @ FI-Butnamn
                    '' when not available Butiker @ FI-Butnamn"
  } 
  */

  DO WITH FRAME {&FRAME-NAME}:
      cTekst = "Butiker".
      IF Bruker.BrukerType = 2 THEN 
      DO:
          RUN JBoxDLookup.w ("Butiker;ButNamn;Butik", "where Butiker.Butik = Bruker.ButikkNr", INPUT-OUTPUT cTekst).
      END.
      ELSE
          RUN JBoxDLookup.w ("Butiker;ButNamn;Butik", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INT(cTekst) NO-ERROR.
      IF AVAILABLE Butiker THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Butik:SCREEN-VALUE   = cTekst
            FI-butNamn:SCREEN-VALUE = Material.MatBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Butik:SCREEN-VALUE    = ''
            FI-butNamn:SCREEN-VALUE  = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans C-Win
ON CHOOSE OF B-VisTrans IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
    IF AVAIL TT_BongLinje THEN
        RUN gviskvittokopi.w (TT_BongLinje.ButikkNr,TT_BongLinje.GruppeNr,TT_BongLinje.KasseNr,TT_BongLinje.Dato,TT_BongLinje.BongNr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BSok C-Win
ON CHOOSE OF BSok IN FRAME DEFAULT-FRAME /* Sök */
DO:
    ASSIGN FI-Butik FI-Dato FI-FraTid FI-TilTid.
  RUN KontrollerInput.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  RUN Rapport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
/*    {winhlp.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato IN FRAME {&FRAME-NAME}
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato
    &Program     = kalender.w
    &Frame       = DEFAULT-FRAME
    &ExtraParam  = "'Datumsökning'"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butik C-Win
ON LEAVE OF FI-Butik IN FRAME DEFAULT-FRAME /* Butikknummer */
DO:
  FIND Butiker WHERE Butiker.Butik = INPUT FI-Butik NO-LOCK NO-ERROR.
  ASSIGN FI-Butnamn:SCREEN-VALUE = IF AVAIL Butiker THEN Butiker.Butnamn ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraTid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraTid C-Win
ON ANY-PRINTABLE OF FI-FraTid IN FRAME DEFAULT-FRAME /* Från/Till kl. */
DO:
/*     IF SELF:CURSOR-OFFSET = 1 AND LASTKEY > 50 THEN */
/*         RETURN NO-APPLY.                            */
/*     IF LASTKEY < 48 OR LASTKEY > 57 THEN                  */
/*         RETURN NO-APPLY.                                  */
/*     IF TRIM(ENTRY(1,SELF:SCREEN-VALUE),":") = "" THEN DO: */
/*         IF LASTKEY > 50 THEN                              */
/*             RETURN NO-APPLY.                              */
/*     END.                                                  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-5
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

FIND Bruker NO-LOCK WHERE
    Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
IF NOT AVAILABLE Bruker THEN
DO:
    MESSAGE "Ukjent bruker: " userid('SkoTex') "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN GetTransTypeTekster IN h_dproclib (OUTPUT cTransTypeTekster).  
  RUN enable_UI.
  
  IF Bruker.BrukerType = 2 THEN
  DO:
      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = Bruker.ButikkNr NO-ERROR.
      ASSIGN
      FI-Butik:SENSITIVE      = FALSE
      B-SokButikk:SENSITIVE   = FALSE
      FI-Butik:SCREEN-VALUE   = STRING(Butiker.Butik)
      FI-Butnamn:SCREEN-VALUE = Butiker.ButNamn.
      APPLY "ENTRY" TO FI-Dato.
  END.
  ELSE DO:
      FIND FIRST Butiker NO-LOCK NO-ERROR.
      APPLY "ENTRY" TO FI-Butik.
  END.
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
  DISPLAY FI-Butik FI-Butnamn FI-Dato FI-FraTid FI-TilTid 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-62 RECT-27 RECT-28 B-VisTrans B-SokButikk B-Excel FI-Butik 
         FI-Dato FI-FraTid FI-TilTid BSok BROWSE-5 Btn_Help BUTTON-Ok 
         BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerInput C-Win 
PROCEDURE KontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cTidFra AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cTidTil AS CHARACTER  NO-UNDO.
 DO WITH FRAME {&FRAME-NAME}:
     FIND Butiker WHERE Butiker.butik = INPUT FI-Butik NO-LOCK NO-ERROR.
     IF NOT AVAIL Butiker THEN DO:
         MESSAGE "Fel butik"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "ENTRY" TO FI-Butik.
         RETURN NO-APPLY.
     END.
     ASSIGN cTidFra = STRING(FI-FraTid,"9999").
     IF INT(SUBSTR(cTidFra,1,2)) > 23 OR INT(SUBSTR(cTidFra,3)) > 59 THEN DO:
         MESSAGE "Fel från kl. ('00.00 - 23.59)"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "ENTRY" TO FI-FraTid.
         RETURN NO-APPLY.
     END.
     ASSIGN cTidTil = STRING(FI-TilTid,"9999").
     IF FI-TilTid > 2400 OR INT(SUBSTR(cTidTil,3)) > 59 THEN DO:
         MESSAGE "Fel til kl. ('<= 24.00)"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "ENTRY" TO FI-TilTid.
         RETURN NO-APPLY.
     END.
     IF FI-FraTid > FI-TilTid THEN DO:
         MESSAGE "Fel från kl., -> större än till kl."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "ENTRY" TO FI-FraTid.
         RETURN NO-APPLY.
     END.
     ASSIGN iFraTid = INT(SUBSTR(cTidFra,1,2)) * 3600 + INT(SUBSTR(cTidFra,3)) * 60.
     IF FI-TilTid = 2400 THEN
         ASSIGN iTilTid = 24 * 3600 - 1.
     ELSE
         ASSIGN iTilTid = INT(SUBSTR(cTidTil,1,2)) * 3600 + INT(SUBSTR(cTidTil,3)) * 60.
 END.
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
  {sww.i}
  DO WITH FRAME {&FRAME-NAME}:
      EMPTY TEMP-TABLE TT_BongLinje.
      {&OPEN-QUERY-{&BROWSE-NAME}}

      FIND butiker WHERE butiker.butik = FI-Butik NO-LOCK NO-ERROR.
      FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK.
          FOR EACH bonghode WHERE 
              bonghode.butik = butiker.butik AND
              bonghode.gruppenr = kasse.gruppenr AND
              bonghode.kassenr  = kasse.kassenr AND
              bonghode.dato = fi-dato           AND
              bonghode.tid >= iFraTid AND
              bonghode.tid <= iTilTid NO-LOCK:
              FOR EACH Bonglinje WHERE BongLinje.b_id = Bonghode.b_id NO-LOCK.
                  CREATE TT_BongLinje.
                  BUFFER-COPY Bonglinje TO TT_BongLinje.
                  ASSIGN TT_BongLinje.cTransType = TransTypeTeks(BongLinje.TTId)
                         TT_BongLinje.cTid = STRING(BongHode.Tid,"HH:MM:SS")
                         TT_BongLinje.Kasserernr = BongHode.Kasserernr.
              END.
          END.
      END.
      {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
 {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartExcel C-Win 
PROCEDURE StartExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chInterior             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER NO-UNDO.
DEFINE VARIABLE iCount2                 AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountH                 AS INTEGER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntallSheets           AS INTEGER NO-UNDO.
DEFINE VARIABLE iInitSheets             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLastColName            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabels   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE numrows   AS INTE INIT 6 NO-UNDO.
DEFINE VARIABLE numcols   AS INTE NO-UNDO.
DEFINE VARIABLE cRangeCol AS CHARACTER  NO-UNDO.
{sww.i}
DO:
  /* create a new Excel Application object */
  CREATE "Excel.Application" chExcelApplication.
  /* launch Excel so it is not visible to the user */
  chExcelApplication:Visible = FALSE.
  /* create a new Workbook */
  chWorkbook = chExcelApplication:Workbooks:Add().
  DO:
    ASSIGN chWorkSheet = chExcelApplication:Sheets:Item(1)
           chWorkSheet:PageSetup:Orientation    = 2 /* Landscape */
           chWorkSheet:NAME = "Butik " + FI-Butik:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        .
  END.
  chWorkSheet = chExcelApplication:Sheets:Item(1).
  chWorkSheet:Range("C:C"):NumberFormat = "@".
  chWorkSheet:PASTE().
  chWorkSheet:Activate().
  chWorkSheet:Range("A1:M1"):Font:Bold = TRUE.
  chWorkSheet:Range("I:I"):NumberFormat = "0".
  chWorkSheet:Range("L:L"):NumberFormat = "0,00".
  chWorkSheet:Columns("A:L"):AutoFit().
  chExcelApplication:Visible = TRUE.
  {swn.i}
  chWorkSheet:SELECT().
  /* release com-handles */
  RELEASE OBJECT chWorksheetRange NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.      
/*   MESSAGE idec1 SKIP idec2               */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TransTypeTeks C-Win 
FUNCTION TransTypeTeks RETURNS CHARACTER
  ( INPUT piTTId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcReturTekst AS CHAR NO-UNDO.
  DEF VAR piEntry      AS INT  NO-UNDO.

  ASSIGN
      piEntry = lookup(string(piTTId,"999"),cTransTypeTekster) - 1
      .

  IF piEntry >= 0 THEN
    ASSIGN
      pcReturTekst = ENTRY(
                           piEntry,
                           cTransTypeTekster)
      .
  ELSE 
      pcReturTekst = "*Ukjent (" + STRING(piTTId,"999") + ")*"
      .

  RETURN pcReturTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

