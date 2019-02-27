&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Butiker NO-UNDO LIKE Butiker
       FIELD xml as char
       .
DEFINE TEMP-TABLE TT_NyKasseButiker NO-UNDO LIKE Butiker.


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

DEFINE VARIABLE iByttButik AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME B-DTLButiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Butiker Kasse TT_NyKasseButiker

/* Definitions for BROWSE B-DTLButiker                                  */
&Scoped-define FIELDS-IN-QUERY-B-DTLButiker TT_Butiker.Butik ~
TT_Butiker.ButNamn TT_Butiker.xml 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-DTLButiker 
&Scoped-define QUERY-STRING-B-DTLButiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-DTLButiker OPEN QUERY B-DTLButiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-DTLButiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-B-DTLButiker TT_Butiker


/* Definitions for BROWSE B-Kasse                                       */
&Scoped-define FIELDS-IN-QUERY-B-Kasse Kasse.ButikkNr Kasse.KasseNr ~
Kasse.ModellNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Kasse 
&Scoped-define QUERY-STRING-B-Kasse FOR EACH Kasse ~
      WHERE Kasse.ButikkNr = TT_Butiker.butik NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-Kasse OPEN QUERY B-Kasse FOR EACH Kasse ~
      WHERE Kasse.ButikkNr = TT_Butiker.butik NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-Kasse Kasse
&Scoped-define FIRST-TABLE-IN-QUERY-B-Kasse Kasse


/* Definitions for BROWSE B-NyKasse                                     */
&Scoped-define FIELDS-IN-QUERY-B-NyKasse Kasse.ButikkNr Kasse.KasseNr ~
Kasse.ModellNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-NyKasse 
&Scoped-define QUERY-STRING-B-NyKasse FOR EACH Kasse ~
      WHERE Kasse.ButikkNr = TT_NykasseButiker.Butik NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-NyKasse OPEN QUERY B-NyKasse FOR EACH Kasse ~
      WHERE Kasse.ButikkNr = TT_NykasseButiker.Butik NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-NyKasse Kasse
&Scoped-define FIRST-TABLE-IN-QUERY-B-NyKasse Kasse


/* Definitions for BROWSE B-XMLButiker                                  */
&Scoped-define FIELDS-IN-QUERY-B-XMLButiker TT_NyKasseButiker.Butik ~
TT_NyKasseButiker.ButNamn TT_NyKasseButiker.EODRapporter 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-XMLButiker 
&Scoped-define QUERY-STRING-B-XMLButiker FOR EACH TT_NyKasseButiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-XMLButiker OPEN QUERY B-XMLButiker FOR EACH TT_NyKasseButiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-XMLButiker TT_NyKasseButiker
&Scoped-define FIRST-TABLE-IN-QUERY-B-XMLButiker TT_NyKasseButiker


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-B-DTLButiker}~
    ~{&OPEN-QUERY-B-Kasse}~
    ~{&OPEN-QUERY-B-NyKasse}~
    ~{&OPEN-QUERY-B-XMLButiker}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS TT_Butiker.Butik TT_Butiker.ButNamn ~
TT_NyKasseButiker.Butik TT_NyKasseButiker.ButNamn 
&Scoped-define ENABLED-TABLES TT_Butiker TT_NyKasseButiker
&Scoped-define FIRST-ENABLED-TABLE TT_Butiker
&Scoped-define SECOND-ENABLED-TABLE TT_NyKasseButiker
&Scoped-Define ENABLED-OBJECTS RECT-62 FI-DTLtxt B-DTLButiker B-Kasse ~
TG-Kommission B-Endra FI-XMLtxt B-XMLButiker B-NyKasse FI-ByteTxt FI-FraTxt ~
FI-TilTxt 
&Scoped-Define DISPLAYED-FIELDS TT_Butiker.Butik TT_Butiker.ButNamn ~
TT_NyKasseButiker.Butik TT_NyKasseButiker.ButNamn 
&Scoped-define DISPLAYED-TABLES TT_Butiker TT_NyKasseButiker
&Scoped-define FIRST-DISPLAYED-TABLE TT_Butiker
&Scoped-define SECOND-DISPLAYED-TABLE TT_NyKasseButiker
&Scoped-Define DISPLAYED-OBJECTS FI-DTLtxt TG-Kommission FI-XMLtxt ~
FI-ByteTxt FI-FraTxt FI-TilTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Endra 
     LABEL "Ändra till XML" 
     SIZE 29 BY 1.14.

DEFINE VARIABLE FI-ByteTxt AS CHARACTER FORMAT "X(256)":U INITIAL "   Byte av kassauppsätt" 
      VIEW-AS TEXT 
     SIZE 30.2 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-DTLtxt AS CHARACTER FORMAT "X(256)":U INITIAL "Butiker med DTL-kassor" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FraTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Butik som skall ändras" 
      VIEW-AS TEXT 
     SIZE 28.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TilTxt AS CHARACTER FORMAT "X(256)":U INITIAL "med samma uppsätt som" 
      VIEW-AS TEXT 
     SIZE 37.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-XMLtxt AS CHARACTER FORMAT "X(256)":U INITIAL "Butiker med XML-kassor" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1.19
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 13.33
     BGCOLOR 1 FGCOLOR 9 .

DEFINE VARIABLE TG-Kommission AS LOGICAL INITIAL no 
     LABEL "Kommission" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-DTLButiker FOR 
      TT_Butiker SCROLLING.

DEFINE QUERY B-Kasse FOR 
      Kasse SCROLLING.

DEFINE QUERY B-NyKasse FOR 
      Kasse SCROLLING.

DEFINE QUERY B-XMLButiker FOR 
      TT_NyKasseButiker SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-DTLButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-DTLButiker C-Win _STRUCTURED
  QUERY B-DTLButiker NO-LOCK DISPLAY
      TT_Butiker.Butik FORMAT ">>>>>9":U
      TT_Butiker.ButNamn FORMAT "x(40)":U WIDTH 49.8
      TT_Butiker.xml COLUMN-LABEL "Xml" FORMAT "x(8)":U WIDTH 6.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69.8 BY 13.19 EXPANDABLE.

DEFINE BROWSE B-Kasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Kasse C-Win _STRUCTURED
  QUERY B-Kasse NO-LOCK DISPLAY
      Kasse.ButikkNr FORMAT ">>>>>9":U
      Kasse.KasseNr FORMAT ">>9":U
      Kasse.ModellNr FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 13.19 EXPANDABLE.

DEFINE BROWSE B-NyKasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-NyKasse C-Win _STRUCTURED
  QUERY B-NyKasse NO-LOCK DISPLAY
      Kasse.ButikkNr FORMAT ">>>>>9":U
      Kasse.KasseNr FORMAT ">>9":U
      Kasse.ModellNr FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 13.19 EXPANDABLE.

DEFINE BROWSE B-XMLButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-XMLButiker C-Win _STRUCTURED
  QUERY B-XMLButiker NO-LOCK DISPLAY
      TT_NyKasseButiker.Butik FORMAT ">>>>>9":U
      TT_NyKasseButiker.ButNamn FORMAT "x(30)":U WIDTH 38.4
      TT_NyKasseButiker.EODRapporter COLUMN-LABEL "Kommission" FORMAT "J/N":U
            WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69.8 BY 13.19 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-DTLtxt AT ROW 3.14 COL 3.2 NO-LABEL
     B-DTLButiker AT ROW 5.1 COL 3.2
     B-Kasse AT ROW 5.1 COL 75.2
     TT_Butiker.Butik AT ROW 7.19 COL 132.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     TT_Butiker.ButNamn AT ROW 8.19 COL 132.4 COLON-ALIGNED FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 37.4 BY 1
     TG-Kommission AT ROW 9.33 COL 134.4
     TT_NyKasseButiker.Butik AT ROW 11.91 COL 132.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     TT_NyKasseButiker.ButNamn AT ROW 12.91 COL 132.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37.4 BY 1
     B-Endra AT ROW 15.52 COL 134
     FI-XMLtxt AT ROW 19.14 COL 3.2 NO-LABEL
     B-XMLButiker AT ROW 21 COL 3.2
     B-NyKasse AT ROW 21 COL 75.2
     FI-ByteTxt AT ROW 4.57 COL 132.6 COLON-ALIGNED NO-LABEL
     FI-FraTxt AT ROW 6.05 COL 121.2 COLON-ALIGNED NO-LABEL
     FI-TilTxt AT ROW 10.71 COL 121.2 COLON-ALIGNED NO-LABEL
     RECT-62 AT ROW 5.05 COL 116.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183.6 BY 34.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Butiker T "?" NO-UNDO SkoTex Butiker
      ADDITIONAL-FIELDS:
          FIELD xml as char
          
      END-FIELDS.
      TABLE: TT_NyKasseButiker T "?" NO-UNDO SkoTex Butiker
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopiering av kassor"
         HEIGHT             = 34.33
         WIDTH              = 183.6
         MAX-HEIGHT         = 40.57
         MAX-WIDTH          = 217.2
         VIRTUAL-HEIGHT     = 40.57
         VIRTUAL-WIDTH      = 217.2
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
/* BROWSE-TAB B-DTLButiker FI-DTLtxt DEFAULT-FRAME */
/* BROWSE-TAB B-Kasse B-DTLButiker DEFAULT-FRAME */
/* BROWSE-TAB B-XMLButiker FI-XMLtxt DEFAULT-FRAME */
/* BROWSE-TAB B-NyKasse B-XMLButiker DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN TT_Butiker.ButNamn IN FRAME DEFAULT-FRAME
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FI-DTLtxt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-XMLtxt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-DTLButiker
/* Query rebuild information for BROWSE B-DTLButiker
     _TblList          = "Temp-Tables.TT_Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_Butiker.Butik
     _FldNameList[2]   > Temp-Tables.TT_Butiker.ButNamn
"ButNamn" ? "x(40)" "character" ? ? ? ? ? ? no ? no no "49.8" yes no no "U" "" ""
     _FldNameList[3]   > "_<CALC>"
"TT_Butiker.xml" "Xml" "x(8)" ? ? ? ? ? ? ? no ? no no "6.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE B-DTLButiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Kasse
/* Query rebuild information for BROWSE B-Kasse
     _TblList          = "SkoTex.Kasse"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SkoTex.Kasse.ButikkNr = TT_Butiker.butik"
     _FldNameList[1]   = SkoTex.Kasse.ButikkNr
     _FldNameList[2]   = SkoTex.Kasse.KasseNr
     _FldNameList[3]   = SkoTex.Kasse.ModellNr
     _Query            is OPENED
*/  /* BROWSE B-Kasse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-NyKasse
/* Query rebuild information for BROWSE B-NyKasse
     _TblList          = "SkoTex.Kasse"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SkoTex.Kasse.ButikkNr = TT_NykasseButiker.Butik"
     _FldNameList[1]   = SkoTex.Kasse.ButikkNr
     _FldNameList[2]   = SkoTex.Kasse.KasseNr
     _FldNameList[3]   = SkoTex.Kasse.ModellNr
     _Query            is OPENED
*/  /* BROWSE B-NyKasse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-XMLButiker
/* Query rebuild information for BROWSE B-XMLButiker
     _TblList          = "Temp-Tables.TT_NyKasseButiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_NyKasseButiker.Butik
     _FldNameList[2]   > Temp-Tables.TT_NyKasseButiker.ButNamn
"TT_NyKasseButiker.ButNamn" ? "x(30)" "character" ? ? ? ? ? ? no ? no no "38.4" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.TT_NyKasseButiker.EODRapporter
"TT_NyKasseButiker.EODRapporter" "Kommission" "J/N" "logical" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE B-XMLButiker */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kopiering av kassor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kopiering av kassor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-DTLButiker
&Scoped-define SELF-NAME B-DTLButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-DTLButiker C-Win
ON VALUE-CHANGED OF B-DTLButiker IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-B-Kasse}
  DISPLAY TT_Butiker.Butik TT_Butiker.ButNamn WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Endra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Endra C-Win
ON CHOOSE OF B-Endra IN FRAME DEFAULT-FRAME /* Ändra till XML */
DO:
    IF NOT AVAIL TT_Butiker OR NOT AVAIL TT_NyKasseButiker THEN
        RETURN.
  MESSAGE "Önskar du kopiera kassadefinitionerna från" SKIP
          "Butik" TT_NyKasseButiker.Butik TT_NyKasseButiker.ButNamn "till" SKIP
          "Butik" TT_Butiker.Butik TT_Butiker.ButNamn "och" SKIP(1)
          "Kommission" STRING(INPUT TG-Kommission,"satt/INTE satt") " (J/N)"
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOG.
  IF choice = TRUE THEN DO:
      RUN BytKassatyp (TT_Butiker.Butik,TT_NyKasseButiker.butik).
      RUN ByggTT.
      {&OPEN-QUERY-B-DTLButiker}
      APPLY "VALUE-CHANGED" TO B-DTLButiker.
      {&OPEN-QUERY-B-XMLButiker}
      FIND TT_NyKasseButiker WHERE TT_NyKasseButiker.butik = iByttButik.
      REPOSITION B-XMLButiker TO ROWID ROWID(TT_NyKasseButiker).
      APPLY "VALUE-CHANGED" TO B-XMLButiker.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-XMLButiker
&Scoped-define SELF-NAME B-XMLButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-XMLButiker C-Win
ON VALUE-CHANGED OF B-XMLButiker IN FRAME DEFAULT-FRAME
DO:
    {&OPEN-QUERY-B-NyKasse}
     DISPLAY TT_NykasseButiker.Butik TT_NykasseButiker.ButNamn WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-DTLButiker
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
  RUN ByggTT.
  RUN enable_UI.
  IF BROWSE B-DTLButiker:FOCUSED-ROW <> ? THEN
      APPLY "VALUE-CHANGED" TO B-DTLButiker.
  IF BROWSE B-XMLButiker:FOCUSED-ROW <> ? THEN
      APPLY "VALUE-CHANGED" TO B-XMLButiker.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT C-Win 
PROCEDURE ByggTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_Butiker:
        DELETE TT_Butiker.
    END.
    FOR EACH TT_NykasseButiker:
        DELETE TT_NykasseButiker.
    END.

    FOR EACH butiker NO-LOCK:
        IF Butiker.ApningsDato = ? THEN
            NEXT.
        IF Butiker.NedlagtDato <> ? THEN
            NEXT.
        IF CAN-FIND(FIRST Kasse WHERE kasse.butik = butiker.butik AND Kasse.modellnr = 51) THEN DO:
            CREATE TT_NyKasseButiker.
            ASSIGN TT_NyKasseButiker.butik = butiker.butik
                   TT_NyKasseButiker.butnamn = butiker.butnamn
                   TT_NyKasseButiker.EODRapporter = Butiker.EODRapporter.
            NEXT.
        END.
/*         IF CAN-FIND(FIRST kasse WHERE kasse.butik = butiker.butik AND kasse.aktiv = TRUE) THEN DO: */
            CREATE TT_Butiker.
            ASSIGN TT_Butiker.butik = butiker.butik
                   TT_butiker.butnamn = butiker.butnamn
                   TT_Butiker.xml     = STRING(CAN-FIND(FIRST filer WHERE filer.filnavn BEGINS "POSEvent_" + STRING(butiker.butik)),"Finns/").
/*         END. */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytKassatyp C-Win 
PROCEDURE BytKassatyp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEFINE INPUT  PARAMETER iKopierTil AS INTEGER    NO-UNDO.
      DEFINE INPUT  PARAMETER iKopierFra AS INTEGER    NO-UNDO.
      DEFINE BUFFER bKasse FOR Kasse.
      FOR EACH Kasse WHERE Kasse.butik = iKopierTil:
          DELETE Kasse.
      END.
      FOR EACH gruppe WHERE gruppe.butik = iKopierTil:
          DELETE gruppe.
      END.

      FOR EACH Kasse WHERE Kasse.butikknr = iKopierFra NO-LOCK.
          CREATE bKasse.
          BUFFER-COPY Kasse EXCEPT butikknr TO bKasse.
          ASSIGN bKasse.Aktiv         = TRUE
                 bKasse.ButikkNr      = iKopierTil
                 bKasse.Navn          = REPLACE(bKasse.Navn,STRING(Kasse.ButikkNr),STRING(iKopierTil))
                 ENTRY(1,bKasse.eljournalid,";") = STRING(iKopierTil)
                 ENTRY(NUM-ENTRIES(bKasse.eljournalkatalog,"\"),bKasse.eljournalkatalog,"\")  = STRING(iKopiertil).
      END.
      CREATE gruppe.
      ASSIGN gruppe.butik = iKopierTil
             gruppe.gruppenr = bKasse.gruppenr.
      IF TG-Kommission:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
          FIND Butiker WHERE Butiker.butik = iKopierTil.
          ASSIGN Butiker.EODRapporter = TRUE.
          TG-Kommission:CHECKED = FALSE.
      END.
      FOR EACH filer WHERE filer.filnavn BEGINS "POSEvent_" + STRING(iKopierTil):
          FOR EACH fillogg OF fillinjer:
              DELETE fillogg.
          END.
          DELETE filer.
      END.
      iByttButik = iKopierTil.
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
  DISPLAY FI-DTLtxt TG-Kommission FI-XMLtxt FI-ByteTxt FI-FraTxt FI-TilTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE TT_Butiker THEN 
    DISPLAY TT_Butiker.Butik TT_Butiker.ButNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE TT_NyKasseButiker THEN 
    DISPLAY TT_NyKasseButiker.Butik TT_NyKasseButiker.ButNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-62 FI-DTLtxt B-DTLButiker B-Kasse TT_Butiker.Butik 
         TT_Butiker.ButNamn TG-Kommission TT_NyKasseButiker.Butik 
         TT_NyKasseButiker.ButNamn B-Endra FI-XMLtxt B-XMLButiker B-NyKasse 
         FI-ByteTxt FI-FraTxt FI-TilTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

