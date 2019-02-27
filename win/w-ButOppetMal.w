&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
&IF "{&UIB_is_Running}" <> "" &THEN
   DEFINE VARIABLE ipRowIdApn    AS ROWID NO-UNDO.
   DEFINE VARIABLE ipButik       AS INTEGER    NO-UNDO.
   DEFINE VARIABLE ipAar         AS INTEGER  INIT 2003  NO-UNDO.
   DEFINE VARIABLE ipAntDar      AS INTEGER  INIT 5     NO-UNDO.
   DEFINE VARIABLE ipBeskrivelse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ipNotat       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ipOpenClosed  AS CHARACTER NO-UNDO.
   FIND FIRST ApnSkjema NO-LOCK.
   ASSIGN ipRowIdApn = ROWID(ApnSkjema).
&ELSE
   DEFINE INPUT-OUTPUT PARAMETER ipRowIdApn    AS ROWID      NO-UNDO.
   DEFINE INPUT PARAMETER        ipButik       LIKE Butiker.Butik NO-UNDO.
   DEFINE INPUT PARAMETER        ipAar         AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER        ipAntDar      AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER        ipBeskrivelse AS CHARACTER  NO-UNDO.
&ENDIF   

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iClosedCol AS INTEGER INIT  255      NO-UNDO. /* rød  */
DEFINE VARIABLE iHelligCol AS INTEGER INIT  11184895 NO-UNDO. /* rosa */
DEFINE VARIABLE iDataOKCol AS INTEGER INIT  7995257    NO-UNDO. /* grøn */
DEFINE VARIABLE iDataNOKCol AS INTEGER INIT 48895    NO-UNDO. /* orange? */
DEFINE VARIABLE iIkkeAktivCol AS INTEGER INIT  16766935 NO-UNDO. /* ljusblå  */
DEFINE VARIABLE cHelligDar AS CHARACTER             NO-UNDO.
DEFINE VARIABLE dFG31Dec   AS DATE                  NO-UNDO. /* 31 dec föregående år */
DEFINE VARIABLE cRowColPos AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cReturVerdi AS CHARACTER INIT "AVBRYT" NO-UNDO.
DEFINE VARIABLE cLand       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cUke_Dag     AS CHARACTER  NO-UNDO. /* Uke/Dag grid 0,0  */
DEFINE VARIABLE cUkeDagListe AS CHARACTER  NO-UNDO. /* mandag,...  */
DEFINE VARIABLE hRapport     AS HANDLE     NO-UNDO.
DEFINE VARIABLE cHKinst      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKontrolltabell AS CHARACTER  NO-UNDO. /* Kontroll av vilken data vi skall testa mot */
DEFINE VARIABLE iSisteUkeNr AS INTEGER     NO-UNDO.

/* cLand sætts i MAIN, skall vara systemparameter */

DEFINE TEMP-TABLE TT_AapenStengt
    FIELD DagNr AS INTEGER
    FIELD Dato  AS DATE LABEL "Stengt"
    FIELD Dag   AS CHAR LABEL "Dag" FORMAT "x(2)"
    FIELD iType AS INTEGER  /* 1=stängd om normal är öppen, 2=öppen om normal är stängd */
    FIELD iRow  AS INTEGER
    FIELD iCol  AS INTEGER
    INDEX Dagnr Dagnr.
{windows.i}
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-EkstraAapen

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_AapenStengt

/* Definitions for BROWSE BROWSE-EkstraAapen                            */
&Scoped-define FIELDS-IN-QUERY-BROWSE-EkstraAapen TT_AapenStengt.Dato TT_AapenStengt.Dag   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-EkstraAapen   
&Scoped-define SELF-NAME BROWSE-EkstraAapen
&Scoped-define QUERY-STRING-BROWSE-EkstraAapen FOR EACH TT_AapenStengt WHERE TT_AapenStengt.iType = 2
&Scoped-define OPEN-QUERY-BROWSE-EkstraAapen OPEN QUERY {&SELF-NAME} FOR EACH TT_AapenStengt WHERE TT_AapenStengt.iType = 2.
&Scoped-define TABLES-IN-QUERY-BROWSE-EkstraAapen TT_AapenStengt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-EkstraAapen TT_AapenStengt


/* Definitions for BROWSE BROWSE-Stengt                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Stengt TT_AapenStengt.Dato TT_AapenStengt.Dag   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Stengt   
&Scoped-define SELF-NAME BROWSE-Stengt
&Scoped-define QUERY-STRING-BROWSE-Stengt FOR EACH TT_AapenStengt WHERE TT_AapenStengt.iType = 1
&Scoped-define OPEN-QUERY-BROWSE-Stengt OPEN QUERY {&SELF-NAME} FOR EACH TT_AapenStengt WHERE TT_AapenStengt.iType = 1.
&Scoped-define TABLES-IN-QUERY-BROWSE-Stengt TT_AapenStengt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Stengt TT_AapenStengt


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-EkstraAapen}~
    ~{&OPEN-QUERY-BROWSE-Stengt}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-Lagre RECT-2 RECT-27 RECT-28 ~
RECT-3 B-Kontroller FI-Beskrivelse ED-Notat Btn_Help-2 BUTTON-Ok ~
BROWSE-Stengt BUTTON-Slett BROWSE-EkstraAapen FI-StengtTxt FI-AapenTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-Beskrivelse FI-Aar FI-DagType ED-Notat ~
TG-LStengt TG-SStengt FI-StengtTxt FI-AapenTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get1OR4 C-Win 
FUNCTION get1OR4 RETURNS CHARACTER
  ( INPUT ipButik AS INTEGER, INPUT ipDato AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOpenClosed C-Win 
FUNCTION getOpenClosed RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-FRAME-A 
       MENU-ITEM m_Finansrapport LABEL "Finansrapport" 
       MENU-ITEM m_Bokfunderlag LABEL "Bokføringsunderlag".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Farger AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chFarger AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Kalender AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chKalender AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Kontroller 
     LABEL "Kontroller" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help-2 DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Lagre AUTO-GO 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagre" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Slett" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE VARIABLE ED-Notat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 40.8 BY 4 NO-UNDO.

DEFINE VARIABLE FI-AapenTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Åpne dager (ekstra)" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Aar AS INTEGER FORMAT "-9999":U INITIAL 0 
     LABEL "År" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1.

DEFINE VARIABLE FI-DagType AS INTEGER FORMAT "-9":U INITIAL 0 
     LABEL "Dager åpent/uke" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-StengtTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Stengte dager" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 9.29.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 7.38.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY .1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 5.67.

DEFINE VARIABLE TG-LStengt AS LOGICAL INITIAL no 
     LABEL "Lørdager" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SStengt AS LOGICAL INITIAL no 
     LABEL "Søndager" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-EkstraAapen FOR 
      TT_AapenStengt SCROLLING.

DEFINE QUERY BROWSE-Stengt FOR 
      TT_AapenStengt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-EkstraAapen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-EkstraAapen C-Win _FREEFORM
  QUERY BROWSE-EkstraAapen DISPLAY
      TT_AapenStengt.Dato LABEL "Åpen"
      TT_AapenStengt.Dag
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52 BY 6.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-Stengt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Stengt C-Win _FREEFORM
  QUERY BROWSE-Stengt DISPLAY
      TT_AapenStengt.Dato
      TT_AapenStengt.Dag
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52 BY 6.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Lagre AT ROW 1.29 COL 7 NO-TAB-STOP 
     B-Kontroller AT ROW 2.91 COL 51
     FI-Beskrivelse AT ROW 2.91 COL 102.4 COLON-ALIGNED HELP
          "Kort beskrivelse av åpningsskjemaet."
     FI-Aar AT ROW 3 COL 6.6 COLON-ALIGNED
     FI-DagType AT ROW 3 COL 37.6 COLON-ALIGNED
     ED-Notat AT ROW 4.1 COL 104.4 NO-LABEL
     TG-LStengt AT ROW 9.1 COL 94.2
     TG-SStengt AT ROW 10.05 COL 94.2
     Btn_Help-2 AT ROW 1.29 COL 140 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.29 COL 145 NO-TAB-STOP 
     BROWSE-Stengt AT ROW 11.05 COL 94.2
     BUTTON-Slett AT ROW 1.29 COL 2 NO-TAB-STOP 
     BROWSE-EkstraAapen AT ROW 18.95 COL 94.2
     FI-StengtTxt AT ROW 8.43 COL 108.4 COLON-ALIGNED NO-LABEL
     FI-AapenTxt AT ROW 18.14 COL 106.2 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 8.71 COL 89
     RECT-2 AT ROW 18.48 COL 89
     RECT-27 AT ROW 1.1 COL 1
     RECT-28 AT ROW 2.43 COL 1
     RECT-3 AT ROW 2.71 COL 89
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.2 BY 26.43.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11 ROW 20
         SIZE 1 BY 2.


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
         TITLE              = "Åpningsskjema"
         HEIGHT             = 26.43
         WIDTH              = 150.2
         MAX-HEIGHT         = 26.43
         MAX-WIDTH          = 150.2
         VIRTUAL-HEIGHT     = 26.43
         VIRTUAL-WIDTH      = 150.2
         MAX-BUTTON         = no
         TOP-ONLY           = yes
         RESIZE             = no
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-Stengt BUTTON-Ok DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-EkstraAapen BUTTON-Slett DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Aar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DagType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-LStengt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SStengt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-A
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-A:POPUP-MENU       = MENU POPUP-MENU-FRAME-A:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-EkstraAapen
/* Query rebuild information for BROWSE BROWSE-EkstraAapen
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_AapenStengt WHERE TT_AapenStengt.iType = 2.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-EkstraAapen */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Stengt
/* Query rebuild information for BROWSE BROWSE-Stengt
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_AapenStengt WHERE TT_AapenStengt.iType = 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Stengt */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Kalender ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 4.33
       COLUMN          = 2
       HEIGHT          = 21.57
       WIDTH           = 83
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Farger ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 26.19
       COLUMN          = 2
       HEIGHT          = .91
       WIDTH           = 83
       HIDDEN          = no
       SENSITIVE       = yes.
/* Kalender OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
/* Farger OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Kalender:MOVE-AFTER(ED-Notat:HANDLE IN FRAME DEFAULT-FRAME).
      Farger:MOVE-AFTER(FRAME FRAME-A:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Åpningsskjema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Åpningsskjema */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON GO OF FRAME FRAME-A
DO:
    run Apply-mouse-menu-click(self:handle).
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kontroller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kontroller C-Win
ON CHOOSE OF B-Kontroller IN FRAME DEFAULT-FRAME /* Kontroller */
DO:
    DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iDag AS INTEGER    NO-UNDO.
    KONTROLLER: 
    DO iRow = 1 TO 53:
        DO iCol = 1 TO 7:
            IF CAN-DO("0,1,2",ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed)) THEN DO:
/*             IF ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "1" OR       */
/*                ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "2" THEN DO: */
                ASSIGN iDag = INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)).
                IF get1OR4(ApnSkjema.ButikkNr,dFG31Dec + iDag) = "4" THEN
                    ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = iDataOKCol.
/*                 IF CAN-FIND(FIRST kas_rap WHERE kas_rap.dato = dFG31Dec +  iDag AND */
/*                                   kas_rap.butik = ApnSkjema.Butik) THEN             */
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-EkstraAapen
&Scoped-define SELF-NAME BROWSE-EkstraAapen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-EkstraAapen C-Win
ON LEFT-MOUSE-DOWN OF BROWSE-EkstraAapen IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE iDagRow AS INTEGER    NO-UNDO.
    IF BROWSE BROWSE-EkstraAapen:FOCUSED-ROW = ? THEN
        RETURN.
    ASSIGN iDagRow = TT_AapenStengt.iRow.
    IF chKalender:RowIsVisible(TT_AapenStengt.iRow) = TRUE THEN
        RETURN.
    ASSIGN chKalender:TopRow = IF iDagRow < chKalender:TopRow THEN 
                               1 ELSE 27.
/*     chKalender:SELECT(TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol). */
/*   ASSIGN chKalender:ROW = TT_AapenStengt.iRow  */
/*          chKalender:COL = TT_AapenStengt.iCol. */
/*   ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = 0. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Stengt
&Scoped-define SELF-NAME BROWSE-Stengt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Stengt C-Win
ON LEFT-MOUSE-DOWN OF BROWSE-Stengt IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE iDagRow AS INTEGER    NO-UNDO.
    IF BROWSE BROWSE-Stengt:FOCUSED-ROW = ? THEN
        RETURN.
    ASSIGN iDagRow = TT_AapenStengt.iRow.
    IF chKalender:RowIsVisible(TT_AapenStengt.iRow) = TRUE THEN
        RETURN.
    ASSIGN chKalender:TopRow = IF iDagRow < chKalender:TopRow THEN 
                               1 ELSE 27.
/*     chKalender:SELECT(TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol). */
/*   ASSIGN chKalender:ROW = TT_AapenStengt.iRow  */
/*          chKalender:COL = TT_AapenStengt.iCol. */
/*   ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = 0. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help-2 C-Win
ON CHOOSE OF Btn_Help-2 IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
    {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-Win
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagre */
DO:
    DEFINE VARIABLE iRow   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCol   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRetur AS CHARACTER  NO-UNDO.
    ASSIGN cRetur = getOpenClosed().
    FIND CURRENT ApnSkjema EXCLUSIVE.
    ASSIGN ApnSkjema.Beskrivelse = INPUT FI-Beskrivelse
           ApnSkjema.Notat       = INPUT ED-Notat
           ApnSkjema.OpenClosed  = cRetur
           cReturVerdi   = "OK".
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  DEFINE VARIABLE cRetur AS CHARACTER  NO-UNDO.
  ASSIGN cRetur = getOpenClosed().
  IF ED-Notat:MODIFIED =TRUE OR
     FI-Beskrivelse:MODIFIED = TRUE OR
      cRetur <> ApnSkjema.OpenClosed THEN DO:
      MESSAGE "Lagre endring?" 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Lagre" UPDATE l_lagre AS LOGICAL.
      IF l_lagre THEN DO:
          APPLY "CHOOSE" TO BUTTON-Lagre.
          RETURN NO-APPLY.
      END.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
    IF AVAIL TT_AapenStengt THEN DO:
        RUN OpenClosed ("Open",TT_AapenStengt.DagNr,"Slett").
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kalender
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kalender C-Win OCX.MouseDown
PROCEDURE Kalender.VSFlexGrid.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.
DEFINE VARIABLE iMC   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMR   AS INTEGER    NO-UNDO.
DEFINE VARIABLE lOpen    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lOpenPre AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iDagNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraDag AS DATE       NO-UNDO.
DEFINE VARIABLE dTilDag AS DATE       NO-UNDO.
DEFINE VARIABLE lEkstra AS LOGICAL    NO-UNDO.
/* DEFINE VARIABLE rRowId AS ROWID      NO-UNDO.           */
/* DEFINE        VARIABLE cVeckodag AS CHARACTER  NO-UNDO. */
/* ASSIGN cVeckoDag = "S,M,Ti,O,To,F,L".                   */
chKalender:RowPos(chKalender:MouseRow).
ASSIGN iMR       = chKalender:MouseRow
       iMC       = chKalender:MouseCol.
  IF iMR < 0 OR iMC < 0 OR chKalender:Cell(6,iMR,iMC,iMR,iMC) = iIkkeAktivCol THEN
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    /* 1:a rad/col eller 'tom' ruta eller om lør/søn stængt -> return */
    IF iMr = 0 OR iMC = 0 OR chKalender:Cell(0,iMR,iMC,iMR,iMC) = "" OR
             ( p-Button = 2 AND ((iMC = 7 AND FI-DagType < 7) OR (iMC = 6 AND FI-DagType = 5))) THEN
          RETURN.
    IF p-Button = 2 AND chKalender:Cell(6,iMR,iMC,iMR,iMC) = iDataOKCol THEN DO:
/*         Run FixPopUp("FinansRapport"). */
        ASSIGN chKalender:ROW = iMR
               chKalender:COL = iMC.
        APPLY "GO" TO FRAME FRAME-A.
        RETURN.
    END.
    ELSE IF chKalender:Cell(6,iMR,iMC,iMR,iMC) = iDataOKCol THEN
        RETURN.
    ASSIGN lEkstra = iMC > FI-DagType. /* om vi trycker på en cell i 'röd' kolumn -> normalt stängd veckodag */
    IF p-Button = 1 THEN DO:
        ASSIGN iDagNr   = INT(chKalender:Cell(20,iMR,iMC,iMR,iMC)).
        IF lEkstra THEN DO:
            ASSIGN lOpen    = chKalender:Cell(6,iMR,iMC,iMR,iMC) <> iClosedCol.
            RUN OpenClosed (STRING(lOpen = TRUE,"EkstraClosed/EkstraOpen"),iDagNr,"MouseDown").
        END.
        ELSE DO:
            ASSIGN lOpen    = chKalender:Cell(6,iMR,iMC,iMR,iMC) <> iClosedCol.
            RUN OpenClosed (STRING(lOpen = TRUE,"Closed/Open"),iDagNr,"MouseDown").
        END.
    END.
    ELSE IF p-Button = 2 THEN DO: /* ange tidsintervall */
        ASSIGN iDagNr   = INT(chKalender:Cell(20,iMR,iMC,iMR,iMC)).
               dFraDag = dFG31Dec + iDagNr.
        RUN d-FraTildag.w (dFraDag, OUTPUT dTilDag).
        IF dTilDag = ? THEN
            LEAVE.
        DO iCount = iDagNr TO dTilDag - dFG31Dec:
            RUN OpenClosed ("Closed",iCount,"InitOpenClosed"). 
        END.
        {&OPEN-QUERY-BROWSE-Stengt}
        {&OPEN-QUERY-BROWSE-EkstraAapen}
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Bokfunderlag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Bokfunderlag C-Win
ON CHOOSE OF MENU-ITEM m_Bokfunderlag /* Bokføringsunderlag */
DO:
  RUN Rapport(2).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Finansrapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Finansrapport C-Win
ON CHOOSE OF MENU-ITEM m_Finansrapport /* Finansrapport */
DO:
    RUN Rapport(1).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-EkstraAapen
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
{syspara.i 1 3 1 cLand}

IF cLand = "" THEN
  ASSIGN cLand = "NOR". /* syspara.i !!!!!! */
  IF cLand = "SVE" THEN
       ASSIGN cUke_Dag     = "Vecka/Dag"   
              cUkeDagListe = "Måndag,Tisdag,Onsdag,Torsdag,Fredag,Lördag,Söndag".
  ELSE IF cLand = "NOR" THEN
      ASSIGN cUke_Dag     = "Uke/Dag"   
             cUkeDagListe = "Mandag,Tirsdag,Onsdag,Torsdag,Fredag,Lørdag,Søndag".
  {syspara.i 1 1 18 cHKinst}
  IF CAN-DO("1,J,yes",cHKinst) THEN DO:
      {syspara.i 1 1 25 cKontrolltabell}
      IF NOT CAN-DO("1,2",cKontrolltabell) THEN
          ASSIGN cKontrolltabell = "1".
  END.
  ELSE
      ASSIGN cKontrolltabell = "1".
IF ipRowIdApn = ? THEN DO:
    ASSIGN dFG31Dec       = DATE(12,31,ipAar - 1)
           FI-Aar         = ipAar
           FI-DagType     = ipAntDar
           FI-Beskrivelse = ipBeskrivelse
           TG-LStengt     = ipAntDar < 6
           TG-SStengt     = ipAntDar < 7
           ED-Notat       = "".
    RUN NyOpenClosed. /* här initieras ipOpenClosed          */
    RETURN "OK".           /* vi avslutar anropande proc skapar ett ApnSkjema */
END.
ELSE DO:
    FIND ApnSkjema WHERE ROWID(ApnSkjema) = ipRowIdApn NO-LOCK NO-ERROR.
    IF NOT AVAIL ApnSkjema THEN DO:
        MESSAGE "Finner ikke skjema"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN cReturVerdi. /* default "AVBRYT" */
    END.
    ASSIGN dFG31Dec       = DATE(12,31,ApnSkjema.Ar - 1)
           FI-Aar         = ApnSkjema.Ar
           FI-DagType     = ApnSkjema.Ukelengde
           FI-Beskrivelse = ApnSkjema.Beskrivelse
           TG-LStengt     = ApnSkjema.Ukelengde < 6
           TG-SStengt     = ApnSkjema.Ukelengde < 7
           ED-Notat       = ApnSkjema.Notat.
END.
RUN finnukenr(DATE(12,31,ApnSkjema.Ar),OUTPUT iSisteUkeNr).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
/*   {lng.i}  /* Oversettelse */ */
  RUN FillUkeNr(INPUT FI-Aar).
/*      RUN FillUkeNr(2000). */
  RUN InitGrid.
  RUN FillBgColor.
  IF ApnSkjema.OpenClosed <> "" THEN
      RUN InitOpenClosed.
  ASSIGN ED-Notat:MODIFIED       = FALSE
         FI-Beskrivelse:MODIFIED = FALSE
         B-Kontroller:SENSITIVE  = dFG31Dec + 1 < TODAY.
  APPLY "ENTRY" TO FI-Aar.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

RETURN cReturVerdi.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply-mouse-menu-click C-Win 
PROCEDURE Apply-mouse-menu-click :
/*------------------------------------------------------------------------------
Purpose:     Programatic click the right mouse button on a widget
Parameters:  Widget-handle on which you want to click
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR ReturnValue AS INTEGER NO-UNDO.
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONDOWN},
                                 INPUT {&MK_RBUTTON},
                                 INPUT 0,
                                 OUTPUT ReturnValue).
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONUP},
                                 INPUT 0, 
                                 INPUT 0,
                                 OUTPUT ReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-ButOppetMal.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chFarger = Farger:COM-HANDLE
    UIB_S = chFarger:LoadControls( OCXFile, "Farger":U)
    Farger:NAME = "Farger":U
    chKalender = Kalender:COM-HANDLE
    UIB_S = chKalender:LoadControls( OCXFile, "Kalender":U)
    Kalender:NAME = "Kalender":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-ButOppetMal.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY FI-Beskrivelse FI-Aar FI-DagType ED-Notat TG-LStengt TG-SStengt 
          FI-StengtTxt FI-AapenTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 BUTTON-Lagre RECT-2 RECT-27 RECT-28 RECT-3 B-Kontroller 
         FI-Beskrivelse ED-Notat Btn_Help-2 BUTTON-Ok BROWSE-Stengt 
         BUTTON-Slett BROWSE-EkstraAapen FI-StengtTxt FI-AapenTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillBgColor C-Win 
PROCEDURE FillBgColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN chKalender:Cell(6,1,6,53,6) = IF FI-DagType = 5 THEN iClosedCol ELSE 0
           chKalender:Cell(6,1,7,53,7) = IF FI-DagType < 7 THEN iClosedCol ELSE iHelligCol.
    IF FI-DagType > 5 THEN DO iCount = 1 TO 53:
        IF chKalender:Cell(0,iCount,6,iCount,6) <> "" AND CAN-DO(cHelligDar,chKalender:Cell(20,iCount,6,iCount,6)) THEN
           chKalender:Cell(6,iCount,6,iCount,6) = iHelligCol.
    END.
/*     DO iCount = 1 TO 7:                                                        */
/*         IF chKalender:Cell(0,1,iCount,1,iCount) = "" THEN                      */
/*             chKalender:Cell(6,1,iCount,1,iCount) = chKalender:Cell(6,0,0,0,0). */
/*         ELSE                                                                   */
/*             LEAVE.                                                             */
/*     END.                                                                       */
    DO iCount = 7 TO 6 BY -1:
        IF chKalender:Cell(0,53,iCount,53,iCount) = "" THEN
            chKalender:Cell(6,53,iCount,53,iCount) = 0.
        ELSE
            LEAVE.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillUkeNr C-Win 
PROCEDURE FillUkeNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iAar AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE wUke AS INTEGER    NO-UNDO.
  RUN finnukenr(DATE(01,01,iAar),OUTPUT wUke).
  /* iSisteUkenr i defblocket, sätts i main */
  IF wUke <> 1 THEN DO:
      ASSIGN chKalender:TextMatrix(1,0) = wUke.
      DO iCount = 1 TO iSisteUkeNr:
          ASSIGN chKalender:TextMatrix(iCount + 1,0) = iCount.
      END.
  END.
  ELSE DO iCount = 1 TO iSisteUkeNr:
      ASSIGN chKalender:TextMatrix(iCount,0) = iCount.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnHellig C-Win 
PROCEDURE FinnHellig :
/*------------------------------------------------------------------------------
  Purpose:     Finner årets helgedager. 
               Påskedag ut fra formel av C. Fr. Gauss.
  Parameters:  Outp hdag kommasep sting med helligdager.
               Outp hmnd kommasep string med korresponderende måneder.
  Notes:       
------------------------------------------------------------------------------*/
 
   DEF INPUT  PARAMETER wAar     AS INTE NO-UNDO.    /* Årests helgedager   */
   DEF OUTPUT PARAMETER cHdagNum AS CHAR NO-UNDO. /* Lista över årets helgdagar */
   DEF VAR              wHDag    AS CHAR NO-UNDO.    /* Årests helgedager   */
   DEF VAR              wHMnd    AS CHAR NO-UNDO.    /* Korrsponderende mnd */
   DEFINE VARIABLE      iCount   AS INTEGER    NO-UNDO.
   DEFINE VARIABLE      dExtraDat AS DATE       NO-UNDO.
   DEF VAR wPdag AS INTE NO-UNDO. /* Påskedag */
   DEF VAR wPmnd AS INTE NO-UNDO. /* Måneden til påskedagen */

   DEF VAR a AS INTE NO-UNDO.
   DEF VAR b AS INTE NO-UNDO.
   DEF VAR c AS INTE NO-UNDO.    
   DEF VAR d AS INTE NO-UNDO.
   DEF VAR e AS INTE NO-UNDO.

   DEF VAR x AS INTE init 24  NO-UNDO.
   DEF VAR y AS INTE init 5   NO-UNDO.

   /* Faste helgedager */
   IF cLand = "NOR" THEN
       ASSIGN wHDag = "1,1,17,25,26"
              wHMnd = "1,5,5,12,12".
   ELSE IF cLand = "SVE" THEN DO:
       ASSIGN wHDag = "1,6,1,6,25,26"
              wHMnd = "1,1,5,6,12,12".
       /* midsommardagen lördag som infaller under tiden den 20--26 juni */
       DO dExtraDat = DATE(06,20,wAar) TO DATE(06,20,wAar) + 6:
           IF WEEKDAY(dExtraDat) = 7 THEN
               LEAVE.
       END.
       ASSIGN wHDag = wHDag + "," + STRING(DAY(dExtraDat))
              wHMnd = wHMnd + "," + STRING(MONTH(dExtraDat)).
       /* alla helgons dag: lördag 31 oktober--den 6 november */
       DO dExtraDat = DATE(10,31,wAar) TO DATE(10,31,wAar) + 6:
           IF WEEKDAY(dExtraDat) = 7 THEN
               LEAVE.
       END.
       ASSIGN wHDag = wHDag + "," + STRING(DAY(dExtraDat))
              wHMnd = wHMnd + "," + STRING(MONTH(dExtraDat)).
   END.

   IF wAar >= 1700 AND wAar <= 1799 THEN ASSIGN x = 23 y = 3. ELSE
   IF wAar >= 1800 AND wAar <= 1899 THEN ASSIGN x = 23 y = 4. ELSE
   IF wAar >= 1900 AND wAar <= 2099 THEN ASSIGN x = 24 y = 5. ELSE
   IF wAar >= 2100 AND wAar <= 2199 THEN ASSIGN x = 24 y = 6. ELSE
   IF wAar >= 2200 AND wAar <= 2299 THEN ASSIGN x = 25 y = 0. 
   ELSE DO:
      ASSIGN 
         wHDag = wHDag + ",,,,,,,,"
         wHMnd = wHMnd + ",,,,,,,,".
      RETURN.
   END.   

   ASSIGN 
      a = wAar MOD 19
      b = wAar MOD 4
      c = wAar MOD 7
      d = (19 * a + x) MOD 30
      e = (2 * b + 4 * c + 6 * d + y) MOD 7
      wPdag = d + e + 22
      wPmnd = 3. 

   IF wPdag > 31 THEN 
      ASSIGN wPdag = (d + e - 9) wPmnd = 4.
   
   /* Unntagelse ihht. Gauss' formel */   
   IF wPmnd = 4 THEN DO:   
      /* Unntak 1? */
      IF wPdag = 26 THEN 
         ASSIGN wPdag = 19. 
      ELSE       
      /* Unntak 2? */
      IF wPdag = 25 AND (a > 10 AND d = 28 AND e = 6) THEN
         ASSIGN wPdag = 18.
   END.      

   ASSIGN 
     wHDag = wHDag 
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 7))     /* Palmesøndag    */
      + (IF cLand <> "SVE" THEN "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 3)) ELSE "")  /* Skjærtorsdag   */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 2))     /* Langfredag     */
      + "," + STRING(wPdag)                               /* Påskedag       */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 1))     /* 2. påskedag    */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 39))    /* Kr. himmelfart */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 49))    /* 1. pinsedag    */
/*       + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 50))    /* 2. pinsedag    */ */
      + (IF cLand <> "SVE" THEN "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 50)) ELSE "")  /* 2. pinsedag   */
     wHmnd = wHmnd
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 7))   /* Palmesøndag    */
      + (IF cLand <> "SVE" THEN "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 3)) ELSE "")  /* Skjærtorsdag   */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 2))   /* Langfredag     */
      + "," + STRING(wPmnd)                               /* Påskedag       */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 1))   /* 2. påskedag    */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 39))  /* Kr. himmelfart */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 49))  /* 1. pinsedag    */
/*       + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 50)). /* 2. pinsedag    */ */
      + (IF cLand <> "SVE" THEN "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 50)) ELSE "").  /* 2. pinsedag   */
    DO iCount = 1 TO NUM-ENTRIES(wHmnd):
        ASSIGN cHdagNum = cHdagNum + (IF cHdagNum = "" THEN "" ELSE ",") + 
                 STRING(DATE(INT(ENTRY(iCount,wHmnd)),INT(ENTRY(iCount,wHdag)),wAar) - dFG31Dec).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnUkeNr C-Win 
PROCEDURE FinnUkeNr :
/*------------------------------------------------------------------------------
  Purpose:     Finner ukenummer for en gitt dato.
  Parameters:  Input dato, output ukenummer
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
  DEFINE OUTPUT PARAMETER UkeNr    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE  yyyyww   AS INT.   /* Output week, eg 9042     */
 
  DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
  DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
                              /* (01/01/90 is a Monday)      */
  DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
  DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */
 
  ASSIGN yr   = YEAR(indate)
         d1   = WEEKDAY(DATE( 1 , 1 , yr))
         dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                          DATE(1, 10, yr) - d1 )
         UkeNr   = TRUNCATE((indate - dat1 + 7) / 7 , 0).
 
  IF UkeNr < 1 THEN       /* Week 52 or 53 previous year ? */
      ASSIGN yr     = yr - 1
             d1     = WEEKDAY(DATE( 1 , 1 , yr))
             dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                               DATE(1, 10, yr) - d1 )
             UkeNr     = TRUNCATE((indate - dat1 + 7) / 7 , 0).
  ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
      ASSIGN yr     = yr + 1
             d1     = WEEKDAY(DATE( 1 , 1 , yr))
             ukeNr  = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1 THEN 
                          53 ELSE 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixPopUp C-Win 
PROCEDURE FixPopUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wMode     AS CHAR NO-UNDO.
        ASSIGN MENU-ITEM m_Finansrapport:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                        wMode = "Finansrapport"
               MENU-ITEM m_Bokfunderlag:SENSITIVE IN MENU POPUP-MENU-FRAME-A = 
                        wMode = "Bokfunderlag".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitGrid C-Win 
PROCEDURE InitGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE iAntDagar AS INTEGER    NO-UNDO.
     DEFINE VARIABLE iDagNr    AS INTEGER    NO-UNDO.
     DEFINE VARIABLE iRowNr    AS INTEGER    NO-UNDO.
     DEFINE VARIABLE iColNr    AS INTEGER    NO-UNDO.
     DEFINE VARIABLE cManTxt   AS CHARACTER  NO-UNDO.
     DEFINE VARIABLE dDec31FG  AS DATE       NO-UNDO.
     DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
     DEFINE VARIABLE cTxt      AS CHARACTER  NO-UNDO.
     DEFINE VARIABLE iAardagar AS INTEGER    NO-UNDO. /* antal dagar i året */
     DEFINE VARIABLE iAar      AS INTEGER    NO-UNDO.
     ASSIGN cManTxt = "JAN,FEB,MAR,APR,MAJ,JUN,JUL,AUG,SEP,OKT,NOV,DEC"
            iRowNr  = 1.
     DO WITH FRAME {&FRAME-NAME}:
         ASSIGN iAar    = FI-Aar.
         RUN FinnHellig(iAar,OUTPUT cHelligDar).
         ASSIGN dDec31FG   = DATE(12,31,iAar - 1)
                iAarDagar  = DATE(12,31,iAar) - DATE(12,31,iAar - 1)
                iColNr     = WEEKDAY(DATE(1,1,iAar))
                iColNr     = IF iColNr = 1 THEN 7 ELSE iColNr - 1
                cRowColPos = STRING(iRowNr) + ";" + STRING(iColNr)
                chKalender:Cell(0,iRowNr,iColNr,iRowNr,iColNr) = "1 JAN"
                chKalender:Cell(20,iRowNr,iColNr,iRowNr,iColNr) = 1.
         IF CAN-DO(cHelligDar,"1") THEN
             ASSIGN chKalender:Cell(6,iRowNr,iColNr,iRowNr,iColNr) = iHelligCol
                        chKalender:Cell(13,iRowNr,iColNr,iRowNr,iColNr) = TRUE.
         ASSIGN iAntDagar = FI-DagType.
         DO iCount = 2 TO iAarDagar:
             ASSIGN cTxt   = IF DAY(dDec31FG + iCount) = 1 THEN " " + ENTRY(MONTH(dDec31FG + iCount),cManTxt) ELSE ""
                    iColNr = iColNr + 1
                    iColNr = IF iColNr = 8 THEN 1 ELSE iColNr
                    iRowNr = IF iColNr = 1 THEN iRowNr + 1 ELSE iRowNr
                    cRowColPos = cRowColPos + "," + STRING(iRowNr) + ";" + STRING(iColNr).
                    chKalender:Cell(0,iRowNr,iColNr,iRowNr,iColNr) = STRING(DAY(dDec31FG + iCount)) + cTxt NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        MESSAGE iCount
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    chKalender:Cell(20,iRowNr,iColNr,iRowNr,iColNr) = iCount.
             IF CAN-DO(cHelligDar,string(iCount)) THEN DO:
                 ASSIGN chKalender:Cell(6,iRowNr,iColNr,iRowNr,iColNr)  = iHelligCol
                        chKalender:Cell(13,iRowNr,iColNr,iRowNr,iColNr) = TRUE.
             END.
         END.

/*      iClosedCol */
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
     ASSIGN chKalender:AllowUserResizing = 0    /* Fixed columns/rows */
            chKalender:Enabled           = TRUE /* Updateable grid */
            chKalender = chKalender:VSFlexGrid
            chKalender:AllowSelection = FALSE
            chKalender:HighLight      = 0
            chKalender:BorderStyle    = 1
            chKalender:Rows           = iSisteUkeNr + 2
            chKalender:Cols           = 8
            chKalender:FixedRows      = 1
            chKalender:FixedCols      = 1
            chKalender:TextStyle      = 0
            chKalender:TextStyleFixed = 0
            chKalender:ColWidth(1)    = 700
         chKalender:ColWidth(2)       = 700
         chKalender:ColWidth(3)       = 700
         chKalender:ColWidth(4)       = 700
         chKalender:ColWidth(5)       = 700
         chKalender:ColWidth(6)       = 700
         chKalender:ColWidth(7)       = 700
         chFarger = chFarger:vsFlexGrid
         chFarger:Rows = 1
         chFarger:FixedRows = 0
         chFarger:FixedCols = 0
         chFarger:Cols = 6
         chFarger:TextMatrix(0,1) = IF cLand = "SVE" THEN "Stängt" ELSE "Stengt"
         chFarger:TextMatrix(0,2) = IF cLand = "SVE" THEN "Helgdag" ELSE "Helligdag"
         chFarger:TextMatrix(0,3) = "Data OK"
         chFarger:TextMatrix(0,4) = IF cLand = "SVE" THEN "Data saknas" ELSE "Data mangler"
         chFarger:TextMatrix(0,5) = IF cLand = "SVE" THEN "Inte aktiv" ELSE "Ikke aktiv"
         chFarger:Cell(6,0,1,0,1) = iClosedCol 
         chFarger:Cell(6,0,2,0,2) = iHelligCol 
         chFarger:Cell(13,0,2,0,2) = TRUE
         chFarger:Cell(6,0,3,0,3) = iDataOKCol 
         chFarger:Cell(6,0,4,0,4) = iDataNOKCol
         chFarger:Cell(6,0,5,0,5) = iIkkeAktivCol
         chFarger:BorderStyle     = 1
         chFarger:ColWidth(0)     = 1
         chFarger:ColWidth(1)     = 1232
         chFarger:ColWidth(2)     = 1232
         chFarger:ColWidth(3)     = 1232
         chFarger:ColWidth(4)     = 1232
         chFarger:ColWidth(5)     = 1232
         chFarger:Enabled         = FALSE /* Updateable grid */
         chFarger:ROW             = 0
         .
     ASSIGN chKalender:TextMatrix(0,0) = cUke_Dag.
     DO iCount = 1 TO 7:
         ASSIGN chKalender:TextMatrix(0,iCount) = ENTRY(iCount,cUkeDagListe).
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitOpenClosed C-Win 
PROCEDURE InitOpenClosed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
    DO iRow = 1 TO 53:
        DO iCol = 1 TO 7:
            IF chKalender:Cell(0,iRow,iCol,iRow,iCol) <> "" THEN DO:
                IF ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "S" THEN
                    ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = iIkkeAktivCol.
                ELSE IF ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "1" AND
                                    dFG31Dec + INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)) < TODAY THEN
                    ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = iDataNOKCol.
                ELSE IF iCol <= FI-DagType AND ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "0" THEN DO:
                    ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = iClosedCol.
                    RUN OpenClosed ("Closed",INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),"InitOpenClosed"). 
                END.
                ELSE IF ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "2" THEN DO:
                    ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = 0.
                    RUN OpenClosed ("EkstraOpen",INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),"InitOpenClosed"). 
                END.
                ELSE IF ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "4" THEN DO:
                    ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = iDataOKCol.
                END.
            END.
            /* 
             iDataOKCol 
             iDataNOKCol
             */
/*             IF chKalender:Cell(0,iRow,iCol,iRow,iCol) <> "" AND                                           */
/*                           ENTRY(INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),ApnSkjema.OpenClosed) = "0" THEN DO: */
/*                 ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = iClosedCol.                               */
/*                 RUN OpenClosed ("Closed",INT(chKalender:Cell(20,iRow,iCol,iRow,iCol)),"InitOpenClosed").  */
/*             END.                                                                                          */
        END.
    END.
    IF CAN-FIND(FIRST TT_AapenStengt WHERE TT_AapenStengt.iType = 1) THEN
        {&OPEN-QUERY-BROWSE-Stengt}
    IF CAN-FIND(FIRST TT_AapenStengt WHERE TT_AapenStengt.iType = 2) THEN
        {&OPEN-QUERY-BROWSE-EkstraAapen}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyOpenClosed C-Win 
PROCEDURE NyOpenClosed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cAapenVeckodag AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cOpenClosed AS CHARACTER  NO-UNDO.
    FIND Butiker WHERE Butiker.Butik = ipButik NO-LOCK NO-ERROR.
    ASSIGN cAapenVeckodag = IF ipAntDar = 7 THEN "1,2,3,4,5,6,7" ELSE
        IF ipAntDar = 6 THEN "2,3,4,5,6,7" ELSE "2,3,4,5,6".
    DO iCount = 1 TO DATE(12,31,ipAar) - dFG31Dec:
        ASSIGN cOpenClosed = cOpenClosed + (IF cOpenClosed = "" THEN "" ELSE ",") 
            + IF Butiker.ApningsDato <> ? AND Butiker.ApningsDato > dFG31Dec + iCount THEN "S" ELSE
              IF Butiker.NedlagtDato <> ? AND Butiker.NedlagtDato < dFG31Dec + iCount THEN "S" ELSE 
            IF CAN-DO(cAapenVeckodag,STRING(WEEKDAY(dFG31Dec + iCount))) THEN get1OR4(ipButik,dFG31Dec + iCount) ELSE "0".
    END.
    CREATE ApnSkjema.
    ASSIGN ApnSkjema.ButikkNr    = ipButik
           ApnSkjema.Ar          = ipAar
           ApnSkjema.Ukelengde   = ipAntDar
           ApnSkjema.Beskrivelse = ipBeskrivelse
           ApnSkjema.Notat       = ""
           ApnSkjema.OpenClosed  = cOpenClosed
           ipRowIdApn            = ROWID(ApnSkjema)
           C-Win:TITLE           = 'Åpningsskjema for butikk ' + STRING(Butiker.Butik) + ' ' + Butiker.ButNamn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenClosed C-Win 
PROCEDURE OpenClosed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipOpenClosed AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iDagNr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFra         AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iRow         AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  iCol         AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  cVeckodag    AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  rRowId       AS ROWID      NO-UNDO.
    DEFINE        VARIABLE  dStengtDato  AS DATE       NO-UNDO.
    DEFINE        VARIABLE  iVeckoDag    AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE cAapenVeckodag AS CHARACTER  NO-UNDO.
    ASSIGN cVeckoDag = "S,M,Ti,O,To,F,L"
           cAapenVeckodag = IF ApnSkjema.Ukelengde = 7 THEN "1,2,3,4,5,6,7" ELSE
        IF ApnSkjema.Ukelengde = 6 THEN "2,3,4,5,6,7" ELSE "2,3,4,5,6".
    IF ipOpenClosed = "Open" OR ipOpenClosed = "EkstraClosed" THEN DO:
        FIND TT_AapenStengt WHERE TT_AapenStengt.DagNr = iDagNr.
        IF TT_AapenStengt.iType = 1 THEN DO:
            REPOSITION BROWSE-Stengt TO ROWID ROWID(TT_AapenStengt).
            BROWSE BROWSE-Stengt:SELECT-FOCUSED-ROW().
            ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = 
                IF dFG31Dec + INT(chKalender:Cell(20,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol)) < TODAY THEN iDataNOKCol
                ELSE IF (TT_AapenStengt.iCol = 7 OR CAN-DO(cHelligDar,chKalender:Cell(20,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol))) THEN iHelligCol 
                    ELSE 0.
        END.
        ELSE IF TT_AapenStengt.iType = 2 THEN DO:
            ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = iClosedCol.
            REPOSITION BROWSE-EkstraAapen TO ROWID ROWID(TT_AapenStengt).
            BROWSE BROWSE-EkstraAapen:SELECT-FOCUSED-ROW().
        END.
        IF TT_AapenStengt.iType = 1 THEN DO:
            DELETE TT_AapenStengt.
            BROWSE BROWSE-Stengt:DELETE-SELECTED-ROWS().
        END.
        ELSE IF TT_AapenStengt.iType = 2 THEN DO:
            DELETE TT_AapenStengt.
            BROWSE BROWSE-EkstraAapen:DELETE-SELECTED-ROWS().
        END.
    END.
    ELSE DO: /* Closed, EkstraOpen */
IF ipOpenClosed = "EkstraClosed" THEN
    MESSAGE "EkstraClosed"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN dStengtDato = dFG31Dec + iDagNr
               iVeckoDag   = WEEKDAY(dStengtDato).
/*         IF ipAntDar < 6 AND (iVeckoDag = 1 OR iVeckoDag = 7) THEN */
/*             RETURN.                                               */
/*         ELSE IF ipAntDar < 7 AND (iVeckoDag = 1) THEN             */
/*             RETURN.                                               */
        IF ENTRY(iDagNr,ApnSkjema.OpenClosed) = "4" OR ENTRY(iDagNr,ApnSkjema.OpenClosed) = "S" THEN
            RETURN.
/*         ELSE IF NOT CAN-DO(cAapenVeckodag,STRING(iVeckoDag)) THEN */
/*             RETURN.                                               */
        FIND TT_AapenStengt WHERE TT_AapenStengt.DagNr = iDagNr NO-ERROR.
        IF AVAIL TT_AapenStengt AND TT_AapenStengt.iType = 1 THEN
            RETURN.
        ELSE IF AVAIL TT_AapenStengt THEN DO:
            REPOSITION BROWSE-EkstraAapen TO ROWID ROWID(TT_AapenStengt).
            BROWSE BROWSE-EkstraAapen:SELECT-FOCUSED-ROW().
            ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = iClosedCol.
            DELETE TT_AapenStengt.
            RETURN.
        END.
        ASSIGN iRow = INT(ENTRY(1,ENTRY(iDagNr,cRowColPos),";"))
               iCol = INT(ENTRY(2,ENTRY(iDagNr,cRowColPos),";")).
/*         IF cFra = "MouseDown" THEN */
            ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) = 
                 IF ipOpenClosed = "Closed" OR ipOpenClosed = "EkstraClosed" THEN iClosedCol ELSE 0.
        CREATE TT_AapenStengt.
        ASSIGN TT_AapenStengt.DagNr = iDagNr
               TT_AapenStengt.Dato  = dStengtDato
               TT_AapenStengt.Dag   = ENTRY(iVeckoDag,cVeckoDag)
               TT_AapenStengt.iType = IF iCol <= FI-DagType THEN 1 ELSE 2
               TT_AapenStengt.iRow  = iRow
               TT_AapenStengt.iCol  = iCol
               rRowId          = ROWID(TT_AapenStengt).
        RELEASE TT_AapenStengt.
        IF cFra = "MouseDown" THEN DO:
            IF ipOpenClosed = "Closed" THEN DO:
                {&OPEN-QUERY-BROWSE-Stengt}
                REPOSITION BROWSE-Stengt TO ROWID rRowId.
            END.
            ELSE DO:
                {&OPEN-QUERY-BROWSE-EkstraAapen}
                REPOSITION BROWSE-EkstraAapen TO ROWID rRowId.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrgOpenClosed C-Win 
PROCEDURE OrgOpenClosed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipOpenClosed AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iDagNr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFra         AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iRow         AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  iCol         AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  cVeckodag    AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  rRowId       AS ROWID      NO-UNDO.
    DEFINE        VARIABLE  dStengtDato  AS DATE       NO-UNDO.
    DEFINE        VARIABLE  iVeckoDag    AS INTEGER    NO-UNDO.
    ASSIGN cVeckoDag = "S,M,Ti,O,To,F,L".
    IF ipOpenClosed = "Open" OR ipOpenClosed = "EkstraClosed" THEN DO:
        FIND TT_AapenStengt WHERE TT_AapenStengt.DagNr = iDagNr.
        IF TT_AapenStengt.iType = 1 THEN DO:
            REPOSITION BROWSE-Stengt TO ROWID ROWID(TT_AapenStengt).
            BROWSE BROWSE-Stengt:SELECT-FOCUSED-ROW().
            ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) =
                IF dFG31Dec + INT(chKalender:Cell(20,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol)) < TODAY THEN iDataNOKCol
                ELSE IF (TT_AapenStengt.iCol = 7 OR CAN-DO(cHelligDar,chKalender:Cell(20,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol))) THEN iHelligCol
                    ELSE 0.
        END.
        ELSE IF TT_AapenStengt.iType = 2 THEN DO:
            ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = iClosedCol.
            REPOSITION BROWSE-EkstraAapen TO ROWID ROWID(TT_AapenStengt).
            BROWSE BROWSE-EkstraAapen:SELECT-FOCUSED-ROW().
        END.
        IF TT_AapenStengt.iType = 1 THEN DO:
            DELETE TT_AapenStengt.
            BROWSE BROWSE-Stengt:DELETE-SELECTED-ROWS().
        END.
        ELSE IF TT_AapenStengt.iType = 2 THEN DO:
            DELETE TT_AapenStengt.
            BROWSE BROWSE-EkstraAapen:DELETE-SELECTED-ROWS().
        END.
    END.
    ELSE DO: /* Closed, EkstraOpen */
        ASSIGN dStengtDato = dFG31Dec + iDagNr
               iVeckoDag   = WEEKDAY(dStengtDato).
/*         IF ipAntDar < 6 AND (iVeckoDag = 1 OR iVeckoDag = 7) THEN */
/*             RETURN.                                               */
/*         ELSE IF ipAntDar < 7 AND (iVeckoDag = 1) THEN             */
/*             RETURN.                                               */
        IF ENTRY(iDagNr,ApnSkjema.OpenClosed) = "4" THEN
            RETURN.
        FIND TT_AapenStengt WHERE TT_AapenStengt.DagNr = iDagNr NO-ERROR.
        IF AVAIL TT_AapenStengt AND TT_AapenStengt.iType = 1 THEN
            RETURN.
        ELSE IF AVAIL TT_AapenStengt THEN DO:
            REPOSITION BROWSE-EkstraAapen TO ROWID ROWID(TT_AapenStengt).
            BROWSE BROWSE-EkstraAapen:SELECT-FOCUSED-ROW().
            ASSIGN chKalender:Cell(6,TT_AapenStengt.iRow,TT_AapenStengt.iCol,TT_AapenStengt.iRow,TT_AapenStengt.iCol) = iClosedCol.
            DELETE TT_AapenStengt.
            RETURN.
        END.
        ELSE IF (ENTRY(iDagNr,ApnSkjema.OpenClosed) = "S" OR ENTRY(iDagNr,ApnSkjema.OpenClosed) = "0") AND
                 ipOpenClosed = "Closed" AND cFra = "InitOpenClosed" THEN
            RETURN.
        ASSIGN iRow = INT(ENTRY(1,ENTRY(iDagNr,cRowColPos),";"))
               iCol = INT(ENTRY(2,ENTRY(iDagNr,cRowColPos),";")).
/*         IF cFra = "MouseDown" THEN */
            ASSIGN chKalender:Cell(6,iRow,iCol,iRow,iCol) =
                 IF ipOpenClosed = "Closed" OR ipOpenClosed = "EkstraClosed" THEN iClosedCol ELSE 0.
        CREATE TT_AapenStengt.
        ASSIGN TT_AapenStengt.DagNr = iDagNr
               TT_AapenStengt.Dato  = dStengtDato
               TT_AapenStengt.Dag   = ENTRY(iVeckoDag,cVeckoDag)
               TT_AapenStengt.iType = IF iCol <= FI-DagType THEN 1 ELSE 2
               TT_AapenStengt.iRow  = iRow
               TT_AapenStengt.iCol  = iCol
               rRowId          = ROWID(TT_AapenStengt).
        RELEASE TT_AapenStengt.
        IF cFra = "MouseDown" THEN DO:
            IF ipOpenClosed = "Closed" THEN DO:
                {&OPEN-QUERY-BROWSE-Stengt}
                REPOSITION BROWSE-Stengt TO ROWID rRowId.
            END.
            ELSE DO:
                {&OPEN-QUERY-BROWSE-EkstraAapen}
                REPOSITION BROWSE-EkstraAapen TO ROWID rRowId.
            END.
        END.
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
    DEFINE INPUT  PARAMETER iRappType AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         dRappDat  AS DATE       NO-UNDO.
    ASSIGN dRappDat = dFG31Dec + INT(chKalender:Cell(20,chKalender:ROW,chKalender:COL,chKalender:ROW,chKalender:COL)).
    CASE iRapptype:
        WHEN 1 THEN DO: /* Finansrapport */
            RUN w-rkassarapportx.w PERSISTENT SET hRapport.
            RUN AutoInit IN hRapport (0,ApnSkjema.ButikkNr,dRappDat).
        END.
        WHEN 2 THEN DO: /* Bokfunderlag */
            RUN w-rkassarapportx.w PERSISTENT SET hRapport.
            RUN AutoInit IN hRapport (1,ApnSkjema.ButikkNr,dRappDat).
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get1OR4 C-Win 
FUNCTION get1OR4 RETURNS CHARACTER
  ( INPUT ipButik AS INTEGER, INPUT ipDato AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF cKontrolltabell = "1" THEN
      RETURN IF CAN-FIND(FIRST Datasett WHERE Datasett.ButikkNr = ipButik AND
                                              Datasett.Dato  = ipDato AND Datasett.SettStatus > 1) THEN "4" ELSE "1".
  ELSE IF cKontrolltabell = "2" THEN
      RETURN IF CAN-FIND(FIRST BokforingsBilag WHERE BokforingsBilag.OmsetningsDato  = ipDato AND 
                                              BokforingsBilag.ButikkNr = ipButik) THEN "4" ELSE "1".
/*   RETURN IF CAN-FIND(FIRST kas_rap WHERE kas_rap.dato  = ipDato AND           */
/*                                   kas_rap.butik = ipButik) THEN "4" ELSE "1". */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOpenClosed C-Win 
FUNCTION getOpenClosed RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iRow   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCol   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRetur AS CHARACTER  NO-UNDO.
    DO iRow = 1 TO 53:
        DO iCol = 1 TO 7:
            IF chKalender:Cell(0,iRow,iCol,iRow,iCol) <> "" THEN
                ASSIGN cRetur = cRetur + (IF cRetur = "" THEN "" ELSE ",") + 
                  IF chKalender:Cell(6,iRow,iCol,iRow,iCol) = iIkkeAktivCol THEN "S"
                  ELSE IF chKalender:Cell(6,iRow,iCol,iRow,iCol) = iDataOKCol THEN "4"
                  ELSE IF iCol <= FI-DagType THEN
                    STRING(chKalender:Cell(6,iRow,iCol,iRow,iCol) = iClosedCol,"0/1") ELSE
                    STRING(chKalender:Cell(6,iRow,iCol,iRow,iCol) = iClosedCol,"0/2").
        END.
    END.
    RETURN cRetur.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

