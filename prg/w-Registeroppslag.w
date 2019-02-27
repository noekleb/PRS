&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
DEF VAR t                    AS INTE         NO-UNDO.
DEF VAR r                    AS INTE         NO-UNDO.
    
DEF VAR i    AS INTE NO-UNDO.
DEF VAR sp   AS CHAR NO-UNDO.
DEF VAR mCol AS INTE NO-UNDO.
DEFINE VARIABLE cInstbutikk AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hAktivFrame   AS HANDLE EXTENT 5  NO-UNDO.
DEFINE VARIABLE iFirstDataRow AS INTEGER  INIT 4  NO-UNDO.
DEFINE VARIABLE iFirstDataCol AS INTEGER  INIT 3  NO-UNDO.
DEFINE VARIABLE iWidthPix  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStorrelser AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLagerInit  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGridModell AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iModellRow AS INTEGER    NO-UNDO.
DEFINE VARIABLE iModellCol AS INTEGER    NO-UNDO.
DEFINE VARIABLE lGetLastTrans AS LOGICAL    NO-UNDO.
DEF VAR flexcpAlignment      AS INTE INIT  2 NO-UNDO.
DEF VAR flexcpForeColor      AS INTE INIT  7 NO-UNDO.
DEF VAR flexcpBackColor      AS INTE INIT  6 NO-UNDO.
DEF VAR flexcpFontBold       AS INTE INIT 13 NO-UNDO.
DEF VAR flexGridNone         AS INTE INIT  0 NO-UNDO.
DEF VAR flexcpText           AS INTE INIT  0 NO-UNDO.
DEF VAR flexcpFontSize       AS INTE INIT 12 NO-UNDO.
DEF VAR flexcpFontItalic     AS INTE INIT 14 NO-UNDO.
DEF VAR flexAutoSizeColWidth AS INTE INIT  0 NO-UNDO.
DEF VAR flexMergeSpill       AS INTE INIT  6 NO-UNDO.
DEF VAR flexOutlineCollapsed AS INTE INIT  2 NO-UNDO.
DEF VAR flexOutlineExpanded  AS INTE INIT  0 NO-UNDO.
DEF VAR flexcpTextDisplay    AS INTE INIT 19 NO-UNDO.
DEF VAR vbBlack              AS CHAR INIT "&H0" NO-UNDO.
DEF VAR vbRed                AS CHAR INIT "&HFF" NO-UNDO.
DEF VAR vbGreen              AS CHAR INIT "&HFF00" NO-UNDO.
DEF VAR vbYellow             AS CHAR INIT "&HFFFF" NO-UNDO.
DEF VAR vbBlue               AS CHAR INIT "&HFF0000" NO-UNDO.
DEF VAR vbMagenta            AS CHAR INIT "&HFF00FF" NO-UNDO.
DEF VAR vbCyan               AS CHAR INIT "&HFFFF00" NO-UNDO.
DEF VAR vbWhite              AS CHAR INIT "&HFFFFFF" NO-UNDO.
DEF VAR flexAlignRightBottom AS CHAR INIT  8 NO-UNDO.
DEF VAR flexAlignLeftBottom AS CHAR INIT  2 NO-UNDO.
  DEFINE BUFFER bArtBas FOR ArtBas.

DEFINE TEMP-TABLE TT_Grid NO-UNDO
    FIELD iGridrow AS INTEGER
    FIELD iOutline AS INTEGER
    FIELD iGridCol AS INTEGER
    FIELD cText    AS CHARACTER
    FIELD cExtraData AS CHARACTER. /* anvænds før eventuell referens till databasen */
DEFINE TEMP-TABLE TT_Lager NO-UNDO
    FIELD iButikkNr LIKE Butiker.butik
    FIELD cButnamn AS CHAR
    FIELD cAntallListe AS CHAR
    INDEX iButikkNr iButikkNr.

DEFINE TEMP-TABLE TT_Trans NO-UNDO
    FIELD iButikkNr LIKE Butiker.butik
    FIELD deAntall     AS DEC
    FIELD dPris        AS DEC
    FIELD dRabkr       AS DEC
    FIELD dLinjeRab    AS DEC
    FIELD dSubtotalrab AS DEC
    FIELD iBongNr      AS INTE
    FIELD cSelger      AS CHAR
    FIELD cBongInfo    AS CHAR
    INDEX iButikkNr iButikkNr.

{incl/DevMode.i}
{incl/CustDevMode.i}
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define QUERY-NAME QUERY-Gave

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Gavekort LevBas Tilgode

/* Definitions for QUERY QUERY-Gave                                     */
&Scoped-define QUERY-STRING-QUERY-Gave FOR EACH Gavekort NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-Gave OPEN QUERY QUERY-Gave FOR EACH Gavekort NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-Gave Gavekort
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Gave Gavekort


/* Definitions for QUERY QUERY-Lev                                      */
&Scoped-define QUERY-STRING-QUERY-Lev FOR EACH LevBas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-Lev OPEN QUERY QUERY-Lev FOR EACH LevBas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-Lev LevBas
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Lev LevBas


/* Definitions for QUERY QUERY-Tilg                                     */
&Scoped-define QUERY-STRING-QUERY-Tilg FOR EACH Tilgode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-Tilg OPEN QUERY QUERY-Tilg FOR EACH Tilgode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-Tilg Tilgode
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Tilg Tilgode


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Done Btn_Help RECT-2 RECT-1 CB-Register 
&Scoped-Define DISPLAYED-OBJECTS CB-Register 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getType C-Win 
FUNCTION getType RETURNS CHARACTER
  ( INPUT iRSType AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE VARIABLE CB-Register AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Register" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Leverandører",1,
                     "Artikkel/Lager",2,
                     "Artikkel/Salgstranser",3
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.4 BY .1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.4 BY .1.

DEFINE BUTTON B-AktiverArt 
     LABEL "Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Sok 
     LABEL "Søk artikkel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE VARIABLE FI-ArtNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DatoArt AS DATE FORMAT "99/99/99":U 
     LABEL "Transdato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrekKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "StrekKode" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE BUTTON B-AktiverGave 
     LABEL "Aktiver" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-GaveEgne AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Egne" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Alle","1",
                     "Egne","2",
                     "Andre","3"
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-GaveBelopFra AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Belop fra/til" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-GaveBelopTil AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-GaveBruktdatoFra AS DATE FORMAT "99/99/99" 
     LABEL "Bruktdato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-GaveBruktdatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Gavebutnr AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FI-GaveDatoFra AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-GaveDatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-GaveIdentNr AS CHARACTER FORMAT "X(20)" 
     LABEL "Identnr" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE BUTTON B-AktiverLager 
     LABEL "Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SokLager 
     LABEL "Søk artikkel" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-StrekKodeLager AS CHARACTER FORMAT "X(256)":U 
     LABEL "StrekKode" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE BUTTON B-AktiverLev 
     LABEL "Aktiver" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Levnavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Levnavn" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE BUTTON B-AktiverTilg 
     LABEL "Aktiver" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-TilgEgne AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Egne" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Alle","1",
                     "Egne","2",
                     "Andre","3"
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilgBelopFra AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Belop fra/til" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-TilgBelopTil AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-TilgBruktdatoFra AS DATE FORMAT "99/99/99" 
     LABEL "Innløstdato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-TilgBruktdatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Tilgbutnr AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FI-TilgDatoFra AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-TilgDatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-TilgIdentNr AS CHARACTER FORMAT "X(20)" 
     LABEL "Identnr" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QUERY-Gave FOR 
      Gavekort SCROLLING.

DEFINE QUERY QUERY-Lev FOR 
      LevBas SCROLLING.

DEFINE QUERY QUERY-Tilg FOR 
      Tilgode SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Done AT ROW 1.24 COL 139.4 NO-TAB-STOP 
     Btn_Help AT ROW 1.24 COL 134.2 NO-TAB-STOP 
     CB-Register AT ROW 1.24 COL 10 COLON-ALIGNED
     RECT-2 AT ROW 2.33 COL 1
     RECT-1 AT ROW 1.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.6 BY 27.1.

DEFINE FRAME FRAME-Tilgode
     FI-Tilgbutnr AT ROW 1.24 COL 10 COLON-ALIGNED HELP
          "Butikknummer"
     CB-TilgEgne AT ROW 1.24 COL 27 COLON-ALIGNED
     FI-TilgDatoFra AT ROW 1.24 COL 57.8 COLON-ALIGNED HELP
          "Dato"
     FI-TilgDatoTil AT ROW 1.24 COL 73.8 COLON-ALIGNED HELP
          "Dato" NO-LABEL
     FI-TilgBruktdatoFra AT ROW 1.24 COL 107.8 COLON-ALIGNED HELP
          "Dato"
     FI-TilgBruktdatoTil AT ROW 1.24 COL 123.6 COLON-ALIGNED HELP
          "Dato" NO-LABEL
     FI-TilgIdentNr AT ROW 2.67 COL 10 COLON-ALIGNED
     FI-TilgBelopFra AT ROW 2.67 COL 57.8 COLON-ALIGNED
     FI-TilgBelopTil AT ROW 2.67 COL 73.8 COLON-ALIGNED NO-LABEL
     B-AktiverTilg AT ROW 2.67 COL 110
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.62
         SIZE 143.4 BY 4.29
         TITLE "Tilgodelapper".

DEFINE FRAME FRAME-Gavekort
     FI-Gavebutnr AT ROW 1.24 COL 10 COLON-ALIGNED HELP
          "Butikknummer"
     CB-GaveEgne AT ROW 1.24 COL 27 COLON-ALIGNED
     FI-GaveDatoFra AT ROW 1.24 COL 57.8 COLON-ALIGNED HELP
          "Dato"
     FI-GaveDatoTil AT ROW 1.24 COL 73.8 COLON-ALIGNED HELP
          "Dato" NO-LABEL
     FI-GaveBruktdatoFra AT ROW 1.24 COL 107.8 COLON-ALIGNED HELP
          "Dato"
     FI-GaveBruktdatoTil AT ROW 1.24 COL 123.6 COLON-ALIGNED HELP
          "Dato" NO-LABEL
     FI-GaveIdentNr AT ROW 2.67 COL 10 COLON-ALIGNED
     FI-GaveBelopFra AT ROW 2.67 COL 57.8 COLON-ALIGNED
     FI-GaveBelopTil AT ROW 2.67 COL 73.8 COLON-ALIGNED NO-LABEL
     B-AktiverGave AT ROW 2.67 COL 110
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.62
         SIZE 143.4 BY 4.29
         TITLE "Gavekort".

DEFINE FRAME FRAME-Lev
     FI-Levnavn AT ROW 2.38 COL 18 COLON-ALIGNED
     B-AktiverLev AT ROW 2.38 COL 50.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.62
         SIZE 143.4 BY 4.29.

DEFINE FRAME FRAME-Lager
     FI-StrekKodeLager AT ROW 2.38 COL 18 COLON-ALIGNED
     B-AktiverLager AT ROW 2.38 COL 50.2
     B-SokLager AT ROW 2.38 COL 66
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.62
         SIZE 143.4 BY 4.29.

DEFINE FRAME FRAME-Art
     BUTTON-Next AT ROW 1.19 COL 82.6 NO-TAB-STOP 
     FI-StrekKode AT ROW 1.19 COL 18 COLON-ALIGNED
     FI-DatoArt AT ROW 1.19 COL 61.4 COLON-ALIGNED
     FI-ArtNr AT ROW 2.38 COL 18 COLON-ALIGNED NO-TAB-STOP 
     B-AktiverArt AT ROW 2.38 COL 50.2
     B-Sok AT ROW 2.38 COL 66
     BUTTON-Prev AT ROW 1.19 COL 78 NO-TAB-STOP 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.62
         SIZE 143.4 BY 4.29.


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
         TITLE              = "Registeroppslag"
         HEIGHT             = 27.1
         WIDTH              = 143.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Art:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Gavekort:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Lager:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Lev:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Tilgode:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-Art
                                                                        */
/* SETTINGS FOR FILL-IN FI-ArtNr IN FRAME FRAME-Art
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-Gavekort
                                                                        */
/* SETTINGS FOR FRAME FRAME-Lager
                                                                        */
/* SETTINGS FOR FRAME FRAME-Lev
                                                                        */
/* SETTINGS FOR FRAME FRAME-Tilgode
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Gave
/* Query rebuild information for QUERY QUERY-Gave
     _TblList          = "SkoTex.Gavekort"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 1.24 , 93 )
*/  /* QUERY QUERY-Gave */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Lev
/* Query rebuild information for QUERY QUERY-Lev
     _TblList          = "SkoTex.LevBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 1.24 , 83 )
*/  /* QUERY QUERY-Lev */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Tilg
/* Query rebuild information for QUERY QUERY-Tilg
     _TblList          = "SkoTex.Tilgode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 1.24 , 103 )
*/  /* QUERY QUERY-Tilg */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Grid ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 7.05
       COLUMN          = 2.2
       HEIGHT          = 20.71
       WIDTH           = 141
       HIDDEN          = no
       SENSITIVE       = yes.
/* Grid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Grid:MOVE-AFTER(FRAME FRAME-Art:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registeroppslag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registeroppslag */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Registeroppslag */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
            {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-Lev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-Lev C-Win
ON ENTRY OF FRAME FRAME-Lev
DO:
    APPLY "ENTRY" TO FI-Levnavn.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Art
&Scoped-define SELF-NAME B-AktiverArt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverArt C-Win
ON CHOOSE OF B-AktiverArt IN FRAME FRAME-Art /* Aktiver */
DO:
    APPLY "RETURN" TO FI-DatoArt.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverArt C-Win
ON TAB OF B-AktiverArt IN FRAME FRAME-Art /* Aktiver */
DO:
/*   APPLY "ENTRY" TO FI-Levnavn. */
/*   RETURN NO-APPLY.             */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Gavekort
&Scoped-define SELF-NAME B-AktiverGave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverGave C-Win
ON CHOOSE OF B-AktiverGave IN FRAME FRAME-Gavekort /* Aktiver */
DO:
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReplaceStr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEgneStr      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatoStr      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBrDatoStr    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBelopStr     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIdentStr     AS CHARACTER  NO-UNDO.
  RUN KontrollerInput ("GAVE").
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  DO WITH FRAME FRAME-Gavekort:
      ASSIGN chGrid:Rows = iFirstDataRow.

      ASSIGN cQry = "FOR EACH Gavekort NO-LOCK XREPLACE BY Gavekort.butnr BY Gavekort.Dato". 
      IF FI-GaveIdentNr <> "" THEN DO:
          cIdentStr = "GaveKort.IdentNr " + (IF NUM-ENTRIES(FI-GaveIdentNr,"*") = 1 THEN "BEGINS " ELSE "MATCHES ") + "'" + FI-GaveIdentNr + "'".
      END.
      IF CB-GaveEgne <> "1" THEN DO:
          cEgneStr = "Gavekort.Eget = " + STRING(CB-GaveEgne = "2","TRUE/FALSE").
      END.
      IF FI-GaveDatoFra <> ? OR FI-GaveDatoTil <> ? THEN DO:
          cDatoStr = IF FI-GaveDatoTil = ? THEN "GaveKort.Dato >= DATE('" + STRING(FI-GaveDatoFra) + "')" ELSE
                     IF FI-GaveDatoFra = ? THEN "GaveKort.Dato <= DATE('" + STRING(FI-GaveDatoTil) + "')" ELSE
                        "GaveKort.Dato >= DATE('" + STRING(FI-GaveDatoFra) + "') AND GaveKort.Dato <= DATE('" + STRING(FI-GaveDatoTil) + "')".
      END.
      IF FI-GaveBruktDatoFra <> ? OR FI-GaveBruktDatoTil <> ? THEN DO:
          cBrDatoStr = IF FI-GaveBruktDatoTil = ? THEN "GaveKort.BruktDato >= DATE('" + STRING(FI-GaveBruktDatoFra) + "')" ELSE
                       IF FI-GaveBruktDatoFra = ? THEN "GaveKort.BruktDato <= DATE('" + STRING(FI-GaveBruktDatoTil) + "')" ELSE
                        "GaveKort.BruktDato >= DATE('" + STRING(FI-GaveBruktDatoFra) + "') AND GaveKort.BruktDato <= DATE('" + STRING(FI-GaveBruktDatoTil) + "')".
      END.
      IF FI-GaveBelopFra <> 0 OR FI-GaveBelopTil <> 0 THEN DO:
          cBelopStr = IF FI-GaveBelopTil = 0 THEN "GaveKort.Belop >= DECI('" + STRING(FI-GaveBelopFra) + "')" ELSE
                      IF FI-GaveBelopFra = 0 THEN "GaveKort.Belop <= DECI('" + STRING(FI-GaveBelopTil) + "')" ELSE
                          "GaveKort.Belop >= DECI('" + STRING(FI-GaveBelopFra) + "') AND GaveKort.Belop <= DECI('" + STRING(FI-GaveBelopTil) + "')".
      END.
      IF cEgneStr <> "" OR cIdentStr <> "" OR cDatoStr <> "" OR cBrDatoStr <> "" OR cBelopStr <> "" THEN DO:
          IF cEgneStr <> "" THEN
              cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cEgneStr.
          IF cIdentStr <> "" THEN
              cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cIdentStr.
          IF cDatoStr <> "" THEN
              ASSIGN cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cDatoStr.
          IF cBrDatoStr <> "" THEN
              ASSIGN cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cBrDatoStr.
          IF cBelopStr <> "" THEN
              ASSIGN cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cBelopStr.
      END.
      ASSIGN cQry = REPLACE(cQry,"XREPLACE",cReplaceStr).
      QUERY QUERY-Gave:QUERY-PREPARE(cQry).
  END.
  {sww.i}
  RUN FillGaveTT.
  {swn.i}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverGave C-Win
ON TAB OF B-AktiverGave IN FRAME FRAME-Gavekort /* Aktiver */
DO:
  APPLY "ENTRY" TO FI-Gavebutnr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-AktiverLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverLager C-Win
ON CHOOSE OF B-AktiverLager IN FRAME FRAME-Lager /* Aktiver */
DO:
    APPLY "RETURN" TO FI-StrekKodeLager.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverLager C-Win
ON TAB OF B-AktiverLager IN FRAME FRAME-Lager /* Aktiver */
DO:
/*   APPLY "ENTRY" TO FI-Levnavn. */
/*   RETURN NO-APPLY.             */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lev
&Scoped-define SELF-NAME B-AktiverLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverLev C-Win
ON CHOOSE OF B-AktiverLev IN FRAME FRAME-Lev /* Aktiver */
DO:
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReplaceStr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStr        AS CHARACTER  NO-UNDO.
  ASSIGN chGrid:Rows = iFirstDataRow.
  ASSIGN cQry = "FOR EACH LevBas NO-LOCK XREPLACE BY Levnamn BY LevNr"
         cStr = TRIM(FI-Levnavn:SCREEN-VALUE)
         cStr = TRIM(cStr,"'")
         FI-Levnavn:SCREEN-VALUE = cStr.
  IF cStr <> "" AND cStr <> "*" THEN
      ASSIGN cReplaceStr = "WHERE LevBas.levnamn " + IF NUM-ENTRIES(cStr,"*") = 1 THEN "BEGINS '" + cStr + "'" ELSE "MATCHES '" + cStr + "'".
  ASSIGN cQry = REPLACE(cQry,"XREPLACE",cReplaceStr).
  QUERY QUERY-Lev:QUERY-PREPARE(cQry).
  {sww.i}
  RUN FillLevTT.
  {swn.i}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverLev C-Win
ON TAB OF B-AktiverLev IN FRAME FRAME-Lev /* Aktiver */
DO:
  APPLY "ENTRY" TO FI-Levnavn.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Tilgode
&Scoped-define SELF-NAME B-AktiverTilg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverTilg C-Win
ON CHOOSE OF B-AktiverTilg IN FRAME FRAME-Tilgode /* Aktiver */
DO:
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReplaceStr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEgneStr      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatoStr      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBrDatoStr    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBelopStr     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIdentStr     AS CHARACTER  NO-UNDO.
  RUN KontrollerInput ("TILG").
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  DO WITH FRAME FRAME-Tilgode:
      ASSIGN chGrid:Rows = iFirstDataRow.

      ASSIGN cQry = "FOR EACH Tilgode NO-LOCK XREPLACE BY Tilgode.butnr BY Tilgode.Dato".
      IF FI-TilgIdentNr <> "" THEN DO:
          cIdentStr = "Tilgode.IdentNr " + (IF NUM-ENTRIES(FI-TilgIdentNr,"*") = 1 THEN "BEGINS " ELSE "MATCHES ") + "'" + FI-TilgIdentNr + "'".
      END.
      IF CB-TilgEgne <> "1" THEN DO:
          cEgneStr = "Tilgode.Eget = " + STRING(CB-TilgEgne = "2","TRUE/FALSE").
      END.
      IF FI-TilgDatoFra <> ? OR FI-TilgDatoTil <> ? THEN DO:
          cDatoStr = IF FI-TilgDatoTil = ? THEN "Tilgode.Dato >= DATE('" + STRING(FI-TilgDatoFra) + "')" ELSE
                     IF FI-TilgDatoFra = ? THEN "Tilgode.Dato <= DATE('" + STRING(FI-TilgDatoTil) + "')" ELSE
                        "Tilgode.Dato >= DATE('" + STRING(FI-TilgDatoFra) + "') AND Tilgode.Dato <= DATE('" + STRING(FI-TilgDatoTil) + "')".
      END.
      IF FI-TilgBruktDatoFra <> ? OR FI-TilgBruktDatoTil <> ? THEN DO:
          cBrDatoStr = IF FI-TilgBruktDatoTil = ? THEN "Tilgode.BruktDato >= DATE('" + STRING(FI-TilgBruktDatoFra) + "')" ELSE
                       IF FI-TilgBruktDatoFra = ? THEN "Tilgode.BruktDato <= DATE('" + STRING(FI-TilgBruktDatoTil) + "')" ELSE
                        "Tilgode.BruktDato >= DATE('" + STRING(FI-TilgBruktDatoFra) + "') AND Tilgode.BruktDato <= DATE('" + STRING(FI-TilgBruktDatoTil) + "')".
      END.
      IF FI-TilgBelopFra <> 0 OR FI-TilgBelopTil <> 0 THEN DO:
          cBelopStr = IF FI-TilgBelopTil = 0 THEN "Tilgode.Belop >= DECI('" + STRING(FI-TilgBelopFra) + "')" ELSE
                      IF FI-TilgBelopFra = 0 THEN "Tilgode.Belop <= DECI('" + STRING(FI-TilgBelopTil) + "')" ELSE
                          "Tilgode.Belop >= DECI('" + STRING(FI-TilgBelopFra) + "') AND Tilgode.Belop <= DECI('" + STRING(FI-TilgBelopTil) + "')".
      END.
      IF cEgneStr <> "" OR cIdentStr <> "" OR cDatoStr <> "" OR cBrDatoStr <> "" OR cBelopStr <> "" THEN DO:
          IF cEgneStr <> "" THEN
              cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cEgneStr.
          IF cIdentStr <> "" THEN
              cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cIdentStr.
          IF cDatoStr <> "" THEN
              ASSIGN cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cDatoStr.
          IF cBrDatoStr <> "" THEN
              ASSIGN cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cBrDatoStr.
          IF cBelopStr <> "" THEN
              ASSIGN cReplaceStr = cReplaceStr + (IF cReplaceStr = "" THEN "WHERE " ELSE (" AND ")) + cBelopStr.
      END.
      ASSIGN cQry = REPLACE(cQry,"XREPLACE",cReplaceStr).
      QUERY QUERY-Tilg:QUERY-PREPARE(cQry).
  END.
  {sww.i}
  RUN FillTilgTT.
  {swn.i}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AktiverTilg C-Win
ON TAB OF B-AktiverTilg IN FRAME FRAME-Tilgode /* Aktiver */
DO:
  APPLY "ENTRY" TO FI-Tilgbutnr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Art
&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok C-Win
ON CHOOSE OF B-Sok IN FRAME FRAME-Art /* Søk artikkel */
DO:
    DEFINE VARIABLE dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
/*     IF CB-Register <> 1 THEN              */
/*         RETURN NO-APPLY.                  */
/*     run d-hsok.w (output dArtikkelNr,""). */
/*     IF NOT dArtikkelNr = ? THEN           */
/*         RUN FillArtTT (dArtikkelNr).      */
    RUN ALTS.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok C-Win
ON TAB OF B-Sok IN FRAME FRAME-Art /* Søk artikkel */
DO:
  APPLY "ENTRY" TO FI-StrekKode.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME B-SokLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLager C-Win
ON CHOOSE OF B-SokLager IN FRAME FRAME-Lager /* Søk artikkel */
DO:
    DEFINE VARIABLE dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    RUN ALTS.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLager C-Win
ON TAB OF B-SokLager IN FRAME FRAME-Lager /* Søk artikkel */
DO:
  APPLY "ENTRY" TO FI-StrekKodeLager.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Ok */
DO:
/*   IF BUTTON-Lagra:SENSITIVE IN FRAME {&FRAME-NAME} = YES THEN DO:    */
/*       MESSAGE "Vill du lagra innan du avslutar"                      */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL         */
/*                     TITLE "" UPDATE choice AS LOGICAL.               */
/*       CASE choice:                                                   */
/*          WHEN TRUE THEN /* Yes */                                    */
/*               APPLY "CHOOSE" TO BUTTON-Lagra IN FRAME {&FRAME-NAME}. */
/*          WHEN ? THEN /* No */                                        */
/*              RETURN NO-APPLY.                                        */
/*       END CASE.                                                      */
/*   END.                                                               */
/*   RETURN NO-APPLY.                                                   */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {WinHlp.i}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Art
&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-Win
ON CHOOSE OF BUTTON-Next IN FRAME FRAME-Art /* Neste */
DO:
    ASSIGN INPUT FI-DatoArt.
    IF FI-DatoArt <> ? THEN DO:
        IF NOT FI-StrekKode:SCREEN-VALUE = "" THEN
            APPLY "RETURN" TO FI-StrekKode.
        ASSIGN FI-DatoArt = FI-DatoArt + 1
               FI-DatoArt:SCREEN-VALUE = STRING(FI-DatoArt).
        IF FI-ArtNr:SCREEN-VALUE <> "0" THEN
            APPLY "RETURN" TO FI-DatoArt.
    END.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-Win
ON CHOOSE OF BUTTON-Prev IN FRAME FRAME-Art /* Forrige */
DO:
    ASSIGN INPUT FI-DatoArt.
    IF FI-DatoArt <> ? THEN DO:
        IF NOT FI-StrekKode:SCREEN-VALUE = "" THEN
            APPLY "RETURN" TO FI-StrekKode.
        ASSIGN FI-DatoArt = FI-DatoArt - 1
               FI-DatoArt:SCREEN-VALUE = STRING(FI-DatoArt).
        IF FI-ArtNr:SCREEN-VALUE <> "0" THEN
            APPLY "RETURN" TO FI-DatoArt.
    END.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME CB-Register
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Register C-Win
ON VALUE-CHANGED OF CB-Register IN FRAME DEFAULT-FRAME /* Register */
DO:
  IF CB-Register <> INPUT CB-Register THEN
      RUN ClearFelter (CB-Register).
  ASSIGN INPUT CB-Register.
  hAktivFrame[CB-Register]:MOVE-TO-TOP().
  APPLY "ENTRY" TO hAktivFrame[CB-Register].
  ASSIGN chGrid:Rows = iFirstDataRow
         chGrid:Cols = 10
         chGrid:Cell(flexcpText, 3, mCol,3,mCol) = getType(CB-Register).
/*   ASSIGN FI-ArtNr:SCREEN-VALUE IN FRAME FRAME-Art = "0" */
/*          FI-StrekKode:SCREEN-VALUE = "".                */
  RELEASE ArtBas.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Art
&Scoped-define SELF-NAME FI-DatoArt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DatoArt C-Win
ON + OF FI-DatoArt IN FRAME FRAME-Art /* Transdato */
DO:
  APPLY "CHOOSE" TO BUTTON-Next.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DatoArt C-Win
ON - OF FI-DatoArt IN FRAME FRAME-Art /* Transdato */
DO:
  APPLY "CHOOSE" TO BUTTON-Prev.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DatoArt C-Win
ON RETURN OF FI-DatoArt IN FRAME FRAME-Art /* Transdato */
DO:
    IF FI-ArtNr:SCREEN-VALUE <> "0" THEN
        FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(FI-ArtNr:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN DO:
        {sww.i}
        RUN FillArtTT (ArtBas.ArtikkelNr).
       {swn.i}
    END.
    ELSE DO:
        MESSAGE "Finner ikke artikkelen."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    
    RELEASE StrekKode.
    RELEASE ArtBas.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lev
&Scoped-define SELF-NAME FI-Levnavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Levnavn C-Win
ON RETURN OF FI-Levnavn IN FRAME FRAME-Lev /* Levnavn */
DO:
  APPLY "CHOOSE" TO B-AktiverLev.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Art
&Scoped-define SELF-NAME FI-StrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode C-Win
ON F10 OF FI-StrekKode IN FRAME FRAME-Art /* StrekKode */
DO:
  APPLY "RETURN" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode C-Win
ON LEAVE OF FI-StrekKode IN FRAME FRAME-Art /* StrekKode */
DO:
  IF NOT SELF:SCREEN-VALUE = "" THEN
      APPLY "RETURN" TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode C-Win
ON RETURN OF FI-StrekKode IN FRAME FRAME-Art /* StrekKode */
DO:
    DEFINE VARIABLE dTst AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cStrekKode AS CHARACTER  NO-UNDO.
    ASSIGN cStrekkode = TRIM(FI-StrekKode:SCREEN-VALUE).
    IF cStrekKode = "" AND FI-ArtNr:SCREEN-VALUE = "0" THEN
        RETURN.
    ASSIGN dTst = DECI(cStrekKode) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        FI-StrekKode:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
    IF LENGTH(cStrekKode) > 8 AND LENGTH(cStrekKode) <> 13 THEN
        ASSIGN cStrekKode = FILL("0",13 - LENGTH(cStrekKode)) + cStrekKode.
    FIND Strekkode WHERE StrekKode.Kode = cStrekKode NO-LOCK NO-ERROR.
    IF AVAIL StrekKode THEN DO:
        ASSIGN FI-StrekKode:SCREEN-VALUE = ""
               FI-ArtNr:SCREEN-VALUE = STRING(StrekKode.Artikkelnr).
    END.
    ELSE DO:
        MESSAGE "Finner ikke artikkelen"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN FI-StrekKode:SCREEN-VALUE = ""
               FI-ArtNr:SCREEN-VALUE = "0".
        RETURN NO-APPLY.
    END.
    APPLY "TAB" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode C-Win
ON TAB OF FI-StrekKode IN FRAME FRAME-Art /* StrekKode */
DO:
  APPLY "RETURN" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Lager
&Scoped-define SELF-NAME FI-StrekKodeLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKodeLager C-Win
ON F10 OF FI-StrekKodeLager IN FRAME FRAME-Lager /* StrekKode */
DO:
    APPLY "RETURN" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKodeLager C-Win
ON RETURN OF FI-StrekKodeLager IN FRAME FRAME-Lager /* StrekKode */
DO:
  DEFINE VARIABLE dTst AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cStrekKode AS CHARACTER  NO-UNDO.
  ASSIGN cStrekkode = TRIM(FI-StrekKodeLager:SCREEN-VALUE).
  IF cStrekKode = "" THEN
      RETURN NO-APPLY.
  ASSIGN dTst = DECI(cStrekKode) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
      FI-StrekKodeLager:SCREEN-VALUE = "".
      RETURN NO-APPLY.
  END.
  IF LENGTH(cStrekKode) > 8 AND LENGTH(cStrekKode) <> 13 THEN
      ASSIGN cStrekKode = FILL("0",13 - LENGTH(cStrekKode)) + cStrekKode.
  FIND Strekkode WHERE StrekKode.Kode = cStrekKode NO-LOCK NO-ERROR.
  IF AVAIL StrekKode THEN
      {sww.i}
      RUN FillLagerTT (StrekKode.ArtikkelNr).
     {swn.i}
  ELSE DO:
      MESSAGE "Finner ikke artikkelen."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN FI-StrekKodeLager:SCREEN-VALUE = "".
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Grid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.BeforeEdit
PROCEDURE Grid.VSFlexGrid.BeforeEdit .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Row
    Col
    Cancel
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT        PARAMETER p-Row    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Col    AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-Cancel AS LOGICAL NO-UNDO.

/*    ' show popup menu on the date */
/*     If p-Row = 3 And p-Col = 8 THEN       */
/*         ASSIGN chGrid:ComboList = "...". */
/*                                           */
/* /*    ' other cells are not editable */   */
/*     ELSE                                  */
/*         ASSIGN chGrid:ComboList = "" */
/*                p-Cancel = True.      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.BeforeScrollTip
PROCEDURE Grid.VSFlexGrid.BeforeScrollTip .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Row
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Row AS INTEGER NO-UNDO.

  IF p-Row < 3 Or chGrid:Cell(flexcpTextDisplay, p-Row, 3) = "" THEN
      RETURN.
  chGrid:ScrollTipText = " " + 
                          TRIM(chGrid:Cell(flexcpTextDisplay, p-Row, 3)) + 
                          " ".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.CellButtonClick
PROCEDURE Grid.VSFlexGrid.CellButtonClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Row
    Col
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Row AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Col AS INTEGER NO-UNDO.

/* ' show a message when user clicks on the ellipsis button */
    MESSAGE  "The next update is scheduled for March 12, 2002" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.Click
PROCEDURE Grid.VSFlexGrid.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iGridRow AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lVis AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cExtra AS CHARACTER  NO-UNDO.
    IF chGrid:COL = 5 AND CB-Register = 1 THEN DO:
        ASSIGN iGridRow =  chGrid:ROW.
        FIND TT_Grid WHERE TT_Grid.iGridRow = iGridRow NO-ERROR.
        IF AVAIL TT_Grid AND ENTRY(1,TT_Grid.cExtraData,CHR(1)) = "BONG" THEN DO:
            MESSAGE "Vis bong? "
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVis.
            IF lVis THEN DO:
                ASSIGN cExtra = ENTRY(2,TT_Grid.cExtraData,CHR(1)).
                RUN gviskvittokopi2.w (INT(ENTRY(1,cExtra)),
                                       INT(ENTRY(2,cExtra)),
                                       INT(ENTRY(3,cExtra)),
                                       DATE(ENTRY(4,cExtra)),
                                       INT(ENTRY(5,cExtra)),
                                       ?).
            END.
        END.
    END.
    ELSE IF chGrid:COL = 5 AND CB-Register = 3 THEN DO:
        ASSIGN iGridRow =  chGrid:ROW.
        FIND TT_Grid WHERE TT_Grid.iGridRow = iGridRow NO-ERROR.
        IF AVAIL TT_Grid AND ENTRY(1,TT_Grid.cExtraData,CHR(1)) = "BONG" THEN DO:
            FIND BongHode WHERE BongHode.b_id = DECI(ENTRY(2,TT_Grid.cExtraData,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL BongHode THEN DO:
                MESSAGE "Vis bong? "
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVis.
                IF lVis THEN
                    RUN gviskvittokopi2.w (BongHode.ButikkNr,1,BongHode.KasseNr,BongHode.Dato,BongHode.BongNr,?).
            END.
        END.
    END.
    ELSE IF chGrid:COL = 3 AND CAN-DO("1,5",STRING(CB-Register)) THEN DO:
        ASSIGN iGridRow =  chGrid:ROW.
        FIND TT_Grid WHERE TT_Grid.iGridRow = iGridRow NO-ERROR.
        IF AVAIL TT_Grid AND ENTRY(1,TT_Grid.cExtraData,CHR(1)) = "ARTBAS" THEN DO:
            FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(ENTRY(2,TT_Grid.cExtraData,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                MESSAGE "Vis artikkelkort? "
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVis.
                IF lVis THEN
                    RUN w-vArtkor.w (RECID(ArtBas),"ENDRE").
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.ComboCloseUp
PROCEDURE Grid.VSFlexGrid.ComboCloseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Row
    Col
    FinishEdit
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT        PARAMETER p-Row        AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Col        AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-FinishEdit AS LOGICAL NO-UNDO.
    IF CAN-DO(REPLACE(cGridModell,"|",","),ENTRY(2,chGrid:ComboItem)) THEN DO:
        APPLY "VALUE-CHANGED" TO CB-Register IN FRAME DEFAULT-FRAME.
        IF CB-Register = 3 THEN
            RUN FillArtTT(DECI(ENTRY(2,chGrid:ComboItem))).
        IF CB-Register = 2 THEN
            RUN FillLagerTT(DECI(ENTRY(2,chGrid:ComboItem))).
    END.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.DblClick
PROCEDURE Grid.VSFlexGrid.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
/*    ' allow collapsing/expanding with double-clicks */
    IF chGrid:ROW < iFirstDataRow THEN
        RETURN.
    IF chGrid:IsCollapsed(chGrid:Row) = flexOutlineCollapsed THEN
        chGrid:IsCollapsed(chGrid:Row) = flexOutlineExpanded.
    ELSE
        chGrid:IsCollapsed(chGrid:Row) = flexOutlineCollapsed.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.EnterCell
PROCEDURE Grid.VSFlexGrid.EnterCell .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
   chGrid:Editable =  
       (iModellRow > 0 AND cGridModell <> "" AND iModellRow = chGrid:ROW AND iModellCol = chGrid:COL).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.KeyDown
PROCEDURE Grid.VSFlexGrid.KeyDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.KeyUp
PROCEDURE Grid.VSFlexGrid.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.
    IF p-KeyCode = 9 THEN DO:
    /*     APPLY "ENTRY" TO hAktivFrame[CB-Register]. */
        CASE CB-Register:
            WHEN 1 THEN
                APPLY "ENTRY" TO FI-StrekKode IN FRAME FRAME-Art.
            WHEN 2 THEN
                APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.
            WHEN 3 THEN
                APPLY "ENTRY" TO FI-Gavebutnr IN FRAME FRAME-Gavekort.
            WHEN 4 THEN
                APPLY "ENTRY" TO FI-Tilgbutnr IN FRAME FRAME-Tilgode.
            WHEN 5 THEN
                APPLY "ENTRY" TO FI-StrekKodeLager IN FRAME FRAME-Lager.
        END CASE.
        RETURN NO-APPLY.
    END.
    ELSE IF p-KeyCode = 65 AND p-Shift = 4 THEN
        RUN AktiverSok.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.MouseMove
PROCEDURE Grid.VSFlexGrid.MouseMove .
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

DEFINE VAR i AS INTE NO-UNDO.

/* /*    ' avoid extra work */                                                                    */
/*     If chGrid:MouseRow /* = r */ < 3 OR chGrid:TextMatrix(chGrid:MouseRow,chGrid:MouseCol) = ? */
/*         THEN RETURN.                                                                           */
/*     r = chGrid:MouseRow.                                                                       */
/* /*    ' move selection (even with no click) */                                                 */
/*     chGrid:Select(r,8).                                                                        */
/*                                                                                                */
/* /*    ' remove cursor image                                                                    */
/* '    chGrid:Cell(flexcpPicture, 4, 7, chGrid:Rows - 1) = imgClear                              */
/* '    chGrid:Cell(flexcpPicture, 4, 9, chGrid:Rows - 1) = imgClear */                           */
/*                                                                                                */
/* /*    ' show cursor image if there is some text in column 5 */                                 */
/*     If r < 4 Or chGrid:Cell(flexcpText, r, 8) = "" THEN                                        */
/*        RETURN.                                                                                 */
/* /* '    chGrid:Cell(flexcpPicture, r, 7) = Image3.value                                        */
/* '    chGrid:Cell(flexcpPicture, r, 9) = Image1.Picture */                                      */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win
ON TAB OF Grid /* VSFlexGrid */
DO:
  APPLY "ENTRY" TO hAktivFrame[CB-Register].
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS
       iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN disable_UI.
    IF VALID-HANDLE(chGrid) THEN
        RELEASE OBJECT chGrid NO-ERROR.
    IF VALID-HANDLE(Grid) THEN
        DELETE OBJECT Grid NO-ERROR.
    ASSIGN chGrid      = ?
           Grid         = ?.
END.
ON 'ALT-A' OF {&WINDOW-NAME} ANYWHERE
DO:
    RUN AktiverSok.
END.
ON 'ALT-S' OF {&WINDOW-NAME} ANYWHERE
DO:
    IF CAN-DO("2,3",STRING(CB-Register)) THEN
        RUN ALTS.
    RETURN NO-APPLY.
END.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {syspara.i 1 1 100 cInstbutikk}
    ASSIGN hAktivFrame[1] = FRAME FRAME-Lev:HANDLE
           hAktivFrame[2] = FRAME FRAME-Lager:HANDLE
           hAktivFrame[3] = FRAME FRAME-Art:HANDLE
           hAktivFrame[4] = FRAME FRAME-Gavekort:HANDLE
           hAktivFrame[5] = FRAME FRAME-Tilgode:HANDLE.
/* 
 Så här skall det se ut när vi har klart med Gavekort och tillgode
 De finns inte med i cOMBON
 Leverandører,1,
Artikkel/Lager,2,
Artikkel/Transer,3,
Gavekort,4,
Tilgodelapper,5

 */


   ASSIGN CB-Register = 1.
  RUN enable_UI.
  {lng.i} /* Oversettelse */
  RUN SetDivResize.
  APPLY "VALUE-CHANGED" TO CB-Register IN FRAME {&FRAME-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverSok C-Win 
PROCEDURE AktiverSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE CB-Register:
        WHEN 1 THEN DO:
            APPLY "CHOOSE" TO B-AktiverLev IN FRAME FRAME-Lev.
        END.
        WHEN 2 THEN DO:
            APPLY "CHOOSE" TO B-AktiverLager IN FRAME FRAME-Lager.
        END.
        WHEN 3 THEN DO:
            APPLY "CHOOSE" TO B-AktiverArt IN FRAME FRAME-Art.
        END.
        WHEN 4 THEN DO:
            APPLY "CHOOSE" TO B-AktiverGave IN FRAME FRAME-Gavekort.
        END.
        WHEN 5 THEN DO:
            APPLY "CHOOSE" TO B-AktiverTilg IN FRAME FRAME-Tilgode.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ALTS C-Win 
PROCEDURE ALTS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
/*     IF CAN-DO("2,3",string(CB-Register)) THEN DO: */
        run d-hsok.w (output dArtikkelNr,"").
        IF NOT dArtikkelNr = ? THEN DO:
/*             APPLY "VALUE-CHANGED" TO CB-Register IN FRAME DEFAULT-FRAME. */
            IF CB-Register = 3 THEN DO:
                ASSIGN FI-ArtNr:SCREEN-VALUE IN FRAME FRAME-Art = STRING(dArtikkelnr).
                APPLY "CHOOSE" TO B-AktiverArt IN FRAME FRAME-Art.
                APPLY "ENTRY" TO FI-DatoArt IN FRAME FRAME-Art.
            END.
            ELSE IF CB-Register = 2 THEN
                RUN FillLagerTT (dArtikkelNr).
        END.
/*     END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFelter C-Win 
PROCEDURE ClearFelter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iFrame AS INTEGER    NO-UNDO.
  CASE iFrame:
      WHEN 1 THEN
          ASSIGN FI-Levnavn:SCREEN-VALUE IN FRAME FRAME-Lev = "" NO-ERROR.
      WHEN 2 THEN
          ASSIGN FI-StrekKodeLager:SCREEN-VALUE IN FRAME FRAME-Lager = "" NO-ERROR.
      WHEN 3 THEN DO WITH FRAME FRAME-Art:
          ASSIGN FI-StrekKode:SCREEN-VALUE = "" 
                 FI-ArtNr:SCREEN-VALUE = "0"
                 FI-DatoArt:SCREEN-VALUE = ""
                 NO-ERROR.
      END.
  END CASE.
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

OCXFile = SEARCH( "w-Registeroppslag.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chGrid = Grid:COM-HANDLE
    UIB_S = chGrid:LoadControls( OCXFile, "Grid":U)
    Grid:NAME = "Grid":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-Registeroppslag.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTT_Grid C-Win 
PROCEDURE CreateTT_Grid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipiRow          AS INTEGER    NO-UNDO.
   DEFINE INPUT  PARAMETER ipiOutline      AS INTEGER    NO-UNDO.
   DEFINE INPUT  PARAMETER ipiFirstDataCol AS INTEGER    NO-UNDO.
   DEFINE INPUT  PARAMETER ipcText        AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER ipcEkstraData    AS CHARACTER  NO-UNDO.
             CREATE TT_Grid.
             ASSIGN TT_Grid.iGridRow   = ipiRow
                    TT_Grid.iOutline   = ipiOutline
                    TT_Grid.iGridCol   = ipiFirstDataCol /* + 1 */
                    TT_Grid.cText      = ipcText
                    TT_Grid.cExtraData = ipcEkstraData
                    .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoGroup C-Win 
PROCEDURE DoGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER Row AS INTE NO-UNDO.
DEFINE INPUT PARAMETER lvl AS INTE NO-UNDO.
    /* set the row as a group */
    ASSIGN
    chGrid:IsSubtotal(Row) = True
    
    /* set the indentation level of the group */
    chGrid:RowOutlineLevel(Row) = lvl.
    
    IF lvl = 0 THEN
        RETURN.
    
    IF lvl = 1 THEN
      ASSIGN
/*         chGrid:IsCollapsed(Row) = flexOutlineCollapsed */
        chGrid:Cell(flexcpForeColor, Row, t,Row,t) = vbWhite. 
/*         chGrid:Cell(flexcpBackColor,Row,t,Row,t) = 1. */
    ASSIGN chGrid:Cell(flexcpFontBold,Row,t,Row,t) = TRUE.
    
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
  DISPLAY CB-Register 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_Done Btn_Help RECT-2 RECT-1 CB-Register 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-StrekKode FI-DatoArt FI-ArtNr 
      WITH FRAME FRAME-Art IN WINDOW C-Win.
  ENABLE BUTTON-Next FI-StrekKode FI-DatoArt B-AktiverArt B-Sok BUTTON-Prev 
      WITH FRAME FRAME-Art IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Art}
  DISPLAY FI-Gavebutnr CB-GaveEgne FI-GaveDatoFra FI-GaveDatoTil 
          FI-GaveBruktdatoFra FI-GaveBruktdatoTil FI-GaveIdentNr FI-GaveBelopFra 
          FI-GaveBelopTil 
      WITH FRAME FRAME-Gavekort IN WINDOW C-Win.
  ENABLE FI-Gavebutnr CB-GaveEgne FI-GaveDatoFra FI-GaveDatoTil 
         FI-GaveBruktdatoFra FI-GaveBruktdatoTil FI-GaveIdentNr FI-GaveBelopFra 
         FI-GaveBelopTil B-AktiverGave 
      WITH FRAME FRAME-Gavekort IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Gavekort}
  DISPLAY FI-StrekKodeLager 
      WITH FRAME FRAME-Lager IN WINDOW C-Win.
  ENABLE FI-StrekKodeLager B-AktiverLager B-SokLager 
      WITH FRAME FRAME-Lager IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Lager}
  DISPLAY FI-Levnavn 
      WITH FRAME FRAME-Lev IN WINDOW C-Win.
  ENABLE FI-Levnavn B-AktiverLev 
      WITH FRAME FRAME-Lev IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Lev}
  DISPLAY FI-Tilgbutnr CB-TilgEgne FI-TilgDatoFra FI-TilgDatoTil 
          FI-TilgBruktdatoFra FI-TilgBruktdatoTil FI-TilgIdentNr FI-TilgBelopFra 
          FI-TilgBelopTil 
      WITH FRAME FRAME-Tilgode IN WINDOW C-Win.
  ENABLE FI-Tilgbutnr CB-TilgEgne FI-TilgDatoFra FI-TilgDatoTil 
         FI-TilgBruktdatoFra FI-TilgBruktdatoTil FI-TilgIdentNr FI-TilgBelopFra 
         FI-TilgBelopTil B-AktiverTilg 
      WITH FRAME FRAME-Tilgode IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Tilgode}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillArtTT C-Win 
PROCEDURE FillArtTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr    NO-UNDO.
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount2 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFill AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cPara AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeftAlignCols AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iErstattId     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dtTst AS DATE       NO-UNDO.
  DEFINE BUFFER bArtBas FOR ArtBas.
  ASSIGN iRow = iFirstDataRow.
  FIND ArtBas WHERE ArtBas.Artikkelnr = dArtikkelNr NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.
  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType OF ArtBas NO-LOCK NO-ERROR.
  FIND Sasong OF Artbas NO-LOCK NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  
  EMPTY TEMP-TABLE TT_Grid.
  EMPTY TEMP-TABLE TT_Trans.
  ASSIGN chGrid:Rows = iFirstDataRow
         chGrid:Cols = t - 1.
  ASSIGN chGrid:Cols = 10.
  DO WITH FRAME FRAME-Art:
      ASSIGN dtTst = DATE(FI-DatoArt:SCREEN-VALUE) NO-ERROR.
      IF AVAIL ArtBas AND dtTst = ? THEN DO:
          FIND LAST TransLogg WHERE Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND Translogg.TTId = 1
            USE-INDEX OppslagDatoTid NO-LOCK NO-ERROR.
          IF AVAIL TransLogg THEN
              ASSIGN FI-DatoArt:SCREEN-VALUE = STRING(TransLogg.Dato).
      END.
      IF AVAIL ArtBas AND CAN-FIND(FIRST Translogg WHERE 
                           Translogg.artikkelnr = artbas.artikkelnr AND Translogg.dato = INPUT FI-DatoArt) THEN
          RUN FillTrans (ArtBas.Artikkelnr,INPUT INPUT FI-DatoArt).
  END.

/* iFirstDataCol */

/* DEFINE TEMP-TABLE TT_Grid        */
/*     FIELD iGridrow AS INTEGER    */
/*     FIELD iOutline AS INTEGER    */
/*     FIELD iGridCol AS INTEGER    */
/*     FIELD cText    AS CHARACTER. */


  DO:
      ASSIGN cGridModell = ""
             cTekst      = sp + STRING(ArtBas.Artikkelnr) + " - " + TRIM(Artbas.Beskr).
      RUN CreateTT_Grid(iRow,2,iFirstDataCol,cTekst,"ARTBAS" + CHR(1) + STRING(ArtBas.ArtikkelNr)).
      IF ArtBas.ModellFarge <> 0 THEN DO:
          ASSIGN iModellRow = iRow
                 iModellCol = iFirstDataCol + 1
                 cGridModell = "".
          RUN GetModell(ArtBas.Artikkelnr,OUTPUT cGridModell).
      END.
      ELSE
          ASSIGN iModellRow = 0
                 iModellCol = 0.

      ASSIGN iRow     = iRow + 1.
/*       CREATE TT_Grid.                                                                                                                                                        */
/*       ASSIGN iGridRow = iRow                                                                                                                                                 */
/*              iRow     = iRow + 1                                                                                                                                             */
/*              iOutline = 2                                                                                                                                                    */
/*              iGridCol = iFirstDataCol                                                                                                                                        */
/* /*              iFill = IF LENGTH(STRING(LevBas.Levnr)) = 1 THEN 7 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 2 THEN 5 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 3 THEN 2 ELSE 0 */ */
/*              cText = sp + STRING(ArtBas.Artikkelnr) + " - " + TRIM(Artbas.Beskr).                                                                                            */
      
      ASSIGN cTekst = sp + "Egenskaper".
      RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
      
      IF AVAIL VarGr THEN DO:
          ASSIGN iFill = IF LENGTH(STRING(ArtBas.Vg)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.Vg)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.Vg)) = 3 THEN 2 ELSE 0
                 cTekst = sp + sp + FILL(" ",iFill) + STRING(ArtBas.Vg) + IF AVAIL VarGr THEN " " + TRIM(VarGr.VgBeskr) ELSE " -" .
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
          ASSIGN iRow     = iRow + 1.
      END.
      IF AVAIL Farg AND TRIM(Farg.FarBeskr) <> "" THEN DO:
          ASSIGN iFill  = IF LENGTH(STRING(ArtBas.Farg)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.Farg)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.Farg)) = 3 THEN 2 ELSE 0
                 cTekst = sp + sp + FILL(" ",iFill) + STRING(ArtBas.Farg) + IF AVAIL Farg THEN " " + TRIM(Farg.FarBeskr) ELSE "".
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
          ASSIGN iRow     = iRow + 1.
      END.
      
      ASSIGN iFill = IF LENGTH(STRING(ArtBas.StrType)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.StrType)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.StrType)) = 3 THEN 2 ELSE 0
             cText = sp + sp + FILL(" ",iFill) + STRING(ArtBas.StrType) + IF AVAIL StrType THEN " " + TRIM(StrType.Beskrivelse) ELSE "".
      RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
      
      IF AVAIL Sasong AND TRIM(Sasong.SasBeskr) <> "" THEN DO:
          ASSIGN iFill = IF LENGTH(STRING(ArtBas.Sasong)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.Sasong)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.Sasong)) = 3 THEN 2 ELSE 0
                 cText = sp + sp + FILL(" ",iFill) + STRING(ArtBas.Sasong) + IF AVAIL Sasong THEN " " + TRIM(Sasong.SasBeskr) ELSE "".
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
          ASSIGN iRow     = iRow + 1.
      END.
      
      ASSIGN iFill = IF LENGTH(STRING(LevBas.Levnr)) = 1 THEN 7 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 2 THEN 5 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 3 THEN 2 ELSE 0
             cText = sp + sp + FILL(" ",iFill) + STRING(ArtBas.LevNr) + IF AVAIL LevBas THEN " " + TRIM(LevBas.levnamn) ELSE "".
      RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
     FIND ArtPris OF ArtBas NO-LOCK NO-ERROR.
     IF AVAIL ArtPris OR AMBIG ArtPris THEN DO:
         ASSIGN cTekst = sp + "Kalkyle".
         RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
         ASSIGN iRow     = iRow + 1.
         
         IF AVAIL Artpris THEN DO:
             IF ArtPris.Tilbud = TRUE THEN DO:
                 ASSIGN cTekst = sp + "På tilbud".
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                 ASSIGN iRow     = iRow + 1.
                 ASSIGN cTekst = sp + sp + "Tilbud   " + STRING(ArtPris.Pris[2],">>,>>9.99") + CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr).
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                 ASSIGN iRow     = iRow + 1.
             END.
             ASSIGN cTekst = sp + sp + "Normal " + STRING(ArtPris.Pris[1],">>,>>9.99")  + 
                             IF NOT ArtPris.Tilbud THEN CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr) ELSE "".
             RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
             ASSIGN iRow     = iRow + 1.
         END.
         ELSE IF AMBIG ArtPris THEN DO:
             FOR EACH ArtPris OF ArtBas.
                 IF ArtPris.Tilbud = TRUE THEN DO:
                     ASSIGN cTekst = sp + sp + "På tilbud".
                     RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                     ASSIGN cTekst = sp + sp + "Tilbud   " + STRING(ArtPris.Pris[2],">>,>>9.99") + CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr).
                     RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                 END.
                 ASSIGN cTekst = sp + sp + "Normal " + STRING(ArtPris.Pris[1],">>,>>9.99")  + 
                                IF NOT ArtPris.Tilbud THEN CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr) ELSE "".
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                 ASSIGN iRow     = iRow + 1.
             END.
         END.
         FIND Erstattningsvare OF ArtBas NO-LOCK NO-ERROR.
         IF AVAIL Erstattningsvare THEN DO:
             ASSIGN iErstattId = Erstattningsvare.ErstattId.
             ASSIGN cTekst = sp + "Erstattningsvare".
             RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
             ASSIGN iRow     = iRow + 1.
             FOR EACH Erstattningsvare WHERE Erstattningsvare.ErstattId = iErstattId AND Erstattningsvare.ArtikkelNr <> ArtBas.ArtikkelNr NO-LOCK.
                 FIND bArtBas WHERE bArtBas.ArtikkelNr = Erstattningsvare.Artikkelnr NO-LOCK NO-ERROR.
                 IF AVAIL bArtBas THEN DO:
                     ASSIGN cTekst = sp + sp + STRING(bArtBas.Artikkelnr) + " - " + TRIM(bArtbas.Beskr).
                     RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                     FOR EACH ArtPris OF bArtBas.
                         IF ArtPris.Tilbud = TRUE THEN DO:
                             ASSIGN cTekst = sp + sp + "På tilbud".
                             RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                             ASSIGN iRow     = iRow + 1.
                             ASSIGN cTekst = sp + sp + sp + "Tilbud   " + STRING(ArtPris.Pris[2],">>,>>9.99") + CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr).
                             RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                             ASSIGN iRow     = iRow + 1.
                         END.
                         ASSIGN cTekst = sp + sp + sp + "Normal " + STRING(ArtPris.Pris[1],">>,>>9.99")  + 
                                        IF NOT ArtPris.Tilbud THEN CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr) ELSE "".
                         RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                         ASSIGN iRow     = iRow + 1.
                     END.
                 END.
             END.
         END.
         /* TRANSAKTIONER */
         IF CAN-FIND(FIRST TT_Trans) THEN DO:
             ASSIGN cTekst = sp + "Transaksjoner".
             RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
             ASSIGN iRow     = iRow + 1.
             FOR EACH TT_Trans BREAK BY TT_Trans.iButikkNr:
                 IF FIRST-OF(iButikkNr) THEN DO:
                     ASSIGN cTekst = sp + "Butikk: " + STRING(TT_Trans.iButikkNr).
                     RUN CreateTT_Grid(iRow,4,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                 END.
                 ASSIGN cTekst = sp + sp + "Pris: " + STRING(TT_Trans.dPris,"->>>,>>9.99") + CHR(1) +
                                           "Rabatt: " + STRING(TT_Trans.dRabkr,"->>>,>>9.99") + CHR(1) +
                                           "Bong: " + STRING(TT_Trans.iBongNr)  + CHR(1) + 
                                           "Selger: " + TT_Trans.cSelger.
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"BONG" + CHR(1) + TT_Trans.cBongInfo).
                 ASSIGN iRow     = iRow + 1.
             END.
         END.
/*          ASSIGN TT_Trans.iButikkNr    = Translogg.Butik                    */
/*                 TT_Trans.deAntall     = TransLogg.Antall                   */
/*                 TT_Trans.dPris        = Translogg.Pris                     */
/*                 TT_Trans.dRabkr       = Translogg.Rabkr                    */
/*                 TT_Trans.dLinjeRab    = Translogg.LinjeRab                 */
/*                 TT_Trans.dSubtotalrab = Translogg.Subtotalrab              */
/*                 TT_Trans.iBongNr      = TransLogg.Bongid                   */
/*                 TT_Trans.cBongInfo    = STRING(Translogg.Butik) + "," +    */
/*                                         STRING(Translogg.Profilnr) + "," + */
/*                                         STRING(Translogg.KassaNr) + "," +  */
/*                                         STRING(Translogg.Dato) + "," +     */
/*                                         STRING(Translogg.Bongid).          */


/*          /* PrisHist */                                                                                                                                  */
/*          IF CAN-FIND(FIRST HPrisKo OF ArtBas) THEN DO:                                                                                                   */
/*              ASSIGN cTekst = sp + "Prishistorikk".                                                                                                       */
/*              RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").                                                                                          */
/*              ASSIGN iRow     = iRow + 1.                                                                                                                 */
/*              FOR EACH HPrisKo OF ArtBas BREAK BY HPrisKo.ProfilNr BY HPrisKo.AktiveresDato DESCENDING BY HPrisKo.AktiveresTid.                           */
/*                  IF FIRST-OF(HPrisKo.ProfilNr) THEN DO:                                                                                                  */
/*                      ASSIGN cTekst = sp + sp + "Profilnr " + STRING(HPrisKo.ProfilNr).                                                                   */
/*                      RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").                                                                                  */
/*                      ASSIGN iRow     = iRow + 1.                                                                                                         */
/*                  END.                                                                                                                                    */
/*                  ASSIGN cTekst = sp + sp + STRING(HPrisKo.TilBud," Tilbud  -/ Normal-") + (IF HPrisKo.Tilbud THEN "  " ELSE "") +                        */
/*                            STRING(HPrisKo.Aktiveresdato) + " " + SUBSTR(STRING(HPrisKo.AktiveresTid,"HH:MM:SS"),1,5)+ STRING(HPrisKo.Pris,">>,>>9.99") + */
/*                            CHR(1) + HPrisKo.BrukerID.                                                                                                    */
/*                  RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").                                                                                      */
/*                  ASSIGN iRow     = iRow + 1.                                                                                                             */
/*              END.                                                                                                                                        */
/*          END.                                                                                                                                            */
     END.
  END.
  IF NOT CAN-FIND(FIRST TT_Grid) THEN DO:
      APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.
      RETURN.
  END.
  ASSIGN chGrid:Rows = iRow.
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      ASSIGN iCol = 0.
      DO iCount = 1 TO NUM-ENTRIES(TT_Grid.cText,CHR(1)):
          ASSIGN iCol = IF iCol = 0 THEN TT_Grid.iGridCol ELSE iCol + 1
                 chGrid:Cell(flexcpText,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = ENTRY(1,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
          IF NUM-ENTRIES(ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)) = 2 THEN DO:
              ASSIGN cPara = ENTRY(2,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
              IF cPara = "RALIGN" THEN
                  ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignRightBottom.
          END.

      END.
  END.
  IF cGridModell <> "" THEN DO:
      chGrid:Cell(flexcpText,iModellRow + 1,iModellCol,iModellRow + 1,iModellCol) = "MODELL".
      chGrid:Cell(flexcpText,iModellRow,iModellCol,iModellRow,iModellCol) = ENTRY(1,cGridModell,"|").
      chGrid:ComboList = cGridModell.
  END.
  RUN DoGroup(4,1).
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      IF TT_Grid.iOutline > 0 THEN
          RUN DoGroup(TT_Grid.iGridRow,TT_Grid.iOutline).
  END.
  ASSIGN
  chGrid:AutoSizeMode = flexAutoSizeColWidth.
  chGrid:AutoSize(mcol,chGrid:Cols - 2).
  /*         chGrid:AutoSize(t). */
  IF cLeftAlignCols <> "" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cLeftAlignCols,CHR(1)):
          DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cLeftAlignCols,CHR(1)))) TO INT(ENTRY(2,ENTRY(iCount,cLeftAlignCols,CHR(1)))):
              chGrid:ColAlignment(iCount2) = flexAlignLeftBottom.
          END.
      END.
  END.
  ASSIGN
  chGrid:MergeCells = flexMergeSpill.
  chGrid:ColWidth(t - 1) = 300.
  /*         chGrid:ColWidth(t + 1) = 300. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillGaveTT C-Win 
PROCEDURE FillGaveTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFill AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cPara AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iButnr AS INTEGER INIT ?   NO-UNDO.
  DEFINE VARIABLE cSelger AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBruktSelger AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEkstra AS CHARACTER  NO-UNDO.
  ASSIGN iRow = iFirstDataRow.

/* iFirstDataCol */

/* DEFINE TEMP-TABLE TT_Grid        */
/*     FIELD iGridrow AS INTEGER    */
/*     FIELD iOutline AS INTEGER    */
/*     FIELD iGridCol AS INTEGER    */
/*     FIELD cText    AS CHARACTER. */

  EMPTY TEMP-TABLE TT_Grid.

  QUERY QUERY-Gave:QUERY-OPEN().
  QUERY QUERY-Gave:GET-NEXT().
  REPEAT WHILE NOT QUERY QUERY-Gave:QUERY-OFF-END:
      IF iButNr <> GaveKort.Butnr THEN DO:
          iButnr = GaveKort.Butnr.
          FIND Butiker WHERE Butiker.butik = GaveKort.Butnr NO-LOCK NO-ERROR.
          ASSIGN cSelger      = "Ukjent"
                 cBruktSelger = "Ukjent".
          IF GaveKort.SelgerNr <> 0 THEN DO:
              FIND Selger WHERE Selger.SelgerNr = GaveKort.SelgerNr NO-LOCK NO-ERROR.
              IF AVAIL Selger THEN
                  cSelger = Selger.navn.
          END.
          IF GaveKort.BruktSelgerNr <> 0 THEN DO:
              FIND Selger WHERE Selger.SelgerNr = GaveKort.BruktSelgerNr NO-LOCK NO-ERROR.
              IF AVAIL Selger THEN
                  cBruktSelger = Selger.navn.
          END.
/*           IF AVAIL Butiker THEN */
          DO:
              ASSIGN iFill = 0
                     cTekst = FILL(" ",iFill) + "Butikk: "  + STRING(GaveKort.butnr) + " " + IF AVAIL Butiker THEN Butiker.butnamn ELSE "Ukjent".
              RUN CreateTT_Grid(iRow,1,iFirstDataCol,cTekst,"").
              ASSIGN iRow     = iRow + 1.
          END.
      END.
      ASSIGN iFill = 0
             cTekst = FILL(" ",iFill) + STRING(GaveKort.butnr) + " " + TRIM(GaveKort.IdentNr) + CHR(1) + 
                                 STRING(Gavekort.belop,"->>>,>>9.99") + CHR(2) + "RALIGN" + chr(1) + STRING(GaveKort.Dato) + CHR(2) + "RALIGN" + 
                                 (IF GaveKort.BruktDato <> ? THEN CHR(1) + "Brukt: " + STRING(GaveKort.BruktDato) ELSE "").
      RUN CreateTT_Grid(iRow,2,iFirstDataCol,cTekst,"").
      ASSIGN iRow = iRow + 1.
      IF GaveKort.FraB_Id <> 0 THEN DO:
          ASSIGN iFill = 0
                 cTekst = sp + sp + "Selger: " + cSelger + CHR(1) + "Utstedt bong:" + chr(1) + STRING(Gavekort.BongNr)
                 cEkstra = "BONG" + CHR(1) + STRING(GaveKort.FraB_Id). /* "30500005483" */
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,cEkstra).
          ASSIGN iRow = iRow + 1.
      END.
      IF GaveKort.BruktB_Id <> 0 THEN DO:
          IF GaveKort.butnr <> Gavekort.BruktButNr THEN DO:
              ASSIGN iFill = 0
                     cTekst  = sp + "Innløst butikk: " + STRING(Gavekort.BruktButNr).
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
              ASSIGN iRow = iRow + 1.
          END.
          ASSIGN iFill = 0
                 cTekst  = sp + sp + "Selger: " + cBruktSelger + chr(1) + "Innløst bong:" + chr(1) + STRING(Gavekort.BruktBongNr)
                 cEkstra = "BONG" + CHR(1) + STRING(GaveKort.BruktB_Id).
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,cEkstra).
          ASSIGN iRow = iRow + 1.
      END.

      QUERY QUERY-Gave:GET-NEXT().
  END.
  QUERY QUERY-Gave:QUERY-CLOSE().
  IF NOT CAN-FIND(FIRST TT_Grid) THEN DO:
      APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.
      RETURN.
  END.
  ASSIGN chGrid:Rows = iRow.
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      ASSIGN iCol = 0.
      DO iCount = 1 TO NUM-ENTRIES(TT_Grid.cText,CHR(1)):
          ASSIGN iCol = IF iCol = 0 THEN TT_Grid.iGridCol ELSE iCol + 1
                 chGrid:Cell(flexcpText,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = ENTRY(1,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
          IF NUM-ENTRIES(ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)) = 2 THEN DO:
              ASSIGN cPara = ENTRY(2,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
              IF cPara = "RALIGN" THEN
                  ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignRightBottom.
          END.

      END.
  END.
  RUN DoGroup(4,1).
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      IF TT_Grid.iOutline > 0 THEN
          RUN DoGroup(TT_Grid.iGridRow,TT_Grid.iOutline).
  END.
/*   RUN DoGroup(3,0). */
/* RUN DoGroup(4,1).   */
/* RUN DoGroup(5,2).   */
/* RUN DoGroup(6,3).   */
/* RUN DoGroup(13,3).  */
/* RUN DoGroup(17,3).  */
/* RUN DoGroup(26,2).  */
/* RUN DoGroup(27,3).  */
/* RUN DoGroup(33,1).  */
/* RUN DoGroup(34,2).  */
/* RUN DoGroup(35,3).  */
/* RUN DoGroup(40,2).  */
FOR EACH TT_Grid BY TT_Grid.iGridRow:
    IF TT_Grid.iOutline = 2 THEN
    ASSIGN chGrid:IsCollapsed(TT_Grid.iGridRow) = flexOutlineCollapsed.
END.
        ASSIGN
        chGrid:AutoSizeMode = flexAutoSizeColWidth.
        chGrid:AutoSize(mCol,t).
/*         chGrid:AutoSize(t). */
        ASSIGN
        chGrid:MergeCells = flexMergeSpill.
        chGrid:ColWidth(t - 1) = 300.
        chGrid:ColWidth(t + 1) = 300.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillLager C-Win 
PROCEDURE FillLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr    NO-UNDO.
  DEFINE        VARIABLE  iEntry AS INTEGER    NO-UNDO.
  ASSIGN cStorrelser = ""
         cLagerInit  = "".
  FOR EACH StrTstr OF StrType NO-LOCK:
    ASSIGN cStorrelser = cStorrelser + (IF cStorrelser <> "" THEN "," ELSE "") + TRIM(StrTStr.SoStorl)
           cLagerInit  = cLagerInit  + (IF cLagerInit <> "" THEN "," ELSE "") + "0".
  END.
  FOR EACH ArtLag WHERE ArtLag.Artikkelnr = dArtikkelnr AND ArtLag.lagant > 0 NO-LOCK.
      FIND TT_Lager WHERE TT_Lager.iButikkNr = ArtLag.butik NO-ERROR.
      IF NOT CAN-DO(cStorrelser,TRIM(ArtLag.storl)) THEN
          NEXT.
      IF NOT AVAIL TT_Lager THEN DO:
          FIND Butiker WHERE Butiker.Butik = ArtLag.butik NO-LOCK NO-ERROR.
          CREATE TT_Lager.
          ASSIGN TT_Lager.iButikkNr    = ArtLag.butik
                 TT_Lager.cButnamn     = IF AVAIL Butiker THEN Butiker.Butnamn ELSE "Ukjent"
                 TT_Lager.cAntallListe = cLagerInit.
      END.
      ASSIGN iEntry = LOOKUP(TRIM(ArtLag.storl),cStorrelser)
             ENTRY(iEntry,TT_Lager.cAntallListe) = STRING(INT(ENTRY(iEntry,TT_Lager.cAntallListe)) + ArtLag.LagAnt).
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillLagerTT C-Win 
PROCEDURE FillLagerTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr    NO-UNDO.
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount2 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFill AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cPara AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeftAlignCols AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iErstattId     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE BUFFER bArtBas FOR ArtBas.
  ASSIGN iRow = iFirstDataRow.
  FIND ArtBas WHERE ArtBas.Artikkelnr = dArtikkelNr NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN.
  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType OF ArtBas NO-LOCK NO-ERROR.
  FIND Sasong OF Artbas NO-LOCK NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  
  EMPTY TEMP-TABLE TT_Grid.
  EMPTY TEMP-TABLE TT_Lager.
  ASSIGN chGrid:Rows = iFirstDataRow
         chGrid:Cols = t - 1.
  ASSIGN chGrid:Cols = 10.
  IF AVAIL StrType AND CAN-FIND(FIRST artlag WHERE Artlag.artikkelnr = artbas.artikkelnr AND ArtLag.lagant > 0) THEN
      RUN FillLager (ArtBas.Artikkelnr).

/* iFirstDataCol */

/* DEFINE TEMP-TABLE TT_Grid        */
/*     FIELD iGridrow AS INTEGER    */
/*     FIELD iOutline AS INTEGER    */
/*     FIELD iGridCol AS INTEGER    */
/*     FIELD cText    AS CHARACTER. */


  DO:
      ASSIGN cGridModell = ""
             cTekst      = sp + STRING(ArtBas.Artikkelnr) + " - " + TRIM(Artbas.Beskr).
      RUN CreateTT_Grid(iRow,2,iFirstDataCol,cTekst,"ARTBAS" + CHR(1) + STRING(ArtBas.ArtikkelNr)).
      IF ArtBas.ModellFarge <> 0 THEN DO:
          ASSIGN iModellRow = iRow
                 iModellCol = iFirstDataCol + 1
                 cGridModell = "".
          RUN GetModell(ArtBas.Artikkelnr,OUTPUT cGridModell).
      END.
      ELSE
          ASSIGN iModellRow = 0
                 iModellCol = 0.

      ASSIGN iRow     = iRow + 1.
      ASSIGN cTekst = sp + "Egenskaper".
      RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
      
      IF AVAIL VarGr THEN DO:
          ASSIGN iFill = IF LENGTH(STRING(ArtBas.Vg)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.Vg)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.Vg)) = 3 THEN 2 ELSE 0
                 cTekst = sp + sp + FILL(" ",iFill) + STRING(ArtBas.Vg) + IF AVAIL VarGr THEN " " + TRIM(VarGr.VgBeskr) ELSE " -" .
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
          ASSIGN iRow     = iRow + 1.
      END.
      IF AVAIL Farg AND TRIM(Farg.FarBeskr) <> "" THEN DO:
          ASSIGN iFill  = IF LENGTH(STRING(ArtBas.Farg)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.Farg)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.Farg)) = 3 THEN 2 ELSE 0
                 cTekst = sp + sp + FILL(" ",iFill) + STRING(ArtBas.Farg) + IF AVAIL Farg THEN " " + TRIM(Farg.FarBeskr) ELSE "".
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
          ASSIGN iRow     = iRow + 1.
      END.
      ASSIGN iFill = IF LENGTH(STRING(ArtBas.StrType)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.StrType)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.StrType)) = 3 THEN 2 ELSE 0
             cText = sp + sp + FILL(" ",iFill) + STRING(ArtBas.StrType) + IF AVAIL StrType THEN " " + TRIM(StrType.Beskrivelse) ELSE "".
      RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
      
      IF AVAIL Sasong AND TRIM(Sasong.SasBeskr) <> "" THEN DO:
          ASSIGN iFill = IF LENGTH(STRING(ArtBas.Sasong)) = 1 THEN 7 ELSE IF LENGTH(STRING(ArtBas.Sasong)) = 2 THEN 5 ELSE IF LENGTH(STRING(ArtBas.Sasong)) = 3 THEN 2 ELSE 0
                 cText = sp + sp + FILL(" ",iFill) + STRING(ArtBas.Sasong) + IF AVAIL Sasong THEN " " + TRIM(Sasong.SasBeskr) ELSE "".
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
          ASSIGN iRow     = iRow + 1.
      END.
      ASSIGN iFill = IF LENGTH(STRING(LevBas.Levnr)) = 1 THEN 7 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 2 THEN 5 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 3 THEN 2 ELSE 0
             cText = sp + sp + FILL(" ",iFill) + STRING(ArtBas.LevNr) + IF AVAIL LevBas THEN " " + TRIM(LevBas.levnamn) ELSE "".
      RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
     FIND ArtPris OF ArtBas NO-LOCK NO-ERROR.
     IF AVAIL ArtPris OR AMBIG ArtPris THEN DO:
         ASSIGN cTekst = sp + "Kalkyle".
         RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
         ASSIGN iRow     = iRow + 1.
         
         IF AVAIL Artpris THEN DO:
             IF ArtPris.Tilbud = TRUE THEN DO:
                 ASSIGN cTekst = sp + "På tilbud".
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                 ASSIGN iRow     = iRow + 1.
                 ASSIGN cTekst = sp + sp + "Tilbud   " + STRING(ArtPris.Pris[2],">>,>>9.99") + CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr).
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                 ASSIGN iRow     = iRow + 1.
             END.
             ASSIGN cTekst = sp + sp + "Normal " + STRING(ArtPris.Pris[1],">>,>>9.99")  + 
                             IF NOT ArtPris.Tilbud THEN CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr) ELSE "".
             RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
             ASSIGN iRow     = iRow + 1.
         END.
         ELSE IF AMBIG ArtPris THEN DO:
             FOR EACH ArtPris OF ArtBas.
                 IF ArtPris.Tilbud = TRUE THEN DO:
                     ASSIGN cTekst = sp + sp + "På tilbud".
                     RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                     ASSIGN cTekst = sp + sp + "Tilbud   " + STRING(ArtPris.Pris[2],">>,>>9.99") + CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr).
                     RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                 END.
                 ASSIGN cTekst = sp + sp + "Normal " + STRING(ArtPris.Pris[1],">>,>>9.99")  + 
                                IF NOT ArtPris.Tilbud THEN CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr) ELSE "".
                 RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                 ASSIGN iRow     = iRow + 1.
             END.
         END.
         FIND Erstattningsvare OF ArtBas NO-LOCK NO-ERROR.
         IF AVAIL Erstattningsvare THEN DO:
             ASSIGN iErstattId = Erstattningsvare.ErstattId.
             ASSIGN cTekst = sp + "Erstattningsvare".
             RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
             ASSIGN iRow     = iRow + 1.
             FOR EACH Erstattningsvare WHERE Erstattningsvare.ErstattId = iErstattId AND Erstattningsvare.ArtikkelNr <> ArtBas.ArtikkelNr NO-LOCK.
                 FIND bArtBas WHERE bArtBas.ArtikkelNr = Erstattningsvare.Artikkelnr NO-LOCK NO-ERROR.
                 IF AVAIL bArtBas THEN DO:
                     ASSIGN cTekst = sp + sp + STRING(bArtBas.Artikkelnr) + " - " + TRIM(bArtbas.Beskr).
                     RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                     ASSIGN iRow     = iRow + 1.
                     FOR EACH ArtPris OF bArtBas.
                         IF ArtPris.Tilbud = TRUE THEN DO:
                             ASSIGN cTekst = sp + sp + "På tilbud".
                             RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                             ASSIGN iRow     = iRow + 1.
                             ASSIGN cTekst = sp + sp + sp + "Tilbud   " + STRING(ArtPris.Pris[2],">>,>>9.99") + CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr).
                             RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                             ASSIGN iRow     = iRow + 1.
                         END.
                         ASSIGN cTekst = sp + sp + sp + "Normal " + STRING(ArtPris.Pris[1],">>,>>9.99")  + 
                                        IF NOT ArtPris.Tilbud THEN CHR(1) + "Prisprofil: " + STRING(ArtPris.Profilnr) ELSE "".
                         RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                         ASSIGN iRow     = iRow + 1.
                     END.
                 END.
             END.
         END.
         IF CAN-FIND(FIRST TT_Lager) THEN DO:
             IF NUM-ENTRIES(cStorrelser) > 2 THEN
                 ASSIGN chGrid:Cols = chGrid:Cols + NUM-ENTRIES(cStorrelser).
/*                  chGrid:AutoSize(t,chGrid:Cols - 1). */
/*                   DO i = t To chGrid:Cols - 1:         */
/*                       ASSIGN chGrid:ColWidth(i) = 400. */
/*                   END.                                 */

             ASSIGN cTekst = sp + "Lager".
             RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,"").
             ASSIGN iRow     = iRow + 1.
             
             ASSIGN cTekst = REPLACE(cStorrelser,",",CHR(1)).
             RUN CreateTT_Grid(iRow,0,t,cTekst,"").
             ASSIGN iRow     = iRow + 1.
             
             ASSIGN cLeftAlignCols = cLeftAlignCols + (IF cLeftAlignCols <> "" THEN CHR(1) ELSE "") + 
                                 STRING(t) + "," + STRING(chGrid:Cols - 1).
            FOR EACH TT_Lager:
                DO iCount = 1 TO NUM-ENTRIES(TT_Lager.cAntallListe):
                    IF ENTRY(iCount,TT_Lager.cAntallListe) = "0" THEN
                        ENTRY(iCount,TT_Lager.cAntallListe) = "".
                END.
                ASSIGN cTekst = sp + sp + TT_Lager.cButnamn + CHR(1) + CHR(1) + CHR(1) + CHR(1) + CHR(1) + REPLACE(TT_Lager.cAntallListe,",",CHR(1)).
                RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
                ASSIGN iRow     = iRow + 1.
            END.
         END.
     END.
  END.
  IF NOT CAN-FIND(FIRST TT_Grid) THEN DO:
      APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.
      RETURN.
  END.
  ASSIGN chGrid:Rows = iRow.
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      ASSIGN iCol = 0.
      DO iCount = 1 TO NUM-ENTRIES(TT_Grid.cText,CHR(1)):
          ASSIGN iCol = IF iCol = 0 THEN TT_Grid.iGridCol ELSE iCol + 1
                 chGrid:Cell(flexcpText,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = ENTRY(1,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
          IF NUM-ENTRIES(ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)) = 2 THEN DO:
              ASSIGN cPara = ENTRY(2,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
              IF cPara = "RALIGN" THEN
                  ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignRightBottom.
          END.

      END.
  END.
  IF cGridModell <> "" THEN DO:
      chGrid:Cell(flexcpText,iModellRow + 1,iModellCol,iModellRow + 1,iModellCol) = "MODELL".
      chGrid:Cell(flexcpText,iModellRow,iModellCol,iModellRow,iModellCol) = ENTRY(1,cGridModell,"|").
      chGrid:ComboList = cGridModell.
  END.
  RUN DoGroup(4,1).
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      IF TT_Grid.iOutline > 0 THEN
          RUN DoGroup(TT_Grid.iGridRow,TT_Grid.iOutline).
  END.
  ASSIGN
  chGrid:AutoSizeMode = flexAutoSizeColWidth.
  chGrid:AutoSize(mcol,chGrid:Cols - 2).
  /*         chGrid:AutoSize(t). */
  IF cLeftAlignCols <> "" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cLeftAlignCols,CHR(1)):
          DO iCount2 = INT(ENTRY(1,ENTRY(iCount,cLeftAlignCols,CHR(1)))) TO INT(ENTRY(2,ENTRY(iCount,cLeftAlignCols,CHR(1)))):
              chGrid:ColAlignment(iCount2) = flexAlignLeftBottom.
          END.
      END.
  END.
  ASSIGN
  chGrid:MergeCells = flexMergeSpill.
  chGrid:ColWidth(t - 1) = 300.
  /*         chGrid:ColWidth(t + 1) = 300. */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillLevTT C-Win 
PROCEDURE FillLevTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFill AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cPara AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN iRow = iFirstDataRow.

  EMPTY TEMP-TABLE TT_Grid.

  QUERY QUERY-Lev:QUERY-OPEN().
  QUERY QUERY-Lev:GET-NEXT().
  REPEAT WHILE NOT QUERY QUERY-Lev:QUERY-OFF-END:
      ASSIGN iFill = IF LENGTH(STRING(LevBas.Levnr)) = 1 THEN 7 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 2 THEN 5 ELSE IF LENGTH(STRING(LevBas.Levnr)) = 3 THEN 2 ELSE 0
             cTekst = FILL(" ",iFill) + STRING(LevBas.levnr) + " " + TRIM(LevBas.levnamn) + CHR(1) + TRIM(LevBas.levtel) + CHR(2) + "RALIGN".
      RUN CreateTT_Grid(iRow,2,iFirstDataCol,cTekst,"").
      ASSIGN iRow     = iRow + 1.
      RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + TRIM(LevBas.levadr) + CHR(1) + TRIM(LevBas.levponr) + " " + TRIM(LevBas.levpadr),"").
      ASSIGN iRow = iRow + 1.
      IF TRIM(LevBas.E_MailLev) <> "" THEN DO:
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + "Email" + CHR(1) + TRIM(LevBas.E_MailLev) + CHR(2) + "RALIGN","").
          ASSIGN iRow = iRow + 1.
      END.
      IF TRIM(LevBas.telefax) <> "" THEN DO:
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + "Fax" + CHR(1) + TRIM(LevBas.telefax) + CHR(2) + "RALIGN","").
          ASSIGN iRow = iRow + 1.
      END.
      IF TRIM(LevBas.levkon) <> "" THEN DO:
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + TRIM(LevBas.levkon) + CHR(1) + TRIM(LevBas.kotel) + CHR(2) + "RALIGN","").
          ASSIGN iRow = iRow + 1.
          IF TRIM(LevBas.kotelex) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Mobil" + CHR(1) + TRIM(LevBas.kotelex) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
          IF TRIM(LevBas.E_MailKontakt) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Email" + CHR(1) + TRIM(LevBas.E_MailKontakt) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
          IF TRIM(LevBas.kotelefax) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Fax" + CHR(1) + TRIM(LevBas.kotelefax) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
      END.
      FOR EACH LevKontakt OF LevBas NO-LOCK:
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + TRIM(LevKontakt.Navn) + CHR(1) + TRIM(LevKontakt.Telefon) + CHR(2) + "RALIGN","").
          ASSIGN iRow = iRow + 1.
          IF TRIM(LevKontakt.Stilling) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Stilling" + CHR(1) + TRIM(LevKontakt.Stilling) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
          IF TRIM(LevKontakt.Mobiltelefon) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Mobil" + CHR(1) + TRIM(LevKontakt.Mobiltelefon) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
          IF TRIM(LevKontakt.E_MailKontakt) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Email" + CHR(1) + TRIM(LevKontakt.E_MailKontakt) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
          IF TRIM(LevKontakt.Telefaks) <> "" THEN DO:
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,sp + sp + sp + sp + "Fax" + CHR(1) + TRIM(LevKontakt.Telefaks) + CHR(2) + "RALIGN","").
              ASSIGN iRow = iRow + 1.
          END.
      END.

      QUERY QUERY-Lev:GET-NEXT().
  END.
  QUERY QUERY-Lev:QUERY-CLOSE().
  IF NOT CAN-FIND(FIRST TT_Grid) THEN DO:
      APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.
      RETURN.
  END.
  ASSIGN chGrid:Rows = iRow.
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      ASSIGN iCol = 0.
      DO iCount = 1 TO NUM-ENTRIES(TT_Grid.cText,CHR(1)):
          ASSIGN iCol = IF iCol = 0 THEN TT_Grid.iGridCol ELSE iCol + 1
                 chGrid:Cell(flexcpText,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = ENTRY(1,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
          IF NUM-ENTRIES(ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)) = 2 THEN DO:
              ASSIGN cPara = ENTRY(2,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
              IF cPara = "RALIGN" THEN
                  ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignRightBottom.
          END.

      END.
  END.
  RUN DoGroup(4,1).
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      IF TT_Grid.iOutline > 0 THEN
          RUN DoGroup(TT_Grid.iGridRow,TT_Grid.iOutline).
  END.
/*   RUN DoGroup(3,0). */
/* RUN DoGroup(4,1).   */
/* RUN DoGroup(5,2).   */
/* RUN DoGroup(6,3).   */
/* RUN DoGroup(13,3).  */
/* RUN DoGroup(17,3).  */
/* RUN DoGroup(26,2).  */
/* RUN DoGroup(27,3).  */
/* RUN DoGroup(33,1).  */
/* RUN DoGroup(34,2).  */
/* RUN DoGroup(35,3).  */
/* RUN DoGroup(40,2).  */
    IF iRow > 25 THEN DO:
        FOR EACH TT_Grid BY TT_Grid.iGridRow:
            IF TT_Grid.iOutline = 2 THEN
            ASSIGN chGrid:IsCollapsed(TT_Grid.iGridRow) = flexOutlineCollapsed.
        END.
    END.
    ASSIGN
    chGrid:AutoSizeMode = flexAutoSizeColWidth.
    chGrid:AutoSize(mCol,t).
    /*         chGrid:AutoSize(t). */
    ASSIGN
    chGrid:MergeCells = flexMergeSpill.
    chGrid:ColWidth(t - 1) = 300.
    chGrid:ColWidth(t + 1) = 300.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTilgTT C-Win 
PROCEDURE FillTilgTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFill AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cPara AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iButnr AS INTEGER INIT ?   NO-UNDO.
  DEFINE VARIABLE cSelger AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBruktSelger AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEkstra AS CHARACTER  NO-UNDO.
  ASSIGN iRow = iFirstDataRow.

/* iFirstDataCol */

/* DEFINE TEMP-TABLE TT_Grid        */
/*     FIELD iGridrow AS INTEGER    */
/*     FIELD iOutline AS INTEGER    */
/*     FIELD iGridCol AS INTEGER    */
/*     FIELD cText    AS CHARACTER. */

  EMPTY TEMP-TABLE TT_Grid.

  QUERY QUERY-Tilg:QUERY-OPEN().
  QUERY QUERY-Tilg:GET-NEXT().
  REPEAT WHILE NOT QUERY QUERY-Tilg:QUERY-OFF-END:
      IF iButNr <> Tilgode.Butnr THEN DO:
          iButnr = Tilgode.Butnr.
          FIND Butiker WHERE Butiker.butik = Tilgode.Butnr NO-LOCK NO-ERROR.
          ASSIGN cSelger      = "Ukjent"
                 cBruktSelger = "Ukjent".
          IF Tilgode.SelgerNr <> 0 THEN DO:
              FIND Selger WHERE Selger.SelgerNr = Tilgode.SelgerNr NO-LOCK NO-ERROR.
              IF AVAIL Selger THEN
                  cSelger = Selger.navn.
          END.
          IF Tilgode.BruktSelgerNr <> 0 THEN DO:
              FIND Selger WHERE Selger.SelgerNr = Tilgode.BruktSelgerNr NO-LOCK NO-ERROR.
              IF AVAIL Selger THEN
                  cBruktSelger = Selger.navn.
          END.
/*           IF AVAIL Butiker THEN */
          DO:
              ASSIGN iFill = 0
                     cTekst = FILL(" ",iFill) + "Butikk: "  + STRING(Tilgode.butnr) + " " + IF AVAIL Butiker THEN Butiker.butnamn ELSE "Ukjent".
              RUN CreateTT_Grid(iRow,1,iFirstDataCol,cTekst,"").
              ASSIGN iRow     = iRow + 1.
          END.
      END.
      ASSIGN iFill = 0
             cTekst = FILL(" ",iFill) + STRING(Tilgode.butnr) + " " + TRIM(Tilgode.IdentNr) + CHR(1) + 
                                 STRING(Tilgode.belop,"->>>,>>9.99") + CHR(2) + "RALIGN" + chr(1) + STRING(Tilgode.Dato) + CHR(2) + "RALIGN" + 
                                 (IF Tilgode.BruktDato <> ? THEN CHR(1) + "Brukt: " + STRING(Tilgode.BruktDato) ELSE "").
      RUN CreateTT_Grid(iRow,2,iFirstDataCol,cTekst,"").
      ASSIGN iRow = iRow + 1.
      IF Tilgode.FraB_Id <> 0 THEN DO:
          ASSIGN iFill = 0
                 cTekst = sp + sp + "Selger: " + cSelger + CHR(1) + "Solgt bong:" + chr(1) + STRING(Tilgode.BongNr)
                 cEkstra = "BONG" + CHR(1) + STRING(Tilgode.FraB_Id). /* "30500005483" */
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,cEkstra).
          ASSIGN iRow = iRow + 1.
      END.
      IF Tilgode.BruktB_Id <> 0 THEN DO:
          IF Tilgode.butnr <> Tilgode.BruktButNr THEN DO:
              ASSIGN iFill = 0
                     cTekst  = sp + "Innløst butikk: " + STRING(Tilgode.BruktButNr).
              RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,"").
              ASSIGN iRow = iRow + 1.
          END.
          ASSIGN iFill = 0
                 cTekst  = sp + sp + "Selger: " + cBruktSelger + chr(1) + "Innløst bong:" + chr(1) + STRING(Tilgode.BruktBongNr)
                 cEkstra = "BONG" + CHR(1) + STRING(Tilgode.BruktB_Id).
          RUN CreateTT_Grid(iRow,0,iFirstDataCol,cTekst,cEkstra).
          ASSIGN iRow = iRow + 1.
      END.

      QUERY QUERY-Tilg:GET-NEXT().
  END.
  QUERY QUERY-Tilg:QUERY-CLOSE().
  IF NOT CAN-FIND(FIRST TT_Grid) THEN DO:
      APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.
      RETURN.
  END.
  ASSIGN chGrid:Rows = iRow.
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      ASSIGN iCol = 0.
      DO iCount = 1 TO NUM-ENTRIES(TT_Grid.cText,CHR(1)):
          ASSIGN iCol = IF iCol = 0 THEN TT_Grid.iGridCol ELSE iCol + 1
                 chGrid:Cell(flexcpText,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = ENTRY(1,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
          IF NUM-ENTRIES(ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)) = 2 THEN DO:
              ASSIGN cPara = ENTRY(2,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
              IF cPara = "RALIGN" THEN
                  ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignRightBottom.
          END.

      END.
  END.
  RUN DoGroup(4,1).
  FOR EACH TT_Grid BY TT_Grid.iGridRow:
      IF TT_Grid.iOutline > 0 THEN
          RUN DoGroup(TT_Grid.iGridRow,TT_Grid.iOutline).
  END.
FOR EACH TT_Grid BY TT_Grid.iGridRow:
    IF TT_Grid.iOutline = 2 THEN
    ASSIGN chGrid:IsCollapsed(TT_Grid.iGridRow) = flexOutlineCollapsed.
END.
        ASSIGN
        chGrid:AutoSizeMode = flexAutoSizeColWidth.
        chGrid:AutoSize(mCol,t).
/*         chGrid:AutoSize(t). */
        ASSIGN
        chGrid:MergeCells = flexMergeSpill.
        chGrid:ColWidth(t - 1) = 300.
        chGrid:ColWidth(t + 1) = 300.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTrans C-Win 
PROCEDURE FillTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dArtikkelNr LIKE Artbas.ArtikkelNr NO-UNDO.
    DEFINE INPUT  PARAMETER dDato       AS DATE                NO-UNDO.
    FOR EACH TransLogg WHERE TransLogg.ArtikkelNr = dArtikkelNr AND TransLogg.Dato = dDato AND Translogg.TTId = 1 NO-LOCK:
        FIND Selger WHERE Selger.Selgernr = TransLogg.SelgerNr NO-LOCK NO-ERROR.
        CREATE TT_Trans.
        ASSIGN TT_Trans.iButikkNr    = Translogg.Butik
               TT_Trans.deAntall     = TransLogg.Antall
               TT_Trans.dPris        = Translogg.Pris - Translogg.Rabkr
               TT_Trans.dRabkr       = Translogg.Rabkr
               TT_Trans.dLinjeRab    = Translogg.LinjeRab
               TT_Trans.dSubtotalrab = Translogg.Subtotalrab
               TT_Trans.iBongNr      = TransLogg.Bongid
               TT_Trans.cSelger      = IF AVAIL Selger THEN Selger.Navn ELSE "Ukjent"
               TT_Trans.cBongInfo    = STRING(Translogg.Butik) + "," + 
                                       STRING(Translogg.Profilnr) + "," + 
                                       STRING(Translogg.KassaNr) + "," + 
                                       STRING(Translogg.Dato) + "," + 
                                       STRING(Translogg.Bongid).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetModell C-Win 
PROCEDURE GetModell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dArtikkelNr  LIKE ArtBas.Artikkelnr NO-UNDO.
  DEFINE OUTPUT PARAMETER cGridModell  AS CHARACTER           NO-UNDO.
  DEFINE        VARIABLE  dModellfarge AS DECIMAL    NO-UNDO.
  FIND bArtBas WHERE bArtBas.ArtikkelNr = dArtikkelnr NO-LOCK NO-ERROR.
  IF AVAIL bArtBas THEN dModellfarge = ArtBas.Modellfarge.
  FOR EACH bArtBas WHERE bArtBas.ModellFarge = dModellfarge AND bArtBas.ArtikkelNr <> dArtikkelNr.
      FIND Farg OF bArtBas NO-LOCK NO-ERROR.
      ASSIGN cGridModell = cGridModell + (IF cGridModell <> "" THEN "|" ELSE "") + 
          ((IF AVAIL Farg AND Farg.farbeskr <> "" THEN Farg.farbeskr ELSE IF AVAIL Farg THEN "Farge ?" ELSE "Feil farge")) + "," + STRING(bArtBas.ArtikkelNr).
  END.
/*   IF NUM-ENTRIES(cGridModell,"|") = 1 THEN    */
/*       ASSIGN cGridModell = cGridModell + "|". */
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
    ASSIGN chGrid = chGrid:vsFlexGrid
           chGrid:fontName = "Tahoma"
           chGrid:fontbold = true
           chGrid:fontsize = "8"
           mCol = 3                /* tree column          */
              t = 8                /* amount column        */
             sp = "     ".         /* outline indentention */
        
assign
chGrid:ExtendLastCol  = "1"
chGrid:OutlineBar     = "1"
chGrid:OutlineCol     = "3"
chGrid:GridLines      = "0"
chGrid:GridLinesFixed = "2"
chGrid:GridLineWidth  = "1"
chGrid:Rows           = "4"
chGrid:Cols           = "10"
chGrid:FixedRows      = "0"
chGrid:FixedCols      = "0"
chGrid:AutoSearch     = "2"
chGrid:TabBehavior    = "1".




    /* set column width */
    DO i = 0 To 5:
        ASSIGN chGrid:ColWidth(i) = 200.
    END.
    ASSIGN chGrid:ColWidth(mCol) = 800 /* more space for the tree column */
        
    /* set global settings */
    /* chGrid:GridLines = flexGridNone */
           chGrid:ScrollTrack = False
           chGrid:ScrollTips = True
           chGrid:Editable = True
           chGrid:BackColorBkg = vbWhite
           chGrid:SheetBorder = vbWhite
/*            chGrid:Cell(flexcpFontBold, 4, t, chGrid:Rows - 1, t) = False   /* set font for amounts */ */
        /* set title */
        chGrid:Cell(flexcpText,1,3,1,3) = cInstbutikk
        chGrid:Cell(flexcpFontSize,1,3,1,3) = 12
        chGrid:Cell(flexcpForeColor,1,3,1,3) = vbBlue
        chGrid:RowHeight(1) = 300
        
        /* set subtitle */
/*         chGrid:Cell(flexcpText,2,3,2,3) = "Selskap i Sport1" */
/*         chGrid:Cell(flexcpFontBold,2,3,2,3) = False          */
/*         chGrid:Cell(flexcpFontItalic,2,3,2,3) = True         */
/*         chGrid:Cell(flexcpText,3,t,3,t) = "As of today" /* ' " & Format$(Now, "mmm dd, yyyy") & "   " */ */
/*         chGrid:Cell(flexcpForeColor,3,3,3,3) = vbBlue.                                                   */
        /* chGrid:Cell(flexcpPicture, 3, t - 1) = Image4chGrid:Picture */
        /* chGrid:Cell(flexcpPicture, 3, t + 1) = Image4chGrid:Picture */
        /*------------------------------------------------------------
        ' create outline structure
        '------------------------------------------------------------ */
/*         RUN DoGroup(3,0).  */
/*         RUN DoGroup(4,1).  */
/*         RUN DoGroup(5,2).  */
/*         RUN DoGroup(6,3).  */
/*         RUN DoGroup(13,3). */
/*         RUN DoGroup(17,3). */
/*         RUN DoGroup(26,2). */
/*         RUN DoGroup(27,3). */
/*         RUN DoGroup(33,1). */
/*         RUN DoGroup(34,2). */
/*         RUN DoGroup(35,3). */
/*         RUN DoGroup(40,2). */
 /*       
        
        '------------------------------------------------------------
        ' create outline nodes and data
        '------------------------------------------------------------ */
        chGrid:Cell(flexcpText, 2, mCol,2,mCol) = "".
/*         chGrid:Cell(flexcpText, 3, mCol,3,mCol) = "LEVERANDØRER". */
/*         chGrid:Cell(flexcpText, 4, t,4,t) = "3,972,500.00". */

/*         chGrid:Cell(flexcpText, 5, mCol,5,mCol) = "Current Assets".                      */
/*         chGrid:Cell(flexcpText, 5, t,5,t) = "1,232,500.00".                              */
/*         chGrid:Cell(flexcpText, 6, mCol,6,mCol) = "Checking/Savings".                    */
/*         chGrid:Cell(flexcpText, 6, t,6,t) = "340,070.00".                                */
/*         chGrid:Cell(flexcpText, 7, mCol,7,mCol) = sp + "Bank - Checking".                */
/*         chGrid:Cell(flexcpText, 7, t,7,t) = "32,500.00".                                 */
/*         chGrid:Cell(flexcpText, 8, mCol,8,mCol) = sp + "Bank - Money Market".            */
/*         chGrid:Cell(flexcpText, 8, t,8,t) = "140,070.00".                                */
/*         chGrid:Cell(flexcpText, 9, mCol,9,mCol) = sp + "Bank VideoSoft -  Money Market". */
/*         chGrid:Cell(flexcpText, 9, t,9,t) = "94,080.00".                                 */
/*         chGrid:Cell(flexcpText, 10, mCol,10,mCol) = sp + "Bank VideoSoft - Savings".     */
/*         chGrid:Cell(flexcpText, 10, t,10,t) = "73,420.00".                               */
/*         chGrid:Cell(flexcpText, 11, mCol,11,mCol) = "Total Checking/Savings".            */
/*         chGrid:Cell(flexcpText, 11, t,11,t) = "340,070.00".                              */
/*                                                                                           */
/*         chGrid:Cell(flexcpText, 13, mCol,13,mCol) = "Accounts Receivable".               */
/*         chGrid:Cell(flexcpText, 13, t,13,t) = "280,500.00".                              */
/*         chGrid:Cell(flexcpText, 14, mCol,14,mCol) = sp + sp + "Accounts Receivable".     */
/*         chGrid:Cell(flexcpText, 14, t,14,t) = "280,500.00".                              */
/*         chGrid:Cell(flexcpText, 15, mCol,15,mCol) = "Total Accounts Receivable".         */
/*         chGrid:Cell(flexcpText, 14, t,14,t) = "280,500.00".                              */
/*         chGrid:Cell(flexcpText, 15, t,15,t) = "280,500.00".                              */
/*                                                                                           */
/*         chGrid:Cell(flexcpText, 17, mCol,17,mCol) = "Other Current Assets".              */
/*         chGrid:Cell(flexcpText, 17, t,17,t) = "611,930.00".                              */
/*         chGrid:Cell(flexcpText, 18, mCol,18,mCol) = sp + sp + "Other Receivables".       */
/*         chGrid:Cell(flexcpText, 18, t,18,t) = "32,500.00".                               */
/*         chGrid:Cell(flexcpText, 19, mCol,19,mCol) = sp + sp + "Short Term Investments".  */
/*         chGrid:Cell(flexcpText, 19, t,19,t) = "432,100.00".                              */
/*         chGrid:Cell(flexcpText, 20, mCol,20,mCol) = sp + sp + "Inventory".               */
/*         chGrid:Cell(flexcpText, 20, t,20,t) = "15,440.00".                               */
/*         chGrid:Cell(flexcpText, 21, mCol,21,mCol) = sp + sp + "Pre-paid Expenses".       */
/*         chGrid:Cell(flexcpText, 21, t,21,t) = "131,890.00".                              */
/*         chGrid:Cell(flexcpText, 22, mCol,22,mCol) = "Total Other Current Assets".        */
/*         chGrid:Cell(flexcpText, 22, t,22,t) = "611,930.00".                              */
/*         chGrid:Cell(flexcpText, 24, mCol,24,mCol) = "Total Current Assets".              */
/*         chGrid:Cell(flexcpText, 24, t,24,t) = "1,232,500.00".                            */
/*         chGrid:Cell(flexcpBackColor, 24, t,24,t) = "&H8000000F".                         */
/*                                                                                           */
/*         ASSIGN                                                                            */
/*         chGrid:Cell(flexcpText, 26, mCol,26,mCol) = "Fixed Assets"                       */
/*         chGrid:Cell(flexcpText, 26, t,26,t) = "2,740,000.00"                             */
/*         chGrid:Cell(flexcpText, 27, mCol,27,mCol) = "Fixed Assets"                       */
/*         chGrid:Cell(flexcpText, 27, t,27,t) = "2,740,000.00"                             */
/*         chGrid:Cell(flexcpText, 28, mCol,28,mCol) = sp + sp + "Accumulated Depreciation" */
/*         chGrid:Cell(flexcpText, 28, t,28,t) = "1,240,000.00"                             */
/*         chGrid:Cell(flexcpText, 29, mCol,29,mCOl) = sp + sp + "Equipment"                */
/*         chGrid:Cell(flexcpText, 29, t,29,t) = "1,500,000.00"                             */
/*         chGrid:Cell(flexcpText, 30, mCol,30,mCOl) = "Total Fixed Assets"                 */
/*         chGrid:Cell(flexcpText, 30, t,30,t) = "2,740,000.00"                             */
/*                                                                                           */
/*         chGrid:Cell(flexcpText, 33, mCol,33,mCol) = "LIABILITIES & EQUITY"               */
/*         chGrid:Cell(flexcpText, 33, t,33,t) = "3,972,500.00"                             */
/*         chGrid:Cell(flexcpText, 34, mCol,34,mCol) = "Liabilities"                        */
/*         chGrid:Cell(flexcpText, 34, t,34,t) = "235,400.00"                               */
/*         chGrid:Cell(flexcpText, 35, mCol,35,mCol) = "Current Liabilities"                */
/*         chGrid:Cell(flexcpText, 35, t,35,t) = "235,400.00"                               */
/*         chGrid:Cell(flexcpText, 36, mCol,36,mCol) = sp + sp + "Accounts Payable"         */
/*         chGrid:Cell(flexcpText, 36, t,36,t) = "64,250.00"                                */
/*         chGrid:Cell(flexcpText, 37, mCol,37,mCol) = sp + sp + "Payroll Liabilities"      */
/*         chGrid:Cell(flexcpText, 37, t,37,t) = "171,150.00"                               */
/*         chGrid:Cell(flexcpText, 38, mCol,38,mCol) = "Total Current Liabilities"          */
/*         chGrid:Cell(flexcpText, 38, t,38,t) = "235,400.00"                               */
/*                                                                                           */
/*         chGrid:Cell(flexcpText, 40, mCol,40,mCol) = "Equity"                             */
/*         chGrid:Cell(flexcpText, 40, t,40,t) = "3,737,100.00"                             */
/*         chGrid:Cell(flexcpText, 41, mCol,41,mCol) = sp + sp + "Retained Earnings"        */
/*         chGrid:Cell(flexcpText, 41, t,41,t) = "570,000.00"                               */
/*         chGrid:Cell(flexcpText, 42, mCol,42,mCol) = sp + sp + "Net Income"               */
/*         chGrid:Cell(flexcpText, 42, t,42,t) = "1,750,000.00"                             */
/*         chGrid:Cell(flexcpText, 43, mCol,43,mCol) = sp + sp + "Common Stock"             */
/*         chGrid:Cell(flexcpText, 43, t,43,t) = "640,000.00"                               */
/*         chGrid:Cell(flexcpText, 44, mCol,44,mCol) = sp + sp + "Paid-In Capital"          */
/*         chGrid:Cell(flexcpText, 44, t,44,t) = "777,100.00"                               */
/*         chGrid:Cell(flexcpText, 45, mCol,45,mCol) = "Total Equity"                       */
/*         chGrid:Cell(flexcpText, 45, t,45,t) = "3,737,100.00".                            */
    /*        
        ' subtotals cell lines
        chGrid:Row = 11
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        chGrid:Row = 15
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        chGrid:Row = 22
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        chGrid:Row = 24
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        chGrid:Row = 30
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        chGrid:Row = 38
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        chGrid:Row = 45
        chGrid:col = t
        chGrid:CellBorder vbBlack, 0, 1, 0, 0, 0, 0
        */
        /* make it look good */
        ASSIGN
        chGrid:AutoSizeMode = flexAutoSizeColWidth.
        chGrid:AutoSize(mCol).
        chGrid:AutoSize(t).
        ASSIGN
        chGrid:MergeCells = flexMergeSpill.
        chGrid:ColWidth(t - 1) = 300.
        chGrid:ColWidth(t)     = 200.
        chGrid:ColWidth(t + 1) = 300.
    
    
    
    
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
   DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
   IF cType = "GAVE" THEN DO WITH FRAME FRAME-Gavekort:
       ASSIGN INPUT FI-GaveButNr
              INPUT CB-GaveEgne
              INPUT FI-GaveIdentNr
              INPUT FI-GaveBelopFra 
              INPUT FI-GaveBelopTil 
              INPUT FI-GaveBruktDatoFra 
              INPUT FI-GaveBruktDatoTil 
              INPUT FI-GaveDatoFra 
              INPUT FI-GaveDatoTil.
       IF FI-GaveDatoTil <> ? AND FI-GaveDatoFra > FI-GaveDatoTil THEN DO:
           MESSAGE "Feil dato"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO FI-GaveDatoFra.
           RETURN "AVBRYT".
       END.
       IF FI-GaveBruktDatoTil <> ? AND FI-GaveBruktDatoFra > FI-GaveBruktDatoTil THEN DO:
           MESSAGE "Feil bruktdato"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO FI-GaveBruktDatoFra.
           RETURN "AVBRYT".
       END.
       IF FI-GaveBelopTil <> 0 AND FI-GaveBelopFra > FI-GaveBelopTil THEN DO:
           MESSAGE "Feil beløp"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO FI-GaveBelopFra.
           RETURN "AVBRYT".
       END.

   END.
   ELSE IF cType = "TILG" THEN DO WITH FRAME FRAME-TilGode:
       ASSIGN INPUT FI-TilgButNr
              INPUT CB-TilgEgne
              INPUT FI-TilgIdentNr
              INPUT FI-TilgBelopFra 
              INPUT FI-TilgBelopTil 
              INPUT FI-TilgBruktDatoFra 
              INPUT FI-TilgBruktDatoTil 
              INPUT FI-TilgDatoFra 
              INPUT FI-TilgDatoTil.
       IF FI-TilgDatoTil <> ? AND FI-TilgDatoFra > FI-TilgDatoTil THEN DO:
           MESSAGE "Feil dato"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO FI-TilgDatoFra.
           RETURN "AVBRYT".
       END.
       IF FI-TilgBruktDatoTil <> ? AND FI-TilgBruktDatoFra > FI-TilgBruktDatoTil THEN DO:
           MESSAGE "Feil bruktdato"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO FI-TilgBruktDatoFra.
           RETURN "AVBRYT".
       END.
       IF FI-TilgBelopTil <> 0 AND FI-TilgBelopFra > FI-TilgBelopTil THEN DO:
           MESSAGE "Feil beløp"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO FI-TilgBelopFra.
           RETURN "AVBRYT".
       END.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     før att inte blæddring i artikkelkortet skall krascha
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cDummy AS CHARACTER  NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDivResize C-Win 
PROCEDURE SetDivResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW, */
/*                                 FRAME DEFAULT-FRAME:HANDLE,    */
/*                                 "Image-Sko,RECT-3,RECT-4").    */
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-1,RECT-2").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME FRAME-Lev:HANDLE,
                                "FRAME-Lev").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME FRAME-Lager:HANDLE,
                                "FRAME-Lager").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME FRAME-Art:HANDLE,
                                "FRAME-Art").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME FRAME-Gavekort:HANDLE,
                                "FRAME-Gavekort").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME FRAME-Tilgode:HANDLE,
                                "FRAME-Tilgode").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                 "Btn_Done,Btn_Help").
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */
/* DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0). */
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getType C-Win 
FUNCTION getType RETURNS CHARACTER
  ( INPUT iRSType AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cText AS CHARACTER  NO-UNDO.
  CASE iRSType:
      WHEN 1 THEN
          ASSIGN cText = "LEVERANDØRER".
      WHEN 2 THEN
          ASSIGN cText = "ARTIKKEL/Lager".
      WHEN 3 THEN
          ASSIGN cText = "ARTIKKEL/Salgstranser".
      WHEN 4 THEN
          ASSIGN cText = "GAVEKORT".
      WHEN 5 THEN
          ASSIGN cText = "TILGODELAPPER".
      OTHERWISE
          ASSIGN cText = "IKKE TILDELT".
  END CASE.

  RETURN cText.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

