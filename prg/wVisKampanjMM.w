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
DEFINE VARIABLE iCurrRow  AS INTEGER    NO-UNDO.  /* Håller reda på kampanjenoden för att färglägga vid fel strekkod */
DEFINE VARIABLE lFeil AS LOGICAL    NO-UNDO.  /* Raden som har fel strekkode skall registreras för färgläggning, måste sättas till false i create_tt_grid */

  DEFINE BUFFER bArtBas FOR ArtBas.

DEFINE TEMP-TABLE TT_Grid NO-UNDO
    FIELD iGridrow AS INTEGER
    FIELD iOutline AS INTEGER
    FIELD iGridCol AS INTEGER
    FIELD cText    AS CHARACTER
    FIELD cExtraData AS CHARACTER /* anvænds før eventuell referens till databasen */
    FIELD lFel        AS LOGICAL.

DEFINE BUFFER bTT_Grid FOR TT_Grid.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEAN C-Win 
FUNCTION getEAN RETURNS CHARACTER
  ( INPUT ipArtikkelnr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHHdar C-Win 
FUNCTION getHHdar RETURNS CHARACTER
  ( INPUT cDagliste AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHighLow C-Win 
FUNCTION getHighLow RETURNS CHARACTER
  ( INPUT dProdFamId AS DECIMAL,INPUT dKampTilbArtId AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRabType C-Win 
FUNCTION getRabType RETURNS CHARACTER
  ( INPUT iKampRabattTypeId AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
     LIST-ITEM-PAIRS "KampanjMM",1
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.4 BY .1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.4 BY .1.

DEFINE BUTTON B-KampId 
     LABEL "..." 
     SIZE 4.4 BY 1.1.

DEFINE VARIABLE ED-notat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 50 BY 3.81 NO-UNDO.

DEFINE VARIABLE FI-KampId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kampanj" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KampNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 53.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sentdatum AS DATE FORMAT "99/99/99" 
     LABEL "Sänt" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Senttid AS CHARACTER FORMAT "X(5)" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1.

DEFINE VARIABLE FI-Slutdatum AS DATE FORMAT "99/99/99" 
     LABEL "Slut" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Sluttid AS CHARACTER FORMAT "X(5)" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1.

DEFINE VARIABLE FI-Startdatum AS DATE FORMAT "99/99/99" 
     LABEL "Start" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Starttid AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1.

DEFINE VARIABLE TG-Aktiv AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.8 BY .81 NO-UNDO.

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
         SIZE 144 BY 27.62.

DEFINE FRAME FRAME-Kampanj
     B-KampId AT ROW 1.19 COL 42.8 WIDGET-ID 6
     FI-KampId AT ROW 1.24 COL 14 COLON-ALIGNED
     FI-KampNavn AT ROW 1.24 COL 45.2 COLON-ALIGNED NO-LABEL
     TG-Aktiv AT ROW 1.33 COL 102.6
     ED-notat AT ROW 2.91 COL 40.4 NO-LABEL
     FI-Startdatum AT ROW 2.95 COL 13.6 COLON-ALIGNED HELP
          "Dato"
     FI-Starttid AT ROW 2.95 COL 29.6 COLON-ALIGNED NO-LABEL
     FI-Slutdatum AT ROW 4.38 COL 13.6 COLON-ALIGNED HELP
          "Dato"
     FI-Sluttid AT ROW 4.38 COL 29.6 COLON-ALIGNED NO-LABEL
     FI-Sentdatum AT ROW 5.76 COL 13.6 COLON-ALIGNED HELP
          "Dato"
     FI-Senttid AT ROW 5.76 COL 29.6 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.62
         SIZE 143.4 BY 6.24.


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
         TITLE              = "Kampanjöversikt"
         HEIGHT             = 27.62
         WIDTH              = 144
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
ASSIGN FRAME FRAME-Kampanj:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-Kampanj
                                                                        */
ASSIGN 
       ED-notat:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

/* SETTINGS FOR FILL-IN FI-KampId IN FRAME FRAME-Kampanj
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KampNavn IN FRAME FRAME-Kampanj
   NO-ENABLE                                                            */
ASSIGN 
       FI-Sentdatum:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

ASSIGN 
       FI-Senttid:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

ASSIGN 
       FI-Slutdatum:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

ASSIGN 
       FI-Sluttid:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

ASSIGN 
       FI-Startdatum:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

ASSIGN 
       FI-Starttid:READ-ONLY IN FRAME FRAME-Kampanj        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TG-Aktiv IN FRAME FRAME-Kampanj
   NO-ENABLE                                                            */
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
       ROW             = 8.95
       COLUMN          = 2
       HEIGHT          = 18.95
       WIDTH           = 141
       HIDDEN          = no
       SENSITIVE       = yes.
/* Grid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Grid:MOVE-AFTER(FRAME FRAME-Kampanj:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kampanjöversikt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kampanjöversikt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Kampanjöversikt */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
            {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Kampanj
&Scoped-define SELF-NAME B-KampId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KampId C-Win
ON CHOOSE OF B-KampId IN FRAME FRAME-Kampanj /* ... */
OR F10 OF FI-KampId
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "KampId,KampNavn".
  RUN JBoxDLookup.w ("kampanjemixmatch;KampId;KampNavn","where KampanjeMixMatch.KampEierId = 1",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    FI-KampId:SCREEN-VALUE = ENTRY(1,cLookupValue,'|').
    FI-KampNavn:SCREEN-VALUE = ENTRY(2,cLookupValue,'|').
    APPLY "VALUE-CHANGED" TO FI-KampId.
  END.
  
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


&Scoped-define SELF-NAME CB-Register
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Register C-Win
ON VALUE-CHANGED OF CB-Register IN FRAME DEFAULT-FRAME /* Register */
DO:
  ASSIGN INPUT CB-Register.
  hAktivFrame[CB-Register]:MOVE-TO-TOP().
  APPLY "ENTRY" TO hAktivFrame[CB-Register].
  ASSIGN chGrid:Rows = iFirstDataRow
         chGrid:Cols = 10
         chGrid:Cell(flexcpText, 3, mCol,3,mCol) = getType(CB-Register).
/*   ASSIGN FI-ArtNr:SCREEN-VALUE IN FRAME FRAME-Art = "0" */
/*          FI-StrekKode:SCREEN-VALUE = "".                */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Kampanj
&Scoped-define SELF-NAME FI-KampId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-KampId C-Win
ON VALUE-CHANGED OF FI-KampId IN FRAME FRAME-Kampanj /* Kampanj */
DO:
    APPLY "VALUE-CHANGED" TO CB-Register IN FRAME DEFAULT-FRAME.
    RUN Viskampanj (DECI(SELF:SCREEN-VALUE)).
    RETURN NO-APPLY. 
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
/*             WHEN 1 THEN                                           */
/*                 APPLY "ENTRY" TO FI-StrekKode IN FRAME FRAME-Art. */
/*             WHEN 2 THEN                                           */
/*                 APPLY "ENTRY" TO FI-Levnavn IN FRAME FRAME-Lev.   */
            WHEN 1 THEN
                APPLY "ENTRY" TO FI-Kampid IN FRAME FRAME-Kampanj.
/*             WHEN 4 THEN                                                  */
/*                 APPLY "ENTRY" TO FI-Tilgbutnr IN FRAME FRAME-Tilgode.    */
/*             WHEN 5 THEN                                                  */
/*                 APPLY "ENTRY" TO FI-StrekKodeLager IN FRAME FRAME-Lager. */
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
/* ON 'ALT-A' OF {&WINDOW-NAME} ANYWHERE */
/* DO:                                   */
/*     RUN AktiverSok.                   */
/* END.                                  */
/* ON 'ALT-S' OF {&WINDOW-NAME} ANYWHERE         */
/* DO:                                           */
/*     IF CAN-DO("2,3",STRING(CB-Register)) THEN */
/*         RUN ALTS.                             */
/*     RETURN NO-APPLY.                          */
/* END.                                          */
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {syspara.i 1 1 100 cInstbutikk}
   ASSIGN hAktivFrame[1] = FRAME FRAME-Kampanj:HANDLE.
   /*RUN InitCBKampid.*/
   ASSIGN CB-Register = 1.
  RUN enable_UI.
  {lng.i} /* Oversettelse */
  FIND FIRST KampanjeMixMatch where KampanjeMixMatch.KampEierId = 1 NO-LOCK NO-ERROR.
  IF AVAILABLE KampanjeMixMatch THEN
      ASSIGN
      FI-KampId:SCREEN-VALUE IN FRAME FRAME-Kampanj   = STRING(KampanjeMixMatch.KampId)
      FI-KampNavn:SCREEN-VALUE IN FRAME FRAME-Kampanj = KampanjeMixMatch.KampNavn
      .
  RUN SetDivResize.
  APPLY "VALUE-CHANGED" TO CB-Register IN FRAME {&FRAME-NAME}.
  IF DEC(FI-KampId:SCREEN-VALUE) > 0 THEN
      RUN viskampanj (DECI(FI-Kampid:SCREEN-VALUE)).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

OCXFile = SEARCH( "wVisKampanjMM.wrx":U ).
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
ELSE MESSAGE "wVisKampanjMM.wrx":U SKIP(1)
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
                    TT_Grid.lFel       = lFeil
                    lFeil          = FALSE
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
  DISPLAY FI-KampId FI-KampNavn TG-Aktiv ED-notat FI-Startdatum FI-Starttid 
          FI-Slutdatum FI-Sluttid FI-Sentdatum FI-Senttid 
      WITH FRAME FRAME-Kampanj IN WINDOW C-Win.
  ENABLE B-KampId ED-notat FI-Startdatum FI-Starttid FI-Slutdatum FI-Sluttid 
         FI-Sentdatum FI-Senttid 
      WITH FRAME FRAME-Kampanj IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Kampanj}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCBKampid C-Win 
PROCEDURE InitCBKampid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    /*
    IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"send_Kampid_CB") THEN DO:
        RUN send_Kampid_CB IN SOURCE-PROCEDURE (OUTPUT cListItemPairs) NO-ERROR.
    END.
    IF cListItemPairs = "" THEN DO:
        FOR EACH kampanjemixmatch NO-LOCK WHERE KampanjeMixMatch.KampEierId = 1 BY KampId DESCENDING:
            cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + STRING(KampanjeMixMatch.KampId) + " - " + 
                                                                                           REPLACE(KampanjeMixMatch.KampNavn,","," ") + "," + 
                                                                                           STRING(KampanjeMixMatch.KampId).
        END.
    END.
    DO WITH FRAME FRAME-Kampanj:
        CB-Kampid:LIST-ITEM-PAIRS = cListItemPairs.
        CB-Kampid = DECI(ENTRY(2,CB-Kampid:LIST-ITEM-PAIRS)).
    END.
    */
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
chGrid:Cols           = "11"
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
                                FRAME FRAME-Kampanj:HANDLE,
                                "FRAME-Kampanj").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                 "Btn_Done,Btn_Help").
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */
/* DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0). */
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Viskampanj C-Win 
PROCEDURE Viskampanj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipKampid AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFill AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cPara AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrisListe AS CHARACTER  NO-UNDO.

  ASSIGN iRow = iFirstDataRow.
  {sww.i}
  EMPTY TEMP-TABLE TT_Grid.
  ASSIGN chGrid:Rows = iFirstDataRow
         chGrid:Cols = t - 1.
  ASSIGN chGrid:Cols = 10.
    DO WITH FRAME FRAME-Kampanj:
        FIND kampanjemixmatch WHERE kampanjemixmatch.kampid = ipKampid NO-LOCK NO-ERROR.
        IF NOT AVAIL kampanjemixmatch THEN
            RETURN.
        ASSIGN TG-Aktiv      = KampanjeMixMatch.KampKlar
               FI-Startdatum = KampanjeMixMatch.KampStartDato
               FI-Starttid   = STRING(KampanjeMixMatch.KampStartTid,"HH:MM")
               FI-Slutdatum  = KampanjeMixMatch.KampSluttDato
               FI-Sluttid    = STRING(KampanjeMixMatch.KampSluttTid,"HH:MM")
               FI-Sentdatum  = KampanjeMixMatch.KampSendtDato 
               FI-Senttid    = STRING(KampanjeMixMatch.KampSendtTid,"HH:MM")
               ED-notat      = KampanjeMixMatch.KampanjeNotat.

        DISPLAY TG-Aktiv     
                FI-Startdatum
                FI-Starttid  
                FI-Slutdatum 
                FI-Sluttid   
                FI-Sentdatum 
                FI-Senttid   
                ED-notat.
    END.
    cTekst      = sp + STRING(KampanjeMixMatch.Kampid) + " - " + TRIM(KampanjeMixMatch.KampNavn).
RUN CreateTT_Grid(iRow,2,iFirstDataCol,cTekst,"KAMPANJ" + CHR(1) + STRING(KampanjeMixMatch.Kampid)).

/* ASSIGN cTekst = sp + "Egenskaper".                 */
/* RUN CreateTT_Grid(iRow,3,iFirstDataCol,cTekst,""). */
ASSIGN iRow     = iRow + 1.

    FOR EACH KampanjeTilbud OF KampanjeMixMatch NO-LOCK:
        FIND kampanjetilbtype OF KampanjeTilbud NO-LOCK NO-ERROR.
        RUN CreateTT_Grid(iRow,3,iFirstDataCol,sp + sp + STRING(KampanjeTilbud.KampTilbId) + " " + TRIM(KampanjeTilbud.KampTilbNavn),"").
        iCurrRow = iRow.
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Kvittotext: " + CHR(1) + TRIM(KampanjeTilbud.KampTilbKvitteringstekst),"").
        ASSIGN iRow     = iRow + 1.
        IF KampanjeTilbud.HapHourId > 0 THEN DO:
            
            
            RELEASE HappyHourPeriode.
            FIND HappyHourHode OF KampanjeTilbud NO-LOCK NO-ERROR.
            IF AVAIL HappyHourHode THEN DO:
                FIND FIRST HappyHourPeriode OF HappyHourHode NO-LOCK NO-ERROR.
                cTekst = FILL(" ",LENGTH(sp) * 4) + "Happy hour:" + CHR(1) + STRING(KampanjeTilbud.HapHourId) + " " + 
                    TRIM(HappyHourHode.HapHourNavn) + TRIM(IF AVAIL HappyHourPeriode THEN "" ELSE "Period saknas").
                RUN CreateTT_Grid(iRow,4,iFirstDataCol,cTekst,"").
                ASSIGN iRow     = iRow + 1.
            END.
            ELSE DO:
                RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "HAPPY HOUR: " + CHR(1) + STRING(KampanjeTilbud.HapHourId) + " SAKNAS","").
                ASSIGN iRow     = iRow + 1.
            END.
            IF AVAIL HappyHourPeriode THEN DO:
                FOR EACH HappyHourPeriode OF HappyHourHode NO-LOCK:
                    PUT UNFORMATTED HappyHourPeriode.HapHourId             CHR(9)
                                    HappyHourPeriode.HapHourPerId          CHR(9)
                                    STRING(HappyHourPeriode.HapHourPerStartTid,"HH:MM:SS")    CHR(9)
                                    STRING(HappyHourPeriode.HapHourPerSluttTid,"HH:MM:SS")    CHR(9)
                                    HappyHourPeriode.HapHourPerUkedagListe SKIP.
                    RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Period: " + STRING(HappyHourPeriode.HapHourPerId) + CHR(1) + 
                                        string(HappyHourPeriode.HapHourPerStartTid,"HH:MM:SS") + "-" + STRING(HappyHourPeriode.HapHourPerSluttTid,"HH:MM:SS") + " " +
                                        getHHdar(HappyHourPeriode.HapHourPerUkedagListe),"").
                    ASSIGN iRow     = iRow + 1.
                END.
            END.
        END.
        IF AVAIL kampanjetilbtype THEN DO:
            RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Typ: " + CHR(1) + TRIM(Kampanjetilbtype.KampTilbTypeNavn),"").
            ASSIGN iRow     = iRow + 1.
        END.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Pris: " + CHR(1) + STRING(KampanjeTilbud.KampTilbBelop,">>>>9.99") + CHR(2) + "LALIGN","").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Max: " + CHR(1) + STRING(KampanjeTilbud.KamptilbGrenseAntall) + CHR(2) + "LALIGN","").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Gräns: " + CHR(1) + STRING(KampanjeTilbud.KamptilbGrenseAntallBruk,"J/N"),"").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Text: " + CHR(1) + STRING(KampanjeTilbud.KampTilbKvitteringstekst),"").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Namn: " + CHR(1) + STRING(KampanjeTilbud.KampTilbNavn),"").
        ASSIGN iRow     = iRow + 1.
/*         KampanjeTilbud.KamptilbNotat            CHR(9)                                                                           */
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Stegvis: " + CHR(1) + STRING(KampanjeTilbud.KampTilbOkning,"J/N"),"").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Popup: " + CHR(1) + STRING(KampanjeTilbud.KamptilbPopUpTekstBruk,"J/N"),"").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Prop fördeln: " + CHR(1) + STRING(KampanjeTilbud.KampTilbPropBetalFor,"J/N"),"").
        ASSIGN iRow     = iRow + 1.
        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "XMLtyp: " + CHR(1) + ENTRY(KampanjeTilbud.KampTilbTypeId,"pay,save,percent,payfor"),"").
        ASSIGN iRow     = iRow + 1.

        RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Art/Fam: ","").
        ASSIGN iRow     = iRow + 1.
        FOR EACH KampanjeTilbArtikkel OF Kampanjetilbud NO-LOCK:
            FIND Produktfamilie OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                RELEASE ArtBas.
            IF KampanjeTilbArtikkel.KampTilbArtId <> 0 THEN
                FIND Artbas WHERE artbas.artikkelnr = KampanjeTilbArtikkel.KampTilbArtId NO-LOCK NO-ERROR.
            IF NOT AVAIL Produktfamilie OR 
                (KampanjeTilbArtikkel.KampTilbArtId <> 0 AND NOT AVAIL Artbas) THEN DO:
                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + (IF NOT AVAIL Produktfamilie THEN STRING(KampanjeTilbArtikkel.ProdFamId) + " Prodfam saknas" ELSE "") + 
                                  (IF KampanjeTilbArtikkel.KampTilbArtId <> 0 AND NOT AVAIL Artbas THEN " Artikkel " + STRING(KampTilbArtId) + " saknas" ELSE ""),"").
                ASSIGN iRow     = iRow + 1.
            END.
            ELSE DO:
                /* prodfamilje */
                IF KampanjeTilbArtikkel.KampTilbArtId = 0 THEN
                    RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + "Prodfam: " + CHR(1) + 
                                      STRING(ProduktFamilie.ProdFamId) + " " + TRIM(ProduktFamilie.ProdFamNavn),"").
                ELSE
                    RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + "Artikel: " + CHR(1) + 
                                      STRING(ArtBas.Artikkelnr) + " " + ArtBas.beskr + " " + STRING(artbas.aktivert = TRUE AND artbas.ikasse = TRUE,"/INTE AKTIV ")  + getEAN(ArtBas.Artikkelnr),"").
                
                ASSIGN iRow     = iRow + 1.
                cPrisListe = getHighLow(KampanjeTilbArtikkel.ProdFamId,KampanjeTilbArtikkel.KampTilbArtId).

                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + (IF KampanjeTilbArtikkel.KampTilbArtId = 0 THEN "Högst: " ELSE "Enhpris: " )
                                   + CHR(1) + ENTRY(1,cPrisListe,CHR(1)) + CHR(2) + "LALIGN","").
                ASSIGN iRow     = iRow + 1.

                IF KampanjeTilbArtikkel.KampTilbArtId = 0 THEN DO:
                    RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Lägst: "
                                       + CHR(1) + ENTRY(1,cPrisListe,CHR(1)) + CHR(2) + "LALIGN","").
                    ASSIGN iRow     = iRow + 1.
                END.
                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Min: " + CHR(1) + STRING(KampanjeTilbArtikkel.KampTilbArtMinAntall) + CHR(2) + "LALIGN","").
                ASSIGN iRow     = iRow + 1.
                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Ant/save++: " + CHR(1) + STRING(KampanjeTilbArtikkel.KampTilbArtBelop) + CHR(2) + "LALIGN","").
                ASSIGN iRow     = iRow + 1.
                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Rabtyp: " + CHR(1) + getRabType(KampanjeTilbArtikkel.KampRabattTypeId),"").
                ASSIGN iRow     = iRow + 1.
/*                                                                                                                                                                    */
                IF KampanjeTilbArtikkel.KampTilbArtId = 0 THEN DO:
                    RUN CreateTT_Grid(iRow,4,iFirstDataCol,sp + sp + sp + sp + "Fammedlemmar: ","").
                    ASSIGN iRow     = iRow + 1.
                    FOR EACH ProduktFamMedlem OF ProduktFamilie NO-LOCK.
                        FIND artbas WHERE artbas.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr NO-LOCK NO-ERROR.
                        IF NOT AVAIL Artbas THEN DO:
                            RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + STRING(ProduktFamMedlem.ProdFamArtikkelNr) + CHR(1) + "Saknas","").
                            ASSIGN iRow     = iRow + 1.
                        END.
                        ELSE DO:
                            RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + "Artikel: " + CHR(1) + 
                                              STRING(ArtBas.Artikkelnr) + " " + ArtBas.beskr + " " + STRING(artbas.aktivert = TRUE AND artbas.ikasse = TRUE,"/INTE AKTIV ")  + getEAN(ArtBas.Artikkelnr),"").
                            ASSIGN iRow     = iRow + 1.
                            FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
                            IF NOT AVAIL artpris THEN DO:
                                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Enhpris: " + CHR(1) + "SAKNAS","").
                                ASSIGN iRow     = iRow + 1.
                            END.
                            ELSE DO:
                                RUN CreateTT_Grid(iRow,5,iFirstDataCol,sp + sp + sp + sp + sp + "Enhpris: " + CHR(1) + 
                                                  STRING(artpris.pris[IF artpris.tilbud THEN 2 ELSE 1],">>>>9.99") + CHR(2) + "LALIGN","").
                                ASSIGN iRow     = iRow + 1.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.


    ASSIGN chGrid:Rows = iRow.
    FOR EACH TT_Grid BY TT_Grid.iGridRow:
        ASSIGN iCol = 0.
        DO iCount = 1 TO NUM-ENTRIES(TT_Grid.cText,CHR(1)):
            ASSIGN iCol = IF iCol = 0 THEN TT_Grid.iGridCol ELSE iCol + 1
                   chGrid:Cell(flexcpText,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = ENTRY(1,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
            IF TT_Grid.lFel = TRUE THEN
                chGrid:Cell(flexcpBackColor,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = 255.
            IF NUM-ENTRIES(ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)) = 2 THEN DO:
                ASSIGN cPara = ENTRY(2,ENTRY(iCount,TT_Grid.cText,CHR(1)),CHR(2)).
                IF cPara = "RALIGN" THEN
                    ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignRightBottom.
                ELSE IF cPara = "LALIGN" THEN
                    ASSIGN chGrid:Cell(flexcpAlignment,TT_Grid.iGridRow,iCol,TT_Grid.iGridRow,iCol) = flexAlignLeftBottom.
            END.

        END.
    END.
    RUN DoGroup(4,1).
    FOR EACH TT_Grid BY TT_Grid.iGridRow:
        IF TT_Grid.iOutline > 0 THEN
            RUN DoGroup(TT_Grid.iGridRow,TT_Grid.iOutline).
    END.
    FOR EACH TT_Grid BY TT_Grid.iGridRow:
    IF TT_Grid.lFel = FALSE AND TT_Grid.iOutline = 3 THEN
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
    {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEAN C-Win 
FUNCTION getEAN RETURNS CHARACTER
  ( INPUT ipArtikkelnr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cKoder AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lOk AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cLengdKontroll AS CHARACTER  NO-UNDO.
  
  FOR EACH strekkode WHERE strekkode.artikkelnr = ipArtikkelnr NO-LOCK.
      cKoder = cKoder + (IF cKoder <> "" THEN "-" ELSE "") + strekkode.kode.
      IF lOk = FALSE AND (LENGTH(strekkode.kode) < 6 OR 
         LENGTH(strekkode.kode) = 8 OR 
         LENGTH(strekkode.kode) = 13) THEN
          lOk = TRUE.
  END.
  IF lOk = FALSE THEN DO:
      lFeil = TRUE.
      FIND bTT_Grid WHERE bTT_grid.iGridrow = iCurrRow NO-ERROR.
      IF AVAIL bTT_Grid THEN
           bTT_Grid.lFel = TRUE. 
  END.

  RETURN "  Streckkoder: " + STRING(lOk,"OK/FEL") + " " + cKoder.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHHdar C-Win 
FUNCTION getHHdar RETURNS CHARACTER
  ( INPUT cDagliste AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDagnamnListe AS CHARACTER  NO-UNDO.
  cDagnamnListe = "söndag,måndag,tisdag,onsdag,torsdag,fredag,lördag".
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DO ii = 1 TO NUM-ENTRIES(cDagListe):
      ENTRY(ii,cDagListe) = ENTRY(INT(ENTRY(ii,cDagListe)),cDagnamnliste).
  END.
  RETURN cDagliste.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHighLow C-Win 
FUNCTION getHighLow RETURNS CHARACTER
  ( INPUT dProdFamId AS DECIMAL,INPUT dKampTilbArtId AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dHigh AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dLow  AS DECIMAL    NO-UNDO.
    IF dKampTilbArtId <> 0 THEN DO:
        FIND FIRST artpris WHERE artpris.artikkelnr = dKampTilbArtId NO-LOCK NO-ERROR.
        IF AVAIL ArtPris THEN
            dHigh = Artpris.pris[IF artpris.tilbud THEN 2 ELSE 1].
    END.
    ELSE DO:
        dLow = 100000.
        FIND ProduktFamilie WHERE ProduktFamilie.ProdFamId = dProdFamId NO-LOCK NO-ERROR.
        FOR EACH ProduktFamMedlem OF ProduktFamilie no-lock.
            FIND FIRST artpris WHERE artpris.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL artpris THEN DO:
                IF Artpris.pris[IF artpris.tilbud THEN 2 ELSE 1] > dHigh THEN
                    dHigh = Artpris.pris[IF artpris.tilbud THEN 2 ELSE 1].
               IF Artpris.pris[IF artpris.tilbud THEN 2 ELSE 1] < dLow THEN
                   dLow = Artpris.pris[IF artpris.tilbud THEN 2 ELSE 1].
            END.
        END.
    END.
  RETURN STRING(dHigh,">>>>9.99") + CHR(1) + STRING(dLow,">>>>9.99").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRabType C-Win 
FUNCTION getRabType RETURNS CHARACTER
  ( INPUT iKampRabattTypeId AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND KampRabattType WHERE KampRabattType.KampRabattTypeId = iKampRabattTypeId NO-LOCK NO-ERROR.
  lFeil = NOT AVAIL KampRabattType.
  RETURN IF AVAIL KampRabattType THEN KampRabattType.KampRabattTypeNavn ELSE "FEL - SAKNAS".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
          ASSIGN cText = "KAMPANJ".
  END CASE.

  RETURN cText.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

