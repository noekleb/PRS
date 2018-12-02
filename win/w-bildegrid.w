&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE tBild NO-UNDO LIKE Bild.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*-----------------------------

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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
   DEFINE VAR wParent AS HANDLE NO-UNDO.
   DEFINE VAR wMenu   AS CHAR INIT "&Ta bort;DE,B&ild;BI,;,Wha&t ever;WE" NO-UNDO.
   DEFINE VAR wScreenSize    AS INTE INIT 1 NO-UNDO.
   define var wNumRecords    as int         no-undo.
&ELSE 
   DEFINE INPUT PARAMETER wParent     AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER wMenu       AS CHAR   NO-UNDO.
   DEFINE INPUT PARAMETER wScreenSize AS INTE   NO-UNDO.
   define input parameter wNumRecords as int    no-undo.
&ENDIF

/* Local Variable Definitions ---                                       */
DEFINE VAR chBild         AS COM-HANDLE NO-UNDO.
DEFINE VAR wFraCellnr     AS INTE INIT ? NO-UNDO.
DEFINE VAR wTopRow        AS INTE        NO-UNDO. /* hantering av scroll */
DEFINE VAR wSisteBildCell AS INTE INIT ? NO-UNDO.
DEFINE VAR wMDRow         AS INTE        NO-UNDO.
DEFINE VAR wMDCol         AS INTE        NO-UNDO.
DEFINE VAR wMenuStr       AS CHAR        NO-UNDO.
DEFINE VAR wMenuValg      AS INTE        NO-UNDO.
DEFINE VAR wError         AS LOGI        NO-UNDO.
DEFINE VAR wAntCol   AS INTE EXTENT 4 INIT [5,7,8,9] NO-UNDO.
DEFINE VAR WinWPix   AS INTE EXTENT 4 INIT [689,944,1075,1200] NO-UNDO.
DEFINE VAR WinHPix   AS INTE EXTENT 4 INIT [548,672,802,925] NO-UNDO.
DEFINE VAR CtrlWPix  AS INTE EXTENT 4 INIT [666,924,1055,1180] NO-UNDO.
DEFINE VAR CtrlHPix  AS INTE EXTENT 4 INIT [504,630,755,878] NO-UNDO.
def    var wRGB      as char no-undo.
DEFINE VARIABLE hJmfRutine AS HANDLE     NO-UNDO.
DEFINE VAR wColor AS INTE EXTENT 3 INIT [5197823,65280,16711680] NO-UNDO.

/*
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEF TEMP-TABLE tBild
&ELSE
    /*DEF SHARED TEMP-TABLE tBild*/
    DEFINE SHARED TEMP-TABLE tbild /* LIKE Bild */
&ENDIF
  FIELD ArtikkelNr AS DECIMAL   DECIMALS 2 FORMAT "zzzzzzzzzzzz9"
  FIELD BestNr     AS INTEGER   FORMAT "zzzzz9"
  FIELD Bild       AS CHARACTER FORMAT "X(30)"
  FIELD BildTxt    AS CHARACTER FORMAT "X(30)"
  FIELD CellNr     AS INTEGER
  FIELD Farg       AS INTEGER   LABEL "Farge"
  FIELD InnkjPris  AS DECIMAL   DECIMALS 2 FORMAT "-zz,zzz,zz9" LABEL ""
  FIELD LevArtNr   AS CHARACTER FORMAT "X(30)" LABEL "Lev.ArtikkelNr"
  FIELD LevNr      AS INTEGER   FORMAT "zzzzz9" LABEL "LevNr"
  FIELD LevTid     AS CHARACTER FORMAT "X(6)" LABEL "Lev.Tid"
  FIELD ListeLinje AS RECID
  FIELD OrdreNr    AS INTEGER   FORMAT "zzzzz9" LABEL "OrdreNr"
  FIELD Valutapris AS DECIMAL   DECIMALS 2 FORMAT "-zz,zzz,zz9" LABEL "Valutapris"
  FIELD VareKost   AS DECIMAL   DECIMALS 2 FORMAT "-zz,zzz,zz9" LABEL "Varekost"
  FIELD VgLopNr    AS CHARACTER FORMAT "X(10)" LABEL "Vg/LøpeNr".
*/

DEF BUFFER bFraBild FOR tBild.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-1 RECT-50 RECT-51 BUTTON-Ner ~
RADIO-SET-1 BUTTON-4 BUTTON-Upp B-Exit Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 COMBO-BOX-WinSize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BildText C-Win 
FUNCTION BildText RETURNS CHARACTER
  ( /* */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BildText2 C-Win 
FUNCTION BildText2 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Hex2Int C-Win 
FUNCTION Hex2Int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Minneserror C-Win 
FUNCTION Minneserror RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE IK AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIK AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Pop AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPop AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE ProgressBar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chProgressBar AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-4 
     LABEL "&Vis fargekoder..." 
     SIZE 23 BY 1.1.

DEFINE BUTTON BUTTON-Ner 
     IMAGE-UP FILE "icon/e-pilned":U NO-FOCUS
     LABEL "Ner" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON BUTTON-Upp 
     IMAGE-UP FILE "icon/e-pilopp":U NO-FOCUS
     LABEL "Upp" 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE COMBO-BOX-WinSize AS CHARACTER FORMAT "X(256)":U 
     LABEL "Upplösning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "800 X 600","1024 X 768","1152 X 864","1280 X 1024" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "adeicon\del-au":U
     SIZE 5 BY 1.19.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sett inn", 1,
"Bytt plass", 2
     SIZE 27 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185 BY .1.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Ner AT ROW 1.38 COL 19
     RADIO-SET-1 AT ROW 1.38 COL 30 NO-LABEL
     COMBO-BOX-WinSize AT ROW 1.38 COL 77 COLON-ALIGNED
     BUTTON-4 AT ROW 1.38 COL 99
     BUTTON-Upp AT ROW 1.38 COL 14
     B-Exit AT ROW 1.38 COL 180.2
     Btn_Help AT ROW 1.38 COL 175.2
     IMAGE-1 AT ROW 1.38 COL 3
     RECT-50 AT ROW 1.14 COL 1
     RECT-51 AT ROW 2.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 186.4 BY 31.1.

DEFINE FRAME FRAME-Last
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 10.29
         SIZE 95 BY 2.38
         TITLE "Leser inn bilder, vent litt....".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tBild T "SHARED" NO-UNDO Temp-DB Bild
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Bildhantering"
         HEIGHT             = 31.05
         WIDTH              = 186.8
         MAX-HEIGHT         = 31.52
         MAX-WIDTH          = 218.8
         VIRTUAL-HEIGHT     = 31.52
         VIRTUAL-WIDTH      = 218.8
         MAX-BUTTON         = no
         RESIZE             = no
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Last:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-WinSize IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-Last
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-Last:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Grid ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.91
       COLUMN          = 3
       HEIGHT          = 27.38
       WIDTH           = 183
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Pop ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 12.91
       COLUMN          = 113
       HEIGHT          = 4.76
       WIDTH           = 20
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME IK ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 17.91
       COLUMN          = 106
       HEIGHT          = 6.19
       WIDTH           = 27
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME ProgressBar ASSIGN
       FRAME           = FRAME FRAME-Last:HANDLE
       ROW             = 1.24
       COLUMN          = 2
       HEIGHT          = .95
       WIDTH           = 93
       HIDDEN          = no
       SENSITIVE       = yes.
/* Grid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
/* Pop OCXINFO:CREATE-CONTROL from: {7823A620-9DD9-11CF-A662-00AA00C066D2} type: IEPOP */
/* IK OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* ProgressBar OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */
      Grid:MOVE-AFTER(BUTTON-4:HANDLE IN FRAME DEFAULT-FRAME).
      Pop:MOVE-AFTER(FRAME FRAME-Last:HANDLE).
      IK:MOVE-AFTER(Pop).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bildhantering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bildhantering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* Vis fargekoder... */
DO:
  run d-bstatusfarger.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ner C-Win
ON CHOOSE OF BUTTON-Ner IN FRAME DEFAULT-FRAME /* Ner */
DO:
    RUN GridScroll("Ner":U).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Upp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Upp C-Win
ON CHOOSE OF BUTTON-Upp IN FRAME DEFAULT-FRAME /* Upp */
DO:
    RUN GridScroll("Upp":U).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-WinSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-WinSize C-Win
ON VALUE-CHANGED OF COMBO-BOX-WinSize IN FRAME DEFAULT-FRAME /* Upplösning */
DO:
/*    RUN FixWinSize.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Grid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.MouseDown
PROCEDURE Grid.VSFlexGrid.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     Håller reda på vilken cell/bild vi kommer från
               (vilken bild vi skall flytta)

  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       Tillåter MouseDown från bild eller tillhörande text
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-Button  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-Shift   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-X       AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER p-Y       AS DECIMAL NO-UNDO.
    DEFINE             VAR wMouseRow AS INTEGER NO-UNDO. 
    IF p-X < 0 OR p-X > chGrid:ClientWidth OR p-Y < 0 OR p-Y > chGrid:ClientHeight THEN
        RETURN.
    ASSIGN wMouseRow = IF chGrid:MouseRow MOD 2 = 0 THEN
                           chGrid:MouseRow ELSE chGrid:MouseRow - 1
           wFraCellnr = (chGrid:MouseCol + chGrid:Cols * wMouseRow) -
                        (wMouseRow / 2 * chGrid:Cols)
           wMDRow     = wMouseRow
           wMDCol     = chGrid:MouseCol.
    FIND bFraBild WHERE bFraBild.Cellnr = wFraCellnr NO-ERROR.
    IF NOT AVAIL bFraBild THEN DO:
        ASSIGN wFraCellnr = ?.
        RETURN.
    END.
    ASSIGN  chGrid:Row = wMDRow
            chGrid:Col = wMDCol.
    &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN          
        if valid-handle(wParent) then
          RUN SkoReposition IN wParent (ROWID(bFraBild)).
    &ENDIF.
    IF p-Button = 1 THEN DO:
        ASSIGN chGrid:MousePointer = 5.
    END.
    ELSE IF p-Button = 2 THEN DO:
        ASSIGN wMenuValg = ?.
        chPop:PopUp().
        PROCESS EVENTS.
        IF wMenuValg <> ? THEN DO:
            IF ENTRY(2,ENTRY(wMenuValg,wMenuStr),";") = "DE" THEN 
                DO:
                MESSAGE "Skall bilden " + bFraBild.Bild + " tas bort?" SKIP(1)
                         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                         TITLE "" UPDATE choice AS LOGICAL.
                IF choice = TRUE THEN
                    RUN DeleteBild(wFraCellnr).
                END.
            ELSE IF ENTRY(2,ENTRY(wMenuValg,wMenuStr),";") = "SAMMENLIGN" THEN 
                DO:
                   IF NOT VALID-HANDLE(hJmfRutine) THEN
                       RUN w-bildejmf.w PERSISTENT SET hJmfRutine.
                   IF NOT bFrabild.BestNr > 0 THEN
                       RUN NyArtBas IN hJmfRutine (bFraBild.ArtikkelNr).
                   ELSE
                       RUN NyBestHode IN hJmfRutine (bFraBild.BestNr).
                END.
            ELSE DO:
                &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN 
                  if valid-handle(wParent) then         
                    do:
                      RUN MenyValg IN wParent (ROWID(bFraBild),ENTRY(2,ENTRY(wMenuValg,wMenuStr),";")).
                      run OppdatCelle. /* Sjekker status. setter ev. ny tekst og farge på cellen. */
                    end.
                  ELSE DO:
                      RUN MenyValg (ROWID(bFraBild),ENTRY(2,ENTRY(wMenuValg,wMenuStr),";")).
                      run OppdatCelle. /* Sjekker status. setter ev. ny tekst og farge på cellen. */
                  END.
                    
                  APPLY "ENTRY" TO Grid.
                &ENDIF.
            END.
        END.
        ASSIGN wFraCellnr = ?.
    END.
    PROCESS EVENTS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.MouseUp
PROCEDURE Grid.VSFlexGrid.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     Håller reda på vilken cell/bild vi kommer till
               (vilken bild vi skall flytta till)
  Parameters:  Required for OCX.
    Button
    Shift
    X 
    Y
  Notes:       Tillåter MouseDown till bild eller tillhörande text
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-Button AS INTEGER         NO-UNDO.
  DEFINE INPUT PARAMETER p-Shift  AS INTEGER         NO-UNDO.
  DEFINE INPUT PARAMETER p-X      AS DECIMAL         NO-UNDO.
  DEFINE INPUT PARAMETER p-Y      AS DECIMAL         NO-UNDO.
  DEFINE VAR wTilCellnr           AS INTE            NO-UNDO.
  DEFINE VAR wTmpCellnr           AS INTE INIT 999   NO-UNDO.
  DEFINE VAR wMURow               AS INTE            NO-UNDO.
  DEFINE VAR wMUCol               AS INTE            NO-UNDO.
  DEFINE VAR wFraTxt              AS CHAR            NO-UNDO.
  DEFINE VAR wTilTxt              AS CHAR            NO-UNDO.
  DEFINE VAR wMouseRow            AS INTE            NO-UNDO. 

  IF p-X >= -15  AND p-X <= 300  AND 
     p-Y >= -465 AND p-Y <= -195 AND wFraCellnr <> ? THEN DO:
      RUN DeleteBild(wFraCellnr).
      ASSIGN wFraCellnr           = ?
             chGrid:MousePointer = 0.
      RETURN.
  END.

  ASSIGN wTopRow    = chGrid:TopRow
         wMouseRow  = IF chGrid:MouseRow MOD 2 = 0 THEN
                          chGrid:MouseRow ELSE chGrid:MouseRow - 1
         wTilCellnr = (chGrid:MouseCol + chGrid:Cols * wMouseRow) -
                      (wMouseRow / 2 * chGrid:Cols)
         wMURow     = wMouseRow
         wMUCol     = chGrid:MouseCol.
  IF wTilCellnr = wFraCellnr OR wFraCellnr = ? OR
                  p-X < 0 OR p-X > chGrid:ClientWidth OR 
                  p-Y < 0 OR p-Y > chGrid:ClientHeight THEN DO:
      ASSIGN wFraCellnr           = ?
             chGrid:MousePointer = 0.
      RETURN.
  END.
  ASSIGN chgrid:Redraw = FALSE.

  FIND tBild WHERE Cellnr = wTilCellnr NO-ERROR.
  IF NOT AVAIL tBild THEN DO:
      ASSIGN wFraCellnr           = ?
             wTilCellnr           = ?
             chGrid:Row          = wMDRow
             chGrid:Col          = wMDCol
             chGrid:MousePointer = 0
             chgrid:Redraw       = TRUE. 
      RETURN.
  END.

  IF RADIO-SET-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN DO:
      RUN InsertBild(wFraCellnr,wTilCellnr).
      ASSIGN chGrid:MousePointer = 0
             chgrid:Redraw       = TRUE
             wFraCellnr           = ?.
      chGrid:Refresh().
  END.
  ELSE DO:    
      FIND bFraBild WHERE bFraBild.Cellnr = wFraCellnr.

      ASSIGN
        chIK:Picture = chGrid:Cell(3,wMURow,wMUCol,wMURow,wMUCol)
        chGrid:Cell(3,wMURow,wMUCol,wMURow,wMUCol) =  chGrid:Cell(3,wMDRow,wMDCol,wMDRow,wMDCol)
        chGrid:Cell(0,wMURow + 1,wMUCol,wMURow + 1,wMUCol) =  chGrid:Cell(0,wMDRow + 1,wMDCol,wMDRow + 1,wMDCol)
        chGrid:Cell(6,wMURow + 1,wMUCol,wMURow + 1,wMUCol) =  chGrid:Cell(6,wMDRow + 1,wMDCol,wMDRow + 1,wMDCol)
        chGrid:Cell(3,wMDRow,wMDCol,wMDRow,wMDCol) =  chIK:Picture
        chGrid:Cell(0,wMDRow + 1,wMDCol,wMDRow + 1,wMDCol) =  BildText() /* tBild.Bildtxt */
        chGrid:Cell(6,wMDRow + 1,wMDCol,wMDRow + 1,wMDCol) =  tBild.Farg
        bFraBild.CellNr      = wTmpCellnr
        tBild.CellNr         = wFraCellnr
        bFraBild.CellNr      = wTilCellnr
        chGrid:MousePointer = 0
        chgrid:Redraw       = TRUE
        wFraCellnr           = ?.

      chGrid:Refresh().
  END.
  &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN          
        FIND tBild WHERE tBild.Cellnr = wTilCellnr.
        if valid-handle(wParent) then
          RUN SkoReposition IN wParent (ROWID(tBild)).
  &ENDIF.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid C-Win OCX.Scroll
PROCEDURE Grid.VSFlexGrid.Scroll .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  IF chGrid:TopRow MOD 2 <> 0 THEN
       ASSIGN chGrid:TopRow = chGrid:TopRow + IF wTopRow < chGrid:Toprow THEN
                        1 ELSE -1
              wTopRow = chGrid:TopRow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pop C-Win OCX.Click
PROCEDURE Pop.IEPOP.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    item
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-item AS INTEGER NO-UNDO.
    ASSIGN wMenuValg = p-item.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       {&WINDOW-NAME}:HIDDEN = YES.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
    IF VALID-HANDLE(wParent) THEN
    DO:
        if can-do(wParent:INTERNAL-ENTRIES,"TaBortPoster") then
            run TaBortPoster in wParent.
        if can-do(wParent:INTERNAL-ENTRIES,"SettCelleNr") then
            run SettCelleNr in wParent.
    END.
    chGrid:CLEAR(0).
    IF VALID-HANDLE(chGrid) THEN
        RELEASE OBJECT chGrid NO-ERROR.
    IF VALID-HANDLE(Grid) THEN
        DELETE WIDGET Grid.
    IF VALID-HANDLE(chIK) THEN
        RELEASE OBJECT chIK   NO-ERROR.
    IF VALID-HANDLE(IK) THEN
        DELETE WIDGET IK.
    IF VALID-HANDLE(chPop) THEN
        RELEASE OBJECT chPop  NO-ERROR.
    IF VALID-HANDLE(Pop) THEN
        DELETE WIDGET Pop.
    IF VALID-HANDLE(chProgressBar) THEN
        RELEASE OBJECT chProgressBar NO-ERROR.
    IF VALID-HANDLE(ProgressBar) THEN
        DELETE WIDGET ProgressBar.
    IF VALID-HANDLE(chBild) THEN
        RELEASE OBJECT chBild NO-ERROR.
    ASSIGN chGrid        = ?
           chIK          = ?
           chPop         = ?
           chProgressBar = ?
           chBild        = ?.
/*

DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE IK AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIK AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Pop AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPop AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE ProgressBar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chProgressBar AS COMPONENT-HANDLE NO-UNDO.


*/



    IF VALID-HANDLE(hJmfRutine) THEN
        APPLY "CLOSE" TO hJmfRutine.
    RUN disable_UI.
  end.   
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Er tabellen tom, ska programmet ikke startes. */
find first tBild where
  tBild.CellNr <> ? no-error.
if not available tBild then
  return.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
       RUN CreateTemp. /* før test */
    &ENDIF
    RUN enable_UI.
    {lng.i} 

    RUN SettTittel ("").
    ASSIGN COMBO-BOX-WinSize:SCREEN-VALUE = ENTRY(wScreenSize,COMBO-BOX-WinSize:LIST-ITEMS)
          {&WINDOW-NAME}:HIDDEN = NO.

    RUN FixWinSize.

    RUN LesInnBild("Ny").
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

OCXFile = SEARCH( "w-bildegrid.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chGrid = Grid:COM-HANDLE
    UIB_S = chGrid:LoadControls( OCXFile, "Grid":U)
    Grid:NAME = "Grid":U
    chIK = IK:COM-HANDLE
    UIB_S = chIK:LoadControls( OCXFile, "IK":U)
    IK:NAME = "IK":U
    chPop = Pop:COM-HANDLE
    UIB_S = chPop:LoadControls( OCXFile, "Pop":U)
    Pop:NAME = "Pop":U
    chProgressBar = ProgressBar:COM-HANDLE
    UIB_S = chProgressBar:LoadControls( OCXFile, "ProgressBar":U)
    ProgressBar:NAME = "ProgressBar":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-bildegrid.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTemp C-Win 
PROCEDURE CreateTemp :
/*------------------------------------------------------------------------------
  Purpose: I test, skapa temp-tableposter     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wAnt AS INTE NO-UNDO.
    DO wAnt = 1 to 61:
        CREATE tBild.
        Assign Cellnr  = wAnt - 1
               Bild    = "bilder\" + STRING(wAnt MOD 9 + 1) + ".bmp"
               Bildtxt = Bild
               Farg    = IF Cellnr < 10 THEN wColor[1] ELSE IF Cellnr < 20 THEN
                            wColor[2] ELSE wColor[3].
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteBild C-Win 
PROCEDURE DeleteBild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wSlettCellnr AS INTE NO-UNDO.
    DEFINE VAR wCell AS INTE INIT 0 NO-UNDO. /* init 0 för att vara tydlig */
    DEFINE VAR wRow  AS INTE NO-UNDO.
    DEFINE VAR wCol  AS INTE NO-UNDO.
    DEFINE VAR wRow2 AS INTE NO-UNDO.
    DEFINE VAR wCol2 AS INTE NO-UNDO.

    FIND tBild WHERE tBild.Cellnr = wSlettCellnr NO-ERROR.
    IF NOT AVAIL tBild THEN DO:
        MESSAGE "Felaktig 'Delete'. Bilden finns inte." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    ASSIGN tBild.Cellnr = ?.
    FOR EACH tBild WHERE tBild.Cellnr <> ?:
        ASSIGN tBild.Cellnr = wCell
               wCell = wCell + 1.
    END.
    IF wSlettCellnr = wSisteBildCell THEN DO:
        ASSIGN wRow2 = 2 * (wSlettCellnr - wSlettCellnr MOD chGrid:Cols) / chGrid:Cols
               wCol2 = wSlettCellnr MOD chGrid:Cols.
    END.
    ELSE DO:
        FOR EACH tBild WHERE tBild.Cellnr <> ? AND tBild.Cellnr >= wSlettCellnr:
            ASSIGN wRow = 2 * (tBild.Cellnr - tBild.Cellnr MOD chGrid:Cols) / chGrid:Cols
                   wCol = tBild.Cellnr MOD chGrid:Cols
                   wRow2 = IF wCol = chGrid:Cols - 1 THEN wRow + 2 ELSE wRow
                   wCol2 = IF wCol = chGrid:Cols - 1 THEN 0 ELSE wCol + 1
                   chGrid:Cell(3,wRow,wCol,wRow,wCol) = chGrid:Cell(3,wRow2,wCol2,wRow2,wCol2)
                   chGrid:Cell(0,wRow + 1,wCol,wRow + 1,wCol) = chGrid:Cell(0,wRow2 + 1,wCol2,wRow2 + 1,wCol2)
                   chGrid:Cell(6,wRow + 1,wCol,wRow + 1,wCol) = chGrid:Cell(6,wRow2 + 1,wCol2,wRow2 + 1,wCol2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                Minneserror().
                ASSIGN wError = TRUE.
                LEAVE.
            END.
        END.
    END.
    IF wError = TRUE THEN DO:
        APPLY "WINDOW-CLOSE" TO C-Win.
        RETURN.
    END.
    ASSIGN wSisteBildCell = wCell - 1
           chGrid:Row = wRow2
           chGrid:Col = wCol2.
    chGrid:Clear(2,0).
    ASSIGN chGrid:Cell(0,wRow2 + 1,wCol2,wRow2 + 1,wCol2) = ""
           chGrid:Cell(6,wRow2 + 1,wCol2,wRow2 + 1,wCol2) = "&HFFFFFF"
           chGrid:Row  = 0
           chGrid:Col  = 0
           chGrid:Rows = 2 * ((wSisteBildCell - wSisteBildCell MOD chGrid:Cols) / chGrid:Cols + 1)
           wTopRow      = 0.
    &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN          
        RUN BrowseRefresh IN wParent.
    &ENDIF.
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
  DISPLAY RADIO-SET-1 COMBO-BOX-WinSize 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-1 RECT-50 RECT-51 BUTTON-Ner RADIO-SET-1 BUTTON-4 BUTTON-Upp 
         B-Exit Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Last}
  VIEW C-Win.
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
    DEF VAR wIdx AS INTE NO-UNDO.
    
    ASSIGN wMenu = wMenu + ",;,Sammenlign;SAMMENLIGN".
    
    DO wIdx = 1 TO NUM-ENTRIES(wMenu):
        ASSIGN wMenuStr = IF ENTRY(1,ENTRY(wIdx,wMenu),";") = "" THEN
                              wMenuStr 
                          ELSE IF wMenuStr = "" THEN
                              ENTRY(wIdx,wMenu) 
                          ELSE wMenuStr + "," + ENTRY(wIdx,wMenu).
        chPop:AddItem(ENTRY(1,ENTRY(wIdx,wMenu),";")).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixWinSize C-Win 
PROCEDURE FixWinSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:  
      ASSIGN {&WINDOW-NAME}:WIDTH-PIXELS  = WinWPix[wScreenSize]
             {&WINDOW-NAME}:HEIGHT-PIXELS = WinHPix[wScreenSize]
             {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS  = WinWPix[wScreenSize]
             {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = WinHPix[wScreenSize]
             FRAME DEFAULT-FRAME:WIDTH-PIXELS  = WinWPix[wScreenSize]
             FRAME DEFAULT-FRAME:HEIGHT-PIXELS  = WinHPix[wScreenSize]
             Grid:WIDTH-PIXELS  = CtrlWPix[wScreenSize]
             Grid:HEIGHT-PIXELS  = CtrlHPix[wScreenSize]
             chGrid:Cols = wAntCol[wScreenSize].
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GridScroll C-Win 
PROCEDURE GridScroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wRiktning AS CHAR NO-UNDO.
    IF wRiktning = "Ner" THEN DO:
      IF  chGrid:BottomRow < chGrid:Rows - 1 THEN
           ASSIGN chGrid:TopRow = chGrid:TopRow + 2.
    END.
    ELSE IF wRiktning = "Upp" THEN DO:
        IF chGrid:TopRow <> 0 THEN
           ASSIGN chGrid:TopRow = chGrid:TopRow - 2.
    END.
    ASSIGN wTopRow = chGrid:TopRow.
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
    DEF VAR wCol AS INTE NO-UNDO.
    FIND LAST tBild WHERE tBild.Cellnr <> ?.
    ASSIGN wSisteBildCell           = tBild.Cellnr
           chGrid                   = chGrid:vsFlexGrid
/*            chCtrlFrame              =  chCtrlFrame:vsFlexGrid */
           chgrid:Cols              = wAntCol[wScreenSize]
           chgrid:Rows              = 1 
           chgrid:FixedRows         = 0
           chgrid:FixedCols         = 0
           chgrid:BorderStyle       = 1
           chgrid:AllowBigSelection = FALSE
           chgrid:AllowSelection    = FALSE
           chgrid:Redraw            = FALSE
           chgrid:GridLineWidth     = 1
           chgrid:GridLines         = 1
           chgrid:ScrollBars        = 2
           chgrid:FocusRect         = 3
           chIK                     = chIK:Picbuf
           chIK:AutoScale           = TRUE
           chPop                    = chPop:IEPOP
           chProgressBar            = chProgressBar:ProgressBar.
    RUN FixPopUp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InsertBild C-Win 
PROCEDURE InsertBild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wFra   AS INTE NO-UNDO.
  DEFINE INPUT PARAMETER wTil   AS INTE NO-UNDO.
  DEFINE VAR             wFRow  AS INTE NO-UNDO.
  DEFINE VAR             wFCol  AS INTE NO-UNDO.
  DEFINE VAR             wTRow  AS INTE NO-UNDO.
  DEFINE VAR             wTCol  AS INTE NO-UNDO.
  DEFINE VAR             wCount AS INTE NO-UNDO.
  DEFINE VAR             wTxt   AS CHAR NO-UNDO.
  DEFINE VAR             wFarg  AS INTE NO-UNDO.

  ASSIGN wFRow = 2 * (wFra - wFra MOD chGrid:Cols) / chGrid:Cols
         wFCol = wFra MOD chGrid:Cols.
  FIND tBild WHERE tBild.Cellnr = wFra.
  ASSIGN tBild.Cellnr = 999.
  WBLOCK: DO:
    IF wFra > wTil THEN DO:
        chIK:Clear(2).
        ASSIGN chIK:Picture = chGrid:Cell(3,wFRow,wFCol,wFRow,wFCol)
               wTxt          = chGrid:Cell(0,wFRow + 1,wFCol,wFRow + 1,wFCol)
               wFarg         = chGrid:Cell(6,wFRow + 1,wFCol,wFRow + 1,wFCol).
        DO wCount = wFra - 1 TO wTil BY -1:
            FIND tBild WHERE tBild.Cellnr = wCount.
            ASSIGN tBild.Cellnr = wCount + 1
                   wFRow = 2 * (wCount - wCount MOD chGrid:Cols) / chGrid:Cols
                   wFCol = wCount MOD chGrid:Cols
                   wTRow = 2 * ((wCount + 1) - (wCount + 1) MOD chGrid:Cols) / chGrid:Cols
                   wTCol = (wCount + 1) MOD chGrid:Cols
                   chGrid:Cell(3,wTRow,wTCol,wTRow,wTCol) =
                              chGrid:Cell(3,wFRow,wFCol,wFRow,wFCol)
                   chGrid:Cell(0,wTRow + 1,wTCol,wTRow + 1,wTCol) =
                              chGrid:Cell(0,wFRow + 1,wFCol,wFRow + 1,wFCol)
                   chGrid:Cell(6,wTRow + 1,wTCol,wTRow + 1,wTCol) =
                              chGrid:Cell(6,wFRow + 1,wFCol,wFRow + 1,wFCol) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                Minneserror().
                ASSIGN wError = TRUE.
                LEAVE WBLOCK.
            END.
        END.
        FIND tBild WHERE tBild.Cellnr = 999.
        ASSIGN tBild.Cellnr = wTil
               wFRow = 2 * (wTil - wTil MOD chGrid:Cols) / chGrid:Cols
               wFCol = wTil MOD chGrid:Cols
               chGrid:Cell(3,wFRow,wFCol,wFRow,wFCol) = chIK:Picture
               chGrid:Cell(0,wFRow + 1,wFCol,wFRow + 1,wFCol) = wTxt
               chGrid:Cell(6,wFRow + 1,wFCol,wFRow + 1,wFCol) = wFarg NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            Minneserror().
            ASSIGN wError = TRUE.
            LEAVE WBLOCK.
        END.
    END.
    ELSE DO:
        chIK:Clear(2).
        ASSIGN chIK:Picture = chGrid:Cell(3,wFRow,wFCol,wFRow,wFCol)
               wTxt          = chGrid:Cell(0,wFRow + 1,wFCol,wFRow + 1,wFCol)
               wFarg         = chGrid:Cell(6,wFRow + 1,wFCol,wFRow + 1,wFCol).
        DO wCount = wFra + 1 TO wTil:
            FIND tBild WHERE tBild.Cellnr = wCount.
            ASSIGN tBild.Cellnr = wCount - 1
                   wFRow = 2 * (wCount - wCount MOD chGrid:Cols) / chGrid:Cols
                   wFCol = wCount MOD chGrid:Cols
                   wTRow = 2 * ((wCount - 1) - (wCount - 1) MOD chGrid:Cols) / chGrid:Cols
                   wTCol = (wCount - 1) MOD chGrid:Cols
                   chGrid:Cell(3,wTRow,wTCol,wTRow,wTCol) =
                              chGrid:Cell(3,wFRow,wFCol,wFRow,wFCol)
                   chGrid:Cell(0,wTRow + 1,wTCol,wTRow + 1,wTCol) =
                              chGrid:Cell(0,wFRow + 1,wFCol,wFRow + 1,wFCol)
                   chGrid:Cell(6,wTRow + 1,wTCol,wTRow + 1,wTCol) =
                              chGrid:Cell(6,wFRow + 1,wFCol,wFRow + 1,wFCol) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                Minneserror().
                ASSIGN wError = TRUE.
                LEAVE WBLOCK.
            END.
        END.
        FIND tBild WHERE tBild.Cellnr = 999.
        ASSIGN tBild.Cellnr = wTil
               wFRow = 2 * (wTil - wTil MOD chGrid:Cols) / chGrid:Cols
               wFCol = wTil MOD chGrid:Cols
               chGrid:Cell(3,wFRow,wFCol,wFRow,wFCol) = chIK:Picture
               chGrid:Cell(0,wFRow + 1,wFCol,wFRow + 1,wFCol) = wTxt
               chGrid:Cell(6,wFRow + 1,wFCol,wFRow + 1,wFCol) = wFarg NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            Minneserror().
            ASSIGN wError = TRUE.
            LEAVE WBLOCK.
        END.
    END.
  END.
  IF wError = TRUE THEN
      APPLY "WINDOW-CLOSE" TO C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnBild C-Win 
PROCEDURE LesInnBild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wTyp AS CHAR NO-UNDO.
  DEF VAR wStartCell AS INTE NO-UNDO.
  DEF VAR wSisteCell AS INTE NO-UNDO.
  DEF VAR wGridRow   AS INTE NO-UNDO.
  DEF VAR wRow AS INTE NO-UNDO.
  DEF VAR wCol AS INTE NO-UNDO.

  FIND LAST tBild WHERE tBild.Cellnr <> ?. 
  ASSIGN wSisteCell = tBild.Cellnr
         chGrid:Redraw = FALSE.
  IF wTyp = "Ny" THEN DO:
      ASSIGN wStartCell = 0.
      chGrid:Clear(0,0).
  END.
  ELSE DO:
      ASSIGN wStartCell = IF tBild.Cellnr > wSisteBildCell THEN wSisteBildCell + 1 ELSE 0.
  END.
      ASSIGN chGrid:Rows = 2 * ((tBild.Cellnr - tBild.Cellnr MOD chGrid:Cols) / chGrid:Cols + 1).
  DO wCol = 0 TO chGrid:Cols - 1:
      chGrid:ColWidth(wCol) = 1936.
  END.
  DO wRow = 0 TO chGrid:Rows - 1:
      chGrid:RowHeight(wRow) = IF wRow MOD 2 = 0 THEN 1452 ELSE 420.
  END.

  if wSisteCell > 1 then
    do:
      assign
        chProgressBar:min   = 1 
        chProgressBar:max   = wSisteCell.
      view frame FRAME-Last.    
    end.
  
  FOR EACH tBild WHERE tBild.Cellnr <> ? AND tBild.Cellnr >= wStartCell:
      chIK:Clear(2).
      ASSIGN chIK:FileName = tBild.Bild.
      IF SEARCH(chIK:FileName) <> ? THEN
          chIK:Load().
/*       IF tBild.Cellnr = 1 THEN */
/*           MESSAGE chIK:XResolution SKIP          */
/*                   chIK:YResolution               */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      chBild = chIK:Picture.
      ASSIGN chGrid:Row = 2 * (tBild.Cellnr - tBild.Cellnr MOD chGrid:Cols) / chGrid:Cols
             chGrid:Col = tBild.Cellnr MOD chGrid:Cols
             chGrid:CellPictureAlignment = 9
             chGrid:CellPicture          = chBild
/*              chGrid:CellPicture          = chIK:Picture */
             chGrid:Row                  = chGrid:Row + 1
             chGrid:Text                 = BildText() /* tBild.Bild */ 
             chGrid:Cell(6,chGrid:Row,chGrid:Col,chGrid:Row,chGrid:Col) = 
                                            tBild.Farg NO-ERROR.
      RELEASE OBJECT chBild.
      if wSisteCell > 1 then
        do:
          if tBild.CellNr modulo 5 = 0 and tBild.CellNr > 0 then        
            chProgressBar:value = tBild.CellNr.
        end.

      IF ERROR-STATUS:ERROR THEN DO:
          Minneserror().
          ASSIGN wError = TRUE.
          LEAVE.
      END.
  END.
  hide frame FRAME-Last no-pause.
  RELEASE OBJECT chProgressBar.

  /*
  IF wError = TRUE THEN DO:
      APPLY "WINDOW-CLOSE" TO C-Win.
      RETURN.
  END.
  */
  ASSIGN chGrid:Row    = 0
         chGrid:Col    = 0
         chGrid:TopRow = 0
         wTopRow        = 0
         wSisteBildCell = wSisteCell
         chGrid:Redraw = TRUE.
  chGrid:Refresh().
  APPLY "ENTRY" TO Grid.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenyValg C-Win 
PROCEDURE MenyValg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wRowId      AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER wMenuAction AS CHAR  NO-UNDO.    
    def var wTekst as char.

    run kollgridmeny.p (wMenuAction, STRING(bFraBild.ArtikkelNr), output wTekst).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatCelle C-Win 
PROCEDURE OppdatCelle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if not available bFraBild then
    do:
      message "Ingen bFraBild tilgjengelig." view-as alert-box.
      return.
    end.

  FIND CURRENT bFraBild.
  
  ASSIGN chGrid:Row = (2 * (bFraBild.Cellnr - bFraBild.Cellnr MOD chGrid:Cols) / chGrid:Cols) + 1
         chGrid:Col = bFraBild.Cellnr MOD chGrid:Cols
         chGrid:Text                 = BildText2() /* bFraBild.Bild */
         chGrid:Cell(6,chGrid:Row,chGrid:Col,chGrid:Row,chGrid:Col) = 
                                        bFraBild.Farg NO-ERROR.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettTittel C-Win 
PROCEDURE SettTittel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTittel AS CHAR NO-UNDO.

  ASSIGN
      C-Win:TITLE = "Bildehåndtering" + " " + pcTittel
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UppNerEnaDis C-Win 
PROCEDURE UppNerEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BildText C-Win 
FUNCTION BildText RETURNS CHARACTER
  ( /* */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN tBild.TekstRad1 + CHR(13) + 
         tBild.TekstRad2.
/*   RETURN tBild.BildTxt + CHR(13) +                               */
/*          string(tBild.ValutaPris) + " " +                        */
/*          string(tBild.VareKost) + " " +                          */
/*          string(tBild.InnkjPris).   /* Function return value. */ */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BildText2 C-Win 
FUNCTION BildText2 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN bFraBild.TekstRad1 + CHR(13) + 
         bFraBild.TekstRad2.
/*                                                                     */
/*   RETURN bFraBild.BildTxt + CHR(13) +                               */
/*          string(bFraBild.ValutaPris) + " " +                        */
/*          string(bFraBild.VareKost) + " " +                          */
/*          string(bFraBild.InnkjPris).   /* Function return value. */ */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Hex2Int C-Win 
FUNCTION Hex2Int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(ENTRY(3,wRGB)) * 65536 +
         INT(ENTRY(2,wRGB)) * 256   +
         INT(ENTRY(1,wRGB)).         

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Minneserror C-Win 
FUNCTION Minneserror RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  MESSAGE "Minneområdet som benyttes for lagring av bilder er fullt!" SKIP
          "Bilder som ikke er lest inn er markert med hvite felt i bildegriden." SKIP
          "Du bør avslutte hele sesjonen. Dvs starte om SkoTex."
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

