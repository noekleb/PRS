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

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VARIABLE iButik          LIKE Butiker.butik INIT 1 NO-UNDO.
    DEFINE VARIABLE rArtBasRowid    AS ROWID           NO-UNDO.
    DEFINE VARIABLE cMottakstr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBrukteStr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lHeleModell     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cArtikkelEti    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEtiketter      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAntallEti      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIndividNr      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iIndividBatchNr AS INTEGER    NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER  iButik       LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT PARAMETER  rArtBasRowid AS ROWID           NO-UNDO.
    DEFINE INPUT  PARAMETER cMottakstr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cBrukteStr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lHeleModell  AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER cArtikkelEti AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cEtiketter   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cAntallEti   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cIndividNr   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER iIndividBatchNr AS INTEGER    NO-UNDO.
&ENDIF


/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cStorlekar AS CHARACTER  NO-UNDO.
DEF VAR wAktivStrl          AS CHAR NO-UNDO.
DEFINE VAR iCol             AS INTE NO-UNDO.
DEFINE VAR wGridInitialized AS LOGI NO-UNDO.
DEFINE VAR wSHC#            AS INTE INIT 18 NO-UNDO. /* Antal hela synliga Cols */
DEFINE VAR iCL              AS INTEGER    NO-UNDO.
DEFINE VARIABLE iStrTypeId  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cArtNrListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dModellNr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE hsuperSkapaIndivid AS HANDLE     NO-UNDO.

DEFINE VARIABLE lFortsett AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMsgTekst AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hKalkyle AS HANDLE     NO-UNDO.
DEF VAR h_PrisKo           AS HANDLE NO-UNDO.
DEFINE VARIABLE lVisTG AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cEanSperrKjedeLev AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iRed   AS INTEGER  INIT 255   NO-UNDO.


/* Temp-Table Definitions ---                                       */

DEFINE TEMP-TABLE TT_Vare
    FIELD iRadNr     AS INTEGER
    FIELD Artikkelnr LIKE ArtBas.ArtikkelNr
    FIELD Farbeskr   LIKE Farg.farbeskr
    FIELD Varekost   AS DECIMAL
    FIELD LevNr      LIKE ArtBas.LevNr
    FIELD Vg         LIKE ArtBas.Vg   
    FIELD LopNr      LIKE ArtBas.LopNr
    FIELD OkStr      AS CHAR
    INDEX Radnr iRadnr.


DEFINE BUFFER clButiker FOR Butiker.

{runlib.i}

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Ok RECT-Skille 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikk FI-ArtBeskr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DataIcell C-Win 
FUNCTION DataIcell RETURNS LOGICAL
  ( INPUT iCol AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstEnabled C-Win 
FUNCTION getFirstEnabled RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextCol C-Win 
FUNCTION getNextCol RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOKstr C-Win 
FUNCTION getOKstr RETURNS CHARACTER
  ( INPUT dArtikkelnr AS DECIMAL, INPUT iLevnr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
        (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Registrerat C-Win 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD strOK C-Win 
FUNCTION strOK RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE vsFlexGrid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chvsFlexGrid AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE FI-ArtBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varebeskr." 
     VIEW-AS FILL-IN 
     SIZE 48.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 48.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-Skille
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 208 BY .24.

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett" 
     SIZE 12 BY 1.1.

DEFINE VARIABLE FI-SperrLbl AS CHARACTER FORMAT "X(256)":U INITIAL "Rødmarkerte celler mangler EAN-koder. Originale EAN koder må registreres:" 
      VIEW-AS TEXT 
     SIZE 72 BY .62 NO-UNDO.

DEFINE VARIABLE FI-SperrTxt AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL "Alt-L - Lagre" 
      VIEW-AS TEXT 
     SIZE 37 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-Skille2
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 207.4 BY .24.

DEFINE VARIABLE TG-Negativ AS LOGICAL INITIAL no 
     LABEL "Negativ" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VisSkjul AS LOGICAL INITIAL no 
     LABEL "Vis alle str" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Ok AT ROW 1.33 COL 204.2 NO-TAB-STOP 
     FI-Butikk AT ROW 1.43 COL 17 COLON-ALIGNED
     FI-ArtBeskr AT ROW 2.43 COL 17 COLON-ALIGNED
     RECT-Skille AT ROW 3.81 COL 1.6
     SPACE(0.00) SKIP(7.57)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

DEFINE FRAME FRAME-Inlev
     TG-VisSkjul AT ROW 1.71 COL 164.2
     TG-Negativ AT ROW 1.71 COL 194.2
     Btn_OK AT ROW 3.1 COL 196.2
     BUTTON-Slett AT ROW 4.29 COL 196.2
     Btn_Help AT ROW 6.67 COL 196.2
     FI-SperrTxt AT ROW 6.81 COL 127.8 COLON-ALIGNED NO-LABEL
     FI-Txt AT ROW 7 COL 14 COLON-ALIGNED NO-LABEL
     FI-SperrLbl AT ROW 7 COL 55.4 COLON-ALIGNED NO-LABEL
     RECT-Skille2 AT ROW 8.14 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4.1
         SCROLLABLE SIZE 208 BY 7.52.


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
         TITLE              = "Forenklet varemottak"
         HEIGHT             = 34.52
         WIDTH              = 208.6
         MAX-HEIGHT         = 36.76
         MAX-WIDTH          = 208.6
         VIRTUAL-HEIGHT     = 36.76
         VIRTUAL-WIDTH      = 208.6
         MAX-BUTTON         = no
         TOP-ONLY           = yes
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
ASSIGN FRAME FRAME-Inlev:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 34.52
       FRAME DEFAULT-FRAME:WIDTH            = 208.6.

/* SETTINGS FOR FILL-IN FI-ArtBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-Inlev
                                                                        */
ASSIGN 
       FRAME FRAME-Inlev:HEIGHT           = 7.52
       FRAME FRAME-Inlev:WIDTH            = 208.

/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME FRAME-Inlev
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Slett:HIDDEN IN FRAME FRAME-Inlev           = TRUE.

/* SETTINGS FOR FILL-IN FI-SperrTxt IN FRAME FRAME-Inlev
   NO-ENABLE                                                            */
ASSIGN 
       FI-SperrTxt:HIDDEN IN FRAME FRAME-Inlev           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME vsFlexGrid ASSIGN
       FRAME           = FRAME FRAME-Inlev:HANDLE
       ROW             = 3.14
       COLUMN          = 6
       HEIGHT          = 1.86
       WIDTH           = 189
       HIDDEN          = no
       SENSITIVE       = yes.
/* vsFlexGrid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      vsFlexGrid:MOVE-AFTER(Btn_OK:HANDLE IN FRAME FRAME-Inlev).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Forenklet varemottak */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Forenklet varemottak */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Inlev
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME FRAME-Inlev /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-Inlev /* OK */
DO:

    RUN LeaveCell.
    IF RETURN-VALUE = "FEL" THEN DO:
        APPLY "ENTRY" TO vsFlexGrid.
        RETURN NO-APPLY.
    END.
    IF NOT Registrerat(?) THEN DO:
        MESSAGE "Det finnes inget å lagre." VIEW-AS ALERT-BOX INFORMATION.
        APPLY "ENTRY" TO vsFlexGrid.
        RETURN NO-APPLY.
    END.

    RUN LagreInnlev.
    APPLY "CLOSE" TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Inlev
&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-Inlev /* Slett */
DO:
    /* tillsvidare hidden, den fungerade fdelaktigt */

    DEF VAR wIdx AS INTE NO-UNDO.
/*     MESSAGE "Ønsker du å slette alt ned til minste tillatte verdi?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO */
/*                                                       UPDATE wSvar AS LOGI.                                   */
                                                      
    DO wIdx = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
        IF INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx)) < 0 THEN DO:
/*             RUN ChangeFri IN hWindow (wIdx - chvsFlexGrid:FixedCols + 1,INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx))). */
            ASSIGN chvsFlexGrid:TextMatrix(2,1) = STRING(INT(chvsFlexGrid:TextMatrix(2,1)) + INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx))) + " "
                   chvsFlexGrid:TextMatrix(2,wIdx) = chvsFlexGrid:TextMatrix(1,wIdx).
        END.
    END.
    ASSIGN chvsFlexGrid:COL = chvsFlexGrid:FixedCols.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Negativ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Negativ C-Win
ON VALUE-CHANGED OF TG-Negativ IN FRAME FRAME-Inlev /* Negativ */
DO:
    DEFINE VAR wColor AS INTE EXTENT 3 INIT [13027071,6979071,16711680] NO-UNDO.
    chvsFlexGrid:Cell(6,chvsFlexGrid:FixedRows,chvsFlexGrid:FixedCols,chvsFlexGrid:Rows - 1,chvsFlexGrid:Cols - 1) =
        IF SELF:CHECKED THEN wColor[1] ELSE 0 NO-ERROR.
    APPLY "ENTRY" TO vsFlexGrid.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-VisSkjul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-VisSkjul C-Win
ON VALUE-CHANGED OF TG-VisSkjul IN FRAME FRAME-Inlev /* Vis alle str */
DO:
  RUN SkjulVis(STRING(SELF:CHECKED,"Vis/Skjul")).
  APPLY "ENTRY" TO vsFlexGrid.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vsFlexGrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid C-Win OCX.EnterCell
PROCEDURE vsFlexGrid.VSFlexGrid.EnterCell .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT wGridInitialized THEN RETURN.
     ASSIGN /* chvsFlexGrid:Col = IF wCol = ? THEN chvsFlexGrid:Col ELSE wCol */
            wAktivStrl = chvsFlexGrid:TextMatrix(0,chvsFlexGrid:Col).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid C-Win OCX.KeyPress
PROCEDURE vsFlexGrid.VSFlexGrid.KeyPress .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyAscii
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER p-KeyAscii AS INTEGER NO-UNDO.
DEFINE VARIABLE               wCentralValue AS INTE    NO-UNDO.
DEFINE VARIABLE               wCellValue    AS INTE    NO-UNDO.
DEFINE VARIABLE               wDiff         AS INTE    NO-UNDO.
DEFINE VARIABLE iCol AS INTEGER     NO-UNDO.
IF p-KeyAscii = 9 THEN
    RETURN NO-APPLY.
IF p-KeyAscii = 13 THEN DO:
    IF  chvsFlexGrid:Col = chvsFlexGrid:Cols - 1 THEN
        APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    ELSE DO:
      IF TG-VisSkjul:CHECKED = TRUE THEN
          ASSIGN chvsFlexGrid:Col = chvsFlexGrid:Col + 1.
      ELSE DO:
          iCol = getNextCol().
          IF iCol <> ? THEN
              ASSIGN chvsFlexGrid:Col = iCol.
          ELSE
              APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
      END.
      IF chvsFlexGrid:Col >= wSHC# AND chvsFlexGrid:LeftCol < 
                chvsFlexGrid:Col - wSHC# + 1 + chvsFlexGrid:FixedCols THEN
          chvsFlexGrid:LeftCol = chvsFlexGrid:Col - wSHC# + + 1 + chvsFlexGrid:FixedCols.
    END.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 13 AND chvsFlexGrid:Col = chvsFlexGrid:Cols - 1 THEN DO:
    APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END.
IF chvsFlexGrid:cell(6,chvsFlexGrid:ROW,chvsFlexGrid:COL,chvsFlexGrid:ROW,chvsFlexGrid:COL) = iRed THEN
    RETURN NO-APPLY.
  
IF p-KeyAscii = 8 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:
        IF INT(chvsFlexGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wDiff = INT(chvsFlexGrid:Text)
               chvsFlexGrid:Text = 
             SUBSTR(chvsFlexGrid:Text,1,LENGTH(chvsFlexGrid:Text) - 2) + " ".
        ASSIGN wDiff = INT(chvsFlexGrid:Text) - wDiff.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF INT(chvsFlexGrid:Text) + 1 > 9999 THEN
            RETURN NO-APPLY.
        ASSIGN chvsFlexGrid:Text = STRING(INT(chvsFlexGrid:Text) + 1) + " "
               wDiff = 1.
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
        IF INT(chvsFlexGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN chvsFlexGrid:Text =
            IF INT(chvsFlexGrid:Text) - 1 = 0 THEN
                "" 
            ELSE            
                STRING(INT(chvsFlexGrid:Text) - 1) + " ".
        ASSIGN wDiff = -1.
    END.
    ELSE IF p-KeyAscii > 48 OR (chvsFlexGrid:Text > "" AND p-KeyAscii = 48) THEN DO:
        IF INT(chvsFlexGrid:Text) * 10 + p-KeyAscii - 48 > 9999 THEN
            chvsFlexGrid:Text.
        ELSE DO:
            ASSIGN wDiff = (INT(chvsFlexGrid:Text) * 10 + p-KeyAscii - 48) - INT(chvsFlexGrid:Text)
                   chvsFlexGrid:Text = STRING(INT(chvsFlexGrid:Text) * 10 + p-KeyAscii - 48) + " ".
        END.
    END.
/*     RUN ChangeFri IN hWindow (chvsFlexGrid:Col - chvsFlexGrid:FixedCols + 1,wDiff). */
    ASSIGN chvsFlexGrid:TextMatrix(chvsFlexGrid:ROW,1) = STRING(INT(chvsFlexGrid:TextMatrix(chvsFlexGrid:ROW,1)) + wDiff) + " ".
/*  tillsvidare dekativert          BUTTON-Slett:SENSITIVE IN FRAME {&FRAME-NAME} = INT(chvsFlexGrid:TextMatrix(chvsFlexGrid:ROW,1)) > 0. */
END.
ELSE
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid C-Win OCX.KeyUp
PROCEDURE vsFlexGrid.VSFlexGrid.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.
    IF p-KeyCode = 76 AND p-Shift = 4 THEN
        APPLY "CHOOSE" TO Btn_OK IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
IF NOT strOK() THEN DO:
    MESSAGE "Ingen godkjente størrelser." SKIP
            "Kontroller størrelsestypen/størrelser på EAN."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(hKalkyle) THEN
        DELETE PROCEDURE hKalkyle.
    IF VALID-HANDLE(chvsFlexGrid) THEN
       RELEASE OBJECT chvsFlexGrid NO-ERROR.
    IF VALID-HANDLE(vsFlexGrid) THEN
       DELETE OBJECT vsFlexGrid NO-ERROR.
    ASSIGN chvsFlexGrid = ?
           vsFlexGrid   = ?.
    RUN disable_UI.
END.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Setter sentrallager butikk */
{syspara.i 5 1 1 iCl INT}
/* SPerr EAN kjedeLev */
{syspara.i 2 4 26 cEanSperrKjedeLev}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    FIND FIRST ArtBas WHERE Artbas.artikkelnr = 9801569 NO-LOCK.
    ASSIGN rArtBasRowid = ROWID(ArtBas).
&ENDIF
    FIND ArtBas WHERE ROWID(ArtBas) = rArtBasRowid NO-LOCK NO-ERROR.

    IF AVAIL ArtBas THEN DO:
        FIND StrType OF ArtBas NO-LOCK NO-ERROR.
        IF NOT AVAIL StrType THEN DO:
            MESSAGE "Finner ikke størrelsetype"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        IF NOT CAN-FIND(FIRST StrTstr OF StrType) THEN DO:
            MESSAGE "Størrelsetypen mangler størrelser."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN.
        END.
    END.
    ELSE DO:
        MESSAGE "Finner ikke varen"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker THEN DO:
        MESSAGE "Finner ikke butik: " iButik
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    /* här kontrollerar vi om det finns inte inleverade best på denna butik och artikkel */
    IF Artbas.modell = 0 THEN DO:
        RUN KontrollerBest (OUTPUT cMsgTekst). /* Här testar vi artikeln om inte modell, nedan om modell */
    END.
    IF cMsgTekst <> "" THEN DO:
        MESSAGE "Det finnes ikke fulleverte bestillinger:" SKIP(1)
            cMsgTekst SKIP(1)
            "Ønsker du å registrere innleveranse?"
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lFortsett.
        IF NOT lFortsett THEN
            RETURN.
    END.
    cMsgTekst = "".
    FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtPris THEN DO:
        FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
        IF AVAIL clButiker THEN
            FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = clButiker.ProfilNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris THEN
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    END.
    IF NOT AVAIL ArtPris THEN DO:
        MESSAGE "Finner ikke kalkyle for varen."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ELSE IF ArtPris.VareKost[1] = 0 THEN DO:
        MESSAGE "Varekost for varen er ikke satt." SKIP
            "Kontroller artikkelens kalkyle og rett varekosten før varemottak gjøres."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    IF ArtBas.Modell <> 0 THEN DO:
/*         MESSAGE "Varen ingår i modell." SKIP                                */
/*             "Skall flere varer i modellen inleveres?"                       */
/*             VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lHeleModell AS LOGICAL. */
        IF lHeleModell THEN /* input param */
            ASSIGN iStrTypeId  = ArtBas.StrTypeId
                   dModellNr   = ArtBas.Modell.

        RUN KontrollerBest (OUTPUT cMsgTekst). /* Här testar vi artikeln om inte modell, nedan om modell */
    END.
    IF cMsgTekst <> "" THEN DO:
        MESSAGE "Det finnes ikke fulleverte bestillinger:" SKIP(1)
            cMsgTekst SKIP(1)
            "Ønsker du å registrere innleveranse?"
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lFortsett.
        IF NOT lFortsett THEN
            RETURN.
    END.
    ASSIGN FI-Butikk   = Butiker.butnamn
           FI-ArtBeskr = ArtBas.beskr.
    FIND Farg OF ArtBas NO-LOCK NO-ERROR.
    CREATE TT_Vare.
    ASSIGN TT_Vare.iRadNr = 1
           TT_Vare.Artikkelnr = ArtBas.Artikkelnr
           TT_Vare.Farbeskr   = IF AVAIL Farg AND Farg.farbeskr <> "" THEN Farg.farbeskr ELSE ArtBas.LevFargKod
           TT_Vare.VareKost   = Artpris.VareKost[1]
           TT_Vare.LevNr      = ArtBas.LevNr
           TT_Vare.Vg         = ArtBas.Vg   
           TT_Vare.LopNr      = ArtBas.LopNr.
           
    TT_Vare.OkStr      = getOKstr(Artbas.artikkelnr,ArtBas.Levnr).

    RUN InitStorrelser.
    ASSIGN TG-Negativ:HIDDEN = ArtBas.IndividType > 0.
    IF lHeleModell THEN DO:
        RUN SkapaTT_Vare (ArtBas.ArtikkelNr).
    END.
    IF cEanSperrKjedeLev = "1" THEN
        FI-SperrTxt:HIDDEN = FALSE.
    
  RUN enable_UI.
  RUN EndreSize.
  FI-SperrTxt:HIDDEN = TRUE.
  FI-SperrTxt:HIDDEN = FALSE.
  TG-VisSkjul:HIDDEN = NOT lVisTG.

FRAME DEFAULT-FRAME:MOVE-TO-TOP().
RUN w-kalkyle.w PERSISTENT SET hKalkyle (ArtBas.ArtikkelNr,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,h_PrisKo,"ENDRE").
FRAME FRAME-Inlev:MOVE-TO-TOP().
  {lng.i}
  ASSIGN wGridInitialized = TRUE.
  IF AVAIL ArtBas AND ArtBas.IndividType > 0 THEN
      ASSIGN TG-Negativ:HIDDEN = TRUE.
/*   APPLY "ENTRY" TO vsFlexGrid. */
  chvsFlexGrid:COL = getFirstEnabled().
  APPLY "ENTRY" TO vsFlexGrid.
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

OCXFile = SEARCH( "w-varemottak.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chvsFlexGrid = vsFlexGrid:COM-HANDLE
    UIB_S = chvsFlexGrid:LoadControls( OCXFile, "vsFlexGrid":U)
    vsFlexGrid:NAME = "vsFlexGrid":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-varemottak.wrx":U SKIP(1)
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
  DISPLAY FI-Butikk FI-ArtBeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-Ok RECT-Skille 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY TG-VisSkjul TG-Negativ FI-SperrTxt FI-Txt FI-SperrLbl 
      WITH FRAME FRAME-Inlev IN WINDOW C-Win.
  ENABLE RECT-Skille2 TG-VisSkjul TG-Negativ Btn_OK Btn_Help FI-Txt FI-SperrLbl 
      WITH FRAME FRAME-Inlev IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Inlev}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreSize C-Win 
PROCEDURE EndreSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dHeight AS DECIMAL    NO-UNDO.
FIND LAST TT_Vare.
/*     ASSIGN FRAME FRAME-Inlev:HEIGHT = FRAME FRAME-Inlev:HEIGHT + ((2 + TT_Vare.iRadNr - 3) * .78) */
/*            FI-Txt:ROW = FI-Txt:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).                            */
IF TT_Vare.iRadNr > 3 THEN DO:
    dHeight = ((2 + TT_Vare.iRadNr - 3) * .78).
    c-Win:HEIGHT = c-Win:HEIGHT + dHeight.
    FRAME DEFAULT-FRAME:VIRTUAL-HEIGHT = FRAME DEFAULT-FRAME:VIRTUAL-HEIGHT + dHeight NO-ERROR.
    FRAME DEFAULT-FRAME:HEIGHT = FRAME DEFAULT-FRAME:HEIGHT + dHeight NO-ERROR. /* FRAME DEFAULT-FRAME:HEIGHT + dHeight. */
/*     PROCESS EVENTS. */
    FRAME FRAME-Inlev:VIRTUAL-HEIGHT = FRAME FRAME-Inlev:VIRTUAL-HEIGHT + dHeight.
    FRAME FRAME-Inlev:HEIGHT = FRAME FRAME-Inlev:HEIGHT + dHeight.
/*     FI-Txt:X = FI-Txt:X + dHeight.           */
/*     FI-SperrTxt:X = FI-SperrTxt:X + dHeight. */
/*     FI-SperrLbl:X = FI-SperrLbl:X + dHeight. */
/*     ASSIGN FRAME FRAME-Inlev:HEIGHT = FRAME FRAME-Inlev:HEIGHT + ((2 + TT_Vare.iRadNr - 3) * .78) */
           FI-Txt:ROW = FI-Txt:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).
           FI-SperrTxt:ROW = FI-SperrTxt:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).
           FI-SperrLbl:ROW = FI-SperrLbl:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).
     RECT-Skille2:ROW = RECT-Skille2:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).
END.
ASSIGN vsFlexGrid:HEIGHT = (2 + TT_Vare.iRadNr) * .78 + .15.

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
DEFINE VAR iIdx        AS INTE NO-UNDO.
DEFINE VAR iRow        AS INTE NO-UNDO.
ASSIGN chvsFlexGrid = chvsFlexGrid:vsFlexGrid.

ASSIGN chvsFlexGrid:CellPictureAlignment = 1
       chvsFlexGrid:Redraw = FALSE. /* disable repaint while populating */
chvsFlexGrid:Clear().
/* FIND LAST TT_Vare.                                                                                                          */
/* /*     ASSIGN FRAME FRAME-Inlev:HEIGHT = FRAME FRAME-Inlev:HEIGHT + ((2 + TT_Vare.iRadNr - 3) * .78) */                     */
/* /*            FI-Txt:ROW = FI-Txt:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).                            */                     */
/* IF TT_Vare.iRadNr > 3 THEN DO:                                                                                              */
/*     dHeight = ((2 + TT_Vare.iRadNr - 3) * .78).                                                                             */
/*     c-Win:HEIGHT = c-Win:HEIGHT + dHeight.                                                                                  */
/*     FRAME DEFAULT-FRAME:VIRTUAL-HEIGHT = FRAME DEFAULT-FRAME:VIRTUAL-HEIGHT + dHeight NO-ERROR.                             */
/*     FRAME DEFAULT-FRAME:HEIGHT = FRAME DEFAULT-FRAME:HEIGHT + dHeight NO-ERROR. /* FRAME DEFAULT-FRAME:HEIGHT + dHeight. */ */
/* /*     PROCESS EVENTS. */                                                                                                   */
/*     FRAME FRAME-Inlev:VIRTUAL-HEIGHT = FRAME FRAME-Inlev:VIRTUAL-HEIGHT + dHeight.                                          */
/*     FRAME FRAME-Inlev:HEIGHT = FRAME FRAME-Inlev:HEIGHT + dHeight.                                                          */
/*     FI-Txt:X = FI-Txt:X + dHeight.                                                                                          */
/* /*     ASSIGN FRAME FRAME-Inlev:HEIGHT = FRAME FRAME-Inlev:HEIGHT + ((2 + TT_Vare.iRadNr - 3) * .78) */                     */
/* /*            FI-Txt:ROW = FI-Txt:ROW + ((2 + TT_Vare.iRadNr - 3) * .78).                            */                     */
/* END.                                                                                                                        */
/* ASSIGN vsFlexGrid:HEIGHT = (2 + TT_Vare.iRadNr) * .78 + .15.                                                                */

ASSIGN chvsFlexGrid:AllowUserResizing = 0 /* user may resize columns/rows */
       chvsFlexGrid:Enabled = TRUE  /* make it an updateable grid */
       chvsFlexGrid:AllowBigSelection = FALSE 
       chvsFlexGrid:Appearance = 1 
       chvsFlexGrid:Rows = TT_Vare.iRadNr + 1
       chvsFlexGrid:Cols = NUM-ENTRIES(cStorlekar," ") + 2
       chvsFlexGrid:FixedRows = 1
       chvsFlexGrid:FixedCols = 2
       chvsFlexGrid:HonorProKeys = FALSE
       chvsFlexGrid:TextStyle = 0
       chvsFlexGrid:TextStyleFixed = 0
       chvsFlexGrid:ColWidth(1) = 615.

ASSIGN chvsFlexGrid:Row = 0.
DO iIdx = 0 TO chvsFlexGrid:Cols - 1:
    ASSIGN chvsFlexGrid:Col = iIdx.
           chvsFlexGrid:CellBackColor = 16777215.
END.

ASSIGN chvsFlexGrid:TextMatrix(0,0) = "Farge "
       chvsFlexGrid:TextMatrix(0,1) = "Total ".

ASSIGN iIdx = 0.
DO iIdx = 1 TO NUM-ENTRIES(cStorlekar," "):
    ASSIGN chvsFlexGrid:ColWidth(iIdx + 1) = 510
           chvsFlexGrid:TextMatrix(0,iIdx + 1) = REPLACE(ENTRY(iIdx,cStorlekar," "),'_',' ') + " ".
END.
FOR EACH TT_Vare:
    ASSIGN iRow = TT_Vare.iRadNr 
           chvsFlexGrid:TextMatrix(iRow,0) = IF TT_Vare.Farbeskr <> "" THEN TT_Vare.Farbeskr ELSE " "
           chvsFlexGrid:TextMatrix(iRow,1) = "0 ".
    DO iIdx = 2 TO chvsFlexGrid:Cols - 1:
        IF NOT CAN-DO(TT_Vare.OKStr,TRIM(chvsFlexGrid:TextMatrix(0,iIdx))) THEN
           chvsFlexGrid:Cell(6,iRow,iIdx,iRow,iIdx) = iRed.
    END.
END.
ASSIGN chvsFlexGrid:Row = 1
       chvsFlexGrid:Col = 2
       chvsFlexGrid:Redraw = TRUE. /* disable repaint while populating */
  RUN SkjulVis("Skjul").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStorrelser C-Win 
PROCEDURE InitStorrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cStrTmp    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBrukteTmp AS CHARACTER  NO-UNDO.
/*     FOR EACH StrTstr OF StrType NO-LOCK:                                                */
/*         ASSIGN cStorlekar = cStorlekar +                                                */
/*                   IF cStorlekar = "" THEN                                               */
/*                       left-trim(StrTStr.SoStorl) ELSE " " + left-trim(StrTStr.SoStorl). */
/*     END.                                                                                */
    DO ii = 1 TO NUM-ENTRIES(cMottakstr,";") - 1:
        IF TRIM(ENTRY(ii,cMottakstr,";")) BEGINS "(" THEN
            LEAVE.
        cStrTmp    = cStrTmp    + (IF cStrTmp <> "" THEN " " ELSE "") + REPLACE(TRIM(ENTRY(ii,cMottakstr,";")),' ','_').
        cBrukteTmp = cBrukteTmp + (IF ii > 1 THEN "," ELSE "") + ENTRY(ii,cBrukteStr).
    END.
    cStorlekar = cStrTmp.
    cBrukteStr = cBrukteTmp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerBest C-Win 
PROCEDURE KontrollerBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cBestillinger AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iAntBest   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLevert    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAvskrevet AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cArtikkelListe AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
    DEFINE BUFFER bArtbas FOR Artbas.

    ASSIGN cArtikkelListe = STRING(ArtBas.Artikkelnr).
    IF dModellNr <> 0 THEN DO:
        FOR EACH bArtBas WHERE bArtBas.modellfarge = dModellNr NO-LOCK.
            IF NOT CAN-DO(cArtikkelListe,STRING(bArtbas.Artikkelnr)) THEN
                cArtikkelListe = cArtikkelListe + "," + STRING(bArtbas.Artikkelnr).
        END.
    END.
    DO iCount = 1 TO NUM-ENTRIES(cArtikkelListe):
        FIND bArtBas WHERE bArtBas.artikkelnr = DECI(ENTRY(icount,cArtikkelListe)) NO-LOCK.
        FOR EACH BestHode WHERE BestHode.Artikkelnr = bArtBas.Artikkelnr AND BestHode.ordrenr <> 0 AND
                                BestHode.BestStat < 6 NO-LOCK:
            iAntBest = 0.
            iLevert  = 0.
            FOR EACH BestStr OF besthode WHERE beststr.butik = iButik AND beststr.beststat = besthode.beststat NO-LOCK.
                iAntbest = iAntbest + beststr.bestilt.
            END.
            IF iAntBest > 0 THEN DO:
                FOR EACH Bestlevert OF Besthode WHERE bestlevert.butik = iButik NO-LOCK.
                    iLevert = iLevert + BestLevert.Levert.
                    IF BestLevert.Avskrevet = TRUE THEN
                        iLevert = iLevert + BestLevert.rest.
                END.
            END.
            IF iAntBest <> iLevert THEN DO:
                cBestillinger = cBestillinger + (IF cBestillinger <> "" THEN CHR(10) ELSE "") + "Ordre: " + STRING(Besthode.ordrenr) +
                                                                                              " Artikkelnr: " + STRING(bArtBas.Artikkelnr) + 
                                                                                              " Bestilling: " + STRING(BestHode.Bestnr)  +
                                                                                             (IF BestHode.EkstId <> "" THEN " Ekstid: " + BestHode.EkstId ELSE "").
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreInnlev C-Win 
PROCEDURE LagreInnlev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR cStorlek      AS CHAR NO-UNDO. /* strl i find, går ej med chvsFlexGrid */
  DEFINE VAR iRow          AS INTE NO-UNDO.
  DEFINE VAR iLevert       AS INTE NO-UNDO.
  DEFINE VAR iBatchNr      AS INTE NO-UNDO.
  DEFINE VAR iTransNr      AS INTE NO-UNDO.
  DEFINE VAR cEtiketterTmp AS CHARACTER  NO-UNDO.
  DEFINE VAR cAntallEtiTmp AS CHARACTER  NO-UNDO.
  DEFINE VAR iCount        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dIndividNr LIKE Individ.IndividNr  NO-UNDO.
  DEFINE VARIABLE cIndividNrTmp AS CHARACTER  NO-UNDO.
  /* Skal følgeseddel ved innleveranse skrives ut. */

/*   IF cPassordkrav = "1" THEN DO:                  */
/*       RUN d-bekreftbruker.w ("Bekreft brukerid"). */
/*       IF RETURN-VALUE = "AVBRYT" THEN DO:         */
/*           MESSAGE "Lagring avbrutt"               */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*           RETURN NO-APPLY.                        */
/*       END.                                        */
/*       ELSE                                        */
/*           ASSIGN cUserid = RETURN-VALUE.          */
/*   END.                                            */
TRANSBLOKK:
DO TRANSACTION:                    
  {sww.i}
  IF dModellNr = 0 THEN
      RUN batchlogg.w (program-name(1), 
                       "Forenklet varemottak: " + string(ArtBas.Artikkelnr),
                        OUTPUT iBatchNr).
  ELSE
      RUN batchlogg.w (program-name(1), 
                       "Forenklet varemottak modell: " + STRING(dModellNr),
                        OUTPUT iBatchNr).
  FOR EACH TT_Vare:
    IF NOT Registrerat(TT_Vare.iRadNr) THEN
        NEXT.
    ASSIGN cEtiketterTmp = ""
           cAntallEtiTmp = ""
           cIndividNrTmp = "".
    FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_Vare.Artikkelnr NO-LOCK.
    IF ArtBas.Inn_Dato = ? THEN
    DO:
        FIND CURRENT ArtBas EXCLUSIVE-LOCK.
        ASSIGN
            ArtBas.Inn_Dato = TODAY.
        FIND CURRENT ArtBas NO-LOCK.
    END.
    IF ArtBas.IndividType > 0 THEN DO:
        FIND HuvGr OF ArtBas NO-LOCK.
        FIND LevBas OF ArtBas NO-LOCK.
        FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        ASSIGN iIndividBatchNr = iBatchNr.
    END.
    FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtPris THEN DO:
        FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
        IF AVAIL clButiker THEN
            FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = clButiker.ProfilNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris THEN
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    END.
    DO WITH FRAME DEFAULT-FRAME:
             .
      DO iRow = TT_Vare.iRadNr TO TT_Vare.iRadNr:
        /* Transaksjonsnummer for butikken. */

        DO iCol = 2 TO chvsFlexGrid:Cols - 1:
          IF INT(chvsFlexGrid:TextMatrix(iRow,iCol)) = 0 THEN
              NEXT.
          FIND LAST TransLogg NO-LOCK WHERE
            TransLogg.Butik = Butiker.Butik USE-INDEX TransLogg NO-ERROR.
          IF AVAILABLE TransLogg THEN
            iTransNr = TransLogg.TransNr + 1.
          ELSE 
            iTransNr = 1.

          ASSIGN cStorlek = ENTRY(iCol - 1,cMottakstr,';')
                 /*cStorlek = FiksStorl(chvsFlexGrid:TextMatrix(0,iCol))    TNC*/
                 iLevert = INT(chvsFlexGrid:TextMatrix(iRow,iCol)).
          IF ArtBas.IndividType > 0 THEN DO:
              FIND StrKonv WHERE StrKonv.Storl = cStorlek NO-LOCK NO-ERROR.
          END.
          /* Oppretter transaksjon */
          LAG_TRANS:
          DO iCount = 1 TO (IF ArtBas.IndividType > 0 THEN iLevert ELSE 1):
            /* Sjekker at transnr er ledig */
            IF CAN-FIND(TransLogg WHERE
                        TransLogg.Butik   = Butiker.Butik AND
                        TransLogg.TransNr = iTransNr) THEN
              NESTE_NR:
              DO WHILE TRUE:
                iTransNr = iTransNr + 1.
                IF CAN-FIND(TransLogg WHERE
                            TransLogg.Butik   = Butiker.Butik AND
                            TransLogg.TransNr = iTransNr) THEN
                  NEXT NESTE_NR.
                ELSE
                  LEAVE NESTE_NR.
              END. /* NESTE_NR */
            IF ArtBas.IndividType > 0 AND AVAIL StrKonv THEN
                RUN SkapaIndivid IN THIS-PROCEDURE (INPUT iBatchNr,INPUT StrKonv.Strkode,INPUT StrKonv.Storl, OUTPUT dIndividNr).
            CREATE TransLogg.
            ASSIGN TransLogg.Butik        = Butiker.Butik
                   TransLogg.TransNr      = iTransNr
                   TransLogg.SeqNr        = 1.
            ASSIGN TransLogg.BatchNr      = iBatchNr
                   TransLogg.KundNr       = 0
                   TransLogg.TTId         = 5 /* Varekjøp */
                   TransLogg.TBId         = 1
                   TransLogg.ArtikkelNr   = TT_Vare.ArtikkelNr
                   TransLogg.LevNr        = TT_Vare.LevNr
                   TransLogg.BongId       = 0
                   TransLogg.BongLinjeNr  = 0
                   TransLogg.KassaNr      = 0
                   TransLogg.Vg           = TT_Vare.Vg
                   TransLogg.LopNr        = TT_Vare.LopNr
                   TransLogg.Antall       = IF ArtBas.IndividType > 0 THEN 1 ELSE iLevert
                   TransLogg.Antall       = IF TG-Negativ:CHECKED IN FRAME FRAME-Inlev THEN -1 * TransLogg.Antall ELSE TransLogg.Antall
                   TransLogg.Pris         = ArtPris.Varekost[1]
                   TransLogg.RabKr        = 0
                   TransLogg.Mva          = 0
                   TransLogg.Plukket      = TRUE /* Skal ikke ut på plukkliste */
                   TransLogg.Dato         = TODAY
                   TransLogg.Tid          = TIME
                   TransLogg.SattVVareKost = FALSE
                   TransLogg.BestNr       = 99 /* förslag när vi gör Forenklet varemottak */
                   TransLogg.Postert      = FALSE
                   TransLogg.IndividNr    = dIndividNr
                   TransLogg.Storl        = cStorlek.
            IF TransLogg.Antall > 0 THEN
            ASSIGN cEtiketterTmp          = cEtiketterTmp + (IF cEtiketterTmp <> "" THEN "," ELSE "") + TransLogg.Storl
                   cAntallEtiTmp          = cAntallEtiTmp + (IF cAntallEtiTmp <> "" THEN "," ELSE "") + STRING(TransLogg.Antall).
                   cIndividNrTmp          = cIndividNrTmp + (IF cIndividNrTmp <> "" THEN "," ELSE "") + STRING(TransLogg.IndividNr).
          END. /* LAG_TRANS */
        END.                       
      END.
    END.
    RUN genStrekKode.p(TT_Vare.ArtikkelNr,iBatchNr,"TRANSLOGG").
    IF cEtiketterTmp <> "" THEN
      ASSIGN cArtikkelEti = cArtikkelEti + (IF cArtikkelEti <> "" THEN CHR(1) ELSE "") + STRING(TT_Vare.ArtikkelNr)
             cEtiketter   = cEtiketter   + (IF cEtiketter   <> "" THEN CHR(1) ELSE "") + cEtiketterTmp
             cAntallEti   = cAntallEti   + (IF cAntallEti   <> "" THEN CHR(1) ELSE "") + cAntallEtiTmp
             cIndividNr   = cIndividNr   + (IF cIndividNr   <> "" THEN CHR(1) ELSE "") + cIndividNrTmp.
    IF AVAILABLE ArtBas THEN
      RELEASE ArtBas.
    IF AVAILABLE TransLogg THEN
      RELEASE TransLogg.
  END.
  RUN batchstatus.p (iBatchNr, 2).
  {swn.i}
/*   RETURN "UTSKRIFT". */
END. /* TRANSBLOKK TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveCell C-Win 
PROCEDURE LeaveCell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyKalkylelagret C-Win 
PROCEDURE NyKalkylelagret :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendYpos C-Win 
PROCEDURE sendYpos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER iYpos AS INTEGER    NO-UNDO.
   iYpos = RECT-Skille2:Y IN FRAME FRAME-Inlev + 70. /* + dHeight. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setGridEntry C-Win 
PROCEDURE setGridEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   APPLY "ENTRY" TO FRAME FRAME-Inlev. */
  APPLY "ENTRY" TO vsFlexGrid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaIndivid C-Win 
PROCEDURE SkapaIndivid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iBatchNr    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER iStrKode   LIKE StrKonv.StrKode   NO-UNDO.
  DEFINE INPUT  PARAMETER cStorl     LIKE StrKonv.Storl     NO-UNDO.
  DEFINE OUTPUT PARAMETER dIndividNr LIKE Individ.individnr NO-UNDO.
  DEFINE        VARIABLE  dSeqNr      AS DECIMAL            NO-UNDO.
  FIND LAST Individ WHERE Individ.butnr = Butiker.Butik USE-INDEX SeqNr NO-LOCK NO-ERROR.
  ASSIGN dSeqnr = IF NOT AVAIL Individ THEN 1 ELSE Individ.SeqNr + 1.
  CREATE Individ.
  REPEAT:
      ASSIGN dIndividNr         = DECI(STRING(Butiker.butik) + STRING(dSeqnr))
             Individ.butnr      = Butiker.Butik
             Individ.SeqNr      = dSeqNr
             Individ.ArtikkelNr = ArtBas.ArtikkelNr
             Individ.StrKode    = iStrKode
             Individ.individnr  = dIndividNr NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE.
      ASSIGN dSeqNr = dSeqNr + 1.
  END.
  ASSIGN Individ.AvdelingNr    = HuvGr.AvdelingNr
         Individ.Beskr         = ArtBas.Beskr
         Individ.Hg            = ArtBas.Hg
         Individ.IndividType   = ArtBas.IndividType
         Individ.LevNamn       = LevBas.Levnamn
         Individ.levnr         = ArtBas.LevNr
         Individ.NyVare        = TRUE
         Individ.Storl         = cStorl
         Individ.StrKode       = iStrKode
         Individ.Vg            = ArtBas.Vg
         Individ.VmBeskrivelse = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE ""
         Individ.VMId          = ArtBas.VMId
         Individ.BatchNr       = iBatchNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_Vare C-Win 
PROCEDURE SkapaTT_Vare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dSkapadArt AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE  iRadNr     AS INTEGER INIT 1 NO-UNDO.
   
   DEFINE BUFFER bArtBas FOR ArtBas.

   FOR EACH bArtBas WHERE bArtBas.Modell = dModellNr AND
                         bArtBas.ArtikkelNr <> dSkapadArt AND
                         bArtBas.StrTypeId = iStrTypeiD NO-LOCK.
       IF bArtBas.IndividType > 0 THEN TG-Negativ:HIDDEN IN FRAME FRAME-Inlev = TRUE.

       FIND ArtPris OF bArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
       IF NOT AVAIL ArtPris THEN 
       DO:
           FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
           IF AVAIL clButiker THEN
               FIND ArtPris OF bArtBas WHERE ArtPris.ProfilNr = clButiker.ProfilNr NO-LOCK NO-ERROR.
           IF NOT AVAIL ArtPris THEN
               FIND FIRST ArtPris OF bArtBas NO-LOCK NO-ERROR.
       END.
       
       IF AVAIL ArtPris AND ArtPris.VareKost[1] > 0 THEN 
       DO:
           ASSIGN iRadNr = iRadNr + 1.
           FIND Farg OF bArtBas NO-LOCK NO-ERROR.
           CREATE TT_Vare.
           ASSIGN TT_Vare.iRadNr     = iRadNr
                  TT_Vare.Artikkelnr = bArtBas.Artikkelnr
                  TT_Vare.Farbeskr   = IF AVAIL Farg AND Farg.farbeskr <> "" THEN Farg.farbeskr ELSE bArtBas.LevFargKod
                  TT_Vare.VareKost   = Artpris.VareKost[1]
                  TT_Vare.LevNr      = bArtBas.LevNr
                  TT_Vare.Vg         = bArtBas.Vg   
                  TT_Vare.LopNr      = bArtBas.LopNr.
                  
            TT_Vare.OkStr      = getOKstr(bArtbas.artikkelnr,bArtBas.Levnr).
       END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkjulVis C-Win 
PROCEDURE SkjulVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cWhat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.

    IF cWhat = "Skjul" THEN DO:
        DO ii = 1 TO NUM-ENTRIES(cStorlekar," "):
            IF DataIcell(ii + 1) THEN
                NEXT.
            chvsFlexGrid:ColHidden(ii + 1) = ENTRY(ii,cBrukteStr) <> "J".
            IF ENTRY(ii,cBrukteStr) <> "J" THEN
                lVisTG = TRUE.
        END.
    END.
    ELSE DO:
        DO ii = 2 TO chvsFlexGrid:Cols - 1:
            chvsFlexGrid:ColHidden(ii) = FALSE.
        END.
    END.
    chvsFlexGrid:COL = getFirstEnabled().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-Win 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hKalkyle) THEN
        DELETE PROCEDURE hKalkyle.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DataIcell C-Win 
FUNCTION DataIcell RETURNS LOGICAL
  ( INPUT iCol AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DO ii = 1 TO chvsFlexGrid:Rows - 1.
      IF INT(chvsFlexGrid:TextMatrix(ii,iCol)) > 0 THEN
          RETURN TRUE.
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/
 ASSIGN
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 OR 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF index(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstEnabled C-Win 
FUNCTION getFirstEnabled RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DO ii = 2 TO chvsFlexGrid:Cols - 1.
      IF chvsFlexGrid:ColHidden(ii) = FALSE THEN
          LEAVE.
  END.
  RETURN ii.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextCol C-Win 
FUNCTION getNextCol RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DO ii = chvsFlexGrid:COL + 1 TO chvsFlexGrid:Cols - 1:
      IF chvsFlexGrid:ColHidden(ii) = FALSE THEN
          RETURN ii.
  END.
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOKstr C-Win 
FUNCTION getOKstr RETURNS CHARACTER
  ( INPUT dArtikkelnr AS DECIMAL, INPUT iLevnr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOKStr AS CHARACTER INIT "*"  NO-UNDO.
  IF cEanSperrKjedeLev = "1" THEN DO:
      FIND levbas WHERE levbas.levnr = iLevNr NO-LOCK.
      IF LevBas.KjedeAvtale = TRUE THEN DO:
          cOKStr = "".
          FOR EACH strekkode WHERE strekkode.artikkelnr = dArtikkelnr AND NOT strekkode.kode BEGINS "02" AND LENGTH(strekkode.kode) = 13 NO-LOCK:
              FIND strkonv OF strekkode NO-LOCK NO-ERROR.
              IF NOT AVAIL strkonv THEN
                  NEXT.
              IF NOT CAN-DO(cOKStr,TRIM(strkonv.storl)) THEN
                  cOKStr = cOKStr + (IF cOKStr <> "" THEN "," ELSE "") + TRIM(strkonv.storl).
          END.
      END.
  END.
  RETURN cOKStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
        (  ):

        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRow      AS INT  NO-UNDO.
DEF VAR iCol      AS INT  NO-UNDO.
DEF VAR cTmpStorl AS CHAR NO-UNDO.

DO iRow = 1 TO chvsFlexGrid:Rows - 1:
  IF NOT Registrerat(iRow) THEN
      NEXT.
  DO iCol = 2 TO chvsFlexGrid:Cols - 1:
    IF INT(chvsFlexGrid:TextMatrix(iRow,iCol)) = 0 THEN NEXT.

    cTmpStorl = IF NOT CAN-DO(cTmpStorl,TRIM(chvsFlexGrid:TextMatrix(0,iCol))) THEN
                  cTmpStorl + TRIM(chvsFlexGrid:TextMatrix(0,iCol)) + ","
                ELSE cTmpStorl.
  END.
END.

RETURN TRIM(cTmpStorl,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Registrerat C-Win 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INT NO-UNDO.
  DEF VAR wCol AS INT NO-UNDO.
  IF ipRow = ? THEN DO wRow = chvsFlexGrid:FixedRows TO chvsFlexGrid:Rows - 1:
      DO wCol = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
          IF INT(chvsFlexGrid:TextMatrix(wRow,wCol)) > 0 THEN
             RETURN TRUE.
      END.
  END.
  ELSE DO wCol = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
      IF INT(chvsFlexGrid:TextMatrix(ipRow,wCol)) > 0 THEN
         RETURN TRUE.
  END.

  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION strOK C-Win 
FUNCTION strOK RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
/*   MESSAGE cBruktestr NUM-ENTRIES(cBruktestr) SKIP */
/*       cMottakstr NUM-ENTRIES(cMottakstr)          */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
  DO ii = 1 TO NUM-ENTRIES(cBrukteStr):
      IF ENTRY(ii,cBrukteStr) = "J" AND NOT ENTRY(ii,cMottakstr,";") BEGINS("(") THEN
          RETURN TRUE.
  END.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

