&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE TT_VareBehBestLinje NO-UNDO LIKE VareBehBestLinje.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VARIABLE dVareBehNr      LIKE VareBehBestLinje.VareBehNr NO-UNDO.
    DEFINE VARIABLE dArtikkelNr     LIKE ArtBas.ArtikkelNr     NO-UNDO.
    DEFINE VARIABLE cBeskr          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cLevnamn        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFarbeskr       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButiker        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStorrelser     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iAntall         AS INTEGER    NO-UNDO.
&ELSE
    DEFINE INPUT  PARAMETER dVareBehNr  LIKE VareBehBestLinje.VareBehNr NO-UNDO.
    DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr     NO-UNDO.
    DEFINE INPUT  PARAMETER cBeskr      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cLevnamn    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cFarbeskr   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cStorrelser AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER iAntall     AS INTEGER    NO-UNDO.
&ENDIF


/* Local Variable Definitions ---                                       */

DEF VAR wAktivStrl          AS CHAR NO-UNDO.
DEFINE VAR iCol             AS INTE NO-UNDO.
DEFINE VAR wGridInitialized AS LOGI NO-UNDO.
DEFINE VAR wSHC#            AS INTE INIT 18 NO-UNDO. /* Antal hela synliga Cols */
DEFINE VAR iCL              AS INTEGER    NO-UNDO.
DEFINE VARIABLE iStrTypeId  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cArtNrListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dModellNr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE hsuperSkapaIndivid AS HANDLE     NO-UNDO.
/* Temp-Table Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevSAnt

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH LevSAnt SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH LevSAnt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame LevSAnt
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame LevSAnt


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK BUTTON-Slett Btn_Cancel Btn_Help ~
FI-Liminn FI-Txt 
&Scoped-Define DISPLAYED-OBJECTS FI-Lev FI-ArtBeskr FI-Farge FI-Liminn ~
FI-Txt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl Dialog-Frame 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Registrerat Dialog-Frame 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett rad" 
     SIZE 12 BY 1.1.

DEFINE VARIABLE FI-ArtBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varebeskr." 
     VIEW-AS FILL-IN 
     SIZE 37.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Farge AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Lev AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 37.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Liminn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lim inn" 
      VIEW-AS TEXT 
     SIZE 15.2 BY .62
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL "Alt-L - Lagre" 
      VIEW-AS TEXT 
     SIZE 37 BY .62 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      LevSAnt SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Lev AT ROW 1.24 COL 12.8 COLON-ALIGNED
     FI-ArtBeskr AT ROW 1.24 COL 63.2 COLON-ALIGNED
     FI-Farge AT ROW 1.24 COL 110.4 COLON-ALIGNED
     Btn_OK AT ROW 2.43 COL 148
     BUTTON-Slett AT ROW 3.62 COL 148
     Btn_Cancel AT ROW 4.81 COL 148
     Btn_Help AT ROW 6 COL 148
     FI-Liminn AT ROW 1.48 COL 141 COLON-ALIGNED
     FI-Txt AT ROW 6.48 COL 2 COLON-ALIGNED NO-LABEL
     SPACE(121.99) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Varebehandlingslinje"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_VareBehBestLinje T "SHARED" NO-UNDO SkoTex VareBehBestLinje
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   EXP-POSITION                                                         */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:ROW              = 5
       FRAME Dialog-Frame:COLUMN           = -4.

/* SETTINGS FOR FILL-IN FI-ArtBeskr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Farge IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Lev IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.LevSAnt"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Grid ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 2.48
       COLUMN          = 3
       HEIGHT          = 1.57
       WIDTH           = 144
       HIDDEN          = no
       SENSITIVE       = yes.
      Grid:NAME = "Grid":U .
/* Grid OCXINFO:CREATE-CONTROL from: {C5DE3F86-3376-11d2-BAA4-04F205C10000} type: vsFlexGrid */
      Grid:MOVE-AFTER(Btn_OK:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Varebehandlingslinje */
DO:
/*  RUN LeaveCell.
  IF RETURN-VALUE = "FEL":U THEN DO:
      APPLY "ENTRY":U TO Grid.
      RETURN NO-APPLY.
  END. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DEFINE VARIABLE lValg AS LOGICAL    NO-UNDO.
    RUN LeaveCell.
    IF RETURN-VALUE = "FEL" THEN DO:
        APPLY "ENTRY" TO Grid.
        RETURN NO-APPLY.
    END.
    IF NOT Registrerat(?) THEN DO:
        IF CAN-FIND(FIRST TT_VareBehBestLinje) THEN DO:
            MESSAGE "Det finnes inget å lagre. Tidligere registrert vil bli slettet." SKIP 
                    "Ønsker du å fortsette registreringen?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lValg.
            IF lValg THEN DO:
                APPLY "ENTRY" TO Grid.
                RETURN NO-APPLY.
            END.
        END.
        ELSE DO:
            MESSAGE "Det finnes inget å lagre." SKIP
                    "Ønsker du å fortsette registreringen?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lValg.
            APPLY "ENTRY" TO Grid.
            IF lValg THEN DO:
                APPLY "ENTRY" TO Grid.
                RETURN NO-APPLY.
            END.
        END.
    END.
    RUN LagreTT_VareBehBestLinje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett Dialog-Frame
ON CHOOSE OF BUTTON-Slett IN FRAME Dialog-Frame /* Slett rad */
DO:
    DEF VAR iCount AS INTE NO-UNDO.
    chGrid:TextMatrix(chGrid:ROW,1) = "0 ".
    DO iCount = 2 TO chGrid:Cols - 1:
        chGrid:TextMatrix(chGrid:ROW,iCount) = " ".
    END.
    ASSIGN chGrid:COL = chGrid:FixedCols.
    APPLY "ENTRY" TO Grid.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Grid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid Dialog-Frame OCX.EnterCell
PROCEDURE Grid.vsFlexGrid.EnterCell .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
/*      DO WITH FRAME {&FRAME-NAME}:                                                                                       */
/*          ASSIGN Button-Slett:SENSITIVE = chGrid:ROW > 0 AND INT(chGrid:TextMatrix(chGrid:ROW,1)) > 0. */
/*      END.                                                                                                               */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid Dialog-Frame OCX.KeyPress
PROCEDURE Grid.vsFlexGrid.KeyPress .
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
ASSIGN FI-Liminn:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
IF p-KeyAscii = 13 THEN DO:

    IF  chGrid:Col = chGrid:Cols - 1 AND 
        chGrid:ROW = chGrid:Rows - 1 THEN DO:
        APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    END.
    ELSE IF chGrid:Col = chGrid:Cols - 1 THEN DO:
        chGrid:SELECT(chGrid:ROW + 1,2).
        wdiff = chGrid:CellLeft.
    END.
    ELSE DO:
      ASSIGN chGrid:Col = chGrid:Col + 1.
      wdiff = chGrid:CellLeft.
    END.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 13 AND chGrid:Col = chGrid:Cols - 1 THEN DO:
    APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 8 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:
        IF INT(chGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wDiff = INT(chGrid:Text)
               chGrid:Text = 
             SUBSTR(chGrid:Text,1,LENGTH(chGrid:Text) - 2) + " ".
        ASSIGN wDiff = INT(chGrid:Text) - wDiff.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF INT(chGrid:Text) + 1 > 9999 THEN
            RETURN NO-APPLY.
        ASSIGN chGrid:Text = STRING(INT(chGrid:Text) + 1) + " "
               wDiff = 1.
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
        IF INT(chGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN chGrid:Text =
            IF INT(chGrid:Text) - 1 = 0 THEN
                "" 
            ELSE            
                STRING(INT(chGrid:Text) - 1) + " ".
        ASSIGN wDiff = -1.
    END.
    ELSE IF p-KeyAscii > 48 OR (chGrid:Text > "" AND p-KeyAscii = 48) THEN DO:
        IF INT(chGrid:Text) * 10 + p-KeyAscii - 48 > 9999 THEN
            chGrid:Text.
        ELSE DO:
            ASSIGN wDiff = (INT(chGrid:Text) * 10 + p-KeyAscii - 48) - INT(chGrid:Text)
                   chGrid:Text = STRING(INT(chGrid:Text) * 10 + p-KeyAscii - 48) + " ".
        END.
    END.
/*     RUN ChangeFri IN hWindow (chGrid:Col - chGrid:FixedCols + 1,wDiff). */
    ASSIGN chGrid:TextMatrix(chGrid:ROW,1) = STRING(INT(chGrid:TextMatrix(chGrid:ROW,1)) + wDiff) + " "
           BUTTON-Slett:SENSITIVE IN FRAME {&FRAME-NAME} = INT(chGrid:TextMatrix(chGrid:ROW,1)) > 0.
END.
ELSE
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid Dialog-Frame OCX.KeyUp
PROCEDURE Grid.vsFlexGrid.KeyUp .
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grid Dialog-Frame OCX.MouseUp
PROCEDURE Grid.vsFlexGrid.MouseUp .
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
DEFINE       VARIABLE  iCount   AS INTEGER NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    IF chGrid:MOUSEROW > 0 AND chGrid:MOUSECOL = 0 AND
       p-Button = 1 AND p-Shift = 2 THEN DO:
        /* Vi har en kopia som skall kopieras */
        IF FI-Liminn:HIDDEN THEN DO:
            IF INT(chGrid:TextMatrix(chGrid:MOUSEROW,1)) > 0 THEN
                ASSIGN FI-Liminn:SCREEN-VALUE = chGrid:TextMatrix(chGrid:MOUSEROW,0)
                       FI-Liminn:HIDDEN = FALSE
                       FI-Liminn:PRIVATE-DATA = STRING(chGrid:MOUSEROW).
        END.
        ELSE IF chGrid:TextMatrix(chGrid:MOUSEROW,0) <> FI-Liminn:SCREEN-VALUE THEN DO:
            DO iCount = 1 TO chGrid:Cols - 1.
                chGrid:TextMatrix(chGrid:MOUSEROW,iCount) = chGrid:TextMatrix(INT(FI-Liminn:PRIVATE-DATA),iCount).
            END.
            chGrid:COL = chGrid:FixedCols.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Setter sentrallager butikk */
{syspara.i 5 1 1 iCl INT}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  APPLY "ENTRY" TO SELF.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    FIND ArtBas WHERE Artbas.artikkelnr = 9000909 NO-LOCK.
    ASSIGN rArtBasRowid = ROWID(ArtBas).
&ENDIF

  ASSIGN FI-ArtBeskr = cBeskr
         FI-Lev      = cLevnamn 
         FI-Farge    = cFarbeskr.
  RUN enable_UI.
/*   {lng.i} */
  ASSIGN FI-Liminn:HIDDEN = TRUE 
         wGridInitialized = TRUE.
  APPLY "ENTRY" TO Grid.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
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

OCXFile = SEARCH( "d-VareBehBest.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chGrid = Grid:COM-HANDLE
    UIB_S = chGrid:LoadControls( OCXFile, "Grid":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-VareBehBest.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY FI-Lev FI-ArtBeskr FI-Farge FI-Liminn FI-Txt 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK BUTTON-Slett Btn_Cancel Btn_Help FI-Liminn FI-Txt 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR iIdx        AS INTE NO-UNDO.
DEFINE VAR iRow        AS INTE NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntBut AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntTmp AS INTEGER    NO-UNDO.
ASSIGN chGrid = chGrid:vsFlexGrid.

ASSIGN chGrid:CellPictureAlignment = 1
       chGrid:Redraw = FALSE. /* disable repaint while populating */
iAntBut = NUM-ENTRIES(cButiker).
chGrid:Clear().
IF iAntBut > 4 THEN
    ASSIGN FRAME Dialog-Frame:HEIGHT = FRAME Dialog-Frame:HEIGHT + ((2 + iAntBut - 4) * .78)
           FI-Txt:ROW = FI-Txt:ROW + ((2 + iAntBut - 4) * .78).
/*            FI-Liminn:ROW = FI-Txt:ROW. */
ASSIGN Grid:HEIGHT = (2 + iAntBut) * .78 + .15.
/* ASSIGN Grid:HEIGHT = (2 + 1) * .78 + .15. */

ASSIGN chGrid:AllowUserResizing = 0 /* user may resize columns/rows */
       chGrid:Enabled = TRUE  /* make it an updateable Grid */
       chGrid:AllowBigSelection = FALSE 
       chGrid:AllowSelection    = FALSE 
       chGrid:Appearance = 1 
       chGrid:Rows = iAntBut + 1
       chGrid:Cols = NUM-ENTRIES(cStorrelser) + 2
       chGrid:FixedRows = 1
       chGrid:FixedCols = 2
       chGrid:HonorProKeys = FALSE
       chGrid:TextStyle = 0
       chGrid:TextStyleFixed = 0
       chGrid:ColWidth(1) = 615.

/*     chGrid:Cell(6,0,0,0,chGrid:Cols - 1) = 16777215. */
ASSIGN chGrid:TextMatrix(0,0) = "Butikk "
       chGrid:TextMatrix(0,1) = "Total ".

ASSIGN iIdx = 0.
DO iIdx = 1 TO NUM-ENTRIES(cStorrelser):
    ASSIGN chGrid:ColWidth(iIdx + 1) = 510
           chGrid:TextMatrix(0,iIdx + 1) = ENTRY(iIdx,cStorrelser) + " ".
END.
DO iCount = 1 TO iAntBut:
    ASSIGN iRow = iCount 
           chGrid:TextMatrix(iRow,0) = ENTRY(iCount,cButiker)
           chGrid:TextMatrix(iRow,1) = "0 ".
END.
ASSIGN chGrid:Row = 1
       chGrid:Col = 2
       chGrid:Redraw = TRUE. /* disable repaint while populating */
FOR EACH TT_VareBehBestLinje:
    IF CAN-DO(cButiker,STRING(TT_VareBehBestLinje.BestiltButikkNr)) AND CAN-DO(cStorrelser,TT_VareBehBestLinje.Storl) THEN DO:
        chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButiker),LOOKUP(TT_VareBehBestLinje.Storl,cStorrelser) + 1) = STRING(TT_VareBehBestLinje.Bestilt) + " ".
        iAntTmp = INT(chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButiker),1)).
        iAntTmp = iAntTmp + TT_VareBehBestLinje.Bestilt.
        chGrid:TextMatrix(LOOKUP(STRING(TT_VareBehBestLinje.BestiltButikkNr),cButiker),1) = STRING(iAntTmp) + " ".
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreTT_VareBehBestLinje Dialog-Frame 
PROCEDURE LagreTT_VareBehBestLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR iRow     AS INTE NO-UNDO.
  DEFINE VAR iCol     AS INTEGER    NO-UNDO.
TRANSBLOKK:
do TRANSACTION:
  {sww.i}
  FOR EACH TT_VareBehBestLinje:
      DELETE TT_VareBehBestLinje.
  END.
  DO iRow = 1 TO chGrid:Rows - 1:
      IF NOT Registrerat(iRow) THEN
          NEXT.
      DO iCol = 2 TO chGrid:Cols - 1:
          IF INT(chGrid:TextMatrix(iRow,iCol)) = 0 THEN
              NEXT.
          CREATE TT_VareBehBestLinje.
          ASSIGN TT_VareBehBestLinje.VareBehNr  = dVareBehNr 
/*                  TT_VareBehBestLinje.ArtikkelNr = dArtikkelNr */
/*                  TT_VareBehBestLinje.Butik      = INT(chGrid:TextMatrix(iRow,0)) */
                 TT_VareBehBestLinje.Bestilt    = INT(chGrid:TextMatrix(iRow,iCol))
                 TT_VareBehBestLinje.Storl      = TRIM(chGrid:TextMatrix(0,iCol)).
      END.
  END.
  {swn.i}
/*   RETURN "UTSKRIFT". */
end. /* TRANSBLOKK TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveCell Dialog-Frame 
PROCEDURE LeaveCell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR wSoStorl AS CHAR NO-UNDO.
  IF INT(chGrid:TextMatrix(2,chGrid:Col)) < INT(chGrid:TextMatrix(1,chGrid:Col)) THEN DO:
     MESSAGE "Otillåtet värde." VIEW-AS ALERT-BOX ERROR TITLE "Fel".
     wCol = chGrid:Col.
     RETURN "FEL".
  END.
  ELSE DO:
     ASSIGN wCol = ?
            wSoStorl = chGrid:TextMatrix(0,chGrid:Col).
     FIND FIRST Fri-Str WHERE Fri-Str.SoStorl = wSoStorl.
     IF INT(chGrid:TextMatrix(2,chGrid:Col)) <> Fri-Str.SoAnt THEN DO:
         ASSIGN  chGrid:TextMatrix(2,1) = STRING(INT(chGrid:TextMatrix(2,1)) +
                    INT(chGrid:TextMatrix(2,chGrid:Col)) - Fri-Str.SoAnt) + " "
                 Fri-Str.SoAnt = INT(chGrid:TextMatrix(2,chGrid:Col)).
         RUN ChangeFri IN hWindow (Fri-Str.SeqNr).
     END.
  END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl Dialog-Frame 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/
 assign
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 or 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Registrerat Dialog-Frame 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INT NO-UNDO.
  DEF VAR wCol AS INT NO-UNDO.
  IF ipRow = ? THEN DO wRow = chGrid:FixedRows TO chGrid:Rows - 1:
      DO wCol = chGrid:FixedCols TO chGrid:Cols - 1:
          IF INT(chGrid:TextMatrix(wRow,wCol)) > 0 THEN
             RETURN TRUE.
      END.
  END.
  ELSE DO wCol = chGrid:FixedCols TO chGrid:Cols - 1:
      IF INT(chGrid:TextMatrix(ipRow,wCol)) > 0 THEN
         RETURN TRUE.
  END.

  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

