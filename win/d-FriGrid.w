&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
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
DEFINE INPUT PARAMETER  hWindow     AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER  wStorlekar  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER  wMin        AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER  wAktuell    AS CHAR   NO-UNDO.

/* Local Variable Definitions ---                                       */

/* Temp-Table Definitions ---                                       */
DEF VAR wAktivStrl    AS CHAR NO-UNDO.
DEFINE VAR wFillInit  AS INT NO-UNDO.
DEFINE VAR wBrow      AS INT NO-UNDO.
DEFINE VAR wFriant    AS INTE NO-UNDO.
DEFINE VAR wCol       AS INTE NO-UNDO.
DEFINE VAR wGridInitialized AS LOGI NO-UNDO.
DEFINE VAR wSHC#      AS INTE INIT 18 NO-UNDO. /* Antal hela synliga Cols */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevSAnt

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH LevSAnt SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH LevSAnt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame LevSAnt
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame LevSAnt


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Help FILL-IN-20 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE vsFlexGrid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chvsFlexGrid AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett" 
     SIZE 12 BY 1.1.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL "Alt-L - Lagre" 
      VIEW-AS TEXT 
     SIZE 37 BY .62 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      LevSAnt SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.71 COL 148
     BUTTON-Slett AT ROW 2.91 COL 148
     Btn_Help AT ROW 4.14 COL 148
     FILL-IN-20 AT ROW 5.76 COL 2 COLON-ALIGNED NO-LABEL
     SPACE(121.99) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrering av fri inndeling"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   EXP-POSITION FRAME-NAME                                              */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:ROW              = 10.5
       FRAME Dialog-Frame:COLUMN           = -4
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME Dialog-Frame
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

CREATE CONTROL-FRAME vsFlexGrid ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 1.71
       COLUMN          = 3
       HEIGHT          = 3.57
       WIDTH           = 144
       HIDDEN          = no
       SENSITIVE       = yes.
/* vsFlexGrid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      vsFlexGrid:MOVE-BEFORE(Btn_OK:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrering av fri inndeling */
DO:
/*  RUN LeaveCell.
  IF RETURN-VALUE = "FEL":U THEN DO:
      APPLY "ENTRY":U TO vsFlexGrid.
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
    RUN LeaveCell.
    IF RETURN-VALUE = "FEL" THEN DO:
        APPLY "ENTRY" TO vsFlexGrid.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett Dialog-Frame
ON CHOOSE OF BUTTON-Slett IN FRAME Dialog-Frame /* Slett */
DO:
    DEF VAR wIdx AS INTE NO-UNDO.
    MESSAGE "Ønsker du å slette alt ned til minste tillatte verdi?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                                                      UPDATE wSvar AS LOGI.
                                                      
    IF wSvar THEN DO wIdx = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
        IF INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx)) < 0 THEN DO:
            RUN ChangeFri IN hWindow (wIdx - chvsFlexGrid:FixedCols + 1,INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx))).
            ASSIGN chvsFlexGrid:TextMatrix(2,1) = STRING(INT(chvsFlexGrid:TextMatrix(2,1)) + INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx))) + " "
                   chvsFlexGrid:TextMatrix(2,wIdx) = chvsFlexGrid:TextMatrix(1,wIdx).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vsFlexGrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid Dialog-Frame OCX.EnterCell
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid Dialog-Frame OCX.KeyPress
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
IF p-KeyAscii = 13 THEN DO:
    IF  chvsFlexGrid:Col = chvsFlexGrid:Cols - 1 THEN
        APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    ELSE DO:
      ASSIGN chvsFlexGrid:Col = chvsFlexGrid:Col + 1.
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
IF p-KeyAscii = 8 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:
        IF INT(chvsFlexGrid:Text) = 0 OR INT(chvsFlexGrid:Text) = INT(chvsFlexGrid:TextMatrix(1,chvsFlexGrid:Col)) /* Fri-Str.Min */ THEN
            RETURN NO-APPLY.
        ASSIGN wDiff = INT(chvsFlexGrid:Text)
               chvsFlexGrid:Text = IF INT(SUBSTR(chvsFlexGrid:Text,1,LENGTH(chvsFlexGrid:Text) - 2)) < INT(chvsFlexGrid:TextMatrix(1,chvsFlexGrid:Col)) /* Fri-Str.Min */ THEN
                             chvsFlexGrid:TextMatrix(1,chvsFlexGrid:Col) /* STRING(Fri-Str.Min) + " " */
        ELSE SUBSTR(chvsFlexGrid:Text,1,LENGTH(chvsFlexGrid:Text) - 2) + " ".
        ASSIGN wDiff = INT(chvsFlexGrid:Text) - wDiff.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF INT(chvsFlexGrid:Text) + 1 > 9999 THEN
            RETURN NO-APPLY.
        ASSIGN chvsFlexGrid:Text = STRING(INT(chvsFlexGrid:Text) + 1) + " "
               wDiff = 1.
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
        IF INT(chvsFlexGrid:Text) = 0 OR INT(chvsFlexGrid:Text) =  INT(chvsFlexGrid:TextMatrix(1,chvsFlexGrid:Col))       /* Fri-Str.Min */ THEN
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
    RUN ChangeFri IN hWindow (chvsFlexGrid:Col - chvsFlexGrid:FixedCols + 1,wDiff).
    ASSIGN chvsFlexGrid:TextMatrix(2,1) = STRING(INT(chvsFlexGrid:TextMatrix(2,1)) + wDiff) + " "
           BUTTON-Slett:SENSITIVE IN FRAME {&FRAME-NAME} = INT(chvsFlexGrid:TextMatrix(2,1)) > 0.
END.
ELSE
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid Dialog-Frame OCX.KeyUp
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  APPLY "ENTRY" TO SELF.
  RUN enable_UI.
  {lng.i}
  ASSIGN wGridInitialized = TRUE
         BUTTON-Slett:SENSITIVE IN FRAME {&FRAME-NAME} = INT(chvsFlexGrid:TextMatrix(2,1)) > 0.
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

OCXFile = SEARCH( "d-FriGrid.wrx":U ).
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
ELSE MESSAGE "d-FriGrid.wrx":U SKIP(1)
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
  DISPLAY FILL-IN-20 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Help FILL-IN-20 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
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
DEFINE VAR wIdx        AS INTE NO-UNDO.
DEFINE VAR wRow        AS INTE NO-UNDO.
DEFINE VAR wTotKassPar AS INTE NO-UNDO.
ASSIGN chvsFlexGrid = chvsFlexGrid:vsFlexGrid.

ASSIGN chvsFlexGrid:CellPictureAlignment = 1
       chvsFlexGrid:Redraw = FALSE. /* disable repaint while populating */

chvsFlexGrid:Clear().

ASSIGN chvsFlexGrid:AllowUserResizing = 0 /* user may resize columns/rows */
       chvsFlexGrid:Enabled = TRUE  /* make it an updateable grid */
       chvsFlexGrid:AllowBigSelection = FALSE 
       chvsFlexGrid:Appearance = 1 
       chvsFlexGrid:Rows = 3
       chvsFlexGrid:Cols = NUM-ENTRIES(wStorlekar," ") + 2
       chvsFlexGrid:FixedRows = 2
       chvsFlexGrid:FixedCols = 2
       chvsFlexGrid:HonorProKeys = FALSE
       chvsFlexGrid:TextStyle = 0
       chvsFlexGrid:TextStyleFixed = 0
       chvsFlexGrid:ColWidth(1) = 615
       chvsFlexGrid:ColWidthMin = 615.
/*        chvsFlexGrid:Editable    = 1. */

ASSIGN chvsFlexGrid:Row = 0.
DO wIdx = 0 TO chvsFlexGrid:Cols - 1:
    ASSIGN chvsFlexGrid:Col = wIdx.
           chvsFlexGrid:CellBackColor = 16777215.
END.

ASSIGN chvsFlexGrid:TextMatrix(0,0) = " "
       chvsFlexGrid:TextMatrix(0,1) = "Total "
       chvsFlexGrid:TextMatrix(1,0) = "Min "
       chvsFlexGrid:TextMatrix(1,1) = " "
       chvsFlexGrid:TextMatrix(2,0) = "Fri ".

ASSIGN wIdx = 0.
DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
    ASSIGN chvsFlexGrid:ColWidth(wIdx + 1) = 510
           chvsFlexGrid:TextMatrix(0,wIdx + 1) = ENTRY(wIdx,wStorlekar," ") + " "
           chvsFlexGrid:TextMatrix(1,wIdx + 1) = ENTRY(wIdx,wMin," ") + " "
           chvsFlexGrid:TextMatrix(2,wIdx + 1) = IF ENTRY(wIdx,wAktuell," ") = "0" THEN " "
                                            ELSE ENTRY(wIdx,wAktuell," ") + " "
           wTotKassPar                    = wTotKassPar + INT(ENTRY(wIdx,wAktuell," ")).
END.
ASSIGN chvsFlexGrid:TextMatrix(2,1) = STRING(wTotKassPar) + " "
    chvsFlexGrid:Redraw = TRUE /* disable repaint while populating */
       chvsFlexGrid:Row = 2
       chvsFlexGrid:Col = 2
    .
chvsFlexGrid:AutoSize(1,chvsFlexGrid:Cols - 1).

RUN vsFlexGrid.vsFlexGrid.EnterCell.
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
  IF INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)) < INT(chvsFlexGrid:TextMatrix(1,chvsFlexGrid:Col)) THEN DO:
     MESSAGE "Otillåtet värde." VIEW-AS ALERT-BOX ERROR TITLE "Fel".
     wCol = chvsFlexGrid:Col.
     RETURN "FEL".
  END.
  ELSE DO:
     ASSIGN wCol = ?
            wSoStorl = chvsFlexGrid:TextMatrix(0,chvsFlexGrid:Col).
     FIND FIRST Fri-Str WHERE Fri-Str.SoStorl = wSoStorl.
     IF INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)) <> Fri-Str.SoAnt THEN DO:
         ASSIGN  chvsFlexGrid:TextMatrix(2,1) = STRING(INT(chvsFlexGrid:TextMatrix(2,1)) +
                    INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)) - Fri-Str.SoAnt) + " "
                 Fri-Str.SoAnt = INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)).
         RUN ChangeFri IN hWindow (Fri-Str.SeqNr).
     END.
  END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

