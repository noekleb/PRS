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
DEFINE INPUT PARAMETER  wBestHodeRecid AS RECID           NO-UNDO.
DEFINE INPUT PARAMETER  wButik         Like Butiker.Butik NO-UNDO.
DEFINE INPUT PARAMETER  wStorlekar     AS CHAR            NO-UNDO.
DEFINE INPUT PARAMETER  wBestString    AS CHAR            NO-UNDO.
DEFINE INPUT PARAMETER  wch_Grid       AS COM-HANDLE      NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VAR  wAntStrl AS INTE NO-UNDO.
DEFINE VAR  hWindow  AS HANDLE NO-UNDO.

/* Temp-Table Definitions ---                                       */

DEFINE VAR wFillInit  AS INT NO-UNDO.
DEFINE VAR wBrow      AS INT NO-UNDO.
DEFINE VAR ch_Grid    AS COM-HANDLE NO-UNDO.
DEFINE VAR wCol       AS INTE NO-UNDO.
DEFINE VAR wGridInitialized AS LOGI NO-UNDO.
DEFINE VAR wAvskrevetCell AS CHAR INIT "Mak" NO-UNDO.

message "Her ommer båten" view-as alert-box.

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
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH LevSAnt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame LevSAnt
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame LevSAnt


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      LevSAnt SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 5.05 COL 2
     Btn_Help AT ROW 5.05 COL 137
     SPACE(1.59) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Inleveranser butik:"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

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

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 1.71
       COLUMN       = 2
       HEIGHT       = 3
       WIDTH        = 149.4
       HIDDEN       = no
       SENSITIVE    = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {6262D3A0-531B-11CF-91F6-C2863C385E30} type: MSFlexGrid */
      CtrlFrame:MOVE-BEFORE(Btn_OK:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Inleveranser butik: */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
ASSIGN FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + IF wButik <> ? THEN STRING(wButik) ELSE
                                                               "Alla".


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   FIND BestHode WHERE RECID(BestHode) = wBestHodeRecid NO-LOCK.
  APPLY "ENTRY" TO SELF.
  RUN enable_UI.
  ASSIGN wGridInitialized = TRUE.
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

OCXFile = SEARCH( "d-visgin2.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-visgin2.wrx":U SKIP(1)
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
  ENABLE Btn_OK Btn_Help 
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
DEFINE VAR wIdx       AS INTE NO-UNDO.
DEFINE VAR wRow       AS INTE NO-UNDO.
DEFINE VAR wTotBest   AS INTE NO-UNDO.
DEFINE VAR wTotInnlev AS INTE NO-UNDO.
ASSIGN ch_Grid = chCtrlFrame:MSFlexGrid.

ASSIGN ch_Grid:CellPictureAlignment = 1
       ch_Grid:Redraw = FALSE. /* disable repaint while populating */

ch_Grid:Clear().

ASSIGN ch_Grid:AllowUserResizing = 3 /* user may resize columns/rows */
       ch_Grid:Enabled = TRUE  /* make it an updateable grid */
       ch_Grid:AllowBigSelection = FALSE 
       ch_Grid:Appearance = 1 
       ch_Grid:Rows = 3
       ch_Grid:Cols = NUM-ENTRIES(wStorlekar," ") + 2
       ch_Grid:FixedRows = 2
       ch_Grid:FixedCols = 2
       ch_Grid:TextStyle = 0
       ch_Grid:TextStyleFixed = 0
       ch_Grid:ColWidth(1) = 615.

ASSIGN ch_Grid:Row = 0.
DO wIdx = 0 TO ch_Grid:Cols - 1:
    ASSIGN ch_Grid:Col = wIdx.
           ch_Grid:CellBackColor = 16777215.
END.

ASSIGN ch_Grid:TextMatrix(0,0) = " "
       ch_Grid:TextMatrix(0,1) = "Total "
       ch_Grid:TextMatrix(1,0) = "Beställt".
/*       ch_Grid:TextMatrix(1,1) = " "
       ch_Grid:TextMatrix(2,0) = "Fri ". */

DO wIdx = 1 TO NUM-ENTRIES(wStorlekar," "):
    ASSIGN ch_Grid:ColWidth(wIdx + 1) = 950
           ch_Grid:TextMatrix(0,wIdx + 1) = ENTRY(wIdx,wStorlekar," ") + " "
           ch_Grid:TextMatrix(1,wIdx + 1) = IF wButik <> ? THEN ENTRY(wIdx,wBestString," ") + " " ELSE
                                            IF NUM-ENTRIES(wch_Grid:TextMatrix(3,wIdx + 1),"/") = 2 THEN
                                            TRIM(ENTRY(1,wch_Grid:TextMatrix(3,wIdx + 1),"/")) + " "
                                            ELSE STRING(INT(wch_Grid:TextMatrix(3,wIdx + 1))) + " "
           ch_Grid:TextMatrix(2,wIdx + 1) = ""
           wTotBest = wTotBest + IF wButik <> ? THEN INT(ENTRY(wIdx,wBestString," ")) ELSE 0.
END.
ASSIGN ch_Grid:TextMatrix(1,1) = IF wButik <> ? THEN STRING(wTotBest) + " " ELSE
                                            IF NUM-ENTRIES(wch_Grid:TextMatrix(3,1),"/") = 2 THEN
                                            TRIM(ENTRY(1,wch_Grid:TextMatrix(3,1),"/")) + " "
                                            ELSE STRING(INT(wch_Grid:TextMatrix(3,1))) + " ".
ASSIGN wRow = 2.
IF wButik <> ? THEN DO:
FOR EACH BestHLev OF BestHode WHERE CAN-FIND(FIRST BestLevert OF BestHLev WHERE BestLevert.Butik = wButik):
    ASSIGN ch_Grid:Rows = wRow + 1
           ch_Grid:TextMatrix(wRow,0) = STRING(BestHLev.LevertDato) + " "
           wTotInnlev = 0.
    FOR EACH BestLevert OF BestHLev WHERE BestLevert.Butik = wButik NO-LOCK.
        ASSIGN wTotInnlev = wTotInnlev + BestLevert.Levert
               wIdx = LOOKUP(TRIM(BestLevert.Storl),wStorlekar," ") + 1
               ch_Grid:TextMatrix(wRow,wIdx) = IF BestLevert.Avskrevet THEN
                    wAvskrevetCell + " " + STRING(BestLevert.Rest) + " "
                    ELSE STRING(BestLevert.Levert) + " ".
    END.
    ASSIGN ch_Grid:TextMatrix(wRow,1) = STRING(wTotInnlev) + " "
           wRow = wRow + 1.
END.
END.
ELSE DO:
FOR EACH BestHLev OF BestHode:
    ASSIGN ch_Grid:Rows = wRow + 1
           ch_Grid:TextMatrix(wRow,0) = STRING(BestHLev.LevertDato) + " "
           wTotInnlev = 0.
    FOR EACH BestLevert OF BestHLev:
        ASSIGN wTotInnlev = wTotInnlev + BestLevert.Levert
               wIdx = LOOKUP(TRIM(BestLevert.Storl),wStorlekar," ") + 1
               ch_Grid:TextMatrix(wRow,wIdx) = IF BestLevert.Avskrevet THEN
                    wAvskrevetCell + " " + STRING(BestLevert.Rest) + " "
                    ELSE STRING(BestLevert.Levert) + " ".
    END.
    ASSIGN ch_Grid:TextMatrix(wRow,1) = STRING(wTotInnlev) + " "
           wRow = wRow + 1.
END.
END.
ASSIGN FRAME {&FRAME-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS +
       ((ch_Grid:Rows - 4 + IF ch_Grid:Cols > 12 THEN 1 ELSE 0) * 16)
       Btn_OK:Y   = Btn_OK:Y + ((ch_Grid:Rows - 4 + IF ch_Grid:Cols > 12 THEN 1 ELSE 0) * 16)
       Btn_Help:Y = Btn_OK:Y
       CtrlFrame:HEIGHT-PIXELS = CtrlFrame:HEIGHT-PIXELS +
                        ((ch_Grid:Rows - 4 + IF ch_Grid:Cols > 12 THEN 1 ELSE 0) * 16)
       CtrlFrame:WIDTH-PIXELS = CtrlFrame:WIDTH-PIXELS - IF ch_Grid:Scrollbars < 2 THEN 16
                               ELSE 0.
       ch_Grid:Redraw = TRUE. /* disable repaint while populating */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

