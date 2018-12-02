&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-VarGr 
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
def var wOk   as log format "Ja/Nei" no-undo.
def var wLoop as int                 no-undo.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-VarGr

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vargr moms huvgr

/* Definitions for BROWSE BROWSE-VarGr                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-VarGr vargr.vg vargr.vgbeskr ~
vargr.momskod moms.momsproc vargr.hg huvgr.hgbeskr vargr.kost_proc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-VarGr vargr.vg vargr.vgbeskr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-VarGr vargr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-VarGr vargr
&Scoped-define OPEN-QUERY-BROWSE-VarGr OPEN QUERY BROWSE-VarGr FOR EACH vargr NO-LOCK, ~
      EACH moms OF vargr NO-LOCK, ~
      EACH huvgr OF vargr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-VarGr vargr moms huvgr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-VarGr vargr
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-VarGr moms
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-VarGr huvgr


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-VarGr}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-VarGr BUTTON-Detalj Btn_Avbryt ~
Btn_Done 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-VarGr AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avbryt AUTO-END-KEY DEFAULT 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Ok" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Detalj 
     LABEL "&Detalj" 
     SIZE 10.8 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-VarGr FOR 
      vargr, 
      moms, 
      huvgr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-VarGr C-VarGr _STRUCTURED
  QUERY BROWSE-VarGr NO-LOCK DISPLAY
      vargr.vg COLUMN-LABEL "VgNr" FORMAT "zzz9":U
      vargr.vgbeskr COLUMN-LABEL "Beskrivning" FORMAT "x(20)":U
      vargr.momskod COLUMN-LABEL "Momskod" FORMAT "z9":U
      moms.momsproc COLUMN-LABEL "Procent" FORMAT "z9.99":U
      vargr.hg COLUMN-LABEL "HgNr" FORMAT "zz9":U
      huvgr.hgbeskr COLUMN-LABEL "Beskrivning" FORMAT "x(20)":U
      vargr.kost_proc COLUMN-LABEL "T/B procent snitt" FORMAT "zz9.9":U
  ENABLE
      vargr.vg
      vargr.vgbeskr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 83.8 BY 11.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-VarGr AT ROW 1 COL 1
     BUTTON-Detalj AT ROW 12.91 COL 1.6
     Btn_Avbryt AT ROW 12.91 COL 54
     Btn_Done AT ROW 12.91 COL 69.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.2 BY 13.19.


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
  CREATE WINDOW C-VarGr ASSIGN
         HIDDEN             = YES
         TITLE              = "Sök i varugruppsregistret"
         HEIGHT             = 13.14
         WIDTH              = 84.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 86.2
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 86.2
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
/* SETTINGS FOR WINDOW C-VarGr
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-VarGr 1 DEFAULT-FRAME */
ASSIGN 
       BROWSE-VarGr:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-VarGr)
THEN C-VarGr:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-VarGr
/* Query rebuild information for BROWSE BROWSE-VarGr
     _TblList          = "SkoTex.vargr,SkoTex.moms OF SkoTex.vargr,SkoTex.huvgr OF SkoTex.vargr"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > SkoTex.vargr.vg
"vargr.vg" "VgNr" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > SkoTex.vargr.vgbeskr
"vargr.vgbeskr" "Beskrivning" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > SkoTex.vargr.momskod
"vargr.momskod" "Momskod" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > SkoTex.moms.momsproc
"moms.momsproc" "Procent" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > SkoTex.vargr.hg
"vargr.hg" "HgNr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > SkoTex.huvgr.hgbeskr
"huvgr.hgbeskr" "Beskrivning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > SkoTex.vargr.kost_proc
"vargr.kost_proc" "T/B procent snitt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-VarGr */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-VarGr C-VarGr
ON END-ERROR OF C-VarGr /* Sök i varugruppsregistret */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-VarGr C-VarGr
ON WINDOW-CLOSE OF C-VarGr /* Sök i varugruppsregistret */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-VarGr
&Scoped-define SELF-NAME BROWSE-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-VarGr C-VarGr
ON MOUSE-SELECT-DBLCLICK OF BROWSE-VarGr IN FRAME DEFAULT-FRAME
DO:
  apply "CHOOSE":U to Btn_Done.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-VarGr C-VarGr
ON RETURN OF BROWSE-VarGr IN FRAME DEFAULT-FRAME
DO:
    apply "CHOOSE":U to Btn_Done.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avbryt C-VarGr
ON CHOOSE OF Btn_Avbryt IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-VarGr
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return string(recid(VarGr)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detalj C-VarGr
ON CHOOSE OF BUTTON-Detalj IN FRAME DEFAULT-FRAME /* Detalj */
DO:
  run v-vargr.w (string(recid(VarGr)), "ENDRE").
  if entry(1,return-value) = "AVBRYT" then
    return no-apply.
  else if entry(1,return-value) = "SLETTET" then
    do:
      wOk = BROWSE-VarGr:DELETE-SELECTED-ROW(1).
      return no-apply.
    end.
  else if entry(1,return-value) = "NY" then
    do:
      if BROWSE-VarGr:FOCUSED-ROW < BROWSE-VarGr:DOWN then
        assign wLoop = BROWSE-VarGr:FOCUSED-ROW + 1.
      else 
        assign wLoop = BROWSE-VarGr:FOCUSED-ROW.  
      wOk = BROWSE-VarGr:insert-row("AFTER").
      wOk = BROWSE-VarGr:CREATE-RESULT-LIST-ENTRY().
      wOk = BROWSE-VarGr:SELECT-ROW(wLoop).
      find VarGr no-lock where
        recid(VarGr) = int(entry(2,return-value)).
    end.

  find Moms of VarGr no-lock no-error.
  find HuvGr of VarGr no-lock no-error.
  display
    VarGr.Vg
    string(VArGr.Vg) @ VarGr.Vg
    VarGr.VgBeskr
    VarGr.MomsKod
    VarGr.Hg
    VarGr.Kost_Proc 
    Moms.MomsProc when available Moms
    HuvGr.HgBeskr when available HuvGr
  with browse BROWSE-VarGr.         
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-VarGr 


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
  RUN enable_UI.
  
  assign
    VarGr.VgBeskr:read-only in browse BROWSE-VarGr = true .
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-VarGr  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-VarGr)
  THEN DELETE WIDGET C-VarGr.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-VarGr  _DEFAULT-ENABLE
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
  ENABLE BROWSE-VarGr BUTTON-Detalj Btn_Avbryt Btn_Done 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-VarGr.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-VarGr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

