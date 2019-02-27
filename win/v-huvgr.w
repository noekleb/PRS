&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-HuvGr 
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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var wHuvGrRecid as recid no-undo.
  def var wModus       as char  no-undo. /* NY, ENDRE, SLETT */
  assign 
    wModus = "ENDRE". /* Default */
  find first HuvGr no-lock where
    HuvGr.Hg > 0 no-error.
  if not available HuvGr then
    find first HuvGr no-error.
  if available HuvGr then
    assign wHuvGrRecid = recid(HuvGr).
&ELSE
  def input parameter wHuvGrRecid as recid no-undo.
  def input parameter wModus      as char  no-undo. /* NY, ENDRE, SLETT */
&ENDIF

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* Buffere */
def buffer bHuvGr for HuvGr.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES huvgr

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME HuvGr.Hg HuvGr.HgBeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME HuvGr.HgBeskr 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME HuvGr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME HuvGr
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH huvgr SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME huvgr
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME huvgr


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS HuvGr.HgBeskr 
&Scoped-define ENABLED-TABLES HuvGr
&Scoped-define FIRST-ENABLED-TABLE HuvGr
&Scoped-define DISPLAYED-TABLES HuvGr
&Scoped-define FIRST-DISPLAYED-TABLE HuvGr
&Scoped-Define ENABLED-OBJECTS BUTTON-Ny BUTTON-Slett BUTTON-Avbryt Btn_OK ~
RECT-1 
&Scoped-Define DISPLAYED-FIELDS HuvGr.Hg HuvGr.HgBeskr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-HuvGr AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 9 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Avbryt 
     LABEL "&Avbryt" 
     SIZE 9 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "&Ny" 
     SIZE 9 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Radera" 
     SIZE 9 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 4.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      huvgr SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     HuvGr.Hg AT ROW 1.71 COL 16 COLON-ALIGNED
          LABEL "Huvudgrupp"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     HuvGr.HgBeskr AT ROW 1.71 COL 22.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     BUTTON-Ny AT ROW 4.1 COL 5
     BUTTON-Slett AT ROW 4.1 COL 14
     BUTTON-Avbryt AT ROW 4.1 COL 28
     Btn_OK AT ROW 4.1 COL 37
     RECT-1 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 49.6 BY 4.91
         DEFAULT-BUTTON Btn_OK.


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
  CREATE WINDOW C-HuvGr ASSIGN
         HIDDEN             = YES
         TITLE              = "HG underhåll"
         HEIGHT             = 4.86
         WIDTH              = 49.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR WINDOW C-HuvGr
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN HuvGr.Hg IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN HuvGr.HgBeskr IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-HuvGr)
THEN C-HuvGr:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.huvgr"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-HuvGr C-HuvGr
ON END-ERROR OF C-HuvGr /* HG underhåll */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-HuvGr C-HuvGr
ON WINDOW-CLOSE OF C-HuvGr /* HG underhåll */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-HuvGr
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  run LagreHuvGr.
  apply "CLOSE":U to THIS-PROCEDURE.
  return wModus + "," + string(recid(HuvGr)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Avbryt C-HuvGr
ON CHOOSE OF BUTTON-Avbryt IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  apply "CLOSE":U to THIS-PROCEDURE.
  return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-HuvGr
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  display 
    "" @ HuvGr.Hg
    "" @ HuvGr.HgBeskr
  with frame {&FRAME-NAME}.
  assign
    wModus = "NY"
    HuvGr.Hg:sensitive = true.
  apply "ENTRY":U to HuvGr.Hg in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-HuvGr
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  def var wOk as log format "Ja/Nei" no-undo.

  assign wOk = false.
  message "Skal hovedgruppen slettes?" view-as alert-box 
    QUESTION BUTTONS YES-NO
    title "Bekreftelse"
    update wOk.
  if wOk = false then
    return no-apply "AVBRYT".  
  else do:
    run SlettHuvGr.
    if return-value = "AVBRYT" then
      return no-apply.
    else
      return no-apply "SLETTET".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-HuvGr 


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

  /* Henter posten. */
  find HuvGr no-lock where
    recid(HuvGr) = wHuvGrRecid.

  run VisHuvGr.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-HuvGr  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-HuvGr)
  THEN DELETE WIDGET C-HuvGr.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-HuvGr  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE HuvGr THEN 
    DISPLAY HuvGr.Hg HuvGr.HgBeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-HuvGr.
  ENABLE HuvGr.HgBeskr BUTTON-Ny BUTTON-Slett BUTTON-Avbryt Btn_OK RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-HuvGr.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-HuvGr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreHuvGr C-HuvGr 
PROCEDURE LagreHuvGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if wModus = "Ny" then
    do:
      find HuvGr no-lock where
        HuvGr.Hg = int(HuvGr.Hg:screen-value in frame {&FRAME-NAME}) no-error.
      if available HuvGr then
        do:
          message "Hovedgruppe med dette nummer finnes fra før!" skip
                  "Angi et annet hovedgruppenummer" view-as alert-box 
                  WARNING title "Registreringsfeil".
          return no-apply "AVBRYT".
        end.
    end.

  /* Lagrer HuvGr. */
  do transaction:
    if wModus = "Ny" then
      do:
        create HuvGr.
        assign
          frame DEFAULT-FRAME HuvGr.Hg
          wHuvGrRecid = recid(HuvGr).
      end.
    find HuvGr exclusive-lock where
      recid(HuvGr) = wHuvGrRecid.
    assign
      frame DEFAULT-FRAME HuvGr.HgBeskr. 
  end. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettHuvGr C-HuvGr 
PROCEDURE SlettHuvGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Kan ikke slette hovedgrupper som er lagt inn på varegrupper. */
  find first VarGr no-lock where
    VarGr.Hg = HuvGr.Hg no-error.
  if available VarGr then
    do:
      message "Hovedgruppen er lagt inn på en eller flere varegrupper." skip
              "Kan ikke slettes!" view-as alert-box 
              WARNING title "Advarsel".
      return no-apply ("AVBRYT").
    end.
  
  /* Sletter hovedgruppen */
  do transaction:
    find bHuvGr exclusive-lock where
      recid(bHuvGr) = recid(HuvGr).
    delete bHuvGr.
    apply "CLOSE":U to this-procedure.
    return "SLETTET".
  end. /* transaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisHuvGr C-HuvGr 
PROCEDURE VisHuvGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if available HuvGr then
    display
      HuvGr.Hg
      HuvGr.HgBeskr
    with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

