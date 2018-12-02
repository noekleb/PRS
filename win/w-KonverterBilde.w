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
DEFINE INPUT  PARAMETER cBildeFil AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iHeight   AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER iWidth    AS INTEGER  NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE wOk AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-Mini AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Mini AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-5 
     LABEL "Manuell go: skall gå automatiskt" 
     SIZE 49 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-5 AT ROW 17.67 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 133.2 BY 20.71.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 20.71
         WIDTH              = 133.2
         MAX-HEIGHT         = 33.38
         MAX-WIDTH          = 216.2
         VIRTUAL-HEIGHT     = 33.38
         VIRTUAL-WIDTH      = 216.2
         SHOW-IN-TASKBAR    = no
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
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 2
       HEIGHT          = 7.14
       WIDTH           = 58
       HIDDEN          = yes
       SENSITIVE       = no.

CREATE CONTROL-FRAME IMAGE-Mini ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 9.57
       COLUMN          = 2
       HEIGHT          = 7.14
       WIDTH           = 58
       HIDDEN          = yes
       SENSITIVE       = no.
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* IMAGE-Mini OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      IMAGE-Sko:MOVE-BEFORE(BUTTON-5:HANDLE IN FRAME DEFAULT-FRAME).
      IMAGE-Mini:MOVE-AFTER(IMAGE-Sko).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Manuell go: skall gå automatiskt */
DO:
      RUN konverter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-Mini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-Mini C-Win OCX.DblClick
PROCEDURE IMAGE-Mini.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available ArtBAs then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-Sko C-Win OCX.DblClick
PROCEDURE IMAGE-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available ArtBAs then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   IF SEARCH(cBildeFil) = ? THEN
        RETURN.
  RUN enable_UI.
  IF iHeight > 0 THEN
      RUN KonverterFixHeight.
  ELSE IF iWidth > 0 THEN
      RUN KonverterFixWidth.
END.
 IF VALID-HANDLE(chIMAGE-Sko) THEN
     RELEASE OBJECT chIMAGE-Sko NO-ERROR.
 IF VALID-HANDLE(IMAGE-Sko) THEN
     DELETE OBJECT IMAGE-Sko NO-ERROR.
 IF VALID-HANDLE(chIMAGE-Mini) THEN
     RELEASE OBJECT chIMAGE-Mini NO-ERROR.
 IF VALID-HANDLE(IMAGE-Mini) THEN
     DELETE OBJECT IMAGE-Mini NO-ERROR.
chIMAGE-Sko = ?.
chIMAGE-Mini = ?.

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

OCXFile = SEARCH( "w-KonverterBilde.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-Mini = IMAGE-Mini:COM-HANDLE
    UIB_S = chIMAGE-Mini:LoadControls( OCXFile, "IMAGE-Mini":U)
    IMAGE-Mini:NAME = "IMAGE-Mini":U
    chIMAGE-Sko = IMAGE-Sko:COM-HANDLE
    UIB_S = chIMAGE-Sko:LoadControls( OCXFile, "IMAGE-Sko":U)
    IMAGE-Sko:NAME = "IMAGE-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-KonverterBilde.wrx":U SKIP(1)
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
   ASSIGN chIMAGE-Mini = chIMAGE-Mini:Picbuf.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonverterFixHeight C-Win 
PROCEDURE KonverterFixHeight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN visbilde.
ASSIGN iWidth  = iHeight * (chIMAGE-Sko:Picbuf:YResolution / chIMAGE-Sko:Picbuf:XResolution).
/*        iWidth  = IF iHeight > 200 THEN 150 ELSE iHeight. */
       chIMAGE-Mini:AutoSize = TRUE.
       chIMAGE-Mini:AssignMode = 3.
       chIMAGE-Mini:ResizeMode = 2.
       chIMAGE-Mini:INIT(chIMAGE-Sko:Picbuf:ColorDepth,iHeight,iWidth,16777215) NO-ERROR.
/*             'Do the assignment                                           */
       chIMAGE-Mini:Assign(False,chIMAGE-Sko:Picbuf,False,0) NO-ERROR.
/*                                                                          */
/*             'Set the PicBuf's FileName to the name we want the new image */
/*             'saved as                                                    */
    ASSIGN chIMAGE-Mini:filename  = chIMAGE-Sko:Picbuf:FILENAME NO-ERROR.
/*             'Save the new image                                          */
           chIMAGE-Mini:Store().
    chIMAGE-Sko:Picbuf:CLEAR(0).
    chIMAGE-Mini:CLEAR(0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonverterFixWidth C-Win 
PROCEDURE KonverterFixWidth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     ASSIGN chSkoImage:filename  = "c:\appdir\skotex\wrk\innfil.bmp". */
/*     chSkoImage:AutoSize = TRUE.                                      */
/*     chSkoImage:LOAD().                                               */
RUN visbilde.

ASSIGN iHeight  = iWidth * (chIMAGE-Sko:Picbuf:XResolution / chIMAGE-Sko:Picbuf:YResolution).
/*        iHeight  = IF iWidth > 200 THEN 150 ELSE iWidth. */
 /* chSko2:AutoSize = TRUE. */
       chIMAGE-Mini:AutoSize = TRUE.
       chIMAGE-Mini:AssignMode = 3.
       chIMAGE-Mini:ResizeMode = 2.
       chIMAGE-Mini:INIT(chIMAGE-Sko:Picbuf:ColorDepth,iHeight,iWidth,16777215).
/*             'Do the assignment                                           */
       chIMAGE-Mini:Assign(False,chIMAGE-Sko:Picbuf,False,0).
/*             'Set the PicBuf's FileName to the name we want the new image */
/*             'saved as                                                    */
    ASSIGN chIMAGE-Mini:filename  = cBildeFil.
/*             'Save the new image                                          */
           chIMAGE-Mini:Store().
           chIMAGE-Sko:Picbuf:CLEAR(0).
           chIMAGE-Mini:CLEAR(0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Win 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:
  /* Nullstiller memory mellom hver lasting av bilde. */
  chIMAGE-Sko:Picbuf:clear(2).
  assign
    chIMAGE-Sko:Picbuf:filename  = cBildeFil
    chIMAGE-Sko:Picbuf:AutoScale = True.
  wOk = chIMAGE-Sko:Picbuf:load.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

