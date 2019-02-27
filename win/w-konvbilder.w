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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE wOk AS CHARACTER  NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Start 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 

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
DEFINE BUTTON B-Start 
     LABEL "Start konvertering" 
     SIZE 21 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 78 BY 9.29
     BGCOLOR 15 FONT 8 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Start AT ROW 1.95 COL 125
     EDITOR-1 AT ROW 16 COL 26 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.2 BY 25.62.


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
         TITLE              = "Skapa mini-bilder"
         HEIGHT             = 25.62
         WIDTH              = 149.2
         MAX-HEIGHT         = 33.38
         MAX-WIDTH          = 182
         VIRTUAL-HEIGHT     = 33.38
         VIRTUAL-WIDTH      = 182
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 2
       HEIGHT          = 7.38
       WIDTH           = 52
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME IMAGE-Mini ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 57
       HEIGHT          = 5.95
       WIDTH           = 38
       HIDDEN          = no
       SENSITIVE       = yes.
      IMAGE-Sko:NAME = "IMAGE-Sko":U .
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      IMAGE-Mini:NAME = "IMAGE-Mini":U .
/* IMAGE-Mini OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      IMAGE-Sko:MOVE-BEFORE(B-Start:HANDLE IN FRAME DEFAULT-FRAME).
      IMAGE-Mini:MOVE-AFTER(IMAGE-Sko).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Skapa mini-bilder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Skapa mini-bilder */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Start konvertering */
DO:
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEF BUFFER BUFbilderegister FOR bilderegister.

    FOR EACH BUFbilderegister NO-LOCK WHERE CAN-FIND(FIRST bildedata OF BUFbilderegister).
/*     chIMAGE-Sko:Picbuf:AutoSize = TRUE. */
      IF CAN-FIND(FIRST bildedata OF BUFbilderegister WHERE bildedata.Teller = 200) THEN
          NEXT.
      RUN visbilde (BUFbilderegister.bildnr,3).
      RUN konverter.
      ASSIGN iCount = iCount + 1.
    END.
    MESSAGE "Antal konverterade bilder: " iCount
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

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
  RUN InitEditor.
  RUN enable_UI.
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

OCXFile = SEARCH( "w-konvbilder.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-Mini = IMAGE-Mini:COM-HANDLE
    UIB_S = chIMAGE-Mini:LoadControls( OCXFile, "IMAGE-Mini":U)
    chIMAGE-Sko = IMAGE-Sko:COM-HANDLE
    UIB_S = chIMAGE-Sko:LoadControls( OCXFile, "IMAGE-Sko":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-konvbilder.wrx":U SKIP(1)
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
  DISPLAY EDITOR-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Start 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitEditor C-Win 
PROCEDURE InitEditor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN Editor-1 = 
        "Konverteringen kör igenom Bilderegister-poster som har bildedata-poster " +
        "men inte någon registrerad mini-bild. Det betyder att " +
        "man kan köra om från där man eventuellet bröt en tidigare körning.".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Konverter C-Win 
PROCEDURE Konverter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFilNavn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iHeight AS INTEGER    NO-UNDO.

ASSIGN iHeight = 200 * (chIMAGE-Sko:Picbuf:YResolution / chIMAGE-Sko:Picbuf:XResolution)
       iHeight = IF iHeight > 200 THEN 150 ELSE iHeight.
 /* chSko2:AutoSize = TRUE. */
       chIMAGE-Mini:AutoSize = TRUE.
       chIMAGE-Mini:AssignMode = 3.
       chIMAGE-Mini:ResizeMode = 2.
       chIMAGE-Mini:INIT(chIMAGE-Sko:Picbuf:ColorDepth,200,iHeight,16777215).
/*             'Do the assignment                                           */
       chIMAGE-Mini:Assign(False,chIMAGE-Sko:Picbuf,False,0).
/*                                                                          */
/*             'Set the PicBuf's FileName to the name we want the new image */
/*             'saved as                                                    */
    ASSIGN cFilNavn = SUBSTR(chIMAGE-Sko:PicBuf:FILENAME,1,R-INDEX(chIMAGE-Sko:Picbuf:FILENAME,"\")) +
             "mini" +
             ENTRY(NUM-ENTRIES(chIMAGE-Sko:Picbuf:FILENAME,"\"),chIMAGE-Sko:Picbuf:FILENAME,"\").
    ASSIGN chIMAGE-Mini:filename  = cFilNavn.
/*                                                                          */
/*             'Save the new image                                          */
           chIMAGE-Mini:Store().
/*     chIMAGE-Sko:Picbuf:CLEAR(0). */
/*     chIMAGE-Mini:CLEAR(0).       */
     OS-DELETE VALUE(chIMAGE-Sko:PicBuf:FILENAME).
     if valid-handle(wLibHandle) then
            run LesInnBilde in wLibHandle (input bilderegister.BildNr, input cFilnavn, output wOK).

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
  DEFINE INPUT PARAMETER iBildnr  AS INTEGER    NO-UNDO.
  /* finns i visbilde.i def input parameter ipMode      as int no-undo. */
  {visbilde.i
    &BldOcx = "chIMAGE-Sko"
    &BildNr = "iBildnr"
  }
END.
/*   def input parameter ipMode      as int no-undo.                                                    */
/*                                                                                                      */
/*   DEF VAR             ipBildNr    as INT NO-UNDO.                                                    */
/*   def var             wBildePeker as char no-undo.                                                   */
/*                                                                                                      */
/*   assign                                                                                             */
/*     ipBildNr = iBildNr.                                                                              */
/*                                                                                                      */
/*   find BildeRegister no-lock where                                                                   */
/*     BildeRegister.BildNr = iBildNr no-error.                                                         */
/*                                                                                                      */
/*   assign                                                                                             */
/*     wBildePeker = if available BildeRegister                                                         */
/*                     then BildeRegister.FilNavn                                                       */
/*                     else "".                                                                         */
/*                                                                                                      */
/*   if valid-handle(wLibHandle) then                                                                   */
/*    run HentBildePeker in wLibHandle (input ipBildNr, input ipMode, wBildePeker, output wBildePeker). */
/*                                                                                                      */
/*   /* Nullstiller memory mellom hver lasting av bilde. */                                             */
/*   chIMAGE-Sko:Picbuf:clear(2).                                                                       */
/*                                                                                                      */
/*   assign                                                                                             */
/*     chIMAGE-Sko:Picbuf:filename  = search(wBildePeker)                                               */
/*     chIMAGE-Sko:Picbuf:AutoScale = True.                                                             */
/*   wOk = chIMAGE-Sko:Picbuf:load.                                                                     */
/* end.                                                                                                 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

