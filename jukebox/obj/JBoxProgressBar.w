&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
     
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Progress.Windows.* FROM ASSEMBLY. 
    
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   

/* Uncomment to enable use of .Net components: */
&SCOPED-DEFINE AdvGuiWin

/*
/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icUserId       AS CHAR NO-UNDO.
  DEF VAR icPrefLanguage AS CHAR NO-UNDO.
  DEF VAR bOK            AS LOG  NO-UNDO.
&ELSE
  DEF INPUT  PARAM icUserId       AS CHAR NO-UNDO.
  DEF INPUT  PARAM icPrefLanguage AS CHAR NO-UNDO.
  DEF OUTPUT PARAM bOK            AS LOG NO-UNDO.
&ENDIF
*/


/* Local Variable Definitions ---                                       */

DEF VAR tries      AS INT NO-UNDO.
DEF VAR passord1   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR passord2   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR enco-pass  AS CHAR FORMAT "x(15)" NO-UNDO.

DEF VAR cOldPsw           AS CHAR   NO-UNDO.
DEF VAR bErr              AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hPwdChange        AS HANDLE NO-UNDO.
DEF VAR bPasswordChanged  AS LOG    NO-UNDO.

DEF VAR hSourceProcedure AS HANDLE NO-UNDO.
DEFINE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.


PROCEDURE SendMessageA EXTERNAL "USER32.dll":
  DEFINE INPUT PARAMETER hHWND AS LONG.
  DEFINE INPUT PARAMETER iCmd  AS LONG.
  DEFINE INPUT PARAMETER iChar AS LONG.
  DEFINE INPUT PARAMETER ulParam AS LONG.
END PROCEDURE.

PROCEDURE PostPWChar:
  DEFINE INPUT PARAMETER hHWND AS INT.
  RUN SendMessageA(hHWND, 204, ASC("*"), 0).
END PROCEDURE.

DEFINE VARIABLE hABLWindow AS HANDLE NO-UNDO. 
DEFINE VARIABLE hFrame AS HANDLE NO-UNDO. 
DEFINE  VARIABLE hPopUpForm_ABLContainer AS Progress.Windows.WindowContainer NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "JBox login"
         COLUMN             = -347.2
         ROW                = 8
         HEIGHT             = 4.62
         WIDTH              = 144.8
         MAX-HEIGHT         = 47.91
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 47.91
         VIRTUAL-WIDTH      = 336
         ALWAYS-ON-TOP      = YES
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/demo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JBox login */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JBox login */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */


hSourceProcedure = SOURCE-PROCEDURE. 

hABLWindow = {&WINDOW-NAME}:HANDLE . 

ASSIGN
hABLWindow:TITLE        = "Win1" 
hABLWindow:VISIBLE      = FALSE  
hABLWindow:SENSITIVE    = TRUE. /* Make the form the parent of the window */
hABLWindow:HIDDEN       = TRUE. 

hPopUpForm_ABLContainer = NEW Progress.Windows.WindowContainer().
hPopUpForm_ABLContainer:Name = "windowContainer1".
/*hPopUpForm_ABLContainer:Dock = System.Windows.Forms.DockStyle:RIGHT. */
hPopUpForm_ABLContainer:Location = NEW System.Drawing.Point(0,0). 

/*
hPopUpForm_ABLContainer:SIZE     = NEW System.Drawing.Size(hABLWindow:WIDTH-PIXELS,hABLWindow:HEIGHT-PIXELS).
*/

hPopUpForm_ABLContainer:TabIndex = 1.
hPopUpForm_ABLContainer:TabStop = FALSE.
hPopUpForm_ABLContainer:EmbeddedWindow =  hABLWindow. 

DEFINE VARIABLE ultraProgressBar1 AS Infragistics.Win.UltraWinProgressBar.UltraProgressBar NO-UNDO.
DEFINE VARIABLE form1 AS Progress.Windows.Form NO-UNDO. 
DEFINE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
  
ultraProgressBar1 = NEW Infragistics.Win.UltraWinProgressBar.UltraProgressBar().
ultraProgressBar1:BorderStyle = Infragistics.Win.UIElementBorderStyle:Raised.
ultraProgressBar1:Dock = System.Windows.Forms.DockStyle:Bottom.
ultraProgressBar1:Location = NEW System.Drawing.Point(0, 110).
ultraProgressBar1:Name = "ultraProgressBar1".
ultraProgressBar1:Size = NEW System.Drawing.Size(402, 18).
ultraProgressBar1:TabIndex = 0.
ultraProgressBar1:Text = "[Formatted]".
ultraProgressBar1:VALUE = 1.

label1           = NEW System.Windows.Forms.label().
/* label1:BackColor = System.Drawing.Color:Transparent. */
/*label1:ForeColor = System.Drawing.Color:Black.*/
label1:Dock      = System.Windows.Forms.DockStyle:Top.
label1:Location  = NEW System.Drawing.Point(0,0).
label1:Name      = "label1".
label1:Size      = NEW System.Drawing.Size(200, 23). 
label1:Text      = "Login ...".
label1:Enabled   = TRUE.
/* label1:UseCompatibleTextRendering = TRUE. */
/* label1:Click:Subscribe("label1_Click"). */



ASSIGN 
    form1 = NEW Progress.Windows.Form()
    form1:TEXT = ""
    /*
    form1:FormBorderStyle = System.Windows.Forms.FormBorderStyle:NONE
    form1:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedToolwindow
    */
    form1:StartPosition   = System.Windows.Forms.FormStartPosition:CenterScreen
/*     form1:BackgroundImage = System.Drawing.Image:FromFile(SEARCH("img\logo.jpg")) */
    form1:Size            = NEW System.Drawing.Size(600,80)
/*     form1:Size            = NEW System.Drawing.Size(hABLWindow:WIDTH-PIXELS,hABLWindow:HEIGHT-PIXELS) */
    form1:ControlBox      = FALSE /* remove close buttons / max min */          
    form1:AutoSize        = FALSE       
    form1:MaximizeBox     = TRUE                                                
    form1:MinimizeBox     = TRUE 
    form1:ShowInTaskbar   = FALSE.
    form1:Controls:Add(ultraProgressBar1).
    form1:Controls:Add(hPopUpForm_ABLContainer).
    form1:Controls:Add(label1).
    form1:TopMost = TRUE.
    form1:Show(). 
    form1:activate(). 
 
hABLWindow:PARENT = form1:ProWinHandle.
hABLWindow:HIDDEN = TRUE. 

ultraProgressBar1:VALUE = 0.
ultraProgressBar1:show(). 
ultraProgressBar1:VISIBLE = FALSE. 
ultraProgressBar1:ENABLED = TRUE. 
ultraProgressBar1:step = 1.

timer1 = NEW System.Windows.Forms.Timer ().
timer1:INTERVAL = 30000. /* every 30 sec */ 
timer1:ENABLED = TRUE.
timer1:Tick:SUBSCRIBE("TimerTick").


FUNCTION SetStartupProgress RETURNS LOGICAL (INPUT  iNum AS INT,INPUT ipcText AS CHAR ):
    DEF VAR Y AS INT NO-UNDO.
    DEF VAR iValue AS INT NO-UNDO. 
    
    ultraProgressBar1:VISIBLE = TRUE. 
    label1:Show().
    label1:Enabled   = TRUE.
    label1:text = FILL(" ",50) + ipcText.
    label1:Update().

    hABLWindow:HIDDEN = FALSE. 
    PAUSE 0 NO-MESSAGE IN WINDOW hABLWindow. 
    DO iValue = ultraProgressBar1:VALUE TO iNum WHILE ultraProgressBar1:VALUE LE 100:
        /*ultraProgressBar1:VALUE = iValue. */
        PAUSE 0.01 NO-MESSAGE IN WINDOW hABLWindow. 
        ultraProgressBar1:performstep(). 
    END.
        
    IF ultraProgressBar1:VALUE = 100 THEN 
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        SESSION:REMOVE-SUPER-PROCEDURE(THIS-PROCEDURE) NO-ERROR.
        DELETE OBJECT hPopUpForm_ABLContainer NO-ERROR. 
        DELETE OBJECT THIS-PROCEDURE NO-ERROR. 
    END. 
    RETURN TRUE.
END.





PROCEDURE TimerTick :
    DEFINE INPUT PARAMETER sender AS System.Object.
    DEFINE INPUT PARAMETER  e AS System.EventArgs.
    
    label1:text = "Login timeout!  Performing auto shutdown ........".
    label1:update().
    timer1:ENABLED = FALSE.
    
    PAUSE 3 NO-MESSAGE IN WINDOW hABLWindow. 
    SetStartupProgress(100,"Login timeout!  Performing auto shutdown ........").

END PROCEDURE.


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   hPopUpForm_ABLContainer:VISIBLE = TRUE.
   RUN disable_UI.
   form1:Dispose().
END.

ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  QUIT.
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

