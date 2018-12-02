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

DEFINE VARIABLE iDayDelay AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFirstDate AS DATE       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 FI-Rappdir FI-Filprefix FI-Butiker 
&Scoped-Define DISPLAYED-OBJECTS FI-Rappdir FI-Filprefix FI-Butiker 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButnamn C-Win 
FUNCTION getButnamn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(50)" 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1.

DEFINE VARIABLE FI-Filprefix AS CHARACTER FORMAT "X(8)" 
     LABEL "FilPrefix" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FI-Rappdir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapportmappe" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.8 BY 3.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Rappdir AT ROW 1.48 COL 20 COLON-ALIGNED NO-TAB-STOP 
     FI-Filprefix AT ROW 2.48 COL 20 COLON-ALIGNED NO-TAB-STOP 
     FI-Butiker AT ROW 3.52 COL 20 COLON-ALIGNED NO-TAB-STOP 
     RECT-1 AT ROW 1.1 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70 BY 3.81.


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
         TITLE              = "Gant Salesthrough"
         HEIGHT             = 3.81
         WIDTH              = 70
         MAX-HEIGHT         = 31.05
         MAX-WIDTH          = 138.2
         VIRTUAL-HEIGHT     = 31.05
         VIRTUAL-WIDTH      = 138.2
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
   FRAME-NAME                                                           */
ASSIGN 
       FI-Butiker:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Filprefix:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Rappdir:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Gant Salesthrough */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Gant Salesthrough */
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
    RUN GetSyspara.
    RUN KontrollerSysPara.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY FI-Rappdir FI-Filprefix FI-Butiker 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 FI-Rappdir FI-Filprefix FI-Butiker 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSyspara C-Win 
PROCEDURE GetSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTG-Ftp AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER  NO-UNDO.
    {syspara.i 210 260 1 FI-Rappdir}
    {syspara.i 210 260 6 FI-Filprefix}.
    FOR EACH SysPara WHERE SysPara.SysHId = 210 AND 
                           SysPara.SysGr  = 261 NO-LOCK.
        FI-Butiker = FI-Butiker + (IF FI-Butiker <> "" THEN "," ELSE "") + STRING(SysPara.ParaNr).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerSysPara C-Win 
PROCEDURE KontrollerSysPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST Kasse WHERE Kasse.butik = butiker.butik AND Kasse.aktiv = TRUE):  */
/*         FIND Syspara WHERE SysPara.SysHId = 210 AND                                                                 */
/*                            SysPara.SysGr  = 255 AND                                                                 */
/*                            SysPara.ParaNr = butiker.butik NO-LOCK NO-ERROR.                                         */
/*         IF NOT AVAIL SysPara THEN DO:                                                                               */
/*             CREATE SysPara.                                                                                         */
/*             ASSIGN SysPara.SysHId = 210                                                                             */
/*                    SysPara.SysGr  = 255                                                                             */
/*                    SysPara.ParaNr = butiker.butik                                                                   */
/*                    SysPara.Beskrivelse = butiker.butnamn                                                            */
/*                    SysPara.Parameter1  = STRING(dFirstDate - 1) .                                                   */
/*         END.                                                                                                        */
/*     END.                                                                                                            */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport C-Win 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplUpdateSyspara AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER ipButikknr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER ipDato           AS DATE       NO-UNDO.
/*     RUN xartstat2timegrip.p (FI-Rappdir:SCREEN-VALUE IN FRAME {&FRAME-NAME},ipButikkNr,ipDato,TG-Ftp:CHECKED,FI-Ftphost,FI-Ftpbruker,FI-Ftppassord). */
    IF iplUpdateSyspara = TRUE THEN DO:
        FIND SysPara WHERE SysPara.SysHId = 210 AND 
                           SysPara.SysGr  = 200 AND 
                           SysPara.ParaNr = ipButikkNr.
        ASSIGN SysPara.Parameter1 = STRING(ipDato).
        FIND CURRENT SysPara NO-LOCK.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButnamn C-Win 
FUNCTION getButnamn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND butiker WHERE butiker.butik = syspara.paranr NO-LOCK NO-ERROR.
  RETURN IF AVAIL butiker THEN butiker.butnamn ELSE "Ukjent".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

