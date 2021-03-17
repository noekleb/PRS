&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR hParent      AS HANDLE NO-UNDO.
DEF VAR hSourceProc  AS HANDLE NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR iPakkeIdx    AS INT    NO-UNDO.
DEF VAR httCalc      AS HANDLE NO-UNDO.
DEF VAR hBuffCalc    AS HANDLE NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmUtvalg

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnFilOmr fiKatalog tbFormat 
&Scoped-Define DISPLAYED-OBJECTS fiKatalog tbFormat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilOmr C-Win 
FUNCTION getFilOmr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFormatFiles C-Win 
FUNCTION getFormatFiles RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFilOmr 
     IMAGE-UP FILE "bmp/open16e.bmp":U
     LABEL "Button 1" 
     SIZE 5.4 BY 1.14.

DEFINE VARIABLE fiKatalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filområde" 
     VIEW-AS FILL-IN 
     SIZE 71.8 BY 1 TOOLTIP "Filområde for lagring av ordrebekreftelse(r)" NO-UNDO.

DEFINE VARIABLE tbFormat AS LOGICAL INITIAL yes 
     LABEL "Formattér filer ved utlegg til filområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 40.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmUtvalg
     btnFilOmr AT ROW 1.19 COL 86.4
     fiKatalog AT ROW 1.24 COL 12.2 COLON-ALIGNED
     tbFormat AT ROW 2.29 COL 14.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.6 BY 2.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 2.1
         WIDTH              = 101.8
         MAX-HEIGHT         = 23.33
         MAX-WIDTH          = 192.6
         VIRTUAL-HEIGHT     = 23.33
         VIRTUAL-WIDTH      = 192.6
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmUtvalg
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



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


&Scoped-define SELF-NAME btnFilOmr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilOmr C-Win
ON CHOOSE OF btnFilOmr IN FRAME frmUtvalg /* Button 1 */
DO:
  DEF VAR cCatalog AS CHAR NO-UNDO.

/*   SYSTEM-DIALOG GET-DIR cCatalog.       */
/*                                         */
/*   FILE-INFO:FILE-NAME = cCatalog.       */
/*                                         */
/*   IF FILE-INFO:FILE-TYPE = "drw" THEN   */
/*     fiKatalog:SCREEN-VALUE = cCatalog.  */


  DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE oFolder AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE oParent AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cFolder AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  CREATE 'Shell.Application' oServer.

  oFolder = oServer:BrowseForFolder(CURRENT-WINDOW:HWND,'Select Folder',0).

  IF VALID-HANDLE(oFolder) = True THEN
  DO:
    ASSIGN cFolder = oFolder:Title
    oParent = oFolder:ParentFolder
    iCount = 0.
    REPEAT:
      IF iCount >= oParent:Items:Count THEN
        DO:
/*           MESSAGE 'No Folder Selected' VIEW-AS ALERT-BOX. */
/*           LEAVE.                                          */
        END.
      ELSE
        IF oParent:Items:Item(iCount):Name = cFolder THEN
        DO:
          fiKatalog:SCREEN-VALUE = oParent:Items:Item(iCount):Path.
          LEAVE.
        END.
      ASSIGN iCount = iCount + 1.
    END.
  END.

  RELEASE OBJECT oParent NO-ERROR.
  RELEASE OBJECT oFolder NO-ERROR.
  RELEASE OBJECT oServer NO-ERROR.

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
ON CLOSE OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(httCalc) THEN DO:
    DELETE OBJECT httCalc NO-ERROR.
    DELETE OBJECT hBuffCalc NO-ERROR.
  END.
  APPLY "close" TO hParent.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  hParent = SOURCE-PROCEDURE.

  DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"fiSumOrdre,fiSumOrdre-2,fiSumOrdreDb,fiSumOrdreDb%,fiSumOrdreDb%-2,fiSumOrdreDb%Label,fiSumOrdreDb-2,fiSumOrdreDbLabel,fiSumOrdreEksMva,fiSumOrdreEksMva-2,fiSumOrdreEksMvaLabel,fiSumOrdreLabel,fiSumRabattKr,fiSumRabattKr-2,fiSumRabattKrLabel").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"frmUtvalg").

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
  /* Hide all frames. */
  HIDE FRAME frmUtvalg.
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
  DISPLAY fiKatalog tbFormat 
      WITH FRAME frmUtvalg.
  ENABLE btnFilOmr fiKatalog tbFormat 
      WITH FRAME frmUtvalg.
  {&OPEN-BROWSERS-IN-QUERY-frmUtvalg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse     AS HANDLE NO-UNDO.
DEF INPUT PARAM ihSourceProc AS HANDLE NO-UNDO.

ASSIGN hBrowse     = ihBrowse
       hSourceProc = ihSourceProc
       .

DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmUtvalg:HANDLE,"frmUtvalg").
DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmUtvalg:HANDLE,"fiKatalog,btnFilOmr").
DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmUtvalg:HANDLE,"frmUtvalg").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilOmr C-Win 
FUNCTION getFilOmr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN fiKatalog:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFormatFiles C-Win 
FUNCTION getFormatFiles RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN tbFormat:INPUT-VALUE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

