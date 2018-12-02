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

DEF VAR bOK           AS LOG    NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.
DEF VAR hDynMenu      AS HANDLE NO-UNDO.
DEF VAR iMenuId       AS INT    NO-UNDO.
DEF VAR wWindows      AS HANDLE NO-UNDO.
DEF VAR ReturnValue   AS INT    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnArtikkelkort btnOnLineRapport RECT-1 ~
RECT-2 FI-Passord 
&Scoped-Define DISPLAYED-OBJECTS FI-Passord 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnArtikkelkort  NO-FOCUS
     LABEL "Artikkelkort" 
     SIZE 47 BY 2.14 TOOLTIP "Kunderodre".

DEFINE BUTTON btnOnLineRapport  NO-FOCUS
     LABEL "ON-Line kassarapport" 
     SIZE 47 BY 2.14 TOOLTIP "ON-Line kassarapport".

DEFINE VARIABLE FI-Passord AS CHARACTER FORMAT "X(999)":U 
     LABEL "Passord" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icon/kundelogo.bmp":U
     STRETCH-TO-FIT RETAIN-SHAPE
     SIZE 46 BY 4.33.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47.2 BY 4.76.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnArtikkelkort AT ROW 1 COL 1
     btnOnLineRapport AT ROW 3.14 COL 1
     FI-Passord AT ROW 10.24 COL 32 COLON-ALIGNED
     IMAGE-1 AT ROW 5.48 COL 1.4
     RECT-1 AT ROW 5.29 COL 1
     RECT-2 AT ROW 10.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 47.4 BY 10.71.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 10.62
         WIDTH              = 47.4
         MAX-HEIGHT         = 22.38
         MAX-WIDTH          = 108.6
         VIRTUAL-HEIGHT     = 22.38
         VIRTUAL-WIDTH      = 108.6
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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


&Scoped-define SELF-NAME btnArtikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnArtikkelkort C-Win
ON CHOOSE OF btnArtikkelkort IN FRAME DEFAULT-FRAME /* Artikkelkort */
DO:
  PUBLISH "ApplyMenu" (hParent,"artko2.p",""). /* Program name (here blank) or menu label */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOnLineRapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOnLineRapport C-Win
ON CHOOSE OF btnOnLineRapport IN FRAME DEFAULT-FRAME /* ON-Line kassarapport */
DO:
  PUBLISH "ApplyMenu" (hParent,"wfrakassarap.w",""). /* Program name (here blank) or menu label */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Passord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Passord C-Win
ON RETURN OF FI-Passord IN FRAME DEFAULT-FRAME /* Passord */
OR TAB OF FI-Passord DO:

    IF NOT VALID-HANDLE(hDynMenu) THEN
    DO WITH FRAME DEFAULT-FRAME:
        RUN JBoxJlwDynMenu.w PERSISTENT SET hDynMenu.

        IF DYNAMIC-FUNCTION("runProc","get_mainmenu.p",FI-Passord:SCREEN-VALUE,?) THEN 
          iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
        RUN InitializeObject IN hDynMenu (iMenuId).
        RUN MoveToTop IN hDynMenu.

        ASSIGN
            FI-Passord:SCREEN-VALUE = "".

        SUBSCRIBE TO "InvalidateHandle" IN hParent.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        RUN MoveToTop IN hDynMenu.

        ASSIGN
            FI-Passord:SCREEN-VALUE = "".

        RETURN NO-APPLY.
    END.


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
  RUN disable_UI.
  APPLY "close" TO hParent.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

/* Starter bibliotek for API manuelt. */
RUN VALUE("windows.p") PERSISTENT SET wWindows.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.


  RUN SendMessageA in wWindows (FI-Passord:hWnd, 
                                204, 
                                ASC("*"),                                0,
                                OUTPUT ReturnValue).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'return' OF {&WINDOW-NAME} ANYWHERE
  APPLY "choose" TO btnOnLineRapport IN FRAME {&FRAME-NAME}.

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
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY FI-Passord 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnArtikkelkort btnOnLineRapport RECT-1 RECT-2 FI-Passord 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DEF VAR cImage AS CHAR NO-UNDO.

ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
       THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
       .

DO WITH FRAME {&FRAME-NAME}:
  PUBLISH "getMenuImage" ("artko2.p",OUTPUT cImage).

  IF cImage NE "" THEN
    btnArtikkelkort:LOAD-IMAGE(cImage).

  PUBLISH "getMenuImage" ("wfrakassarap.w",OUTPUT cImage).

  IF cImage NE "" THEN
    btnOnLineRapport:LOAD-IMAGE(cImage).
END.

DYNAMIC-FUNCTION("setResizeTypes",THIS-PROCEDURE:CURRENT-WINDOW,"frame,image").

/* Resize settings (always needed since it also brings up the help menu): */
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).

/* IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser", "WHERE cJBoxUserId = " + QUOTER(DYNAMIC-FUNCTION("getASUserId")),"bSuperUser")) = NO */
/*    AND DYNAMIC-FUNCTION("getFieldValues","JBoxUserGroupMembers"                                                                               */
/*                         ,"WHERE cJBoxUserId = " + QUOTER(DYNAMIC-FUNCTION("getASUserId")) + " AND iJBoxUserGroupId = 1"                       */
/*                         ,"iJBoxUserGroupMembersId") NE ? THEN                                                                                 */
/*   btnJournal:HIDDEN = TRUE.                                                                                                                   */
/*                                                                                                                                               */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF INPUT PARAMETER hProgram AS HANDLE NO-UNDO.
  
  IF hProgram = hParent THEN APPLY 'CLOSE' TO hDynMenu.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

