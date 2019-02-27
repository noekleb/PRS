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

/* Start a utility built with JukeBox */               

DEF VAR hUtil           AS HANDLE NO-UNDO.
DEF VAR cSessfile       AS CHAR   NO-UNDO.
DEF VAR cLine           AS CHAR   NO-UNDO.
DEF VAR cSessId         AS CHAR   NO-UNDO.
DEF VAR cAppFarmFile    AS CHAR   NO-UNDO.
DEF VAR cAppFarmConnect AS CHAR   NO-UNDO INIT "-H 192.168.105.11 -S 13456 -AppService Appfarm -DirectConnect".
DEF VAR hAppFarm        AS HANDLE NO-UNDO.

RUN JBoxLoadLib.p ("JBoxUIlib.p,JBoxASlib.p,ResizeLib.p"
                   + (IF PROVERSION BEGINS "1" THEN ",JBoxFUlib.p" ELSE "")).

cSessfile = SEARCH("incl/custdevmode.i").
IF cSessfile NE ? THEN DO:
  INPUT FROM VALUE(cSessfile).
  REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine MATCHES '*setSessionId*' THEN
      cSessId = REPLACE(REPLACE(REPLACE(ENTRY(NUM-ENTRIES(cLine),cLine),'"',""),".",""),")","").
  END.
  INPUT CLOSE.
END.
IF cSessId = "" OR cSessId = "ocSessionId" THEN cSessId = "validsession".

cAppFarmFile = SEARCH("appFarmConnect.txt").
IF cAppFarmFile NE ? THEN DO:
  INPUT FROM VALUE(cAppFarmFile).
  REPEAT:
    IMPORT UNFORMATTED cLine.
    IF NOT TRIM(cLine) BEGINS '#' THEN
      cAppFarmConnect = cLine.
  END.
  INPUT CLOSE.
END.
  

DYNAMIC-FUNCTION("setSessionId",cSessId).
DYNAMIC-FUNCTION("setLanguageCode","EN").
DYNAMIC-FUNCTION("setAppTitle","AppComp").
DYNAMIC-FUNCTION("setBehaviour",
                  "DefaultSortFont|6," +   
                  "DefaultSortColor|15," + 
                  "BrowseSearchDefault|goto," +
                  "TabOnReturn|yes," +       
                  "SetSortLabel|yes"
                  ).

DEF TEMP-TABLE ttHandles
    FIELD hUtil     AS HANDLE
    FIELD cProgram  AS CHAR
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExec cmbTool tbAlwaysOnTop 
&Scoped-Define DISPLAYED-OBJECTS cmbTool tbAlwaysOnTop 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExec 
     IMAGE-UP FILE "bmp/active16.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Activate" 
     SIZE 4.8 BY 1.14.

DEFINE VARIABLE cmbTool AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tool" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Dictionary Viewer;DictView.w;","Clipboard Converter;ClipboardConversion.w" 
     DROP-DOWN-LIST
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE tbAlwaysOnTop AS LOGICAL INITIAL yes 
     LABEL "Always on top" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 TOOLTIP "Keep clipboard conversion always on top" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExec AT ROW 1.29 COL 36.2 WIDGET-ID 6
     cmbTool AT ROW 1.38 COL 5.4 COLON-ALIGNED WIDGET-ID 2
     tbAlwaysOnTop AT ROW 2.57 COL 7.2 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 53 BY 2.52 WIDGET-ID 100.


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
         TITLE              = "JukeBox Tool Selector"
         HEIGHT             = 2.52
         WIDTH              = 40.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80.4
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
       FRAME DEFAULT-FRAME:HEIGHT           = 2.52
       FRAME DEFAULT-FRAME:WIDTH            = 53.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox Tool Selector */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox Tool Selector */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExec C-Win
ON CHOOSE OF btnExec IN FRAME DEFAULT-FRAME /* Activate */
DO:
  DEF VAR cSelectedTool AS CHAR NO-UNDO.
  
  cSelectedTool = ENTRY(1,cmbTool:SCREEN-VALUE,"|").
  
  IF SEARCH(cSelectedTool) NE ? THEN DO:
    IF cSelectedTool MATCHES "*w" THEN DO:
      IF cSelectedTool = "JBoxMessageMaint.w" AND 
         DYNAMIC-FUNCTION("getFieldValues","_file","WHERE _file-name = 'JBoxMessage'","_file-name") = ? THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"JBoxMessage table not installed","","").
        RETURN NO-APPLY.
      END.
      ELSE IF cSelectedTool = "JBoxGenCode.w" AND
           DYNAMIC-FUNCTION("getFieldValues","_file","WHERE _file-name = 'JBoxGenCode'","_file-name") = ? THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"JBoxGenCode / JBoxGenCodeType table not installed","","").
        RETURN NO-APPLY. 
      END.
      ELSE IF cSelectedTool = "JBoxSysParam.w" AND
           DYNAMIC-FUNCTION("getFieldValues","_file","WHERE _file-name = 'JBoxSysParam'","_file-name") = ? THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"JBoxSysParam table not installed","","").
        RETURN NO-APPLY. 
      END.
      ELSE IF cSelectedTool = "JBoxPackage.w" THEN DO:
        IF NOT VALID-HANDLE(hAppFarm) THEN DO:
          CREATE SERVER hAppFarm.
          hAppFarm:CONNECT(cAppFarmConnect) NO-ERROR.
        END.
      END.

      FIND FIRST ttHandles 
           WHERE ttHandles.cProgram = cSelectedTool
           NO-ERROR.
      IF NOT AVAIL ttHandles OR NOT VALID-HANDLE(ttHandles.hUtil) THEN DO:
        IF NOT AVAIL ttHandles THEN DO:
          CREATE ttHandles.
          ttHandles.cProgram = cSelectedTool.
        END.
        RUN VALUE(cSelectedTool) PERSIST SET ttHandles.hUtil.
        IF CAN-DO(ttHandles.hUtil:INTERNAL-ENTRIES,"InitializeObject") THEN
          RUN InitializeObject IN ttHandles.hUtil.
        IF CAN-DO(ttHandles.hUtil:INTERNAL-ENTRIES,"MoveToTop") THEN
          RUN MoveToTop IN ttHandles.hUtil.
      END.
      ELSE DO:
        IF CAN-DO(ttHandles.hUtil:INTERNAL-ENTRIES,"MoveToTop") THEN
          RUN MoveToTop IN ttHandles.hUtil.
        ELSE
          ttHandles.hUtil:CURRENT-WINDOW:MOVE-TO-TOP() NO-ERROR.
      END.
    END.
    ELSE RUN VALUE(cSelectedTool).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAlwaysOnTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAlwaysOnTop C-Win
ON VALUE-CHANGED OF tbAlwaysOnTop IN FRAME DEFAULT-FRAME /* Always on top */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = tbAlwaysOnTop:CHECKED.
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
  RUN enable_UI.
  ASSIGN cmbTool:DELIMITER = ";".
         cmbTool:LIST-ITEM-PAIRS =
                 "Application compiler;AppComp.w"
               + ";Application Client compiler;clientComp.p"
               + ";Clipboard converter;ClipboardConversion.w"
               + ";Dictionary Viewer;DictView.w"
               + ";Server proc. templates;selectServerTemplate.w"
               + ";Explore TEMP-DIR;ExploreTempDir.p"
               + ";Extract images;findImages.w"
               + ";Find in files;Extractfromfiles.w"
               + ";General code maintenance;JBoxGenCode.w"
               + ";JukeBox Help;startJukeBoxhelp.p"
               + ";Message maintenance;JBoxMessageMaint.w"
               + ";System parameters;JBoxSysParam.w"
               + ";AB Hack (Sebastien L);protools\abhack\abhackwin.w"
/*               + (IF cAppFarmConnect NE "" THEN*/
/*                  ";AppFarm;JBoxPackage.w"     */
/*                  ELSE "")                     */
/*                + ";"  */
                 .
  APPLY "value-changed" TO tbAlwaysOnTop.
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
  DISPLAY cmbTool tbAlwaysOnTop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnExec cmbTool tbAlwaysOnTop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

