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
&SCOPED-DEFINE PureABLWin 1

DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hTBFiles          AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR cFileTypes        AS CHAR   NO-UNDO.
DEF VAR iErrorCount       AS INT    NO-UNDO.
DEF VAR iCompileCount     AS INT    NO-UNDO.
DEF VAR cCompileStatus    AS CHAR   NO-UNDO.
DEF VAR bCancelCompile    AS LOG    NO-UNDO.
DEF VAR ghMainAppBuilderWindow AS HANDLE NO-UNDO.
DEF VAR hMenuItemOpenFile AS HANDLE NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwFiles rectTBFiles SearchField ~
btnStartCompile cmbDefaults cmbSaveInto fiFileTypes tbSave tbCompileSubdir 
&Scoped-Define DISPLAYED-OBJECTS cmbDefaults cmbSaveInto fiFileTypes tbSave ~
tbCompileSubdir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildDefault C-Win 
FUNCTION BuildDefault RETURNS LOGICAL
  ( INPUT icDefault AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadDefaults C-Win 
FUNCTION LoadDefaults RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadFiles C-Win 
FUNCTION LoadFiles RETURNS CHARACTER
  ( INPUT icFileNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadFilesForDefault C-Win 
FUNCTION LoadFilesForDefault RETURNS LOGICAL
  ( INPUT icKey AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewRecordCount C-Win 
FUNCTION ViewRecordCount RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnStartCompile 
     LABEL "Compile now" 
     SIZE 47.4 BY 1.14 TOOLTIP "Drop files here for immediate compile" DROP-TARGET.

DEFINE VARIABLE cmbDefaults AS CHARACTER FORMAT "X(256)":U 
     LABEL "Defaults" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "","",
                     "JukeBox project","JBpro",
                     "JukeBox","JB"
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cmbSaveInto AS CHARACTER 
     LABEL "Save into" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "","Server","Winsrc" 
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 50 BY 1 TOOLTIP "Default folder for r-code. Can be overridden for each file/folder" NO-UNDO.

DEFINE VARIABLE fiFileTypes AS CHARACTER FORMAT "X(256)":U INITIAL "*.p *.w *.cls" 
     LABEL "File types" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrwFiles
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 15.24.

DEFINE RECTANGLE rectTBFiles
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.

DEFINE RECTANGLE SearchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY .95.

DEFINE VARIABLE tbCompileSubdir AS LOGICAL INITIAL yes 
     LABEL "Compile sub-folders" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY .81 NO-UNDO.

DEFINE VARIABLE tbSave AS LOGICAL INITIAL yes 
     LABEL "Save r-code" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnStartCompile AT ROW 19.33 COL 2.6
     cmbDefaults AT ROW 1.38 COL 27.8 COLON-ALIGNED
     cmbSaveInto AT ROW 19.38 COL 59 COLON-ALIGNED
     fiFileTypes AT ROW 1.38 COL 70 COLON-ALIGNED
     tbSave AT ROW 1.52 COL 91.2
     tbCompileSubdir AT ROW 1.52 COL 107.6
     rectBrwFiles AT ROW 3.86 COL 1.8
     rectTBFiles AT ROW 1.43 COL 2
     SearchField AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.6 BY 19.71.


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
         TITLE              = "Compile procedures"
         HEIGHT             = 19.67
         WIDTH              = 130.6
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME L-To-R,COLUMNS                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Compile procedures */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Compile procedures */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Compile procedures */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartCompile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartCompile C-Win
ON CHOOSE OF btnStartCompile IN FRAME DEFAULT-FRAME /* Compile now */
DO:
  DEF VAR cCompList AS CHAR NO-UNDO.

  IF btnStartCompile:LABEL = "Cancel (press once)" THEN DO:
    bCancelCompile = YES.
/*     MESSAGE PROGRAM-NAME(1) SKIP            */
/*              SKIP                           */
/*             VIEW-AS ALERT-BOX.              */
/*     SESSION:SET-WAIT-STATE("general").      */
/*     btnStartCompile:LABEL = "Compile now".  */
  END.
  ELSE DO:
/*     btnStartCompile:LABEL = "Cancel (press once)".  */
    SESSION:SET-WAIT-STATE("general").
    IF hBrowse:NUM-SELECTED-ROWS > 0 THEN
      DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
        IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
          cCompList = cCompList + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) + ",".
      END.
    RUN StartCompileRecord (cCompList).
    btnStartCompile:LABEL = "Compile now".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartCompile C-Win
ON DROP-FILE-NOTIFY OF btnStartCompile IN FRAME DEFAULT-FRAME /* Compile now */
DO:
  DEF VAR cFileNames AS CHAR   NO-UNDO.

  hBuffer:EMPTY-TEMP-TABLE().

  DO ix = 1 TO SELF:NUM-DROPPED-FILES:
    cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".
  END.
  IF cFileNames NE "" THEN 
    RUN StartCompileRecord (LoadFiles(TRIM(cFileNames,";"))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbDefaults C-Win
ON VALUE-CHANGED OF cmbDefaults IN FRAME DEFAULT-FRAME /* Defaults */
DO:
  IF cmbDefaults:SCREEN-VALUE BEGINS "jb" OR cmbDefaults:SCREEN-VALUE = ? THEN
    BuildDefault(IF cmbDefaults:SCREEN-VALUE = ? THEN "" ELSE cmbDefaults:SCREEN-VALUE).
  ELSE LoadFilesForDefault(cmbDefaults:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFileTypes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFileTypes C-Win
ON LEAVE OF fiFileTypes IN FRAME DEFAULT-FRAME /* File types */
DO: 
  cFileTypes = TRIM(REPLACE(fiFileTypes:SCREEN-VALUE," ",","),",").
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.  
    
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'drop-file-notify' OF hBrowse DO:
  DEF VAR cFileNames AS CHAR   NO-UNDO.

  DO ix = 1 TO SELF:NUM-DROPPED-FILES:
    cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".
  END.
  IF cFileNames NE "" THEN 
    LoadFiles(TRIM(cFileNames,";")).
END.

ON 'window-resized' OF {&WINDOW-NAME} DO:
  DEF VAR hColumn AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  hColumn = hBrowse:GET-BROWSE-COLUMN(2).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompileMe C-Win 
PROCEDURE CompileMe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icFileOrDir AS CHAR NO-UNDO.
DEF INPUT  PARAM icFileType  AS CHAR NO-UNDO.
DEF INPUT  PARAM icSaveInto  AS CHAR NO-UNDO.
DEF INPUT  PARAM iiSeq       AS INT  NO-UNDO.

DEF VAR cOSfile             AS CHAR NO-UNDO.
DEF VAR cSaveInto           AS CHAR NO-UNDO.
DEF VAR iSeq                AS INT  NO-UNDO.
DEF VAR bMatch              AS LOG  NO-UNDO.
DEF VAR ix                  AS INT  NO-UNDO.
DEF VAR cSubDirList         AS CHAR NO-UNDO.
DEF VAR cSubDirSaveIntoList AS CHAR NO-UNDO.

ASSIGN iSeq      = iiSeq
       cSaveInto = icSaveInto
       .
IF cSaveInto = "" AND cmbSaveInto:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ? THEN
  cSaveInto = cmbSaveInto:SCREEN-VALUE.

IF icFileType BEGINS "DR" THEN DO:
  INPUT FROM OS-DIR(icFileOrDir).

  IF cSaveInto NE ""  THEN
    OS-COMMAND SILENT mkdir VALUE('"' + cSaveInto + '"').

  REPEAT:
    IMPORT cOSfile.
    FILE-INFO:FILE-NAME = icFileOrDir + "\" + cOsFile.

    bMatch = NO.
    DO ix = 1 TO NUM-ENTRIES(cFileTypes):
      IF cOSfile MATCHES ENTRY(ix,cFileTypes) THEN 
        bMatch = YES.
    END.
    IF bMatch AND FILE-INFO:FILE-TYPE NE "DRW" THEN DO:
      bOk = hBuffer:FIND-FIRST("WHERE FileName = '" + icFileOrDir + "\" + cOsFile + "'") NO-ERROR.
      IF NOT bOK THEN DO:
        hBuffer:BUFFER-CREATE().
        ASSIGN iSeq = iSeq + 1
               hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
               hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = icFileOrDir + "\" + cOsFile
               hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = SUBSTR(cOsFile,LENGTH(cOsFile) - 1)
               hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cSaveInto
               hBuffer:BUFFER-FIELD("Generated"):BUFFER-VALUE = YES
               hBuffer:BUFFER-FIELD("CompileDate"):BUFFER-VALUE = TODAY
               hBuffer:BUFFER-FIELD("CompileTime"):BUFFER-VALUE = STRING(TIME,"HH:MM:SS")
               .
      END.

      IF tbSave:CHECKED THEN DO:
        IF cSaveInto NE "" THEN
          COMPILE VALUE(icFileOrDir + "\" + cOsFile) SAVE INTO VALUE(cSaveInto) NO-ERROR.
        ELSE DO:
          COMPILE VALUE(icFileOrDir + "\" + cOsFile) SAVE NO-ERROR.
        END.
      END.
      ELSE COMPILE VALUE(icFileOrDir + "\" + cOsFile) NO-ERROR.
        
/*       PROCESS EVENTS.                 */
/*       IF bCancelCompile THEN DO:      */
/*         MESSAGE PROGRAM-NAME(1) SKIP  */
/*                 "ca1" SKIP            */
/*                 VIEW-AS ALERT-BOX.    */
/*         RETURN "cancel".              */
/*       END.                            */

      ASSIGN hBuffer:BUFFER-FIELD("ErrorMessage"):BUFFER-VALUE = ERROR-STATUS:GET-MESSAGE(1)
             iCompileCount = iCompileCount + 1.
      IF hBuffer:BUFFER-FIELD("ErrorMessage"):BUFFER-VALUE NE "" THEN
        iErrorCount = iErrorCount + 1.
    END.
    ELSE IF cOsFile NE ".." AND cOsFile NE "." AND FILE-INFO:FILE-TYPE BEGINS "DR" AND tbCompileSubdir:CHECKED THEN 
      ASSIGN cSubDirList         = cSubDirList         + FILE-INFO:FULL-PATHNAME + "|"
             cSubDirSaveIntoList = cSubDirSaveIntoList + (IF cSaveInto NE "" THEN cSaveInto + "\" ELSE "") + cOsFile + "|"
             .
  END.
  INPUT CLOSE.

  RUN InvokeMethod(hBrowse,"OpenQuery").

  cSubDirList = TRIM(cSubDirList,"|").
  DO ix = 1 TO NUM-ENTRIES(cSubDirList,"|"):
    iSeq = iSeq + 1000.
    RUN CompileMe (ENTRY(ix,cSubDirList,"|"),"DRW",ENTRY(ix,cSubDirSaveIntoList,"|"),iiSeq + iSeq) NO-ERROR.
    IF RETURN-VALUE = "cancel" THEN DO:
      MESSAGE PROGRAM-NAME(1) SKIP
               SKIP
              VIEW-AS ALERT-BOX.
      RETURN "cancel".
    END.

    IF ix MOD 20 = 0 THEN DO:
      cCompileStatus = " [" + STRING(iCompileCount) + " files compiled. " + STRING(iErrorCount) + " with errors]". 

/*       PROCESS EVENTS.        */
/*       IF bCancelCompile THEN */
/*         RETURN NO.           */
    END.
  END.

END.
ELSE DO:
  hBuffer:FIND-FIRST("WHERE FileName = '" + icFileOrDir + "'").

  IF tbSave:CHECKED THEN DO:
    IF cSaveInto NE "" THEN
      COMPILE VALUE(icFileOrDir) SAVE INTO VALUE(cSaveInto) NO-ERROR.
    ELSE 
      COMPILE VALUE(icFileOrDir) SAVE NO-ERROR.
  END.
  ELSE COMPILE VALUE(icFileOrDir) NO-ERROR.

  ASSIGN hBuffer:BUFFER-FIELD("ErrorMessage"):BUFFER-VALUE = ERROR-STATUS:GET-MESSAGE(1)
         hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cSaveInto
         hBuffer:BUFFER-FIELD("CompileDate"):BUFFER-VALUE = TODAY
         hBuffer:BUFFER-FIELD("CompileTime"):BUFFER-VALUE = STRING(TIME,"HH:MM:SS")
         iCompileCount = iCompileCount + 1
         .
  IF hBuffer:BUFFER-FIELD("ErrorMessage"):BUFFER-VALUE NE "" THEN
    iErrorCount = iErrorCount + 1.

/*   PROCESS EVENTS.                 */
/*   IF bCancelCompile THEN DO:      */
/*     MESSAGE PROGRAM-NAME(1) SKIP  */
/*              SKIP                 */
/*             VIEW-AS ALERT-BOX.    */
/*     RETURN "cancel".              */
/*   END.                            */

END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRowRecord C-Win 
PROCEDURE DeselectRowRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowse:DESELECT-ROWS().
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL THEN DO:
  IF hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "drw" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","OpenFile").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledEvents","").
END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditDefaultRecord C-Win 
PROCEDURE EditDefaultRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN CompileDefault.w PERSISTENT.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.
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
  DISPLAY cmbDefaults cmbSaveInto fiFileTypes tbSave tbCompileSubdir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrwFiles rectTBFiles SearchField btnStartCompile cmbDefaults 
         cmbSaveInto fiFileTypes tbSave tbCompileSubdir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findAppBuilder C-Win 
PROCEDURE findAppBuilder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:   14-SEP-2007 sla: more reliable proof to catch the AppBuilder Window
------------------------------------------------------------------------------*/

/* 05-JAN-2007 sla: We will probably need this guy soon to add a penguin button on it */
DEFINE VARIABLE hCheckFirstMenuItem    AS HANDLE     NO-UNDO.
ghMainAppBuilderWindow = SESSION:FIRST-CHILD.

DO WHILE ghMainAppBuilderWindow <> ?:
    /* 14-SEP-2007 sla: as asked by Jan Keirse, some people change the title of the AppBuilder, so let's be more flexible' */
    /* IF ghMainAppBuilderWindow:TITLE = "AppBuilder" THEN DO: */
    IF ghMainAppBuilderWindow:TITLE MATCHES "*AppBuilder" THEN DO:
        
        /* 14-SEP-2007 sla: safer test afterward */
        /* hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR. */
        /*IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:FIRST-CHILD NO-ERROR. */
        /*IF   VALID-HANDLE(hCheckFirstMenuItem)                                                                    */
        /* AND hCheckFirstMenuItem:LABEL = "&File"                                                                  */
        /* THEN LEAVE.                                                                                              */
    
        hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR.
        IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.
        
        /* 06-DEC-2007 bot: Make sure we have the "&Help" menu, since this is not always the last one with modified appbuilders */
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL <> "&Help":U THEN DO WHILE hCheckFirstMenuItem <> ?:
           ASSIGN hCheckFirstMenuItem = hCheckFirstMenuItem:PREV-SIBLING NO-ERROR. /*Walk back from the last menuitem*/
           IF   VALID-HANDLE(hCheckFirstMenuItem)
            AND hCheckFirstMenuItem:LABEL = "&Help":U
              THEN LEAVE. /* We found the correct menuitem*/
         END.
        /* 06-DEC-2007 bot: now return to the "About AppBuilder" check*/
        
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL = "&Help" THEN DO:
            hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.
            
            IF VALID-HANDLE(hCheckFirstMenuItem)
             AND hCheckFirstMenuItem:TYPE = "MENU-ITEM"
             AND hCheckFirstMenuItem:LABEL = "About AppBuilder"
             THEN LEAVE.  /* OK , found a reliable way */
         END.
    
    END.
    ghMainAppBuilderWindow = ghMainAppBuilderWindow:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSaveInto     AS HANDLE NO-UNDO.
DEF VAR hSearchField  AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cFileTypes  = TRIM(REPLACE(fiFileTypes:SCREEN-VALUE," ",","),",")
         cmbDefaults:DELIMITER = "|"
         cmbDefaults:LIST-ITEM-PAIRS = RIGHT-TRIM("||JukeBox project|JBpro|JukeBox|JB|" + LoadDefaults(),"|")
         .
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                                 rectBrwFiles:HANDLE,
                                 100,
                                 "multiple",
                                 "temp-table"
                               + ";+Seq|INTEGER|>>>>>>9||Seq"
                               + ";+FileName|CHARACTER|x(256)||File"
                               + ";+FileType|CHARACTER|x(10)||Type"
                               + ";+SaveInto|CHARACTER|x(256)||Save into"
                               + ";+ErrorMessage|CHARACTER|x(256)||Message"
                               + ";+CompileDate|DATE|99/99/9999||Compile date"
                               + ";+CompileTime|CHARACTER||Time"
                               + ";+!Generated|LOGICAL|Y/N"
                                ,"WHERE false"
                                ,"SORT|Seq").
  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 200.
  hBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 200.
  hBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 200.

  RUN findAppBuilder.

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse,
                   "MultiSortBrowse;Sort on multiple columns"
                 + ",DeselectRow;Deselect rows"
                 + (IF VALID-HANDLE(ghMainAppBuilderWindow) AND 
                       SEARCH("protools/abhack/openFileInAB.p") NE ? THEN
                     ",OpenFile;Open file in AppBuilder using ABhack method (doesn't need ABhack to run)"
                    ELSE "")
                  ,"").
  hMenuItemOpenFile = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemOpenFile")) NO-ERROR.

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hTBFiles = DYNAMIC-FUNCTION("NewToolbar",
                                rectTBFiles:HANDLE,
                                "File",
                                "New,Delete,Filter,Excel"
                              + ",EditDefault;Edit &defaults¤menu enable"
                              + ",rule¤menu"
                              + ",Close;Exit¤menu enable"
                                ,"maxborder").

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hBrowse:DROP-TARGET = TRUE.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"copytoolbartobrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"nodeletewarning","yes").

  hSaveInto = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "SaveInto",     
                    "SaveInto",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSaveInto,"SaveInto").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"copytoolbartobrowse","yes").

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hTBFiles).

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"SearchField").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBFiles").

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,330,100,0,0).

  APPLY "value-changed" TO hBrowse.
END.
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
cmbDefaults:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = RIGHT-TRIM("||JukeBox project|JBpro|JukeBox|JB|" + LoadDefaults(),"|").
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
BuildDefault("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenFileRecord C-Win 
PROCEDURE OpenFileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hOpenFile AS HANDLE NO-UNDO.

RUN protools/abhack/openFileInAB.p 
     PERSISTENT SET hOpenFile
    (hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE, ghMainAppBuilderWindow).

/* DELETE PROCEDURE hOpenFile.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartCompileRecord C-Win 
PROCEDURE StartCompileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icCompList AS CHAR NO-UNDO.

DEF VAR cCompList AS CHAR NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.

ASSIGN iErrorCount    = 0
       iCompileCount  = 0
       bCancelCompile = NO.

/* btnStartCompile:LABEL IN FRAME {&FRAME-NAME} = "Cancel".  */

IF icCompList = "" THEN DO:
  hBrowse:QUERY:GET-FIRST().
  REPEAT WHILE hBuffer:AVAIL:
    IF NOT hBuffer:BUFFER-FIELD("Generated"):BUFFER-VALUE THEN 
      cCompList = cCompList + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) + ",".
    hBrowse:QUERY:GET-NEXT().
  END.
END.
ELSE cCompList = icCompList.
  
cCompList = TRIM(cCompList,",").

DO ix = 1 TO NUM-ENTRIES(cCompList):
  hBuffer:FIND-FIRST("WHERE Seq = " + ENTRY(ix,cCompList)).

  RUN CompileMe(
            hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE,
            hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE,
            hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE,
            hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE).
  IF RETURN-VALUE = "cancel" THEN DO:
    MESSAGE PROGRAM-NAME(1) SKIP
             SKIP
            VIEW-AS ALERT-BOX.
    LEAVE.      
  END.

  IF ix MOD 20 = 0 THEN DO:
    cCompileStatus = " [" + STRING(iCompileCount) + " files compiled. " + STRING(iErrorCount) + " with errors]". 

/*     PROCESS EVENTS.         */
/*     IF bCancelCompile THEN  */
/*       LEAVE.                */
  END.
END.

ASSIGN btnStartCompile:LABEL IN FRAME {&FRAME-NAME} = "Compile now"
       cCompileStatus = " [" + STRING(iCompileCount) + " files compiled. " + STRING(iErrorCount) + " with errors]"
       bCancelCompile = NO. 

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildDefault C-Win 
FUNCTION BuildDefault RETURNS LOGICAL
  ( INPUT icDefault AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iSeq AS INT NO-UNDO.

DEF VAR cJBroot AS CHAR NO-UNDO.

hBuffer:EMPTY-TEMP-TABLE().

ASSIGN cJBroot = SEARCH("jbserv_gettemptablejoin.p")
       cJBroot = SUBSTR(cJBroot,1,R-INDEX(cJBroot,"\") - 1)
       cJBroot = SUBSTR(cJBroot,1,R-INDEX(cJBroot,"\"))
       fiFileTypes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*.p *.w"
       tbSave:SCREEN-VALUE = "yes"
       tbCompileSubdir:SCREEN-VALUE = "yes"
       .
CASE icDefault:
  WHEN "JBpro" THEN DO:
    FILE-INFO:FILE-NAME = ".".
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = FILE-INFO:FULL-PATHNAME + "\winsrc"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "server"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = "server"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = FILE-INFO:FULL-PATHNAME + "\server"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = "server"
           .
  END.
  WHEN "JB" THEN DO:
    tbCompileSubdir:CHECKED IN FRAME {&FRAME-NAME} = NO.
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "oo"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\oo"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "oo\uc"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\oo\uc"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "obj"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\obj"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "lib"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\lib"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "winsrc"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\winsrc"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "winsrc\admin"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\winsrc\admin"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "winsrc\adm2"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\winsrc\adm2"
           .
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = cJBroot + "tools"
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = cJBroot + "build\" + PROVERSION + "\tools"
           .
  END.
  WHEN "" THEN DO:
    IF hBrowse:QUERY:IS-OPEN THEN
      hBrowse:QUERY:QUERY-CLOSE().
  END.

END CASE.
  
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadDefaults C-Win 
FUNCTION LoadDefaults RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cKey    AS CHAR NO-UNDO.
DEF VAR cValue  AS CHAR NO-UNDO.
DEF VAR cDef    AS CHAR NO-UNDO.

DO ix = 1 TO 50:
  cKey = "Default" + STRING(ix).
  GET-KEY-VALUE SECTION "JBoxCompiler" KEY cKey VALUE cValue. 
  IF cValue NE ? THEN
    cDef = cDef + ENTRY(1,cValue,"|") + "|" + cKey + "|". 
END.
  
RETURN cDef.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadFiles C-Win 
FUNCTION LoadFiles RETURNS CHARACTER
  ( INPUT icFileNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iSeq      AS INT  NO-UNDO.
DEF VAR bMatch    AS LOG  NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.
DEF VAR cCompList AS CHAR NO-UNDO.

hBuffer:FIND-LAST() NO-ERROR.
IF hBuffer:AVAIL THEN
  iSeq = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE.

DO ix = 1 TO NUM-ENTRIES(icFileNames,";"):
  FILE-INFO:FILE-NAME = ENTRY(ix,icFileNames,";").
 
  bMatch = NO.
  IF FILE-INFO:FILE-TYPE BEGINS "DR" THEN bMatch = YES.
  ELSE 
    DO iy = 1 TO NUM-ENTRIES(cFileTypes):
      IF ENTRY(ix,icFileNames,";") MATCHES ENTRY(iy,cFileTypes) THEN DO:
        bMatch = YES.
        LEAVE.
      END.
    END.
  IF bMatch THEN DO:
    bOk = hBuffer:FIND-FIRST("WHERE FileName = '" + FILE-INFO:FULL-PATHNAME + "'") NO-ERROR.
    IF NOT bOK THEN DO:
      hBuffer:BUFFER-CREATE().
      ASSIGN iSeq = iSeq + 1000
             hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
             hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = FILE-INFO:FULL-PATHNAME
             hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = IF FILE-INFO:FILE-TYPE BEGINS "DR" THEN 
                                                               FILE-INFO:FILE-TYPE
                                                             ELSE ENTRY(iy,cFileTypes)
             .
    END.
    cCompList = cCompList + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) + ",".
  END.
END.

RUN InvokeMethod(hBrowse,"OpenQuery").
/* DYNAMIC-FUNCTION("setCurrentObject",hBrowse). */
/* RUN OpenQuery.                                */

RETURN cCompList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadFilesForDefault C-Win 
FUNCTION LoadFilesForDefault RETURNS LOGICAL
  ( INPUT icKey AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cKey    AS CHAR NO-UNDO.
DEF VAR cValue  AS CHAR NO-UNDO.
DEF VAR cDef    AS CHAR NO-UNDO.
DEF VAR iSeq    AS INT  NO-UNDO.

hBuffer:EMPTY-TEMP-TABLE().

GET-KEY-VALUE SECTION "JBoxCompiler" KEY icKey VALUE cValue. 
DO ix = 2 TO NUM-ENTRIES(cValue,"|") WITH FRAME {&FRAME-NAME}:
  IF ix = 2 THEN
    fiFileTypes:SCREEN-VALUE = ENTRY(ix,cValue,"|").
  ELSE IF ix = 3 THEN
    tbSave:SCREEN-VALUE = ENTRY(ix,cValue,"|").
  ELSE IF ix = 4 THEN
    tbCompileSubdir:SCREEN-VALUE = ENTRY(ix,cValue,"|").
END.

DO ix = 1 TO 50:
  cKey = icKey + "_" + STRING(ix).
  GET-KEY-VALUE SECTION "JBoxCompiler" KEY cKey VALUE cValue. 
  IF cValue NE ? THEN DO:
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1000
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = ENTRY(1,cValue,"|")
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = ENTRY(2,cValue,"|")
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           .
  END.
END.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewRecordCount C-Win 
FUNCTION ViewRecordCount RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ihObject:HELP = "[Count: " 
              + STRING(INT(DYNAMIC-FUNCTION("getAttribute",ihObject,"recordcount")) + INT(DYNAMIC-FUNCTION("getAttribute",ihObject,"rowsadded")) - INT(DYNAMIC-FUNCTION("getAttribute",ihObject,"rowsdeleted"))) + "]" 
              + cCompileStatus NO-ERROR.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

