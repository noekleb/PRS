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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.                          

DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hSeqOverlay       AS HANDLE NO-UNDO.
DEF VAR hFieldNameOverlay AS HANDLE NO-UNDO.
DEF VAR hDBtableOverlay   AS HANDLE NO-UNDO.
DEF VAR hDBfieldOverlay   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrowse C-Win 
FUNCTION setBrowse RETURNS LOGICAL
  (INPUT iphBrowse AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 5.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrowse AT ROW 1.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 54.6 BY 5.86.


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
         HEIGHT             = 5.91
         WIDTH              = 54.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
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
  ENABLE rectBrowse 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraValChngBrowseDropDown C-Win 
PROCEDURE ExtraValChngBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihCombo  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK    AS LOG NO-UNDO INIT TRUE.

DEF VAR cValueList AS CHAR NO-UNDO.
DEF VAR fValue     AS DEC NO-UNDO.

IF ihCombo = hDBtableOverlay THEN DO:

  IF ihCombo:SCREEN-VALUE <> ? THEN
    hDBfieldOverlay:LIST-ITEM-PAIRS = "||" +
                                 DYNAMIC-FUNCTION("GetFieldList",
                                                  "_file;!_file-name,_field;_field-name;_field-name",
                                                  "WHERE _file-name = '" + ihCombo:SCREEN-VALUE + "'"
                                                + ",EACH _field OF _file NO-LOCK").
  ELSE
    hDBfieldOverlay:LIST-ITEM-PAIRS = "|".

  hDBfieldOverlay:SCREEN-VALUE = ENTRY(1,hDBfieldOverlay:LIST-ITEM-PAIRS,"|").
  APPLY "value-changed" TO hDBfieldOverlay.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FileExportFromTable C-Win 
PROCEDURE FileExportFromTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDBtable AS CHAR NO-UNDO.

IF VALID-HANDLE(hDBfieldOverlay) AND VALID-HANDLE(hBrowse) THEN DO:
  IF icDBtable NE ? THEN
    hDBfieldOverlay:LIST-ITEM-PAIRS = "||" +
                                 DYNAMIC-FUNCTION("GetFieldList",
                                                  "_file;!_file-name,_field;_field-name;_field-name",
                                                  "WHERE _file-name = '" + icDBtable + "'"
                                                + ",EACH _field OF _file NO-LOCK").
  ELSE
    hDBfieldOverlay:LIST-ITEM-PAIRS = "|".
  
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN DisplayRecord.
END.

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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           rectBrowse:HANDLE,        /* Coordinates */
                           100,                      /* Batchsize */
                           "",                       /* Attributes that must be set at creation time for the browse */
                           "JBoxFileExportSpec"              /* Buffers and fields (and calculated fields) for browse */
                           + ";iSeq"
                           + ";+cAlfaSeq|CHARACTER|x(3)|jbserv_seqno_to_alpha.p(iSeq)|Alpha"
                           + ";cFieldName;cDBfieldName"
                           , 
                           "WHERE false",  
                                                    /* Query string (where clause is split into 3 parts internally: BaseQuery, QueryFilter and QueryWhere) */
                           "sort|iSeq").         /* Initial sort column */
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  hDBfieldOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                    hBrowse,          
                    "cDBfieldName",     
                    "cDBfieldName",     
                    "","","|").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hDBfieldOverlay,"cDBfieldName").

  hSeqOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "iSeq",     
                    "iSeq",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSeqOverlay,"iSeq").
  DYNAMIC-FUNCTION("setAttribute",hSeqOverlay,"refreshrow","yes").

  hFieldNameOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "cFieldName",     
                    "cFieldName",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFieldNameOverlay,"cFieldName").

  SUBSCRIBE TO "FileExportFromTable" ANYWHERE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.
hColumn = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hColumn.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

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
DEF VAR rRepos    AS ROWID NO-UNDO.
DEF VAR iNextSeq  AS INT NO-UNDO.

hParentBrowse = DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"parent","from").

IF DYNAMIC-FUNCTION("DoCreate",hBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME,"",
                    "iJBoxFileExportHeaderId",
                    STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxFileExportHeaderId"):BUFFER-VALUE),
                    TRUE) THEN DO:

  IF hBrowse:QUERY:IS-OPEN THEN DO:
    hBrowse:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
      IF hBrowse:QUERY:GET-BUFFER-HANDLE:BUFFER-FIELD("iSeq"):BUFFER-VALUE > iNextSeq THEN 
        iNextSeq = hBrowse:QUERY:GET-BUFFER-HANDLE:BUFFER-FIELD("iSeq"):BUFFER-VALUE.
      hBrowse:QUERY:GET-NEXT().
    END.
  END.
  iNextSeq = iNextSeq + 1.

  hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-CREATE.
  rRepos = hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID.
  
  DYNAMIC-FUNCTION("DoRefetchTrans",hBrowse:QUERY:GET-BUFFER-HANDLE(1),"FIRST","").
  DYNAMIC-FUNCTION("refreshRow",hBrowse,
                    DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields"),
                    DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin")).


  hBrowse:QUERY:QUERY-OPEN().
  hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).

  hBrowse:QUERY:GET-BUFFER-HANDLE:BUFFER-FIELD("iSeq"):BUFFER-VALUE = iNextSeq.

  APPLY "value-changed" TO hBrowse.
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).

  APPLY "entry" TO hSeqOverlay.
  ASSIGN hSeqOverlay:MODIFIED = TRUE
         hDBfieldOverlay:MODIFIED = TRUE
         .
  APPLY "return" TO hSeqOverlay.
  APPLY "value-changed" TO hDBfieldOverlay.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrowse C-Win 
FUNCTION setBrowse RETURNS LOGICAL
  (INPUT iphBrowse AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
  hBrowse = iphBrowse.
  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  hParent = ihParent.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

