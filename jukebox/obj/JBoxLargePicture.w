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

DEF VAR bOK       AS LOG    NO-UNDO.
DEF VAR ix        AS INT    NO-UNDO.
DEF VAR cContext  AS CHAR   NO-UNDO.
DEF VAR hParent   AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttPicture NO-UNDO
    FIELD iJBoxDocumentId  AS INT
    FIELD blDocument       AS BLOB
    FIELD iDocSize         AS INT
    FIELD cFileType        AS CHAR
    FIELD RowIdent         AS CHAR
    .
DEF VAR hBufPicture AS HANDLE.
hBufPicture = BUFFER ttPicture:HANDLE.

DEF STREAM strPicture.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmImage

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edPictureDesc 
&Scoped-Define DISPLAYED-OBJECTS edPictureDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPictureFile C-Win 
FUNCTION LoadPictureFile RETURNS LOGICAL
  ( INPUT icPictureFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PictureToDisk C-Win 
FUNCTION PictureToDisk RETURNS CHARACTER
  ( INPUT iiDocId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setContext C-Win 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPictureDesc C-Win 
FUNCTION setPictureDesc RETURNS LOGICAL
  ( INPUT icPictureDesc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowPicture C-Win 
FUNCTION ShowPicture RETURNS LOGICAL
  ( INPUT iiDocId   AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-view AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-view AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE edPictureDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 124.8 BY 3.81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmImage
     edPictureDesc AT ROW 19.81 COL 2.2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127 BY 22.76.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 22.76
         WIDTH              = 127
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmImage
   FRAME-NAME                                                           */
ASSIGN 
       edPictureDesc:READ-ONLY IN FRAME frmImage        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-view ASSIGN
       FRAME           = FRAME frmImage:HANDLE
       ROW             = 1.24
       COLUMN          = 2
       HEIGHT          = 18.33
       WIDTH           = 125
       HIDDEN          = no
       SENSITIVE       = yes.
/* IMAGE-view OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      IMAGE-view:MOVE-BEFORE(edPictureDesc:HANDLE IN FRAME frmImage).

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* <insert window title> */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-view
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-view C-Win OCX.DblClick
PROCEDURE IMAGE-view.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
APPLY "close" TO THIS-PROCEDURE.

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
ON CLOSE OF THIS-PROCEDURE DO:
  DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).  
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

OCXFile = SEARCH( "JBoxLargePicture.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-view = IMAGE-view:COM-HANDLE
    UIB_S = chIMAGE-view:LoadControls( OCXFile, "IMAGE-view":U)
    IMAGE-view:NAME = "IMAGE-view":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "JBoxLargePicture.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeletePictureRecord C-Win 
PROCEDURE DeletePictureRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("DoMessage",0,4,
                    IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett bilde?" ELSE "Delete picture?","","") = 6 THEN DO:
  RUN DeletePicture IN hParent NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
    APPLY "close" TO THIS-PROCEDURE.
END.
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
  DISPLAY edPictureDesc 
      WITH FRAME frmImage IN WINDOW C-Win.
  ENABLE edPictureDesc 
      WITH FRAME frmImage IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmImage}
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
DEF VAR cPictureDesc AS CHAR NO-UNDO.
DEF VAR bCanDelete   AS LOG  NO-UNDO.

IF VALID-HANDLE(hParent) THEN DO WITH FRAME {&FRAME-NAME}:                                         
  cPictureDesc = DYNAMIC-FUNCTION("getPictureDesc" IN hParent) NO-ERROR.
  IF cPictureDesc NE "" AND cPictureDesc NE ? THEN
    edPictureDesc:SCREEN-VALUE = cPictureDesc.
  ELSE
    ASSIGN edPictureDesc:HIDDEN = TRUE
           IMAGE-view:HEIGHT-PIXELS = IMAGE-view:HEIGHT-PIXELS + 80.

  IF CAN-DO(hParent:INTERNAL-ENTRIES,"canDeletePicture") 
     AND CAN-DO(hParent:INTERNAL-ENTRIES,"DeletePicture")
     AND DYNAMIC-FUNCTION("canDeletePicture" IN hParent) THEN 
    DYNAMIC-FUNCTION("NewMenuBand",THIS-PROCEDURE:CURRENT-WINDOW
                    ,"DeletePicture;" + IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett bilde" ELSE "Delete picture"
                    ,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File").
END.

DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"edPictureDesc").
DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-view").
DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-view").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,100,100).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPictureFile C-Win 
FUNCTION LoadPictureFile RETURNS LOGICAL
  ( INPUT icPictureFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF SEARCH(icPictureFile) NE ? THEN DO:
  ASSIGN chIMAGE-view:Picbuf:FILENAME = icPictureFile
         chIMAGE-view:Picbuf:AutoScale = TRUE.
  chIMAGE-view:Picbuf:LOAD NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) BEGINS "Error" THEN
    DYNAMIC-FUNCTION('setWebDoc','open',icPictureFile).
  FRAME frmImage:MOVE-TO-TOP().
  SESSION:SET-WAIT-STATE("").
  RETURN TRUE. 
END.
ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PictureToDisk C-Win 
FUNCTION PictureToDisk RETURNS CHARACTER
  ( INPUT iiDocId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPictureFile AS CHAR   NO-UNDO.
DEF VAR mpDocument   AS MEMPTR NO-UNDO.

EMPTY TEMP-TABLE ttPicture.

DYNAMIC-FUNCTION("getTempTable","jbserv_gettemptable.p","JBoxDocument|WHERE iJBoxDocumentId = " + STRING(iiDocId),hBufPicture).

FIND FIRST ttPicture NO-ERROR.
IF NOT AVAIL ttPicture THEN 
  RETURN "".
ELSE cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + "." + ttPicture.cFileType.

SET-SIZE(mpDocument) = ttPicture.iDocSize.
COPY-LOB FROM OBJECT ttPicture.blDocument TO OBJECT mpDocument.
OUTPUT STREAM strPicture TO VALUE(cPictureFile) NO-MAP BINARY NO-CONVERT.
EXPORT STREAM strPicture mpDocument.
OUTPUT STREAM strPicture CLOSE.
  
RETURN cPictureFile.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setContext C-Win 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cContext = icContext.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
-----------------------------------------------------------------------------*/
hParent = ihParent.
RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPictureDesc C-Win 
FUNCTION setPictureDesc RETURNS LOGICAL
  ( INPUT icPictureDesc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF icPictureDesc NE "" THEN DO:
    IF edPictureDesc:HIDDEN THEN 
      ASSIGN IMAGE-view:HEIGHT-PIXELS   = IMAGE-view:HEIGHT-PIXELS - 80
             edPictureDesc:HIDDEN       = FALSE.
    edPictureDesc:SCREEN-VALUE = icPictureDesc.
  END.
  ELSE
    ASSIGN IMAGE-view:HEIGHT-PIXELS   = IMAGE-view:HEIGHT-PIXELS + 80
           edPictureDesc:HIDDEN       = TRUE.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowPicture C-Win 
FUNCTION ShowPicture RETURNS LOGICAL
  ( INPUT iiDocId   AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPictureFile AS CHAR NO-UNDO.

chIMAGE-view:Picbuf:CLEAR(2).

IF iiDocId = 0 OR iiDocId = ? THEN
  RETURN FALSE.

IF cContext = "" THEN cContext = "img".

cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".jpg".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".jpeg".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".bmp".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".tif".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".tiff".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".pcx".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = PictureToDisk (iiDocId). 

RETURN LoadPictureFile(cPictureFile).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

