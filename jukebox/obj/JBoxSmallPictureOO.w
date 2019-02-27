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
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR cContext      AS CHAR   NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.
DEF VAR cPictureFile  AS CHAR   NO-UNDO.
DEF VAR hLargePicture AS HANDLE NO-UNDO.
DEF VAR cPictureDesc  AS CHAR   NO-UNDO.

DEFINE VARIABLE oJboxImage     AS uc.JboxImage      NO-UNDO. 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPictureDesc C-Win 
FUNCTION getPictureDesc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAsDropTarget C-Win 
FUNCTION setAsDropTarget RETURNS LOGICAL
  ( INPUT ibDropTarget AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setContext C-Win 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR )  FORWARD.

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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmImage
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 8.14 BY 1.58.


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
         HEIGHT             = 3.62
         WIDTH              = 28
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 86.8
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 86.8
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
/* SETTINGS FOR FRAME frmImage
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


&Scoped-define SELF-NAME frmImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmImage C-Win
ON DROP-FILE-NOTIFY OF FRAME frmImage
DO:
  PUBLISH "LoadDroppedFiles" (FRAME frmImage:HANDLE).
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
  oJBoxImage:clear().
  DELETE OBJECT oJboxImage NO-ERROR.
  IF VALID-HANDLE(hParent) THEN DO:
    IF CAN-DO(hParent:INTERNAL-ENTRIES,"CloseFromPictureView") THEN
      RUN CloseFromPictureView IN hParent.
    ELSE
      APPLY "close" TO hParent.
  END.
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
  hParent = SOURCE-PROCEDURE.
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
  HIDE FRAME frmImage.
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
  VIEW FRAME frmImage.
  {&OPEN-BROWSERS-IN-QUERY-frmImage}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImageClickEvent C-Win 
PROCEDURE ImageClickEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "ShowLargePicture" (cPictureFile,OUTPUT bOk).

IF NOT bOk THEN DO:
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    IF NOT VALID-HANDLE(hLargePicture) THEN
      RUN JBoxLargePicture.w PERSIST SET hLargePicture.
  &ELSE
    PUBLISH "StartChildWindow" ("JBoxLargePictureOO.w",
                                cPictureFile,
                                THIS-PROCEDURE,
                                YES,
                                OUTPUT hLargePicture).  
  &ENDIF
  DYNAMIC-FUNCTION("LoadPictureFile" IN hLargePicture,cPictureFile). 
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
DEF INPUT PARAM ihRectangle AS HANDLE NO-UNDO.

DO WITH FRAME frmImage:
  
   /*
   /* Point to parent-procedure */
   oJboxImage = NEW uc.JboxImage (SOURCE-PROCEDURE, ihRectangle:HANDLE).
   oJboxImage:RegisterWithJukeBox(YES).
   oJboxImage:dock.
   oJboxImage:BringToFront().
   */
    
    ASSIGN FRAME frmImage:X             = ihRectangle:FRAME:X + ihRectangle:X
           FRAME frmImage:Y             = ihRectangle:FRAME:Y + ihRectangle:Y
           FRAME frmImage:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS + 2
           FRAME frmImage:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS + 2
           ihRectangle:HIDDEN           = IF ihRectangle:EDGE-PIXELS LE 2 THEN TRUE ELSE FALSE
           NO-ERROR.
         
         
  oJboxImage = NEW uc.JboxImage (SOURCE-PROCEDURE,ihRectangle:HANDLE).
  oJboxImage:RegisterWithJukeBox(NO).
  oJboxImage:dock.
  oJboxImage:BringToFront().


  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-view").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-view").
  DYNAMIC-FUNCTION("setNoMoveX"  ,THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"IMAGE-view").
  DYNAMIC-FUNCTION("setNoMoveY"  ,THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"IMAGE-view").
  FRAME frmImage:SCROLLABLE = NO.
  
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

RETURN FRAME frmImage:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPictureDesc C-Win 
FUNCTION getPictureDesc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cPictureDesc.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPictureFile C-Win 
FUNCTION LoadPictureFile RETURNS LOGICAL
  ( INPUT icPictureFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    cPictureFile = icPictureFile.
    IF SEARCH(cPictureFile) NE ? THEN 
    oJboxImage:LoadFile(cPictureFile) .
    ELSE oJboxImage:LoadFile(?) .
    oJboxImage:BringToFront().

/*
IF SEARCH(cPictureFile) NE ? THEN DO:
  ASSIGN chIMAGE-view:Picbuf:FILENAME = cPictureFile
         chIMAGE-view:Picbuf:AutoScale = TRUE.
  chIMAGE-view:Picbuf:LOAD NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) BEGINS "Error" THEN
    DYNAMIC-FUNCTION('setWebDoc','open',cPictureFile).
  FRAME frmImage:MOVE-TO-TOP().
  RETURN TRUE. 
END.
ELSE RETURN FALSE.
*/
    RETURN TRUE. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAsDropTarget C-Win 
FUNCTION setAsDropTarget RETURNS LOGICAL
  ( INPUT ibDropTarget AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FRAME frmImage:DROP-TARGET = ibDropTarget.

RETURN YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPictureDesc C-Win 
FUNCTION setPictureDesc RETURNS LOGICAL
  ( INPUT icPictureDesc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cPictureDesc = icPictureDesc.

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

oJboxImage:CLEAR(). 
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

