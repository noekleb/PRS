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
DEF VAR hVisBilde     AS HANDLE NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.
DEF VAR hLargePicture AS HANDLE NO-UNDO.

DEF VAR cBildeKatalog  AS CHAR   NO-UNDO.
DEF VAR iBildeNr       AS INT    NO-UNDO.


DEFINE STREAM Stream1.

DEF TEMP-TABLE ttBildeData
    FIELD BildNr    AS INT
    FIELD Teller    AS INT
    FIELD RawData   AS RAW
    FIELD RowIdent  AS CHAR
    .
DEF VAR hBufBildeData AS HANDLE.
hBufBildeData = BUFFER ttBildeData:HANDLE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlFrameHandle C-Win 
FUNCTION getControlFrameHandle RETURNS HANDLE
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

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-sko AS COMPONENT-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmImage
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 14.8 BY 2.33.


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
         HEIGHT             = 2.57
         WIDTH              = 23
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-sko ASSIGN
       FRAME           = FRAME frmImage:HANDLE
       ROW             = 1.14
       COLUMN          = 1.4
       HEIGHT          = 1.57
       WIDTH           = 10.6
       HIDDEN          = no
       SENSITIVE       = yes.
      IMAGE-sko:NAME = "IMAGE-sko":U .
/* IMAGE-sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */

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


&Scoped-define SELF-NAME IMAGE-sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-sko C-Win OCX.DblClick
PROCEDURE IMAGE-sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
IF iBildeNr NE 0 THEN DO:
  IF NOT VALID-HANDLE(hVisBilde) THEN
    RUN VisBilde.w PERSIST SET hVisBilde.
  
  RUN VisBilde IN hVisBilde (iBildeNr).
END.

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
  IF VALID-HANDLE(hParent) THEN
    APPLY "close" TO hParent.
  IF VALID-HANDLE(hVisBilde) THEN APPLY "close" TO hVisBilde.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeTilDisk C-Win 
PROCEDURE BildeTilDisk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER cBildeFil AS CHARACTER  NO-UNDO.

DEFINE VARIABLE rawData    AS RAW        NO-UNDO.
DEFINE VARIABLE cRowIdent1 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRowIdent2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTellerStr AS CHARACTER  NO-UNDO.

IF iBildeNr = ? OR iBildeNr = 0 THEN
  ASSIGN cBildeFil = "".
ELSE DO:
  ASSIGN cTellerStr = " AND Teller " +
    IF ENTRY(NUM-ENTRIES(cBildeFil,"\"),cBildeFil,"\") BEGINS "mini" THEN
       ">= 200" ELSE "< 200".

  EMPTY TEMP-TABLE ttBildeData.

  DYNAMIC-FUNCTION("getTempTable","jbserv_gettemptable.p","BildeData|WHERE Bildnr = " + STRING(iBildeNr) + cTellerStr,hBufBildeData).

  LENGTH(rawData) = 30000.

  FIND FIRST ttBildeData NO-ERROR.
  IF NOT AVAIL ttBildeData THEN DO:
    cBildeFil = "".
    RETURN.
  END.

  ASSIGN rawData    = ttBildeData.RawData
         cRowIdent1 = ttBildeData.RowIdent.
  IF rawData = ? THEN DO:
    cBildeFil = "".
    RETURN.
  END.
  ELSE DO:
    OUTPUT STREAM Stream1 TO VALUE(cBildeFil) NO-MAP NO-CONVERT.
    PUT STREAM Stream1 CONTROL rawData.
    HENT:
    REPEAT:
      FIND NEXT ttBildeData.
      IF cRowIdent1 = ttBildeData.RowIdent THEN 
        LEAVE HENT.
      ASSIGN cRowIdent1 = ttBildeData.RowIdent
             rawData    = ttBildeData.RawData.
      IF rawData = ? THEN
        LEAVE HENT.
      PUT STREAM Stream1 CONTROL rawData.
    END.
    OUTPUT STREAM Stream1 CLOSE.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "VisMiniBilde.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-sko = IMAGE-sko:COM-HANDLE
    UIB_S = chIMAGE-sko:LoadControls( OCXFile, "IMAGE-sko":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "VisMiniBilde.wrx":U SKIP(1)
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
  RUN control_load.
  VIEW FRAME frmImage.
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
DEF INPUT PARAM ihRectangle AS HANDLE NO-UNDO.

cBildeKatalog   = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                    "WHERE SysHId = 10 and SysGr = 1 and ParaNr = 2","Parameter1").

DO WITH FRAME frmImage:
  ASSIGN FRAME frmImage:X             = ihRectangle:FRAME:X + ihRectangle:X
         FRAME frmImage:Y             = ihRectangle:FRAME:Y + ihRectangle:Y
         IMAGE-sko:X                  = 1
         IMAGE-sko:Y                  = 1
         IMAGE-sko:WIDTH-PIXELS       = ihRectangle:WIDTH-PIXELS - 4
         IMAGE-sko:HEIGHT-PIXELS      = ihRectangle:HEIGHT-PIXELS - 4
         FRAME frmImage:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS + 2
         FRAME frmImage:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS + 2
         ihRectangle:HIDDEN           = IF ihRectangle:EDGE-PIXELS LE 2 THEN TRUE ELSE FALSE
         NO-ERROR.

  DYNAMIC-FUNCTION("NewObject",ihRectangle:WINDOW,THIS-PROCEDURE,"procedure").

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-sko").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-sko").
  DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"IMAGE-sko").
  DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"IMAGE-sko").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisMiniBilde C-Win 
PROCEDURE VisMiniBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iiBildeNr AS INT NO-UNDO.

DEF VAR cBildeFil AS CHAR NO-UNDO.

iBildeNr = iiBildeNr.

chIMAGE-Sko:Picbuf:CLEAR(2).

IF iBildeNr = 0 THEN RETURN.

cBildeFil = DYNAMIC-FUNCTION("getFieldValues","Bilderegister","where BildNr = " + STRING(iBildeNr),"Filnavn").

IF cBildeFil = "" OR cBildeFil = ? THEN
  RETURN.
ELSE 
  cBildeFil = TRIM(cBildeKatalog,"\") + "\mini" + cBildeFil.

IF SEARCH(cBildeFil) = ? THEN
  RUN BildeTilDisk (INPUT-OUTPUT cBildeFil).

IF SEARCH(cBildeFil) <> ? AND cBildeFil NE "" THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  ASSIGN chIMAGE-Sko:Picbuf:FILENAME = cBildeFil
         chIMAGE-Sko:Picbuf:AutoScale = TRUE.
         chIMAGE-Sko:Picbuf:LOAD.
   IF ERROR-STATUS:GET-MESSAGE(1) BEGINS "Error" THEN
     DYNAMIC-FUNCTION('setWebDoc','open',cBildeFil).
   FRAME frmImage:MOVE-TO-TOP().

   DYNAMIC-FUNCTION("DoLockWindow",?).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlFrameHandle C-Win 
FUNCTION getControlFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN image-sko:HANDLE.

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

RETURN FRAME frmImage:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

