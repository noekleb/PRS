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
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.

DEF VAR hItemOverlay      AS HANDLE NO-UNDO.
DEF VAR hPriceOverlay     AS HANDLE NO-UNDO.
DEF VAR hQtyOverlay       AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS OrderLineBrowse 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE OrderLineBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109.4 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     OrderLineBrowse AT ROW 1 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.6 BY 7.95.


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
         TITLE              = "Orders"
         HEIGHT             = 7.95
         WIDTH              = 110.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
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
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders */
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
  PUBLISH "InvalidateHandle".

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

{incl/supptrigg.i hBrowse}

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
  ENABLE OrderLineBrowse 
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
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    OrderLineBrowse:HANDLE,   
                    100,                 
                    "",                  
                    "OrderLine"
                    + ";LineNum|Pos|>>9"
                    + ";ItemNum|Item|>>>9"
                    + ";OrderLineStatus"
                    + ";+OrderPrice|DECIMAL|>><>>9.99|orderline_price|Price"
                    + ";Qty"
                    + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
                    + ";!OrderNum"
                    + ";!Price"
                  + ",Item"
                    + ";ItemName@3"
                    ,"WHERE false"
                    + ",FIRST Item NO-LOCK OF OrderLine"
                    ,"sort|LineNum").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableondblclick","yes").
  DYNAMIC-FUNCTION("setATtribute",hBrowse,"setReadOnlyOnReturn","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=orderline_delete.p").
/* Other options: */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"setReadOnlyOnReturnOfLastField","yes"). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"gotoFirstEnabledFromBackTab","yes").  */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"gotoLastEnabledFromTab","yes").       */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowseLinje,"stayInFieldOnEnter","yes"). */

  hItemOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,
                    "ItemNum",
                    "ItemNum",
                    "Item;ItemNum;ItemName;Onhand;OnOrder;CatDescription","WHERE false","ItemNum", /* Parameters for the lookup */
                    "").
  DYNAMIC-FUNCTION("setAttribute",hItemOverlay,"refreshrow","yes"). 
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hItemOverlay,"ItemNum").   

  hPriceOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "OrderPrice",     
                    "Price",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hPriceOverlay,"OrderPrice").
  DYNAMIC-FUNCTION("setAttribute",hPriceOverlay,"refreshrow","yes"). 

  hQtyOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "Qty",     
                    "Qty",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hQtyOverlay,"Qty").
  DYNAMIC-FUNCTION("setAttribute",hQtyOverlay,"refreshrow","yes"). 

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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
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
DEF VAR rRepos       AS ROWID NO-UNDO.
DEF VAR iNextLineNum AS INT   NO-UNDO INIT 1.
DEF VAR cLineNums    AS CHAR  NO-UNDO.

cLineNums = DYNAMIC-FUNCTION("getFieldList","OrderLine;LineNum","WHERE OrderNum = " + DYNAMIC-FUNCTION("getOrderNum" IN hParent)).

IF NUM-ENTRIES(cLineNums,"|") > 0 THEN
  iNextLineNum = INT(ENTRY(NUM-ENTRIES(cLineNums,"|"),cLineNums,"|")) + 1.
  
IF DYNAMIC-FUNCTION("DoCreate",hBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME,"",
                    "OrderNum,LineNum",
                    DYNAMIC-FUNCTION("getOrderNum" IN hParent) + "|" + STRING(iNextLineNum),
                    TRUE) THEN DO:

  hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-CREATE.

  rRepos = hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID.
  DYNAMIC-FUNCTION("DoRefetchTrans",hBrowse:QUERY:GET-BUFFER-HANDLE(1),"FIRST","").
  DYNAMIC-FUNCTION("refreshRow",hBrowse,
                    DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields"),
                    DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin")).

  hBrowse:QUERY:QUERY-OPEN().
  hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableupdate","yes").
  APPLY "value-changed" TO hBrowse.
/*   hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).  */
  APPLY "entry" TO hItemOverlay.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

