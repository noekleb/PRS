&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hParent      AS HANDLE NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR hABLattr     AS HANDLE NO-UNDO.
DEF VAR cABLattrList AS CHAR   NO-UNDO
    INIT "DATA-TYPE,OWNER,SENSITIVE,PARENT,FORMAT,POPUP-MENU,MENU-ITEM,MENU-BAR,SUB-MENU,LABEL,SUBTYPE,CURRENT-WINDOW,STRING-VALUE,BUFFER-VALUE,HELP,HEIGHT,WIDTH,HEIGHT-PIXELS,WIDTH-PIXELS,INITIAL,LIST-ITEMS,LIST-ITEM-PAIRS,SCREEN-VALUE,INPUT-VALUE,EXTENT,PRIVATE-DATA,PREV-SIBLING,NEXT-SIBLING,FIRST-CHILD,ROWID,TABLE,RECID,TABLE-HANDLE,TABLE-NUMBER,ERROR,AVAILABLE,INDEX-INFORMATION,NAME,TYPE,PREPARE-STRING,IS-OPEN,NUM-RESULTS,QUERY-OFF-END,CURRENT-RESULT-ROW,CURRENT-COLUMN,DOWN,DROP-TARGET,FOCUSED-ROW,FONT,FRAME,FIT-LAST-COLUMN,FOCUSED-ROW,HEIGHT-PIXELS,WIDTH-PIXELS,MAX-DATA-GUESS,HIDDEN,MULTIPLE,NEXT-TAB-ITEM,PREV-TAB-ITEM,NUM-COLUMNS,QUERY,SELECTABLE,SELECTED,ROW-MARKERS,TITLE,TOOLTIP,VISIBLE,WINDOW,X,Y,INSTANTIATING-PROCEDURE,LIST-ITEMS,LIST-ITEM-PAIRS,READ-ONLY".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwObjectLink 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setObjectHandle C-Win 
FUNCTION setObjectHandle RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrwObjectLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 133 BY 21.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrwObjectLink AT ROW 1.23 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 135 BY 21.81.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 21.81
         WIDTH              = 135.14
         MAX-HEIGHT         = 23.54
         MAX-WIDTH          = 150.57
         VIRTUAL-HEIGHT     = 23.54
         VIRTUAL-WIDTH      = 150.57
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21.81
       FRAME DEFAULT-FRAME:WIDTH            = 135.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hABLattr) THEN APPLY "close" TO hABLattr.
  RUN disable_UI.
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

  hParent = SOURCE-PROCEDURE.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hh AS HANDLE NO-UNDO.

hh = WIDGET-HANDLE(hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE) NO-ERROR.
IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(hh) THEN DO:
  IF NOT VALID-HANDLE(hABLattr) THEN DO:
    RUN ABLattributes.w PERSIST SET hABLattr.
    RUN InitializeObject IN hABLattr.
  END.
  RUN MoveToTop IN hABLattr.
  DYNAMIC-FUNCTION("setObjectHandle" IN hABLattr,hh).
END.
ELSE MESSAGE "Invalid value (handle) for drill-down on attributes: " hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE
             VIEW-AS ALERT-BOX ERROR.
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
  ENABLE rectBrwObjectLink 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwObjectLink:HANDLE,
                                   1000,
                                   "",
                                   "temp-table"
                                   + ";+cABLattrName|CHARACTER|x(40)||ABL attribute"
                                   + ";+cABLattrValue|CHARACTER|x(256)||Value"
                                   + ";+cABLattrType|CHARACTER|x(256)||Derived"
                                   ,
                                   "where false",
                                   "sort|cABLattrName").
  DYNAMIC-FUNCTION("NewMenuBand",hBrowse,"Filter,Excel,MultiSortBrowse","").

  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 200.

  hBrowse:TOOLTIP = "Double-click on handles to drill down on attributes".

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setObjectHandle C-Win 
FUNCTION setObjectHandle RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VARIABLE cListed AS CHAR   NO-UNDO.
DEF VARIABLE hCall   AS HANDLE NO-UNDO.
DEF VAR hWidget      AS HANDLE NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "(Some) ABL Attributes [" + ihObject:NAME + " - " + STRING(ihObject) + "]".

hBuffer:EMPTY-TEMP-TABLE().

DO ix = 1 TO NUM-ENTRIES(LIST-QUERY-ATTRS(ihObject)):

  IF NOT CAN-DO(cABLattrList,ENTRY(ix,LIST-QUERY-ATTRS(ihObject))) THEN NEXT.

  CREATE CALL hCall.
  ASSIGN
      hCall:CALL-TYPE = GET-ATTR-CALL-TYPE
      hCall:IN-HANDLE = ihObject
      hCall:CALL-NAME = ENTRY(ix,LIST-QUERY-ATTRS(ihObject)).

  hCall:INVOKE() NO-ERROR.

  IF NOT ERROR-STATUS:ERROR THEN DO:
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cABLattrName"):BUFFER-VALUE = ENTRY(ix,LIST-QUERY-ATTRS(ihObject))
           hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE  = STRING(IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
                                                                   ERROR-STATUS:GET-MESSAGE(1)
                                                                 ELSE IF hCall:RETURN-VALUE = ? THEN '?'
                                                                 ELSE hCall:RETURN-VALUE)
           cListed = cListed + ENTRY(ix,LIST-QUERY-ATTRS(ihObject)) + ","
           NO-ERROR.
  END.

  DELETE OBJECT hCall.
END.

cListed = TRIM(cListed,",").
 
DO ix = 1 TO NUM-ENTRIES(LIST-SET-ATTRS(ihObject)):
   
  IF NOT CAN-DO(cABLattrList,ENTRY(ix,LIST-SET-ATTRS(ihObject))) THEN NEXT.
  IF CAN-DO(cListed,ENTRY(ix,LIST-SET-ATTRS(ihObject))) THEN NEXT.

  CREATE CALL hCall.
  ASSIGN
     hCall:CALL-TYPE = GET-ATTR-CALL-TYPE
     hCall:IN-HANDLE = ihObject
     hCall:CALL-NAME = ENTRY(ix,LIST-SET-ATTRS(ihObject)).

  hCall:INVOKE() NO-ERROR.

  IF NOT ERROR-STATUS:ERROR THEN DO:
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cABLattrName"):BUFFER-VALUE = ENTRY(ix,LIST-SET-ATTRS(ihObject))
           hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE  = STRING(IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
                                                                   ERROR-STATUS:GET-MESSAGE(1)
                                                                 ELSE IF hCall:RETURN-VALUE = ? THEN '?'
                                                                 ELSE hCall:RETURN-VALUE)
           NO-ERROR.
  END.

  DELETE OBJECT hCall.

END.

IF ihObject:TYPE = "query" THEN DO:
  hBuffer:BUFFER-CREATE().
  ASSIGN hBuffer:BUFFER-FIELD("cABLattrName"):BUFFER-VALUE = "Buffer"
         hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE  = STRING(ihObject:GET-BUFFER-HANDLE(1))
         hBuffer:BUFFER-FIELD("cABLattrType"):BUFFER-VALUE = "GET-BUFFER-HANDLE(1)"
         .
END.
ELSE IF ihObject:TYPE = "buffer" THEN DO:
  DO ix = 1 TO ihObject:NUM-FIELDS:
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cABLattrName"):BUFFER-VALUE = ihObject:BUFFER-FIELD(ix):NAME 
           hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE  = STRING(ihObject:BUFFER-FIELD(ix))
           hBuffer:BUFFER-FIELD("cABLattrType"):BUFFER-VALUE = "BUFFER-FIELD(" + STRING(ix) + ")"
           .
  END.
END.
ELSE IF ihObject:TYPE = "browse" THEN DO:
  DO ix = 1 TO ihObject:NUM-COLUMNS:
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cABLattrName"):BUFFER-VALUE = ihObject:GET-BROWSE-COLUMN(ix):NAME 
           hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE  = STRING(ihObject:GET-BROWSE-COLUMN(ix))
           hBuffer:BUFFER-FIELD("cABLattrType"):BUFFER-VALUE = "GET-BROWSE-COLUMN(" + STRING(ix) + ")"
           .
  END.
END.
ELSE IF ihObject:TYPE = "frame" THEN DO:
  hWidget = ihObject:FIRST-CHILD.
  hWidget = hWidget:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hWidget):
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("cABLattrName"):BUFFER-VALUE = (IF CAN-QUERY(hWidget,"name") AND hWidget:NAME NE ? THEN hWidget:NAME + " " ELSE "")
           hBuffer:BUFFER-FIELD("cABLattrValue"):BUFFER-VALUE  = STRING(hWidget)
           hBuffer:BUFFER-FIELD("cABLattrType"):BUFFER-VALUE = hWidget:TYPE  
           .
    hWidget = hWidget:NEXT-SIBLING.
  END.
END.

RUN InvokeMethod(hBrowse,"OpenQuery").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

