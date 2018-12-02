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

DEFINE VARIABLE bOk           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hFieldMap     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParent       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentQuery  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar      AS HANDLE    NO-UNDO.
DEFINE VARIABLE iWarehouseNum AS INTEGER   NO-UNDO.
DEFINE VARIABLE ix            AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmOrderView

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCalOrdered btnWarehouse Ordernum ~
OrderStatus SalesRep edWarehouse OrderDate PromiseDate ShipDate Creditcard ~
Terms Carrier Instructions btnCalPromised 
&Scoped-Define DISPLAYED-OBJECTS Ordernum OrderStatus SalesRep edWarehouse ~
OrderDate PromiseDate ShipDate Creditcard Terms Carrier Instructions 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setToolbarHandle C-Win 
FUNCTION setToolbarHandle RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewWarehouse C-Win 
FUNCTION ViewWarehouse RETURNS LOGICAL
  ( INPUT icValues AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalOrdered 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCalPromised 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnWarehouse 
     LABEL "&Warehouse:" 
     SIZE 13 BY 1.

DEFINE VARIABLE Creditcard AS CHARACTER FORMAT "x(20)" INITIAL "Visa" 
     LABEL "Credit Card" 
     VIEW-AS COMBO-BOX 
     LIST-ITEMS "Visa","American Express","Master Card" 
     DROP-DOWN-LIST
     SIZE 25.8 BY 1.

DEFINE VARIABLE OrderStatus AS CHARACTER FORMAT "x(20)" INITIAL "Ordered" 
     LABEL "Order Status" 
     VIEW-AS COMBO-BOX 
     LIST-ITEMS "Ordered","Back Ordered","Partially Shipped","Shipped" 
     DROP-DOWN-LIST
     SIZE 25.8 BY 1.

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Salesrep" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE edWarehouse AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 35.6 BY 4.1 NO-UNDO.

DEFINE VARIABLE Carrier AS CHARACTER FORMAT "x(256)" 
     LABEL "Carrier" 
     VIEW-AS FILL-IN 
     SIZE 41.2 BY 1.

DEFINE VARIABLE Instructions AS CHARACTER FORMAT "x(256)" 
     LABEL "Instructions" 
     VIEW-AS FILL-IN 
     SIZE 41.2 BY 1.

DEFINE VARIABLE OrderDate AS DATE FORMAT "99/99/99" INITIAL 10/07/05 
     LABEL "Ordered" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Ordernum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Order Num" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE PromiseDate AS DATE FORMAT "99/99/99" 
     LABEL "Promised" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ShipDate AS DATE FORMAT "99/99/99" 
     LABEL "Shipped" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Terms AS CHARACTER FORMAT "x(20)" INITIAL "Net30" 
     LABEL "Terms" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmOrderView
     btnCalOrdered AT ROW 1.24 COL 77
     btnWarehouse AT ROW 4.52 COL 2 NO-TAB-STOP 
     Ordernum AT ROW 1.19 COL 13.4 COLON-ALIGNED HELP
          "Please enter an order number."
     OrderStatus AT ROW 2.24 COL 13.4 COLON-ALIGNED HELP
          "Please enter the Order Status."
     SalesRep AT ROW 3.33 COL 13.4 COLON-ALIGNED
     edWarehouse AT ROW 4.52 COL 15.4 NO-LABEL NO-TAB-STOP 
     OrderDate AT ROW 1.19 COL 61.8 COLON-ALIGNED HELP
          "Please enter the date of order."
     PromiseDate AT ROW 2.24 COL 61.8 COLON-ALIGNED HELP
          "Please enter the Promise Date."
     ShipDate AT ROW 3.33 COL 61.8 COLON-ALIGNED HELP
          "Please enter the ship date."
     Creditcard AT ROW 4.48 COL 61.8 COLON-ALIGNED HELP
          "Please enter the credit card."
     Terms AT ROW 5.57 COL 61.8 COLON-ALIGNED HELP
          "Please enter the terms."
     Carrier AT ROW 6.67 COL 61.8 COLON-ALIGNED HELP
          "Please enter the carrier."
     Instructions AT ROW 7.71 COL 61.8 COLON-ALIGNED HELP
          "Please enter Instructions"
     btnCalPromised AT ROW 2.29 COL 77
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105 BY 8.


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
         TITLE              = "Order details"
         HEIGHT             = 8
         WIDTH              = 105
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
/* SETTINGS FOR FRAME frmOrderView
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       edWarehouse:READ-ONLY IN FRAME frmOrderView        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmOrderView
/* Query rebuild information for FRAME frmOrderView
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmOrderView */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Order details */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order details */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalOrdered
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalOrdered C-Win
ON CHOOSE OF btnCalOrdered IN FRAME frmOrderView /* ... */
DO:
  RUN Cal.w (OrderDate:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalPromised
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalPromised C-Win
ON CHOOSE OF btnCalPromised IN FRAME frmOrderView /* ... */
DO:
  RUN Cal.w (PromiseDate:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWarehouse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWarehouse C-Win
ON CHOOSE OF btnWarehouse IN FRAME frmOrderView /* Warehouse: */
DO:
  DEF VAR cWarehouseFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Warehouse"
                     + ";WarehouseNum"
                     + ";WarehouseName"
                     + ";City"
                     + ";!Address;!PostalCode;!State;!Country;!Phone"
                     ,
                   "WHERE true"
                    ,""
                    ,"WarehouseNum,WarehouseName,Address,City,PostalCode,State,Country,Phone",
                    OUTPUT cWarehouseFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cWarehouseFieldList NE "" THEN DO:
    ViewWarehouse(cWarehouseFieldList).
    APPLY "any-printable" TO Instructions.
  END.
  APPLY "entry" TO Instructions.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PromiseDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PromiseDate C-Win
ON CURSOR-DOWN OF PromiseDate IN FRAME frmOrderView /* Promised */
DO:
  MESSAGE 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

{incl/supptrigg.i hFieldMap}

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
  HIDE FRAME frmOrderView.
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
RUN SUPER.
ViewWarehouse("").
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
  DISPLAY Ordernum OrderStatus SalesRep edWarehouse OrderDate PromiseDate 
          ShipDate Creditcard Terms Carrier Instructions 
      WITH FRAME frmOrderView.
  ENABLE btnCalOrdered btnWarehouse Ordernum OrderStatus SalesRep edWarehouse 
         OrderDate PromiseDate ShipDate Creditcard Terms Carrier Instructions 
         btnCalPromised 
      WITH FRAME frmOrderView.
  {&OPEN-BROWSERS-IN-QUERY-frmOrderView}
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
DO WITH FRAME frmOrderView:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,
                            "",
                            "Order"
                          + ",SalesRep"
                            + ";Repname"
                          + ",Warehouse"
                            + ";Address"
                            + ";Address2"
                            + ";City"
                            + ";Country"
                            + ";Phone"
                            + ";PostalCode"
                            + ";State"
                            + ";WarehouseName"
                            + ";WarehouseNum"
                            ,"where false"
                            + ",FIRST SalesRep OF Order NO-LOCK OUTER-JOIN"
                            + ",FIRST Warehouse OF Order NO-LOCK OUTER-JOIN"
                            ,"").

  ASSIGN SalesRep:DELIMITER = "|"
         SalesRep:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SalesRep;RepName|SalesRep;SalesRep","WHERE TRUE").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME frmOrderView:HANDLE,
                             "OrderStatus,PromiseDate,SalesRep,ShipDate,Terms,Carrier,Creditcard,Instructions,OrderDate","",
                             "OrderNum","",
                             "btnWarehouse,btnShipTo,btnCalOrdered,btnCalPromised"). 
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","WarehouseNum").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"saveonlymodified","yes").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,THIS-PROCEDURE).

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
FRAME frmOrderView:MOVE-TO-TOP().
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
ASSIGN iWarehouseNum = 0
       edWarehouse:SCREEN-VALUE IN FRAME frmOrderView = ""
       .
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bNewCount AS LOG NO-UNDO.

IF DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new" OR OrderStatus:MODIFIED IN FRAME {&FRAME-NAME} THEN
  bNewCount = TRUE.

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(iWarehouseNum)).

RUN SUPER.

IF bNewCount THEN PUBLISH "NewOrderCount" (hParentQuery).
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

RETURN FRAME frmOrderView:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  The function is invoked by AddFolder in JBoxTabFolder
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmOrderView:HANDLE,"edWarehouse").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmOrderView:HANDLE,"edWarehouse").
DYNAMIC-FUNCTION("setAddResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmOrderView:HANDLE,"Carrier,Instructions").
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setToolbarHandle C-Win 
FUNCTION setToolbarHandle RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hToolbar = ihToolbar.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewWarehouse C-Win 
FUNCTION ViewWarehouse RETURNS LOGICAL
  ( INPUT icValues AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  "WarehouseNum,WarehouseName,Address,City,PostalCode,State,Country,Phone"
------------------------------------------------------------------------------*/
IF icValues = "" AND hFieldMap:AVAIL THEN  /* <- when called from DisplayRecord */
  icValues = STRING(hFieldMap:BUFFER-FIELD("WarehouseNum"):BUFFER-VALUE) + "|" +
             hFieldMap:BUFFER-FIELD("WarehouseName"):BUFFER-VALUE + "|" +
             hFieldMap:BUFFER-FIELD("Address"):BUFFER-VALUE + "|" +
             hFieldMap:BUFFER-FIELD("City"):BUFFER-VALUE + "|" +
             hFieldMap:BUFFER-FIELD("PostalCode"):BUFFER-VALUE + "|" +
             hFieldMap:BUFFER-FIELD("State"):BUFFER-VALUE + "|" +
             hFieldMap:BUFFER-FIELD("Country"):BUFFER-VALUE + "|" +
             hFieldMap:BUFFER-FIELD("Phone"):BUFFER-VALUE
             .

DO WITH FRAME frmOrderView:
  edWarehouse:SCREEN-VALUE = "".

  IF icValues NE "" THEN DO ix = 1 TO NUM-ENTRIES(icValues,"|"):
    IF ix = 1 THEN iWarehouseNum = INT(ENTRY(ix,icValues,"|")).
    ELSE DO:
      edWarehouse:SCREEN-VALUE = edWarehouse:SCREEN-VALUE + ENTRY(ix,icValues,"|").
      IF ix NE 5 AND ix NE 6 THEN
        edWarehouse:SCREEN-VALUE = edWarehouse:SCREEN-VALUE + CHR(10).
      ELSE
        edWarehouse:SCREEN-VALUE = edWarehouse:SCREEN-VALUE + " ".
    END.
  END.
  ELSE iWarehouseNum = 0.
END.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

