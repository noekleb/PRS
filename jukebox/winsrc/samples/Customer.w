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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
                          
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.
DEF VAR hFilter           AS HANDLE NO-UNDO.
DEF VAR hEmailOverlay     AS HANDLE NO-UNDO.
DEF VAR hSalesRepOverlay  AS HANDLE NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hSalesRepCombo    AS HANDLE NO-UNDO.
DEF VAR hOrders           AS HANDLE NO-UNDO.
DEF VAR hCustDetail       AS HANDLE NO-UNDO.
DEF VAR hFilterWindow     AS HANDLE NO-UNDO.
DEF VAR hBrwOrder         AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttCustomer
    FIELD CustNum AS INT.
DEF VAR httCustomer AS HANDLE NO-UNDO.
httCustomer = BUFFER ttCustomer:HANDLE:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwCustomer rectToolbar rectWinToolbar ~
RectBrowseSearch brwOrder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UpdateManyRecords C-Win 
FUNCTION UpdateManyRecords RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwOrder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159 BY 8.81.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY .95.

DEFINE RECTANGLE rectBrwCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.6 BY 11.29.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrwCustomer AT ROW 3.76 COL 1.4
     rectToolbar AT ROW 1.33 COL 2.4
     rectWinToolbar AT ROW 1.33 COL 152
     RectBrowseSearch AT ROW 2.67 COL 2
     brwOrder AT ROW 15.76 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 160.6 BY 23.81.


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
         TITLE              = "Customers"
         HEIGHT             = 23.81
         WIDTH              = 161
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
       FRAME DEFAULT-FRAME:HEIGHT           = 23.81
       FRAME DEFAULT-FRAME:WIDTH            = 160.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customers */
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
  IF VALID-HANDLE(hOrders) THEN APPLY "close" TO hOrders.
  IF VALID-HANDLE(hCustDetail) THEN APPLY "close" TO hCustDetail.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DEF VAR hColumn AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  hColumn = hBrowse:GET-BROWSE-COLUMN(2).
  APPLY "end-resize" TO hColumn.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdjustLimitRecord C-Win 
PROCEDURE AdjustLimitRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iConfirm    AS INT NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdateVal.w ("Update credit limit",
                              hBrowse:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"),
                              "DECIMAL|>>>,>>9.99|10000",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).
IF DEC(ocValue) = 0 AND iReturn NE 0 THEN 
  iConfirm = DYNAMIC-FUNCTION("DoMessage",0,1,"Confirm that credit limit shoult be 0","Confirm","").

IF iReturn NE 0 AND iConfirm NE 2 THEN
  UpdateManyRecords("CreditLimit",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* RUN EditCustomerRecord. */
RUN Super.

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
/* Example on how to disable a cell for update programmatically: */

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EmailAddress"):BUFFER-VALUE NE "" THEN
    DYNAMIC-FUNCTION("DeleteObject",hEmailOverlay).
  ELSE IF NOT VALID-HANDLE(hEmailOverlay) THEN DO:
    hEmailOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "EmailAddress",     
                      "EmailAddress",     
                      "","","","").                
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hEmailOverlay,"EmailAddress").
  END.
END.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF VALID-HANDLE(hOrders) THEN DO:
    hOrders:CURRENT-WINDOW:TITLE = "Orders [" + 
                                   hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Name"):BUFFER-VALUE + "]".
    RUN MoveToTop IN hOrders.
  END.
  IF VALID-HANDLE(hCustDetail) THEN DO:
    hCustDetail:CURRENT-WINDOW:TITLE = "Details [" + 
                                   hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Name"):BUFFER-VALUE + "]".
    RUN MoveToTop IN hCustDetail.
  END.

END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  IF VALID-HANDLE(hOrders) THEN
    hOrders:CURRENT-WINDOW:TITLE = "Orders []".
  IF VALID-HANDLE(hCustDetail) THEN
    hCustDetail:CURRENT-WINDOW:TITLE = "Details []".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hCustDetail) THEN DO:   
  RUN CustomerDetail.w PERSIST SET hCustDetail.
  RUN InitializeObject IN hCustDetail.
  DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hCustDetail),hBrowse,"custnum").
  RUN InvokeMethod(hBrowse,"DisplayRecord").
END.

RUN MoveToTop IN hCustDetail.
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
  ENABLE rectBrwCustomer rectToolbar rectWinToolbar RectBrowseSearch brwOrder 
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
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hMenuItem AS HANDLE NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

DO WITH FRAME {&FRAME-NAME}:
  /* Create the browse: */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                             rectBrwCustomer:HANDLE,    /* Coordinates */
                             100,                       /* Batchsize */
                             "MULTIPLE",                /* Attributes that must be set at creation time for the browse */
                             "Customer"                 /* Buffers and fields (and calculated fields) for browse */
                              + ";CustNum"
                              + ";Name"
                              + ";EmailAddress"
                              + ";Terms"
                              + ";Balance"
                              + ";CreditLimit"
                              + ";+OrderCount|INTEGER|>>>9|customer_ordercount.p(CustNum)|Order count"
                              + ";!Address2"
                              + ";!City"                /* !-prefix to retrieve columns that should no be displayed */
                              + ";!Country"
                              + ";!State"
                              + ";!Phone"
                              + ";!Address"
                              + ";!PostalCode"
                              + ";SalesRep@4"
                            + ",SalesRep"
                              + ";RepName@5"
                              + ";Region"
                            ,"WHERE false"
                           + ",FIRST SalesRep OF Customer NO-LOCK"
                                                      /* Query string (where clause is split into 3 parts internally: BaseQuery, QueryFilter and QueryWhere) */
                            ,"").      
  /* If the query returns a lot of data, disable sorting on calculated fields
     (which requires a scan of alle records): */
/*   DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"OrderCount").   */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"keepSearchValue","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"searchDefault","filter").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  DYNAMIC-FUNCTION("setSortString",hBrowse,"custnum").
  /* To enable rows on dbl-cliok (won't work here since DefaultAction is captured) */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"setReadOnlyOnReturn","yes").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"setReadOnlyOnReturnOfLastField","yes").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"gotoFirstEnabledFromBackTab","yes").  */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"gotoLastEnabledFromTab","yes").       */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"stayInFieldOnEnter","yes").

  /* Create search field object (requires that browse has initial sort column */
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  /* Create a popup-menu (this object is independent of JukeBox as such) */
  DYNAMIC-FUNCTION("NewMenuBand",
                   hBrowse,                                      /* Parent widget */
                   "MultiSortBrowse;Sort on multiple columns"    /* List of actions: <action>;<label>;<method (opt)>,<action>.. */
                   + ",AdjustLimit;Adjust credit limit"
                   + ",UpdateEmail;Update email address"
                   + ",ViewOrder;View orders"
                   + ",EditCustomer;Edit customer"
                   + ",UpdateTerms;Update terms"
                   ,"").

  /* Update salesrep either in a drop-down for the browse cell */
  hSalesRepCombo = DYNAMIC-FUNCTION("NewBrowseDropDown",
                    hBrowse,                            /* Handle to browse */
                    "RepName",                          /* Browse column (display) */
                    "SalesRep",                          /* Buffer column (to update - foreign key. Maps to value - under) */
                    "SalesRep;RepName;SalesRep",         /* DB buffers and fields for drop-down values: Label,value */
                    "where true",                       /* Query to get drop-down values */
                    "").                                 /* I've got the values. Don't go to the database */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSalesRepCombo,"RepName"). /* Link the dropdown to the browse with column-name info */
  DYNAMIC-FUNCTION("setAttribute",hSalesRepCombo,"refreshrow","yes").          /* Refresh the row after update */

  /* Or update the salesrep with a overlay fill-in with corresponding lookup */
  hSalesRepOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,
                    "SalesRep",
                    "SalesRep",
                    "SalesRep;SalesRep;RepName;Region","WHERE false","SalesRep", /* Parameters for the lookup */
                    "").
  DYNAMIC-FUNCTION("setAttribute",hSalesRepOverlay,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSalesRepOverlay,"SalesRep").   

  /* Simple fill-in to update email address: */
  hEmailOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "EmailAddress",     
                    "EmailAddress",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hEmailOverlay,"EmailAddress").

  /* Browse toolbar: */
  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,      /* Coordinates */
                             "File",                  /* Defaule menu (if blank the button will not be flat-buttons) */
                             "new,edit,first|Navigate,prev|Navigate,next|Navigate,last|Navigate,rule,excel,filter,accum,rule,browseconfig",
                                                      /* <action>[|<Menu label>][;<Label>][;<Method>][;image],<action>... */
                             "maxborder").            /* Other attributes are 'border', 'right' (alignment) and 'enable' */
  /* Here the filter values are set in custom .w: */
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"filterwindow","JBoxFilterTemplate.w").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,THIS-PROCEDURE).


  /* Link the browse and the toolbar */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  
  hBrwOrder = DYNAMIC-FUNCTION("NewBrowse"
    ,brwOrder:HANDLE
    ,100
    ,""
    ,"Order"
     + ";Ordernum;Salesrep"
    ,"WHERE false"
    ,"").
  
  DYNAMIC-FUNCTION("CreateParentLink",hBrwOrder,hBrowse,"CustNum").
  

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "File",
                             "close;E&xit",
                             "right|enable").


  /* Resize settings: */ 
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RectBrowseSearch").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar,brwOrder," + hBrwOrder:NAME).
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,200).
          
  /* Load any saved filter and open the query accordingly (TRUE): */
  DYNAMIC-FUNCTION("LoadQueryFilter",hBrowse,TRUE).

  SUBSCRIBE TO "InvalidateHandle" ANYWHERE.

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

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
DEF INPUT PARAM ihChild AS HANDLE NO-UNDO.

IF ihChild = hOrders THEN hOrders = ?.
ELSE IF ihChild = hCustDetail AND VALID-HANDLE(hBrowse) THEN DO:
/*   DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).  */
  DYNAMIC-FUNCTION("SetAttribute",hToolbar,"disabledevents","").
/*   DYNAMIC-FUNCTION("setToolbar",hToolbar,                                    */
/*                    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN "avail"  */
/*                    ELSE "not avail").                                        */
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
IF VALID-HANDLE(hSearchField) THEN
  APPLY "entry" TO hSearchField.
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
hFilterWindow = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"filterhandle")).
IF VALID-HANDLE(hFilterWindow) THEN APPLY "close" TO hFilterWindow.

IF NOT VALID-HANDLE(hCustDetail) THEN DO:   
  RUN CustomerDetail.w PERSIST SET hCustDetail.
  RUN InitializeObject IN hCustDetail.
  DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hCustDetail),hBrowse,"custnum").
END.

RUN NewRecord IN hCustDetail.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateEmailRecord C-Win 
PROCEDURE UpdateEmailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseMsgUpdateVal.w ("Update email address",
                              hBrowse:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"),
                              "CHARACTER|x(40)",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn NE 0 THEN
  UpdateManyRecords("EmailAddress",ocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateTermsRecord C-Win 
PROCEDURE UpdateTermsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue  AS CHARACTER NO-UNDO.
DEF VAR iReturn  AS INTEGER   NO-UNDO.

/* (ABhack: bmu) */
RUN JBoxBrowseMsgUpdateVal.w ("Select payment terms",
        hBrowse:NUM-SELECTED-ROWS,?,
/*         DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordcount"), */
        "CHARACTER|X(10)|Net30",
        OUTPUT ocValue,
        OUTPUT iReturn).

/* If you have structured data for the term you can create a drop-down list in the message/select box: 

RUN JBoxBrowseMsgUpdateVal.w ("Select payment terms",
                              hBrowse:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordcount"),
                              "CHARACTER|x(40)||COMBO-BOX|"
                              + DYNAMIC-FUNCTION("getFieldList",
                                                 "PaymentTermTable;PaymentTermName;PaymentTermId",
                                                 "WHERE true"),
                              OUTPUT ocValue,
                              OUTPUT iReturn).

*/
                

IF iReturn = 1 THEN DO:
  DYNAMIC-FUNCTION("ProcessQuery",hBrowse,
                        "jbserv_update_browse_column.p",
                        "customer.terms|" + ocValue + "|RowIdent1").
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE IF iReturn = 2 THEN
  DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,
                        "jbserv_update_browse_column.p",
                        "customer.terms|" + ocValue + "|RowIdent1").

/* If the value applies to the primary table of the browse you can omit the table name and the RowIdentn specification where n refers to the position of the joined buffer */

APPLY "entry" TO hBrowse.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewOrderRecord C-Win 
PROCEDURE ViewOrderRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF NOT VALID-HANDLE(hOrders) THEN
    RUN Order.w PERSIST SET hOrders.
  RUN MoveToTop IN hOrders.
  APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UpdateManyRecords C-Win 
FUNCTION UpdateManyRecords RETURNS LOGICAL
  ( INPUT icField AS CHAR,
    INPUT icValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.

IF iReturn = 0 THEN RETURN FALSE.

IF iReturn = 1 THEN DO:
  EMPTY TEMP-TABLE ttCustomer.
  hBrowse:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
    CREATE ttCustomer.
    BUFFER ttCustomer:HANDLE:BUFFER-COPY(hBrowse:QUERY:GET-BUFFER-HANDLE(1)).
    hBrowse:QUERY:GET-NEXT().
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","customer_update_records.p",icField + "|" + icValue,httCustomer).
END.
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|". 
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","customer_update_records.p",
                              icField + "|" + icValue
                            + "|ROWID|" + TRIM(cRowIdList,"|"),
                              ?).
END.

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Error when adjusting " + icField,""). 
ELSE IF cRowIdList NE "" THEN DO:
  DYNAMIC-FUNCTION("refreshRowids",hBrowse,REPLACE(cRowIdList,"|",",")).
  APPLY "value-changed" TO hBrowse.
END.
ELSE RUN OpenQuery.

RETURN bOk. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

