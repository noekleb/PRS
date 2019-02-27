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

DEF VAR hLookup         AS HANDLE NO-UNDO.
DEF VAR hBrwLookup      AS HANDLE NO-UNDO.
DEF VAR hTBlookup       AS HANDLE NO-UNDO.
DEF VAR hCustomerDetail AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnShippedOrdersWin btnShippedOrdersDialog ~
btnCustomers btnTempTable 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCustomers 
     LABEL "Look up on Customer with filter and maintenance option (only for window version)" 
     SIZE 78 BY 1.91.

DEFINE BUTTON btnShippedOrdersDialog 
     LABEL "Look up only shipped orders promised after 01/01/1998 (dialog version)" 
     SIZE 78 BY 1.91.

DEFINE BUTTON btnShippedOrdersWin 
     LABEL "Look up only shipped orders promised after 01/01/1998 (window version)" 
     SIZE 78 BY 1.91.

DEFINE BUTTON btnTempTable 
     LABEL "Look up on Temp-table" 
     SIZE 78 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnShippedOrdersWin AT ROW 1.71 COL 4
     btnShippedOrdersDialog AT ROW 4.1 COL 4
     btnCustomers AT ROW 7.48 COL 4
     btnTempTable AT ROW 9.76 COL 4
     "Promise date is only a suggestion. The user can query on any date" VIEW-AS TEXT
          SIZE 65 BY .62 AT ROW 6.48 COL 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 82.2 BY 11.38.


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
         HEIGHT             = 11.38
         WIDTH              = 82.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 96.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 96.4
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 82.2.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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


&Scoped-define SELF-NAME btnCustomers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustomers C-Win
ON CHOOSE OF btnCustomers IN FRAME DEFAULT-FRAME /* Look up on Customer with filter and maintenance option (only for window version) */
DO:  
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /* See also procedure setLookupAttributes */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Customer"
                    + ";Custnum"
                    + ";Name"
                   ,"WHERE false"
                    ,""                /* [ ABL attribute list (MULTIPLE,NUM-LOCKED-COLUMNS|2,..) ] */                                        
                    ,"CustNum,Name",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN 
    MESSAGE PROGRAM-NAME(1) SKIP
            cReturnValues 
            VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShippedOrdersDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShippedOrdersDialog C-Win
ON CHOOSE OF btnShippedOrdersDialog IN FRAME DEFAULT-FRAME /* Look up only shipped orders promised after 01/01/1998 (dialog version) */
DO:  
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  /* See also procedure setLookupAttributes */

  cLookupValue = "OrderNum".

  RUN JBoxDLookup.w ("Order"
                   + ";PromiseDate"
                   + ";OrderNum"
                   + ";OrderStatus"
                   + ";OrderDate"
                   + ";CustNum"
                 + ",Customer"
                   + ";Name"
                  ,"WHERE false"
                 + ",FIRST Customer OF Order NO-LOCK"
                  ,INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
    MESSAGE PROGRAM-NAME(1) SKIP
            cLookupValue
            VIEW-AS ALERT-BOX.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShippedOrdersWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShippedOrdersWin C-Win
ON CHOOSE OF btnShippedOrdersWin IN FRAME DEFAULT-FRAME /* Look up only shipped orders promised after 01/01/1998 (window version) */
DO:  
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /* See also procedure setLookupAttributes */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Order"
                    + ";PromiseDate"
                    + ";OrderNum"
                    + ";OrderStatus"
                    + ";OrderDate"
                    + ";CustNum"
                  + ",Customer"
                    + ";Name"
                   ,"WHERE false"
                  + ",FIRST Customer OF Order NO-LOCK"
                    ,""                                                  
                    ,"OrderNum,CustNum",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN 
    MESSAGE PROGRAM-NAME(1) SKIP
            cReturnValues 
            VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTempTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTempTable C-Win
ON CHOOSE OF btnTempTable IN FRAME DEFAULT-FRAME /* Look up on Temp-table */
DO:  
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /* See also procedure setLookupAttributes */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "temp-table ttList"
                    + ";Identifier|INTEGER|>>9||Id"          /* Fieldname|Datatype|Format|Initial value|Label */
                    + ";Description|CHARACTER|x(30)||Desc"
                   ,"WHERE false"
                    ,"MULTIPLE"                                                  
                    ,"Identifier",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN 
    MESSAGE PROGRAM-NAME(1) SKIP
            cReturnValues 
            VIEW-AS ALERT-BOX.
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
  IF VALID-HANDLE(hCustomerDetail) THEN APPLY "close" TO hCustomerDetail.
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

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"keepInitialSize","yes").
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
  ENABLE btnShippedOrdersWin btnShippedOrdersDialog btnCustomers btnTempTable 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeChild C-Win 
PROCEDURE InitializeChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihContactDetail AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setParentQuery" IN ihContactDetail,hBrwLookup).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:    Run when "new" button is pressed in the lookup 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hTBContainer AS HANDLE NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hTBlookup THEN DO:
  /* If JBoxDynMenu.w is running you can start the detail window via the menu system  
  PUBLISH "StartChildWindow" 
          ("CustomerDetail.w",
           "Customer",
           THIS-PROCEDURE,
           NO,
           OUTPUT hCustomerDetail).     
  */            
  
  RUN CustomerDetail.w PERSIST SET hCustomerDetail.
  RUN InitializeObject IN hCustomerDetail.
  DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getQuery" IN hCustomerDetail),hBrwLookup,"custnum").

/* If the toolbar for details is in the detail-container */

/*   DYNAMIC-FUNCTION("setParentToolbar" IN hCustomerDetail,hTBlookup).       */
/*   hTBContainer = DYNAMIC-FUNCTION("getToolbarHandle" IN hCustomerDetail).  */
/*   IF hTBContainer NE hTBLookup THEN                                        */
/*     DYNAMIC-FUNCTION("MergeToolbars",hTBlookup,hTBContainer).              */
/*   DYNAMIC-FUNCTION("setAttribute",hTBContainer,"disabledevents","filter"). */

  DYNAMIC-FUNCTION("setCloseOnSave" IN hCustomerDetail,YES).
  RUN NewRecord IN hCustomerDetail.
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "InvalidateHandle" IN hCustomerDetail.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     Set the query string for lookup
  Parameters:  <none>
  Notes:       The procedure is automatically invoked by the lookup
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTBrect  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihOther   AS HANDLE NO-UNDO.

DEF VAR httBuffer AS HANDLE NO-UNDO.
DEF VAR ix        AS INT    NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "Order" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"basequery","WHERE OrderStatus = 'Shipped'").
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"querywhere"," AND PromiseDate GE DATE('01/01/1998')").
  DYNAMIC-FUNCTION("setCurrentObject",ihBrowse).
  RUN OpenQuery.
  publish "ExpandSearchDialog"(ihBrowse,"x",50).
END.
ELSE IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "Identifier" THEN DO:
  httBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
  DO ix = 1 TO 10:
    httBuffer:BUFFER-CREATE().
    ASSIGN httBuffer:BUFFER-FIELD("Identifier"):BUFFER-VALUE = ix
           httBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = "Ident " + STRING(ix).
  END.
  DYNAMIC-FUNCTION("setCurrentObject",ihBrowse). 
  RUN OpenQuery.
END.
ELSE DO:
  ASSIGN hLookup    = SOURCE-PROCEDURE
         hBrwLookup = ihBrowse.

  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"insertbrowserow","no").
  hTBlookup = DYNAMIC-FUNCTION("NewToolbar",ihTBrect,"File",
                   "New,Filter"
                   ,"enable").
  DYNAMIC-FUNCTION("CreateObjectLink",ihBrowse,hTBlookup).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

