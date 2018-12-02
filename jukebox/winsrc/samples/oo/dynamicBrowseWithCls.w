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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

DEF VAR oBrwCustomer AS JBoxBrowse   NO-UNDO.
DEF VAR oFmCustomer  AS JBoxFieldMap NO-UNDO.


DEF VAR otbCustomer AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwCustomer tbCustomer new_tbCustomer ~
edit_tbCustomer copy_tbCustomer undo_tbCustomer delete_tbCustomer ~
save_tbCustomer excel_tbCustomer filter_tbCustomer refresh_tbCustomer ~
browseconfig_tbCustomer CustNum Name Address SalesRep btnSalesRep RepName 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name Address SalesRep RepName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbCustomer 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Column setup" 
     SIZE 6 BY 1.52 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON btnSalesRep 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON copy_tbCustomer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbCustomer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbCustomer 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 6 BY 1.52 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbCustomer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbCustomer 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON new_tbCustomer 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON refresh_tbCustomer 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 6 BY 1.52 TOOLTIP "Refresh (F5)".

DEFINE BUTTON save_tbCustomer 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbCustomer 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Address AS CHARACTER FORMAT "x(35)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 TOOLTIP "Please enter an address.".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter a customer number.".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a name.".

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 35.2 BY 1 TOOLTIP "Please enter the Name of the Salesperson.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please Enter a Sales Rep.".

DEFINE RECTANGLE brwCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 9.29.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 89.8 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbCustomer AT ROW 1.33 COL 2.4 WIDGET-ID 12
     edit_tbCustomer AT ROW 1.33 COL 8.4 WIDGET-ID 14
     copy_tbCustomer AT ROW 1.33 COL 14.6 WIDGET-ID 16
     undo_tbCustomer AT ROW 1.33 COL 20.6 WIDGET-ID 18
     delete_tbCustomer AT ROW 1.33 COL 26.8 WIDGET-ID 20
     save_tbCustomer AT ROW 1.33 COL 32.8 WIDGET-ID 22
     excel_tbCustomer AT ROW 1.33 COL 39 WIDGET-ID 24
     filter_tbCustomer AT ROW 1.33 COL 45 WIDGET-ID 26
     refresh_tbCustomer AT ROW 1.33 COL 51.2 WIDGET-ID 28
     browseconfig_tbCustomer AT ROW 1.33 COL 57.2 WIDGET-ID 30
     CustNum AT ROW 13 COL 14 COLON-ALIGNED
     Name AT ROW 13 COL 31 COLON-ALIGNED
     Address AT ROW 14.14 COL 14 COLON-ALIGNED
     SalesRep AT ROW 15.29 COL 14 COLON-ALIGNED
     btnSalesRep AT ROW 15.29 COL 25.6 WIDGET-ID 8 NO-TAB-STOP 
     RepName AT ROW 15.29 COL 27.8 COLON-ALIGNED NO-LABEL
     brwCustomer AT ROW 3.24 COL 2.4 WIDGET-ID 2
     tbCustomer AT ROW 1.24 COL 2.2 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 93.2 BY 15.71 WIDGET-ID 100.


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
         HEIGHT             = 15.71
         WIDTH              = 93.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
       FRAME DEFAULT-FRAME:HEIGHT           = 15.71
       FRAME DEFAULT-FRAME:WIDTH            = 93.2.

ASSIGN 
       tbCustomer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,filter;Filter,refresh;Refresh,browseconfig;Column setupmaxborder".

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


&Scoped-define SELF-NAME btnSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalesRep C-Win
ON CHOOSE OF btnSalesRep IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "Salesrep"
                    + ";SalesRep"
                    + ";Region"
                   ,"WHERE true"
                    ,""                                                  
                    ,"SalesRep,Region",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN SalesRep:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           .

    APPLY "any-printable" TO SalesRep.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
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
  DISPLAY CustNum Name Address SalesRep RepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwCustomer tbCustomer new_tbCustomer edit_tbCustomer copy_tbCustomer 
         undo_tbCustomer delete_tbCustomer save_tbCustomer excel_tbCustomer 
         filter_tbCustomer refresh_tbCustomer browseconfig_tbCustomer CustNum 
         Name Address SalesRep btnSalesRep RepName 
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
DEF VAR oCont AS JBoxContainer NO-UNDO.
RUN enable_UI.

oCont = NEW JBoxContainer().
oCont:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:
  oBrwCustomer = NEW JBoxBrowse(brwCustomer:HANDLE,YES,"Kunder").
  oBrwCustomer:buffersAndFields = 
    "Customer"
    + ";CustNum"
    + ";Name"
    + ";Salesrep"
    + ";Address"
    + ";+OrderTotal|DECIMAL|>>>>>>>>9.99|CustOrderTotal(CustNum)"
  + ",Order"
    + ";OrderNum"  
    + ";Salesrep"
  + ",Salesrep"
    + ";repname@4"
//    + ";accum monthQuota[1]@5"
  + ",buf1_Salesrep"
    + ";repname"
    .
  oBrwCustomer:queryJoin = "FIRST Order OF Customer,FIRST Salesrep OF Customer,FIRST buf1_Salesrep OF Order".  
  oBrwCustomer:altPrimaryBufferList = "Order,SalesRep,buf1_Salesrep".
//  oBrwCustomer:preScanBaseQuery = "Item WHERE itemname matches '*r',EACH OrderLine OF Item,FIRST Order OF OrderLine,FIRST Customer OF Order".

/*  oBrwCustomer:queryStatFields = "monthquota[1]".*/
  oBrwCustomer:setPreScanQuery("buf1_Salesrep", "EACH Order OF buf1_Salesrep,FIRST Customer OF Order").
  oBrwCustomer:calcFieldProc = "customer_browsecalc.p".
  oBrwCustomer:queryStopAfter = 3.
  
//  oBrwCustomer:setPreScanQuery("SalesRep", "EACH Customer OF SalesRep").
  oBrwCustomer:allowCan-DoFilterOperator = "salesrep".
  
  otbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE).
  oBrwCustomer:TOOLBAR-OBJECT = otbCustomer.
  
//  oBrwCustomer:setFilter("name begins 'a'").
  
  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
  oFmCustomer:updateFields = "CustNum,Name,SalesRep,Address".
  oFmCustomer:primaryKeyFields = "CustNum". /* disables custnum when in edit-mode */
  oFmCustomer:displayFields = "repname".
  oFmCustomer:TOOLBAR-OBJECT = otbCustomer.

  oBrwCustomer:openQuery().
  
/*  MESSAGE oBrwCustomer:getQueryStatValue("monthquota[1]")*/
/*  VIEW-AS ALERT-BOX.                                     */
END.

oCont:initResize(800,200). /* X > 800: Move right, Y > 200: Move down */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "SalesRep" THEN cReturn = DYNAMIC-FUNCTION("getFieldValues","Salesrep","WHERE SalesRep = '" + ihField:SCREEN-VALUE + "'","Region").
  END CASE.

  MESSAGE PROGRAM-NAME(1) SKIP 
          cReturn
          VIEW-AS ALERT-BOX.
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
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
RUN SUPER.

IF oBrwCustomer:getQueryStatValue("querystop") NE "" THEN
  MESSAGE "Query time-out after " oBrwCustomer:getQueryStatValue("querystop") " sec"
  VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

