&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:               JBoxQueryWrapper

  Description:        Wrapper for JBoxDataBrw 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           05.jun.2008

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

&IF DEFINED(UIB_is_Running) NE 0 &THEN

  DEF VAR hDataBrwWin AS HANDLE NO-UNDO.

  RUN JBoxDataBrw.w PERSIST SET hDataBrwWin.

  hDataBrwWin:CURRENT-WINDOW:TITLE = "Order drill-down".

  SUBSCRIBE TO "InvalidateHandle" IN hDataBrwWin.

&ELSE
  DEF INPUT PARAM hDataBrwWin AS HANDLE NO-UNDO.
&ENDIF

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     "Query wrapper: Order drill-down" VIEW-AS TEXT
          SIZE 38 BY 2.14 AT ROW 1.48 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.8 BY 3.52.


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
         HEIGHT             = 3.57
         WIDTH              = 62.6
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
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

  RUN InitializeObject.


  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  &ENDIF
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelDataReady C-Win 
PROCEDURE ExcelDataReady :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.

MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
IF NOT ihBrowse = hBrowse THEN
  RETURN.



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
DEF VAR hHelpText AS HANDLE NO-UNDO.

hBrowse = DYNAMIC-FUNCTION("getBrowseHandle" IN hDataBrwWin). /* Placeholder */

SUBSCRIBE TO "ExcelDataReady" IN hDataBrwWin.

/* The server procedure containing calculations for orderline total (and  price). 
   The calc.proc is loaded persistent before the query executes: */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
/* Automatically add calculated fields for month and quarter based on order date: */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"addperiodOrderDate","mq"). /* Possible: wmqy (week,month,quarter,year) */

/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"useExcelViewer","yes").  */

DYNAMIC-FUNCTION("setAttribute",hBrowse,"userSettingContext",THIS-PROCEDURE:FILE-NAME).
DYNAMIC-FUNCTION("setEnableSaveQuery" IN hDataBrwWin,YES).

/* Set the fields available for calculation of totals by rigth-click (default is all DECIMAL fields): */
DYNAMIC-FUNCTION("setTotalFields" IN hDataBrwWin,"Qty,LineTotal").

RUN InitializeObject IN hDataBrwWin (
    "Order"
      + ";OrderNum"
      + ";OrderDate"
      + ";OrderStatus"
      + ";Terms"
      + ";Carrier"
      + ";CustNum"
      + ";SalesRep"
    + ",SalesRep"
      + ";RepName"
    + ",Customer"
      + ";Name@9"   /* <- move column to pos 9, taken in account the two extra calculated fields for OrderDate */
    + ",OrderLine"
      + ";ItemNum|Item|>>>9"
      + ";OrderLineStatus"
      + ";+OrderPrice|DECIMAL|->><>>><>>9.99|orderline_price|Price"   /* <- the slightly altered format syntax is needed to keep the string intact */
      + ";Qty"
      + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
      + ";!Price"
    + ",Item"
      + ";ItemName@13"
    ,"WHERE false"
      + ",FIRST SalesRep NO-LOCK OF Order"
      + ",FIRST Customer NO-LOCK OF Order"
      + ",EACH OrderLine NO-LOCK OF Order WHERE LineNum > -10000"  /* <- watch out! This is neccessary to make sure that the orderline index is always used */
      + ",FIRST Item NO-LOCK OF OrderLine"
     ,"ordernum"  /* Initial sort -> neccessary to create search field */
     ,NO). /* <- do record count */
   
hBrowse = DYNAMIC-FUNCTION("getBrowseHandle" IN hDataBrwWin).
hBrowse:SEPARATORS = NO.

/* Enable shift-cursor-up/down to select rows: */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").

/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"extraFilterFields",            */
/*                  "Item.itemname|Item bought,OrderLine.Qty|Order qty").  */

/* Set attributes to define lookups (and drop-downs) in the dynamic filter: */
{incl/dynfilterlookups.i hBrowse}

/* Depends on session color "RowShade" */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").
/* GLOBAL setting for row-shade color: */
/* DYNAMIC-FUNCTION("AddColor","RowShade",240,240,240). */


/* Assist in describing the prescan query route for Item since it is not directly joined from Order: */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryItem","EACH OrderLine OF Item NO-LOCK,FIRST Order OF OrderLine NO-LOCK"). 

DYNAMIC-FUNCTION("setAttribute",hBrowse,"allowCAN-DOfilterOperator","Salesrep,custnum").

/* If a prescan (subselect) happens on orderline it's probable that it will be faster to swap to orderline as the primary buffer for the query: */
/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"altprimarybufferlist","OrderLine").  */

/* Disable filtering on these fields: */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"FilterExcludeFields","OrderLineStatus").

/* Disable sorting on these fields: */
DYNAMIC-FUNCTION("setAttribute",hBrowse,"NoColumnSort","jb_quarter_orderDate").

DYNAMIC-FUNCTION("setAttribute",hBrowse,"NoMandatoryCalcFields","LineTotal").

DYNAMIC-FUNCTION("setAttribute",hBrowse,"availAccumFields","LineTotal").
/* Alternatively, specify the datatypes available for accumulation (default DECIMAL) */
/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumDataTypes","DECIMAL,INTEGER"). */

DYNAMIC-FUNCTION("setAttribute",hBrowse,"distinctDataTypes","integer,date,character"). /* Default: CHARACTER,DATE,LOGICAL */
/* Alternatively, specify the exact field names available for distinct selection: */
/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"availDistinctColumns",                                                                                                */
/*                  "OrderDate,jb_month_OrderDate,jb_quarter_OrderDate,OrderStatus,Terms,Carrier,CustNum,Name,SalesRep,RepName,Itemnum,ItemName,OrderLineStatus"). */

/* To set overall limit to result set: */
/*DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE Order.OrderDate > DATE('06/01/08')").*/

/* To start the query with initial filter and/or accumulation but check for user-defined query first: */
/*
RUN InitQuery IN hDataBrwWin (
                    "ItemName"   /* Filter fields (comma sep) */
                   ,"BEGINS"   /* Filter operators (comma sep) */
                   ,"golf"   /* Filter values (pipe sep) */
                   ,""   /* Initial sort column(s) (comma sep) */
                   ,""   /* Distinct columns (comma sep) */
                   ,""   /* Columns to accumulate (comma sep) */
                   ).
*/

/* To only check for user-defined query to start: */
RUN InitWithUserQuery IN hDataBrwWin (OUTPUT bOk).
IF NOT bOk THEN RUN InvokeMethod (hBrowse,"OpenQuery").

/*
/* To alway start with THIS query filter and accum setting: */
DYNAMIC-FUNCTION("StartQuery" IN hDataBrwWin,
                  "ItemName"   /* Filter fields (comma sep) */
                 ,"BEGINS"   /* Filter operators (comma sep) */
                 ,"golf"   /* Filter values (pipe sep) */
                 ,""   /* Initial sort column(s) (comma sep) */
                 ,""   /* Distinct columns (comma sep) */
                 ,""   /* Columns to accumulate (comma sep) */
                 ).
*/

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
DEF INPUT PARAM ihProc AS HANDLE NO-UNDO.

IF ihProc = hDataBrwWin THEN
  APPLY "close" TO THIS-PROCEDURE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

