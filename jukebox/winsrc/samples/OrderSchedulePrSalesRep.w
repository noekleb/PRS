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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR iSuccess    AS INT    NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hQueryEdit  AS HANDLE NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR hBufferEdit AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.

{JukeBoxControlsGeneral.i}
{JukeBoxScheduler.i}

DEF VAR hControlsLibrary    AS HANDLE NO-UNDO.
DEF VAR hSched              AS INT    NO-UNDO.

DEF VAR hOrderDetail        AS HANDLE NO-UNDO.
DEF VAR hOrderQuery         AS HANDLE NO-UNDO.
DEF VAR hOrderToolbar       AS HANDLE NO-UNDO.
DEF VAR hOrderFieldMap      AS HANDLE NO-UNDO.
                            
DEF VAR cSalesRepList       AS CHAR   NO-UNDO.
DEF VAR cRepNames           AS CHAR   NO-UNDO.
DEF VAR dCurrStartDate      AS DATE   NO-UNDO.
DEF VAR dCurrEndDate        AS DATE   NO-UNDO.
DEF VAR dEarliestStartDate  AS DATE   NO-UNDO.
DEF VAR dLastEndDate        AS DATE   NO-UNDO.
DEF VAR cRowIdSalesRepList  AS CHAR   NO-UNDO.
DEF VAR EventDeleteHandle   AS INT    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPrint cmbCategory2 GoToDate cmbView ~
btnCalMyDate btnCategoryColor btnSalesRep 
&Scoped-Define DISPLAYED-OBJECTS cmbCategory2 GoToDate cmbView 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillSchedule C-Win 
FUNCTION fillSchedule RETURNS LOGICAL
  ( INPUT iiStartRow AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD goToDate C-Win 
FUNCTION goToDate RETURNS LOGICAL
  ( INPUT ihDateFillIn AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRGBvalues C-Win 
FUNCTION setRGBvalues RETURNS LOGICAL
  ( INPUT icRGB AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalMyDate 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnCategoryColor 
     IMAGE-UP FILE "bmp/color.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 11" 
     SIZE 4.4 BY 1.05 TOOLTIP "Select background color for over-due orders".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "bmp/print16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Print" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON btnSalesRep  NO-FOCUS FLAT-BUTTON
     LABEL "Select salesrep" 
     SIZE 19 BY 1.14.

DEFINE VARIABLE cmbCategory2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item category" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cmbView AS INTEGER FORMAT "9":U INITIAL 4 
     LABEL "View" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Days",0,
                     "Weeks",1,
                     "Months",2,
                     "Year",3,
                     "Time-grid",4
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE GoToDate AS DATE FORMAT "99/99/9999" 
     LABEL "Go to date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnPrint AT ROW 1.19 COL 155.2
     cmbCategory2 AT ROW 1.24 COL 34 COLON-ALIGNED
     GoToDate AT ROW 1.24 COL 79 COLON-ALIGNED
     cmbView AT ROW 1.24 COL 132.8 COLON-ALIGNED
     btnCalMyDate AT ROW 1.38 COL 97.2
     btnCategoryColor AT ROW 1.19 COL 160
     btnSalesRep AT ROW 1.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 164 BY 29.57.

DEFINE FRAME frmSchedule
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.43
         SIZE 163 BY 28.1.


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
         TITLE              = "Order delivery schedule"
         HEIGHT             = 29.57
         WIDTH              = 163.8
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 235.4
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 235.4
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
/* REPARENT FRAME */
ASSIGN FRAME frmSchedule:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frmSchedule:MOVE-AFTER-TAB-ITEM (cmbView:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME frmSchedule
                                                                        */
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
ON END-ERROR OF C-Win /* Order delivery schedule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order delivery schedule */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frmSchedule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmSchedule C-Win
ON LEFT-MOUSE-UP OF FRAME frmSchedule
DO:
  DEF VAR iStartDate  AS INT  NO-UNDO.
  DEF VAR iStartTime  AS INT  NO-UNDO.
  DEF VAR iEndDate    AS INT  NO-UNDO.
  DEF VAR iEndTime    AS INT  NO-UNDO.
  DEF VAR iResourceId AS INT  NO-UNDO.
  DEF VAR cRowId      AS CHAR NO-UNDO.
  DEF VAR iRecId      AS INT  NO-UNDO.

  DEF VAR hPromiseDateFld AS HANDLE NO-UNDO.
  DEF VAR hSalesRepFld    AS HANDLE NO-UNDO.

  ASSIGN cRowId ="XXXXXXXXXX".  /*Need as place holder for variable.*/
  IF PROVERSION > "10.1a" THEN
    cRowId ="XXXXXXXXXXXXXXXXXX".

  DEF VAR cAction         AS CHAR NO-UNDO.

  IF {&SCH_EVENT_NEW} THEN
    cAction = "NEW".
  ELSE IF {&SCH_EVENT_EDIT} THEN do:
    RUN SchedGetRowId(hSched,OUTPUT cRowId,OUTPUT iSuccess).
    cAction = "edit".
  END.
  ELSE IF {&SCH_EVENT_DELETE} THEN DO:
    RUN SchedGetRowId(hSched, OUTPUT cRowId, OUTPUT iSuccess).
    cAction = "Delete".
  END.
  ELSE IF {&SCH_EVENT_SCROLL-PAST} THEN DO:
    /*In this event, you can get more scheduled dates from the database
      and populate the scheduler control.
      
      The "Past Dates" represent, the furthest date in the past due to the
      scroll, up to the oldest date in the past that you previously added.
      The "Future Dates" represents the last future date you previously added to
      the most future date due to the scroll event.
      
      The "All Dates" is usefull when the view is set to years.  It returns
      the furthest date in the past represented by the scroll and the most 
      future date in the future represented by the scroll regardless of what 
      events have already been entered. You must keep track of events
      you have already added when using this date range.*/

    RUN SchedGetScrollRange(hSched,OUTPUT iStartDate, OUTPUT iEndDate,
                           {&SCH_SCROLL-RANGE-PAST-DATES},
                            OUTPUT iSuccess).

    ASSIGN dCurrEndDate   = dCurrEndDate + 35 /* dEarliestStartDate - 1 */
           dCurrStartDate = DATE(iStartDate).

    fillSchedule(-1).
    dEarliestStartDate = MIN(dEarliestStartDate,dCurrStartDate).

    RETURN.
  END.
  ELSE IF {&SCH_EVENT_SCROLL-FUTURE} THEN DO:    
    RUN SchedGetScrollRange(hSched,OUTPUT iStartDate, OUTPUT iEndDate,
                           {&SCH_SCROLL-RANGE-FUTURE-DATES},OUTPUT iSuccess).

    ASSIGN dCurrStartDate = dLastEndDate + 1
           dCurrEndDate   = DATE(iEndDate)
           dLastEndDate   = MAX(dLastEndDate,dCurrEndDate). 

    fillSchedule(-1).
    RETURN.
  END.
  ELSE DO:
    MESSAGE PROGRAM-NAME(1) SKIP
            "Not handled" 
            VIEW-AS ALERT-BOX.
    RETURN.
  END.
  IF NOT VALID-HANDLE(hOrderDetail) THEN DO:
    RUN OrderDetailAndItems.w PERSIST SET hOrderDetail.
    RUN InitializeObject IN hOrderDetail.
  END.
  hOrderQuery     = DYNAMIC-FUNCTION("getQuery" IN hOrderDetail).
  hOrderToolbar   = DYNAMIC-FUNCTION("getToolbarHandle" IN hOrderDetail).
  DYNAMIC-FUNCTION("setAttribute",hOrderToolbar,"enabledEvents","FIRST,PREV,NEXT,LAST").
  hOrderFieldMap  = DYNAMIC-FUNCTION("getLinkedObject",hOrderToolbar,"FieldMap","from").
  hPromiseDateFld = DYNAMIC-FUNCTION("getFieldHandle",hOrderFieldMap,"PromiseDate").
  hSalesRepFld    = DYNAMIC-FUNCTION("getFieldHandle",hOrderFieldMap,"SalesRep").
  DYNAMIC-FUNCTION("CreateOneToOneLink",hOrderQuery,hQuery,"OrderNum").

  RUN MoveToTop IN hOrderDetail.

  IF CAN-DO("edit,Delete",cAction) THEN DO:      
    bOK = hBuffer:FIND-FIRST("WHERE RowIdent1 = '" + cRowId + "'") NO-ERROR.
    IF bOk THEN DO:
      hQuery:REPOSITION-TO-ROWID(hBuffer:ROWID).
      hQuery:GET-NEXT().
      DYNAMIC-FUNCTION("applyEvent",hQuery,"VALUE-CHANGED").
    END.
    ELSE MESSAGE "Not found: " cRowId.

    IF cAction = "Delete" THEN 
      RUN InvokeMethod(hOrderToolbar,"DeleteRecord").
      
  END.
  ELSE IF cAction = "new" THEN DO:
    RUN SchedGetSelectedStartStopDateTimes(hSched,
                                           OUTPUT iStartDate,
                                           OUTPUT iStartTime,
                                           OUTPUT iEndDate,
                                           OUTPUT iEndTime,
                                           OUTPUT iResourceId,
                                           OUTPUT iSuccess).
    RUN InvokeMethod(hOrderToolbar,"NewRecord").  
    ASSIGN hPromiseDateFld:SCREEN-VALUE = STRING(DATE(iStartDate))
           hSalesRepFld:SCREEN-VALUE    = ENTRY(iResourceId,cSalesRepList,"|")
           NO-ERROR.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalMyDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalMyDate C-Win
ON CHOOSE OF btnCalMyDate IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (GoToDate:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCategoryColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCategoryColor C-Win
ON CHOOSE OF btnCategoryColor IN FRAME DEFAULT-FRAME /* Button 11 */
DO:
  DEF VAR iColor  AS INT NO-UNDO.
  DEF VAR iColNum AS INT NO-UNDO.

  /* See function setRGBvalue for setting the categories: */
  RUN JBoxDSelectColor.w ("",INPUT-OUTPUT iColor,OUTPUT iColNum,OUTPUT bOK).
  IF bOk AND hQuery:NUM-RESULTS > 0 THEN
    APPLY "value-changed" TO cmbCategory2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME DEFAULT-FRAME /* Print */
DO:
  RUN SchedPrintPreview(hSched,OUTPUT iSuccess).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalesRep C-Win
ON CHOOSE OF btnSalesRep IN FRAME DEFAULT-FRAME /* Select salesrep */
DO:
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SalesRep"      
                      + ";RepName"
                      + ";SalesRep"  
                      ,"where true",
                      INPUT-OUTPUT cRowIdSalesRepList,
                      "SalesRep", /* Primary key */
                      INPUT-OUTPUT cSalesRepList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    RUN SchedDeleteAllEvents(hSched,OUTPUT iSuccess).
    DO ix = 1 TO NUM-ENTRIES(cRepNames,"|"):
      RUN SchedDeleteResource(hSched,ix,OUTPUT iSuccess).
    END.
/*     RUN SchedDeleteAllResources(hSched,OUTPUT iSuccess). */

    cRepNames     = DYNAMIC-FUNCTION("getFieldList","SalesRep;RepName","WHERE CAN-DO('" + REPLACE(cSalesRepList,"|",",") + "',SalesRep) BY repname").

    DO ix = 1 TO NUM-ENTRIES(cSalesRepList,"|"):
      RUN SchedAddResource (hSched,ix,-1 ,0,ENTRY(ix,cRepNames,"|"),1,OUTPUT iSuccess).
    END.

    ASSIGN dCurrStartDate = dEarliestStartDate
           dCurrEndDate   = dLastEndDate.
    DYNAMIC-FUNCTION("setAttribute",hQuery,"baseQuery","WHERE CAN-DO('" + REPLACE(cSalesRepList,"|",",") + "',SalesRep)").
    fillSchedule(0).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCategory2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCategory2 C-Win
ON VALUE-CHANGED OF cmbCategory2 IN FRAME DEFAULT-FRAME /* Item category */
DO:
  RUN SchedDeleteAllEvents(hSched,OUTPUT iSuccess).

  IF cmbCategory2:SCREEN-VALUE NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",hQuery,"preScanQuery",
                     "ITEM WHERE Category2 = '" + cmbCategory2:SCREEN-VALUE
                   + "',EACH OrderLine OF ITEM NO-LOCK,FIRST Order OF OrderLine NO-LOCK").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hQuery,"preScanQuery","").

  IF cSalesRepList = "" THEN DO:
    cSalesRepList = DYNAMIC-FUNCTION("getFieldList","SalesRep;SalesRep","WHERE TRUE BY repname").
    cRepNames     = DYNAMIC-FUNCTION("getFieldList","SalesRep;RepName","WHERE TRUE BY repname").

    DO ix = 1 TO NUM-ENTRIES(cSalesRepList,"|"):
      RUN SchedAddResource (hSched,ix,-1 ,0,ENTRY(ix,cRepNames,"|"),1,OUTPUT iSuccess).
    END.
  END.

  ASSIGN dCurrStartDate = dEarliestStartDate
         dCurrEndDate   = dLastEndDate.
  fillSchedule(0).

  FRAME frmSchedule:HIDDEN = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbView C-Win
ON VALUE-CHANGED OF cmbView IN FRAME DEFAULT-FRAME /* View */
DO:
  ASSIGN cmbView.
  RUN SchedSetViewType(hSched, cmbView,OUTPUT iSuccess).

  IF cmbView = 3 THEN DO: 
    ASSIGN dCurrStartDate     = DATE(01,01,YEAR(dEarliestStartDate))
           dCurrEndDate       = DATE(12,31,YEAR(dLastEndDate))
           dEarliestStartDate = dCurrStartDate
           dLastEndDate       = dCurrEndDate.
    fillSchedule(-1).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME GoToDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL GoToDate C-Win
ON RETURN OF GoToDate IN FRAME DEFAULT-FRAME /* Go to date */
DO:
  goToDate(GoToDate:HANDLE).
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

/*   RUN Controls.p PERSISTENT SET hControlsLibrary. */

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
RUN SUPER.
IF hBuffer:AVAIL THEN
  RUN SchedMakeEventVisible (hSched,hBuffer:BUFFER-FIELD("EventHandle"):BUFFER-VALUE,OUTPUT iSuccess).
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
  DISPLAY cmbCategory2 GoToDate cmbView 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnPrint cmbCategory2 GoToDate cmbView btnCalMyDate btnCategoryColor 
         btnSalesRep 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME frmSchedule IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSchedule}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndDeleteRecord C-Win 
PROCEDURE EndDeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihObject AS HANDLE NO-UNDO.

IF ihObject = hOrderToolbar THEN DO:
  RUN SchedDeleteEvent(hSched,EventDeleteHandle,OUTPUT iSuccess).
/*   RUN SchedAcknowledgeDelete(hSched,OUTPUT iSuccess).  */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndSaveRecord C-Win 
PROCEDURE EndSaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihObject      AS HANDLE NO-UNDO.
DEF INPUT PARAM icAction      AS CHAR   NO-UNDO. /* New / edit */

DEF VAR cCurrPreScanQuery AS CHAR NO-UNDO.

IF ihObject NE hOrderToolbar THEN RETURN.

cCurrPreScanQuery = DYNAMIC-FUNCTION("getAttribute",hQuery,"preScanQuery").

DYNAMIC-FUNCTION("setAttribute",hQuery,"preScanQuery","").
DYNAMIC-FUNCTION("setAttribute",hQuery,"QueryWhere",
                 " AND ROWID(Order) = TO-ROWID('" + hOrderQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "')").
fillSchedule(-1).
DYNAMIC-FUNCTION("setAttribute",hQuery,"preScanQuery",cCurrPreScanQuery).

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
RUN enable_UI.

RUN SchedulerCreate(FRAME frmSchedule:HWND,OUTPUT hSched).
/* RUN SchedLoadImageList(hSched,{&SCH_RESOURCES-IMAGE-LIST},  */
/*                        "D:\ProWrk\James\bmp",               */
/*                        "jlw.bmp",                           */
/*                        OUTPUT iSuccess).                    */
RUN SchedSetViewType(hSched, 4,OUTPUT iSuccess). /* Timegrid view */
RUN SchedSetIntegerProperty(hSched,
                            {&SCH_PROP_VIEWTIMEGRID_SCALES_MAJORUNIT},
                            2 /*Week*/ ,OUTPUT iSuccess).
RUN SchedSetIntegerProperty(hSched,{&SCH_PROP_VIEWTIMEGRID_SCALES_MINORUNIT},
                            0 /*Day*/ ,OUTPUT iSuccess).
RUN SchedSetIntegerProperty(hSched,{&SCH_PROP_VIEWTIMEGRID_SCALES_MINORWIDTH},
                            100 /* 200 pixels */ ,OUTPUT iSuccess).
/* RUN SchedSetBooleanProperty(hSched,182,{&GEN_ENABLE},OUTPUT iSuccess). /*Gradient Drawing Style*/  */
RUN SchedSetBooleanProperty(hSched,66,{&GEN_DISABLE},OUTPUT iSuccess). /*Rotate Caption*/
RUN SchedSetBooleanProperty(hSched,0,1,OUTPUT iSuccess). /* Use Progress dialog for details */
RUN SchedSetBooleanProperty(hSched,{&SCH_PROP_POPUPMENU-NEWALLDAYEVENT},{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu NewAllDayEvent */
RUN SchedSetBooleanProperty(hSched,{&SCH_PROP_POPUPMENU-NEWRECURRINGEVENT},{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu NewReccuringEvent */
RUN SchedSetBooleanProperty(hSched,{&SCH_PROP_POPUPMENU-TODAY},{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu Today */
RUN SchedSetBooleanProperty(hSched,{&SCH_PROP_POPUPMENU-GOTODATE},{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu GoToDate */
RUN SchedSetBooleanProperty(hSched,{&SCH_PROP_POPUPMENU-GOTOTHISDAY},{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu GoToThisDay */
RUN SchedSetBooleanProperty(hSched,15,{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu Resources layout */
RUN SchedSetBooleanProperty(hSched,44,{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu ShowTimeAs layout */
RUN SchedSetBooleanProperty(hSched,45,{&GEN_DISABLE},OUTPUT iSuccess). /* Disable popupmenu Label layout */

RUN SchedSetBooleanProperty(hSched,41,{&GEN_DISABLE},OUTPUT iSuccess). /* Disable sizing of event */
/* RUN SchedSetBooleanProperty(hSched,49,{&GEN_DISABLE},OUTPUT iSuccess). /* Disable sizing of date navigator */  */


RUN SchedSetIntegerProperty(hSched,67, MAX(10,NUM-ENTRIES(cSalesRepList,"|")),OUTPUT iSuccess). /* 67    OptionsView->ResourcesPerPage */
/* RUN SchedLockDateRange(hSched,INT(TODAY),INT(TODAY) + 31,OUTPUT iSuccess).  */
/* RUN SchedAddCategory(hSched,"Not due",16777215,OUTPUT iSuccess). */
/* RUN SchedAddCategory(hSched,"Overdue",16711261,OUTPUT iSuccess). */
/*                                                                  */
ASSIGN cmbCategory2:DELIMITER IN FRAME {&FRAME-NAME} = "|"
       cmbCategory2:LIST-ITEMS = "|" + DYNAMIC-FUNCTION("getFieldList","Item;DISTINCT Category2","WHERE TRUE").


SUBSCRIBE TO "EndDeleteRecord" ANYWHERE.
SUBSCRIBE TO "EndSaveRecord"   ANYWHERE.
SUBSCRIBE TO "StartJBoxEvent"  ANYWHERE.

hQuery = DYNAMIC-FUNCTION("NewQuery"
        ,5000
        ,""
        ,"Order"
         + ";OrderNum"
         + ";PromiseDate"
         + ";ShipDate"
         + ";SalesRep"
         + ";+ItemList|CHARACTER|X(100)|order_itemlist(OrderNum)"
         + ";+EventHandle|INTEGER|>>>>>>>>9"
         + ";+TimeFrom|INTEGER|>>>>>>>>9"
         + ";+TimeTo|INTEGER|>>>>>>>>9"
       + ",Customer"
         + ";NAME"
       + ",SalesRep"
         + ";RepName"
        ,"WHERE FALSE"
       + ",FIRST Customer OF Order NO-LOCK"
       + ",FIRST SalesRep OF Order NO-LOCK"
        ,"").
  
DYNAMIC-FUNCTION("setAttribute",hQuery,"calcFieldProc","order_browsecalc.p").
DYNAMIC-FUNCTION("setAttribute",hQuery,"getRecordCount","yes").
hBuffer = hQuery:GET-BUFFER-HANDLE(1).
DYNAMIC-FUNCTION("setSortString",hQuery,"PromiseDate,RepName,OrderNum").

ASSIGN dCurrStartDate     = TODAY - DAY(TODAY) - WEEKDAY(TODAY - DAY(TODAY)) + 2
       dCurrEndDate       = TODAY - DAY(TODAY) - WEEKDAY(TODAY - DAY(TODAY)) + 2 + 41
       dEarliestStartDate = dCurrStartDate
       dLastEndDate       = dCurrEndDate.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,200,0,0).

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
APPLY "entry" TO btnSalesRep IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MyCalenderAction C-Win 
PROCEDURE MyCalenderAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihDate AS HANDLE NO-UNDO.

IF ihDate = GoToDate:HANDLE IN FRAME {&FRAME-NAME} THEN
  goToDate(GoToDate:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartJBoxEvent C-Win 
PROCEDURE StartJBoxEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Must grab the handle to the calender event here cause the one-to-one link
              between the order detail and navigation query automatically facilitates delete of the record in the navigation buffer 
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihObject      AS HANDLE NO-UNDO.
DEF INPUT PARAM ihSourceProc  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihWidget      AS HANDLE NO-UNDO.
DEF INPUT PARAM icMethod      AS CHAR   NO-UNDO.

IF icMethod = "DeleteRecord" AND ihObject = hOrderToolbar THEN     
  EventDeleteHandle = hBuffer:BUFFER-FIELD("EventHandle"):BUFFER-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillSchedule C-Win 
FUNCTION fillSchedule RETURNS LOGICAL
  ( INPUT iiStartRow AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: iiStartRow: -1 to keep retrieved rows, 0 to purge retrieved rows (filter on category)
------------------------------------------------------------------------------*/
DEF VAR iEventHandle     AS INT   NO-UNDO.
DEF VAR iToTime          AS INT   NO-UNDO.
DEF VAR cCurrSalesRepDay AS CHAR  NO-UNDO.
DEF VAR iCnt             AS INT   NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",hQuery,"queryWhere") = "" THEN
  DYNAMIC-FUNCTION("setAttribute",hQuery,"queryFilter",
                   " AND PromiseDate GE DATE('" + STRING(dCurrStartDate) + "')"
                 + " AND PromiseDate LE DATE('" + STRING(dCurrEndDate) + "')"
                   ).
ELSE 
  DYNAMIC-FUNCTION("setAttribute",hQuery,"queryFilter","").

DYNAMIC-FUNCTION("fillQuery",hQuery,
                  INT(DYNAMIC-FUNCTION("getAttribute",hQuery,"rowstobatch")),
                  iiStartRow, 
                  DYNAMIC-FUNCTION("getAttribute",hQuery,"buffersandfields"),
                  DYNAMIC-FUNCTION("getAttribute",hQuery,"basequery") + 
                    DYNAMIC-FUNCTION("getAttribute",hQuery,"queryfilter") + 
                    DYNAMIC-FUNCTION("getAttribute",hQuery,"querywhere") + 
                    DYNAMIC-FUNCTION("getAttribute",hQuery,"queryjoin")).

RUN SchedBeginEventTransaction(hSched).
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF hBuffer:BUFFER-FIELD("SalesRep"):BUFFER-VALUE + STRING(hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE) NE cCurrSalesRepDay THEN
    iToTime = 8 * 3600.

  iToTime = iToTime + 3600.

  IF hBuffer:BUFFER-FIELD("EventHandle"):BUFFER-VALUE = 0 THEN DO:

    RUN SchedAddCalenderEvent(hSched,
                          hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                          INTEGER(hBuffer:RECID),
                          LOOKUP(hBuffer:BUFFER-FIELD("SalesRep"):BUFFER-VALUE,cSalesRepList,"|"), /* Resource Identifier */
                          STRING(hBuffer:BUFFER-FIELD("OrderNum"):BUFFER-VALUE) + " - "
                          + hBuffer:BUFFER-FIELD("NAME"):BUFFER-VALUE,
                          hBuffer:BUFFER-FIELD("RepName"):BUFFER-VALUE,
                          hBuffer:BUFFER-FIELD("ItemList"):BUFFER-VALUE,
                          IF hBuffer:BUFFER-FIELD("ShipDate"):BUFFER-VALUE LE hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE OR hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE GT TODAY THEN 0 ELSE 1, /* Category ,see setRGBvalue function */
                          0, /* REMIND 0 = no, 1=yes:*/
                          10, /* Remind minutes */
                          2,
                          INT(hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE),
                          iToTime - 3600,
                          INT(hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE),
                          iToTime,
                          OUTPUT iEventHandle).
  
    iCnt = iCnt + 1.

    ASSIGN hBuffer:BUFFER-FIELD("EventHandle"):BUFFER-VALUE = iEventHandle
           hBuffer:BUFFER-FIELD("TimeFrom"):BUFFER-VALUE    = iToTime - 3600
           hBuffer:BUFFER-FIELD("TimeTo"):BUFFER-VALUE      = iToTime.

    IF DYNAMIC-FUNCTION("getAttribute",hQuery,"queryWhere") NE "" THEN
      LEAVE.
  END.
  ELSE IF DYNAMIC-FUNCTION("getAttribute",hQuery,"queryWhere") NE "" THEN DO:      
    RUN SchedModifyEvent(hSched,
                          hBuffer:BUFFER-FIELD("EventHandle"):BUFFER-VALUE,
                          hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                          INTEGER(hBuffer:RECID),
                          LOOKUP(hBuffer:BUFFER-FIELD("SalesRep"):BUFFER-VALUE,cSalesRepList,"|"), /* Category */
                          STRING(hBuffer:BUFFER-FIELD("OrderNum"):BUFFER-VALUE) + " - "
                          + hBuffer:BUFFER-FIELD("NAME"):BUFFER-VALUE,
                          hBuffer:BUFFER-FIELD("RepName"):BUFFER-VALUE,
                          hBuffer:BUFFER-FIELD("ItemList"):BUFFER-VALUE,
                          IF hBuffer:BUFFER-FIELD("ShipDate"):BUFFER-VALUE LE hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE OR hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE GT TODAY THEN 0 ELSE 1, /* Resource Identifier */
                          0, /* REMIND 0 = no, 1=yes:*/
                          10, /* Remind minutes */
                          2,
                          INT(hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE),
                          hBuffer:BUFFER-FIELD("TimeFrom"):BUFFER-VALUE,
                          INT(hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE),
                          hBuffer:BUFFER-FIELD("TimeTo"):BUFFER-VALUE,
                          OUTPUT iSuccess).
    LEAVE.
  END.
  cCurrSalesRepDay = hBuffer:BUFFER-FIELD("SalesRep"):BUFFER-VALUE + STRING(hBuffer:BUFFER-FIELD("PromiseDate"):BUFFER-VALUE).
  hQuery:GET-NEXT().
END.

RUN SchedCommitEventTransaction(hSched).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION goToDate C-Win 
FUNCTION goToDate RETURNS LOGICAL
  ( INPUT ihDateFillIn AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR dDate AS DATE NO-UNDO.

dDate = DATE(ihDateFillIn:SCREEN-VALUE) NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN DO:
  IF dDate GT dLastEndDate OR dDate LT dEarliestStartDate THEN DO:
    IF dDate GT dLastEndDate THEN
      ASSIGN dCurrEndDate       = dDate - DAY(dDate) - WEEKDAY(dDate - DAY(dDate)) + 2 + 41
             dLastEndDate       = dCurrEndDate.
    ELSE IF dDate LT dEarliestStartDate THEN
      ASSIGN dCurrStartDate     = dDate - DAY(dDate) - WEEKDAY(dDate - DAY(dDate)) + 2
             dEarliestStartDate = dCurrStartDate.
    fillSchedule(-1).
  END.
  RUN SchedGoToDate(hSched,INT(dDate),OUTPUT iSuccess).
  bOK = hBuffer:FIND-FIRST("WHERE PromiseDate = DATE('" + ihDateFillIn:SCREEN-VALUE + "')") NO-ERROR.
  IF bOk THEN
    RUN SchedMakeEventVisible(hSched,hBuffer:BUFFER-FIELD("EventHandle"):BUFFER-VALUE,OUTPUT iSuccess).
END.
ELSE DO:
  MESSAGE "Invalid date"
          VIEW-AS ALERT-BOX ERROR.  
  RETURN NO.   
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRGBvalues C-Win 
FUNCTION setRGBvalues RETURNS LOGICAL
  ( INPUT icRGB AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iColNum AS INT NO-UNDO.

iColNum = INT(ENTRY(3,icRGB)) * 65536 + INT(ENTRY(2,icRGB)) * 256 + INT(ENTRY(1,icRGB)).

RUN SchedDeleteAllCategories(hSched,OUTPUT iSuccess).

/* RUN SchedAddCategory(hSched,"Not due",6536,OUTPUT iSuccess).  */
RUN SchedAddCategory(hSched,"Overdue",iColNum,OUTPUT iSuccess).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

