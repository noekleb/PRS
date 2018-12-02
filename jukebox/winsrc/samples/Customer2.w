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
/*&SCOPED-DEFINE AdvGuiWin*/

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.

&IF DEFINED(AdvGuiWin) &THEN
  DEF VAR oTextBox  AS JBoxDevExEdit NO-UNDO.
  DEF VAR oCalcEdit AS JBoxDevExCalcEdit NO-UNDO.
  DEF VAR oControl  AS System.Windows.Forms.Control NO-UNDO.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CustNum Name Phone CreditLimit Comments ~
Customer Toolbar 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name Phone CreditLimit Comments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Comments AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 52 BY 4.05 NO-UNDO.

DEFINE VARIABLE CreditLimit AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 1500 
     LABEL "Credit Limit" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter a Credit Limit.".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Phone AS CHARACTER FORMAT "x(20)" 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE RECTANGLE Customer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.43.

DEFINE RECTANGLE Toolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CustNum AT ROW 9.33 COL 12 COLON-ALIGNED HELP
          "Please enter a customer number."
     Name AT ROW 10.33 COL 12 COLON-ALIGNED HELP
          "Please enter a name."
     Phone AT ROW 11.38 COL 12 COLON-ALIGNED HELP
          "Please enter a phone number"
     CreditLimit AT ROW 12.43 COL 12 COLON-ALIGNED
     Comments AT ROW 9.33 COL 51 NO-LABEL WIDGET-ID 6
     Customer AT ROW 2.67 COL 2
     Toolbar AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.8 BY 12.62.


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
         TITLE              = "Customer2"
         HEIGHT             = 12.57
         WIDTH              = 103
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 133.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 133.6
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
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer2 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer2 */
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

{incl/wintrigg.i}
/* ON 'window-resized':U OF {&WINDOW-NAME} DO:                                                                     */
/*   DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  */
/*   RUN ResizeComponents.                                                                                         */
/* END.                                                                                                            */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*   RUN enable_UI.  */

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm.
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
/*
&IF DEFINED(PureABLWin) = 0 &THEN
  oTextBox:DisableTextChanged(YES).
  IF hFieldMap:AVAIL THEN DO:
    oTextBox:textBox1:Text = hFieldMap:BUFFER-FIELD("Comments"):BUFFER-VALUE.
    /* Trick since DisplayRecord is executed twice because of the overlay field(s) */
    IF oTextBox:textBox1:Text = "" THEN
      oTextBox:DisableTextChanged(NO).
  /*   IF hFieldMap:BUFFER-FIELD("Balance"):BUFFER-VALUE > 100 THEN                              */
  /*     oTextBox:BackgroundImage = CAST(resGlobal:GetObject(icBgImage), System.Drawing.Image).  */
  END.
  ELSE DO:
    oTextBox:textBox1:Text = "".
  END.
&ENDIF
*/
RUN SUPER.

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
  DISPLAY CustNum Name Phone CreditLimit Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CustNum Name Phone CreditLimit Comments Customer Toolbar 
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
DEF VAR hOverlay AS HANDLE NO-UNDO.

RUN enable_UI.

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

/*DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).*/

DO WITH FRAME {&FRAME-NAME}:
/*
  RUN CreateChildControl ("System.Windows.Forms.TextBox", 
                           "textBox0",
                           OUTPUT oControl).

  RUN SetChildControlProperty (oControl, 
                               "Text", 
                               BOX("Schönen Abend, Stefan!")).
                               
  RUN SetChildControlProperty (oControl, 
                               "Left", 
                               BOX(55)).
                               
  RUN SetChildControlProperty (oControl, 
                               "Top", 
                               BOX(300)).
                               
  RUN SetChildControlProperty (oControl, 
                               "Width", 
                               BOX(110)).
*/

  DYNAMIC-FUNCTION("setAttribute",Customer:HANDLE,"getrecordcount","yes").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
            ,Customer:HANDLE
            ,100
            ,"multiple"
            ,"customer"
             + ";custnum|knr|>>>>>>>9"
             + ";Name"
             + ";SalesRep"
             + ";CreditLimit"
             + ";Balance"
             + ";Phone"
             + ";PostalCode"
             + ";Address"
             + ";City"
             + ";Comments"
            + ",salesrep"
             + ";repname@4"
            ,"WHERE false"
           + ",first salesrep of customer no-lock outer-join"
            ,"").
  hBrowse:TOOLTIP = "Doubleclick to edit salesrep".

/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumfields","custnum").       */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"distinctcolumns","salesrep").  */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"allowMultiDelete","yes"). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"insertBrowseRow","yes").  */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"altprimarybufferlist","salesrep").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").

  hOverlay  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"Salesrep",""
            ,"Salesrep;Repname;Salesrep","WHERE true","Salesrep"
            ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hOverlay,"Salesrep").
  DYNAMIC-FUNCTION("setAttribute",hOverlay,"refreshRow","yes").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
            ,"MultiSortBrowse;Sorter på flere kolonner"
            ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
            ,hBrowse:QUERY
            ,FRAME {&FRAME-NAME}:HANDLE
            ,"Name,Phone,Comments,CreditLimit",""
            ,"CustNum",""
            ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  &IF DEFINED(AdvGuiWin) &THEN
/*     RUN NewRtfTextBox(Comments:HANDLE,YES,"Comments",hFieldMap,"",OUTPUT oTextBox). */
    oTextBox = NEW JBoxDevExEdit(THIS-PROCEDURE,Comments:HANDLE).
    oTextBox:RegisterWithJukeBox(YES).
    oTextBox:CreateDisplayLink(hFieldMap,"Comments").
    oTextBox:cDocLoadContext = "Customer".
    oTextBox:cDocLoadIdFields = "CustNum".
    oTextBox:cDocLoadFormat = "html".
/*     oTextBox:InitializeMenu("standard").  */

    oCalcEdit = NEW JBoxDevExCalcEdit(THIS-PROCEDURE,CreditLimit:HANDLE).
    oCalcEdit:RegisterWithJukeBox(YES).
    oCalcEdit:CreateDisplayLink(hFieldMap,"CreditLimit").
    oCalcEdit:calcEdit1:Scale(0,100000).
    oCalcEdit:calcEdit1:Properties:Mask:EditMask = "n0".
  &ENDIF

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
            ,ToolBar:HANDLE
            ,"File"
            ,"new,copy,undo,delete,save,excel"
           + ",BrowseConfig,Filter"
            ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
END.

RUN InvokeMethod(hBrowse,"OpenQuery").

/* oControl:BringToFront(). */

DYNAMIC-FUNCTION("setResizeYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,
                 "Customer,Comments," + hBrowse:NAME).

DYNAMIC-FUNCTION("setMoveYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,
                 "BUTTON-1,Comments,CustNum,Name,Phone,CreditLimit").

/* DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "Comments").  */
/* DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "Comments"). */
/* DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"Comments").   */
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,100,0,0).
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm ("").
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

