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
/*&SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hWinToolbar AS HANDLE NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RecordToolbar WindowToolbar CustomerBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE CustomerBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 14.76.

DEFINE RECTANGLE RecordToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE RECTANGLE WindowToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 10 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RecordToolbar AT ROW 1.24 COL 2
     WindowToolbar AT ROW 1.24 COL 95
     CustomerBrowse AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.6 BY 16.71.


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
         TITLE              = "Toolbar and menu usage"
         HEIGHT             = 16.71
         WIDTH              = 105.6
         MAX-HEIGHT         = 17.33
         MAX-WIDTH          = 106.2
         VIRTUAL-HEIGHT     = 17.33
         VIRTUAL-WIDTH      = 106.2
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
ON END-ERROR OF C-Win /* Toolbar and menu usage */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Toolbar and menu usage */
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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        "You could copy a record in the browse but that would require overlay fields for input" SKIP
        "plus a trigger to assign the next customer number"
        VIEW-AS ALERT-BOX.
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
  ENABLE RecordToolbar WindowToolbar CustomerBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImgButtonMethod C-Win 
PROCEDURE ImgButtonMethod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:    Demonstrate usage of the JukeBox toolbar 
  Parameters:  <none>
  Notes:      Buttons and menu-items can only map at the first level of sub-menues
              Menu-items for 2nd level sub-menus must be defined by adding a menu-band to the sub-menu
              Buttons
              
              To set consitent accelerator keys throughout the session you can use these attributes:
              DYNAMIC-FUNCTION("setAttribute",SESSION,"CtrlHotkeyActions").  <- comma-separated list of actions, ie "new,save"
              DYNAMIC-FUNCTION("setAttribute",SESSION,"CtrlHotkeys").        <- corresponding comma-separated list of ctrl-keys, ie "N,S"
              DYNAMIC-FUNCTION("setAttribute",SESSION,"AltHotkeyActions").   
              DYNAMIC-FUNCTION("setAttribute",SESSION,"AltHotkeys").         
              DYNAMIC-FUNCTION("setAttribute",SESSION,"FunctionKeys").       
              DYNAMIC-FUNCTION("setAttribute",SESSION,"FunctionKeyActions").
              
              To accomodate these setting you might need to change the triggers defined in 
              winsrc\incl\supptrigg.i (for suppressed windows) 
              winsrc\incl\conttrigg.i (for containers)
              
              Sample from a modified conttrigg.i where F8 is the standard trigger for "new" (and Ctrl-N also is supported):
              
              ON 'ctrl-n' OF FRAME {&FRAME-NAME} OR "f8" OF FRAME {&FRAME-NAME} ANYWHERE DO:
                IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN 
                  DYNAMIC-FUNCTION("ApplyEvent",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from"),"new").
              END.
   
              {1} would here be the handle to a browse or a query
------------------------------------------------------------------------------*/
RUN enable_UI.

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    CustomerBrowse:HANDLE,   
                    100,                 
                    "MULTIPLE",                  
                    "Customer"
                    + ";Name"
                    + ";CustNum"
                    + ";Address"
                    + ";SalesRep"
                   ,"WHERE false"
                   ,"sort|Name").

  /* Add a popup-menu on the browse: */
  DYNAMIC-FUNCTION("NewMenuBand",
                   hBrowse,
                   "MultiSortBrowse;Sort on multiple columns"
                 + ",MyRightClick;My right-click action;MyRightClick;toggle" 
                   /* If 3rd element is ommitted the method name defaults to MyRightClickRecord 
                      4th element (toggle) is optional */
                   ,"").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    RecordToolbar:HANDLE,             
                    "File",                      /* Default sub-menu. Created now if it doesn't exist */             
                    "new,copy,delete"            /* <- a few standard JukeBox buttons. Others are new,delete,copy,etc. Default buttons, accelerator keys and tooltip are attributes on the SESSION object */
                  + ",sub-menu|File;Send To"     /* Create a sub-menu under the File sub-menu with label "Send to" */
                  + ",rule"                      /* Create a separator for the toolbar */
                  + ",|Edit"                     /* Create a new sub-menu to the menubar */
                  + ",browseConfig|Edit;C&olumn setup"  /* The browseConfig action should be parented by the Edit sub-menu */
                  + ",filter|Edit"               /* The filter action should be parented by the Edit sub-menu */
                  + ",flatview;Drill down"       /* Change the label for flatview action to Drill down */
                  + ",accum;;;;gif/pages16.gif"  /* Change the image for standard action accumulate (note: No tooltip) */
                  + ",rule"                      /* Create a separator for the toolbar */
                  + ",|Navigate"                  /* Create a sub-menu on the menubar for navigation */
                  + ",first|Navigate,prev|Navigate,next|Navigate,last|Navigate"   /* Add buttons for navigation and create corresponding menu-items in the navigation sub-menu */
                  + ",rule"
                  + ",sub-menu|Navigate;Go"
                  + ",GoToOrders|Go;Go to orders¤menu"
                  + ",Print|"                    /* Print button - just the button due to the blank sub-menu reference */
                  + ",PrintWithSelection;Print ctrl-p¤menu"   /* Print with printer selection - just for the menu because of ¤menu */
                  + ",MyImageButton;Image &button label;Image button tooltip;ImgButtonMethod;gif/graph.gif¤enable"
                  + ",MyTextButton;Text btn&¤enable"  /* To avoid hot-key end the lable with &. The button should always be enabled - independent of record availability */
                  + ",AnotherTextButton|;Text bt&n 2;Text button with very long tooltip text" + CHR(10) + "to demonstrate how a tooltip can span" + CHR(10) + "multiple lines"
                  + ",ToggleSaveSettings|Settings;Save settings;toggle"
                    ,"maxborder"). /* View the border and expand it to span the frame */
/*                     ,"border"). /* View the border NB! Remember to add rectangle to NoResizeX rule */  */
/*                      ,"").  /* No borders (remove the rulers */ */

  /* Handles to sub-menus are now available as attributes on the toolbar. */
  DYNAMIC-FUNCTION("NewMenuBand",
                   WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"sub-menuSend To")),
                   "Excel"
                  ,"test").

  /* If we want the toolbar actions also to be available as right-click menu in the browse: */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"CopyToolbarToBrowse","yes").
  /* Note: The toolbar-events are copied when the browse and toolbar objects are linked
           Hence to add the custom right-click actions AFTER the toolbar actions define the additional menu-band after linking
           Copying toolbars to browses can also be a SESSION or CONTAINER attribute 
           */ 

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).


  hWinToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    WindowToolbar:HANDLE,             
                    "File",                   /* <- Default: Append the actions in this menu to the File menu */       
                    "close;E&xit"
                  + ",help|Help"              /* The Help action should have a separate sub-menu */ 
                    ,"enable,right").         /* Enable the menu independent of record availability. Add buttons from right to left */

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,200,0,0).

  RUN InvokeMethod (hBrowse,"OpenQuery").

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MyRightClick C-Win 
PROCEDURE MyRightClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MyTextButtonRecord C-Win 
PROCEDURE MyTextButtonRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        "Note the default naming convention for custom buttons:" SKIP
        "<Action>Record"
        VIEW-AS ALERT-BOX.
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
MESSAGE PROGRAM-NAME(1) SKIP
        "You could add a record in the browse but that would require overlay fields for input"
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintWithSelectionRecord C-Win 
PROCEDURE PrintWithSelectionRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bOk AS LOG NO-UNDO.

SYSTEM-DIALOG PRINTER-SETUP UPDATE bOk.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToggleSaveSettingsRecord C-Win 
PROCEDURE ToggleSaveSettingsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrWidget AS HANDLE NO-UNDO.

hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").

MESSAGE PROGRAM-NAME(1) SKIP
        hCurrWidget:NAME
        hCurrWidget:CHECKED
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

