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

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hToolbarObject  AS HANDLE NO-UNDO.
DEF VAR hFilterWidget   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbDialogType rsCurrentFilter tbPermanent ~
btnOk btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cmbDialogType rsCurrentFilter tbPermanent 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbDialogType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 67 BY 1 NO-UNDO.

DEFINE VARIABLE rsCurrentFilter AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Dette filteret", 1,
"Alle filtre", 2
     SIZE 28 BY 1.1 NO-UNDO.

DEFINE VARIABLE tbPermanent AS LOGICAL INITIAL no 
     LABEL "Permanent" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .76 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbDialogType AT ROW 1.33 COL 9 COLON-ALIGNED
     rsCurrentFilter AT ROW 2.38 COL 10.8 NO-LABEL
     tbPermanent AT ROW 3.43 COL 10.8
     btnOk AT ROW 4.24 COL 48
     btnCancel AT ROW 4.24 COL 63.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 4.65
         DEFAULT-BUTTON btnOk CANCEL-BUTTON btnCancel.


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
         TITLE              = "Velg dialogtype for filter"
         HEIGHT             = 4.67
         WIDTH              = 80.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/app16.ico":U) THEN
    MESSAGE "Unable to load icon: ico/app16.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Velg dialogtype for filter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Velg dialogtype for filter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
  DEF VAR hFilterWin AS HANDLE NO-UNDO.
  ASSIGN rsCurrentFilter tbPermanent.
  IF cmbDialogType:SCREEN-VALUE NE ? AND cmbDialogType:SCREEN-VALUE NE "" THEN DO:
    IF rsCurrentFilter = 1 THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hToolbarObject,"filterwindow",cmbDialogType:SCREEN-VALUE).
      IF tbPermanent THEN 
        DYNAMIC-FUNCTION("setUserSetting",
                          DYNAMIC-FUNCTION("getObjectSourceFile",hToolbarObject),
                          DYNAMIC-FUNCTION("getObjectName",hToolbarObject),
                          DYNAMIC-FUNCTION("getAttribute",hToolbarObject,"usersettingcontext"),
                          "filterwindow",
                          cmbDialogType:SCREEN-VALUE).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION("msetAttribute",?,"filterwindow","").
      DYNAMIC-FUNCTION("setAttribute",SESSION,"filterwindow",cmbDialogType:SCREEN-VALUE).
      IF tbPermanent THEN
        DYNAMIC-FUNCTION("setUserSetting",
                          "session",
                          "",
                          "",
                          "filterwindow",
                          cmbDialogType:SCREEN-VALUE).
    END.
  END.
  IF VALID-HANDLE(hFilterWidget) AND hFilterWidget:NAME MATCHES "*filter*" AND 
     (hFilterWidget:TYPE = "button" OR hFilterWidget:TYPE = "menu-item") THEN DO:
    APPLY "choose" TO hFilterWidget.
    hFilterWin = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",DYNAMIC-FUNCTION("getCurrentObject"),"filterHandle")) NO-ERROR.
  END.
  APPLY "close" TO THIS-PROCEDURE.
  IF VALID-HANDLE(hFilterWin) THEN RUN MoveToTop IN hFilterWin NO-ERROR.
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
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
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
  DISPLAY cmbDialogType rsCurrentFilter tbPermanent 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmbDialogType rsCurrentFilter tbPermanent btnOk btnCancel 
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
DEF VAR cCustomFilter   AS CHAR   NO-UNDO.
DEF VAR cCurrFilter     AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  hToolbarObject = DYNAMIC-FUNCTION("getCurrentObject").
  hFilterWidget  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbarObject,"buttonFilter")) NO-ERROR.

  cCustomFilter = DYNAMIC-FUNCTION("getAttribute",hToolbarObject,"customfilterwindow").
  IF cCustomFilter NE "" THEN
    cCustomFilter = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "|Tilpasset" ELSE "|Customized") + "|"
                  + cCustomFilter.

  ASSIGN cmbDialogType:DELIMITER = "|"
         cmbDialogType:LIST-ITEM-PAIRS = (IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                           "Operator/verdi med mulighet for avansert filter" 
                                          ELSE "Operator/value style with advanced option")
                                       + "|JBoxDynFilter.w|"
                                       + (IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                           "Fra/til verdi samt kolonne for tekstsøk" 
                                          ELSE "From/to value plus text search")
                                       + "|JBoxDynamicsFilter.w"
                                       + cCustomFilter
         cCurrFilter  = DYNAMIC-FUNCTION("getAttribute",hToolbarObject,"filterwindow")
         .
  IF cCurrFilter = "" THEN
    cCurrFilter  = DYNAMIC-FUNCTION("getAttribute",hToolbarObject,"customfilterwindow").
  IF cCurrFilter = "" THEN
    cCurrFilter  = DYNAMIC-FUNCTION("getAttribute",SESSION,"filterwindow").
  IF cCurrFilter = "" THEN
    cCurrFilter = "JBoxDynFilter.w".

  cmbDialogType:SCREEN-VALUE = cCurrFilter NO-ERROR.

  IF NOT DYNAMIC-FUNCTION("getIsUserSettingInstalled") THEN
    tbPermanent:HIDDEN =  YES.

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

END.

LocalTranslation().

SUBSCRIBE TO "InitStringTranslation" ANYWHERE.
  
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

APPLY "entry" TO cmbDialogType.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAM ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR NO-UNDO.

IF NOT CAN-DO(iocTypeList,"type-combo") THEN
  iocTypeList = iocTypeList + ",TYPE-COMBO".

cmbDialogType:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TYPE-COMBO",cmbDialogType:LIST-ITEM-PAIRS).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("Scandinavian") THEN
  ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:TITLE    = "Select dialog type for filter"
         btnCancel:LABEL IN FRAME {&FRAME-NAME} = "Cancel"
         rsCurrentFilter:RADIO-BUTTONS = "This filter,1,All filters,2".

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

