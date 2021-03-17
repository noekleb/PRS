&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
{oposglob.i}
/* {runlib.i} */
DEFINE VAR OPOSRetval AS INTEGER.
DEF VAR wFlagg AS CHAR NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiDevName fiDevDesc btnOpen btnClaim ~
btnClose fiReaderCount btnScan fiRetval 
&Scoped-Define DISPLAYED-OBJECTS fiDevName fiDevDesc fiReaderCount fiRetval 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE OCX AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chOCX AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClaim 
     LABEL "Claim" 
     SIZE 7 BY 1.

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 7 BY 1.

DEFINE BUTTON btnOpen 
     LABEL "Open" 
     SIZE 7 BY 1.

DEFINE BUTTON btnScan 
     LABEL "Scan" 
     SIZE 10 BY 1.

DEFINE VARIABLE fiDevDesc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .92 NO-UNDO.

DEFINE VARIABLE fiDevName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .92 NO-UNDO.

DEFINE VARIABLE fiReaderCount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fiRetval AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .92 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiDevName AT ROW 1.15 COL 15 COLON-ALIGNED
     fiDevDesc AT ROW 2.15 COL 15 COLON-ALIGNED
     btnOpen AT ROW 3.19 COL 2
     btnClaim AT ROW 3.19 COL 9
     btnClose AT ROW 3.19 COL 16
     fiReaderCount AT ROW 3.19 COL 21.86 COLON-ALIGNED NO-LABEL
     btnScan AT ROW 3.19 COL 36.57
     fiRetval AT ROW 4.35 COL 1.86 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56.4 BY 4.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "MS-Symbol-Scanner"
         COLUMN             = 60.14
         ROW                = 4.85
         HEIGHT             = 4.35
         WIDTH              = 55.14
         MAX-HEIGHT         = 40.23
         MAX-WIDTH          = 230
         VIRTUAL-HEIGHT     = 40.23
         VIRTUAL-WIDTH      = 230
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
IF NOT C-Win:LOAD-ICON("adeicon/proexp.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/proexp.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiRetval IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME OCX ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 1.38
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.
/* OCX OCXINFO:CREATE-CONTROL from: {027D61A3-4251-11D0-B7A4-80BBFFC00000} type: Scanner */
      OCX:MOVE-BEFORE(fiDevName:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* MS-Symbol-Scanner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* MS-Symbol-Scanner */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClaim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClaim C-Win
ON CHOOSE OF btnClaim IN FRAME DEFAULT-FRAME /* Claim */
DO:
    RUN Claim NO-ERROR.
    ASSIGN fiRetval:SCREEN-VALUE = STRING(OPOSRetval).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
    RUN Close NO-ERROR.
    ASSIGN fiRetval:SCREEN-VALUE = STRING(OPOSRetval).
    ASSIGN fiDevName:SCREEN-VALUE = "".
    ASSIGN fiDevDesc:SCREEN-VALUE =  "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpen C-Win
ON CHOOSE OF btnOpen IN FRAME DEFAULT-FRAME /* Open */
DO:
/*   MESSAGE 'RUN Open("PRS_STI_SCANNER") NO-ERROR.' */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
    DEFINE VARIABLE txt AS CHARACTER NO-UNDO.
    RUN Open("STI_SCANNER") NO-ERROR.
    ASSIGN fiRetval:SCREEN-VALUE = STRING(OPOSRetval).
    RUN GetDeviceName(OUTPUT txt) NO-ERROR.
    ASSIGN fiDevName:SCREEN-VALUE = txt.
    RUN GetDeviceDescription(OUTPUT txt) NO-ERROR.
    ASSIGN fiDevDesc:SCREEN-VALUE = txt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnScan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnScan C-Win
ON CHOOSE OF btnScan IN FRAME DEFAULT-FRAME /* Scan */
DO:
    /*
    IF (chOCX:Scanner:DataCount = 0) THEN
        ASSIGN fiRetval:SCREEN-VALUE ="Tomt".
    ELSE    
    DO:
        ASSIGN fiRetval:SCREEN-VALUE = (chOCX:Scanner:ScanData).    
        chOCX:Scanner:ClearInput.
    END.
    ASSIGN fiReaderCount:SCREEN-VALUE = (chOCX:Scanner:DataCount).
    chOCX:Scanner:DataEventEnabled = TRUE.
    */
    PUBLISH "STRECKKOD" (fiRetval:SCREEN-VALUE,"scanner").
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
    RUN ScanRelease IN THIS-PROCEDURE.
    RUN CLOSE IN THIS-PROCEDURE.
    IF VALID-HANDLE(chOCX) THEN
       RELEASE OBJECT chOCX NO-ERROR.
    IF VALID-HANDLE(OCX) THEN
       DELETE OBJECT OCX NO-ERROR.
    chOCX = ?.
    OCX = ?.
    RUN disable_UI.
END.

/* {genlib.i                                                                        */
/*   &NoLibCall      = "Nei"                                                        */
/*   &WindowName     = "Driver NCR-Scanner"                                         */
/*   &PreIClose      = "if valid-handle(wLibHandle) then                            */
/*                        RUN ReadMultiSale IN wLibHandle (OUTPUT wFLagg) NO-ERROR. */
/*                      IF wFLagg = 'MultiSale' then                                */
/*                      DO:                                                         */
/*                        RUN Show (0).                                             */
/*                        RETURN 'AVBRYT'.                                          */
/*                      END."                                                       */
/*   &PostIClose     = " "                                                          */
/*   &PostDisable_ui = " "                                                          */
/* }                                                                                */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN show(1).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckHealth C-Win 
PROCEDURE CheckHealth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Level AS INTEGER.
    ASSIGN OPOSRetval = chOCX:Scanner:CheckHealth(Level).
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Claim C-Win 
PROCEDURE Claim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE INPUT PARAMETER Timeout AS INTEGER. */
    ASSIGN OPOSRetval = chOCX:Scanner:ClaimDevice(5000).
    chOCX:Scanner:DeviceEnabled = TRUE.
    chOCX:Scanner:DataEventEnabled = TRUE.
    chOCX:Scanner:DecodeData = TRUE.
    chOCX:Scanner:AutoDisable = FALSE.
    chOCX:Scanner:ClearInput.
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Claimed C-Win 
PROCEDURE Claimed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER lClaimed AS INTEGER.
    ASSIGN lClaimed = chOCX:Scanner:Claimed.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearInput C-Win 
PROCEDURE ClearInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN OPOSRetval = chOCX:Scanner:ClearInput().
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearOutput C-Win 
PROCEDURE ClearOutput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN OPOSRetval = chOCX:Scanner:ClearOutput().
    RETURN STRING(OPOSRetval).
/*     ASSIGN OPOSRetval = {&OPOS_E_NOSERVICE}. */
/*     RETURN STRING(OPOSRetval). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Close C-Win 
PROCEDURE Close :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN OPOSRetval = chOCX:Scanner:CLOSE().
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "symbolscanner.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chOCX = OCX:COM-HANDLE
    UIB_S = chOCX:LoadControls( OCXFile, "OCX":U)
    OCX:NAME = "OCX":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "symbolscanner.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DirectIO C-Win 
PROCEDURE DirectIO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER COMMAND AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pData AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pString AS CHARACTER.
    ASSIGN OPOSRetval = chOCX:Scanner:DirectIO(COMMAND,pData,pString).
    RETURN STRING(OPOSRetval).
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
  RUN control_load.
  DISPLAY fiDevName fiDevDesc fiReaderCount fiRetval 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiDevName fiDevDesc btnOpen btnClaim btnClose fiReaderCount btnScan 
         fiRetval 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAutoDisable C-Win 
PROCEDURE GetAutoDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Bool AS LOGICAL.
    Bool = chOCX:Scanner:AutoDisable().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBinaryConversion C-Win 
PROCEDURE GetBinaryConversion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:BinaryConversion().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCapPowerReporting C-Win 
PROCEDURE GetCapPowerReporting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:CapPowerReporting().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCheckHealthText C-Win 
PROCEDURE GetCheckHealthText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:CheckHealthText().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetClaimed C-Win 
PROCEDURE GetClaimed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Bool AS LOGICAL.
    Bool = chOCX:Scanner:Claimed().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetControlObjectDescription C-Win 
PROCEDURE GetControlObjectDescription :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:ControlObjectDescription().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetControlObjectVersion C-Win 
PROCEDURE GetControlObjectVersion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:ControlObjectVersion().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataCount C-Win 
PROCEDURE GetDataCount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:DataCount().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataEventEnabled C-Win 
PROCEDURE GetDataEventEnabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Bool AS LOGICAL.
    Bool = chOCX:Scanner:DataEventEnabled().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDecodeData C-Win 
PROCEDURE GetDecodeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Bool AS LOGICAL.
    Bool = chOCX:Scanner:DecodeData().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDeviceDescription C-Win 
PROCEDURE GetDeviceDescription :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:DeviceDescription().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDeviceEnabled C-Win 
PROCEDURE GetDeviceEnabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Bool AS LOGICAL.
    Bool = chOCX:Scanner:DeviceEnabled().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDeviceName C-Win 
PROCEDURE GetDeviceName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:DeviceName().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFreezeEvents C-Win 
PROCEDURE GetFreezeEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Bool AS LOGICAL.
    Bool = chOCX:Scanner:FreezeEvents().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPowerNotify C-Win 
PROCEDURE GetPowerNotify :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:PowerNotify().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPowerState C-Win 
PROCEDURE GetPowerState :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:PowerState().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetResultCode C-Win 
PROCEDURE GetResultCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:ResultCode().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetResultCodeExtended C-Win 
PROCEDURE GetResultCodeExtended :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:ResultCodeExtended().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetScanData C-Win 
PROCEDURE GetScanData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:ScanData().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetScanDataLabel C-Win 
PROCEDURE GetScanDataLabel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:ScanDataLabel().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetScanDataType C-Win 
PROCEDURE GetScanDataType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:SCanDataType().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetServiceObjectDescription C-Win 
PROCEDURE GetServiceObjectDescription :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Txt AS CHARACTER.
    Txt = chOCX:Scanner:ServiceObjectDescription().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetServiceObjectVersion C-Win 
PROCEDURE GetServiceObjectVersion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:ServiceObjectVersion().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetState C-Win 
PROCEDURE GetState :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER Val AS INTEGER.
    Val = chOCX:Scanner:State().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OCX.Scanner.DataEvent C-Win 
PROCEDURE OCX.Scanner.DataEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Stat AS INTEGER.
    DEFINE VARIABLE cScanned AS CHARACTER   NO-UNDO.
/*     MESSAGE "SVAR" chOCX:Scanner:ScanDataLabel */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
    cScanned = chOCX:Scanner:ScanDataLabel.
    IF cScanned BEGINS "A" OR cScanned BEGINS "I" THEN
        cScanned = SUBSTR(cScanned,2).
    ASSIGN fiRetval:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cScanned.
    chOCX:Scanner:DataEventEnabled = TRUE.
    PUBLISH "STRECKKOD" (fiRetval:SCREEN-VALUE IN FRAME {&FRAME-NAME},"scanner").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OCX.Scanner.DirectIOEvent C-Win 
PROCEDURE OCX.Scanner.DirectIOEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER EventNumber AS INTEGER.
    DEFINE INPUT PARAMETER Data AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pString AS CHARACTER.
    /* DO SOMETHING!!!*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OCX.Scanner.ErrorEvent C-Win 
PROCEDURE OCX.Scanner.ErrorEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ResultCode AS INTEGER.
    DEFINE INPUT PARAMETER ResultCodeExtended AS INTEGER.
    DEFINE INPUT PARAMETER ErrorLocus AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pErrorResponse AS INTEGER.
    /* DO SOMETHING!!!*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OCX.Scanner.StatusUpdateEvent C-Win 
PROCEDURE OCX.Scanner.StatusUpdateEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Data AS INTEGER.
    /* DO SOMETHING!!!*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open C-Win 
PROCEDURE Open :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DevName AS CHARACTER.
    ASSIGN OPOSRetval = chOCX:Scanner:OPEN(DevName).
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanRelease C-Win 
PROCEDURE ScanRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN OPOSRetval = chOCX:Scanner:RELEASE().
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAutoDisable C-Win 
PROCEDURE SetAutoDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Bool AS LOGICAL.
    chOCX:Scanner:AutoDisable = Bool.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBinaryConversion C-Win 
PROCEDURE SetBinaryConversion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Val AS INTEGER.
    chOCX:Scanner:BinaryConversion = Val.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDataEventEnabled C-Win 
PROCEDURE SetDataEventEnabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Bool AS LOGICAL.
    chOCX:Scanner:DataEventEnabled = Bool.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDecodeData C-Win 
PROCEDURE SetDecodeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Bool AS LOGICAL.
    chOCX:Scanner:DecodeData = Bool.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDeviceEnabled C-Win 
PROCEDURE SetDeviceEnabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Bool AS LOGICAL.
    chOCX:Scanner:DeviceEnabled = Bool.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFreezeEvents C-Win 
PROCEDURE SetFreezeEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Bool AS LOGICAL.
    chOCX:Scanner:FreezeEvents = Bool.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPowerNotify C-Win 
PROCEDURE SetPowerNotify :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Val AS INTEGER.
    chOCX:Scanner:PowerNotify = Val.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Show C-Win 
PROCEDURE Show :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER Show AS INTEGER.    
    IF (Show = 0) THEN
    DO:
        ASSIGN c-win:HIDDEN = true.
        FRAME default-frame:HIDDEN = true.
    END.
    ELSE
    DO:
        ASSIGN c-win:HIDDEN = FALSE.
        FRAME default-frame:HIDDEN = FALSE.    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SOData C-Win 
PROCEDURE SOData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Stat AS INTEGER.
    ASSIGN OPOSRetval = chOCX:Scanner:SOData(Stat).
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SODirectIO C-Win 
PROCEDURE SODirectIO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER EventNumber AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pData AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pString AS CHARACTER.
    ASSIGN OPOSRetval = chOCX:Scanner:SODirectIO(EventNumber,pData,pString).
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SOError C-Win 
PROCEDURE SOError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ResultCode AS INTEGER.
    DEFINE INPUT PARAMETER ResultCodeExtended AS INTEGER.
    DEFINE INPUT PARAMETER ErrorLocus AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER pResponse AS INTEGER.
    ASSIGN OPOSRetval = chOCX:Scanner:SODirectIO(ResultCode,ResultCodeExtended,ErrorLocus,pResponse).
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SOProcessID C-Win 
PROCEDURE SOProcessID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN OPOSRetval = chOCX:Scanner:SOProcessID().
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SOStatusUpdate C-Win 
PROCEDURE SOStatusUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Data AS INTEGER.
    ASSIGN OPOSRetval = chOCX:Scanner:SOStatusUpdate(Data).
    RETURN STRING(OPOSRetval).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Start C-Win 
PROCEDURE Start :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "CHOOSE":U TO btnOpen IN FRAME  {&FRAME-NAME}.
    RETURN STRING(OPOSRetval).
/*     APPLY "CHOOSE":U TO btnClaim IN FRAME  {&FRAME-NAME}. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stop C-Win 
PROCEDURE Stop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Close NO-ERROR.
    ASSIGN fiRetval:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(OPOSRetval).
    ASSIGN fiDevName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    ASSIGN fiDevDesc:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Test C-Win 
PROCEDURE Test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    chOCX:PRINTER:PrintNormal(2, "NisseGurra" + CHR(13)).
    chOCX:PRINTER:PrintBarCode(2, "0123456789012", 104, 100, 200, -2, -13).
    chOCX:PRINTER:PrintNormal(2,CHR(10)). 
    chOCX:PRINTER:PrintBitmap(2,"C:\splash.bmp",100,-2).
    chOCX:PRINTER:PrintNormal(2,CHR(10)). 
    chOCX:PRINTER:PrintNormal(2,CHR(10)). 
    chOCX:PRINTER:PrintNormal(2,CHR(10)). 
    chOCX:PRINTER:PrintNormal(2,CHR(10)). 
    chOCX:PRINTER:PrintNormal(2,CHR(10)). 
    chOCX:PRINTER:CutPaper(100). 
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

