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

/* USING DevExpress.* FROM ASSEMBLY. */
USING uc.* FROM PROPATH.
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
&SCOPED-DEFINE AdvGuiWin 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR iCl         AS INT    NO-UNDO.

DEFINE VARIABLE cBrukerId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGruppeNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKasseNr  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDato     AS DATE        NO-UNDO.
DEFINE VARIABLE iCnt AS INT NO-UNDO.
DEFINE VARIABLE hBrowser  AS osk.OSKGrid  EXTENT   NO-UNDO.
DEFINE VARIABLE hDataQuery AS HANDLE  EXTENT  NO-UNDO. 

DEFINE VARIABLE hDataSet AS HANDLE NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHAR INIT "-URL http://appfarm.netextend.no/aia/Aia?AppService=ASTest" NO-UNDO.
DEFINE VARIABLE lOK AS LOG NO-UNDO. 
DEFINE VARIABLE cDirection AS CHAR INIT "C" NO-UNDO. 
DEFINE VARIABLE cEnabledFolders AS CHARACTER   NO-UNDO.

DEF VAR oUltraTabFolder AS uc.JBoxUltraTabFolder NO-UNDO.
/* DEF VAR oDevExDate      AS JBoxDevExDateEdit NO-UNDO. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBackward UltraTabFolder btnButikkSok ~
btnfi-Date btnCurrent btnForward 
&Scoped-Define DISPLAYED-OBJECTS fi-Date FI-butikkNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumFolders C-Win 
FUNCTION getNumFolders RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBackward  NO-FOCUS
     LABEL "<" 
     SIZE 6 BY 1.

DEFINE BUTTON btnButikkSok 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCurrent  NO-FOCUS
     LABEL "Idag" 
     SIZE 9 BY 1.

DEFINE BUTTON btnfi-Date 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnForward  NO-FOCUS
     LABEL ">" 
     SIZE 6 BY 1.

DEFINE VARIABLE FI-butikkNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Date AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 73.2 BY 1
     FONT 8 NO-UNDO.

DEFINE RECTANGLE UltraTabFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 206 BY 30.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnBackward AT ROW 1.48 COL 39.4 WIDGET-ID 10
     fi-Date AT ROW 1.48 COL 3 NO-LABEL
     FILL-IN-2 AT ROW 1.48 COL 59 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-butikkNr AT ROW 1.48 COL 140.6 COLON-ALIGNED
     btnButikkSok AT ROW 1.48 COL 165.8 WIDGET-ID 18 NO-TAB-STOP 
     btnfi-Date AT ROW 1.48 COL 30.2 WIDGET-ID 20 NO-TAB-STOP 
     btnCurrent AT ROW 1.48 COL 51.4 WIDGET-ID 14
     btnForward AT ROW 1.48 COL 45.4 WIDGET-ID 12
     UltraTabFolder AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 207 BY 32.57.


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
         TITLE              = "On-Line kassarapport"
         HEIGHT             = 32.57
         WIDTH              = 207
         MAX-HEIGHT         = 32.57
         MAX-WIDTH          = 207
         VIRTUAL-HEIGHT     = 32.57
         VIRTUAL-WIDTH      = 207
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
/* SETTINGS FOR FILL-IN FI-butikkNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-Date IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* On-Line kassarapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* On-Line kassarapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBackward
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBackward C-Win
ON CHOOSE OF btnBackward IN FRAME DEFAULT-FRAME /* < */
DO:

   fi-date = fi-date - 1. 
   fi-date:SCREEN-VALUE = string(fi-date). 
   iButikknr = INT(FI-ButikkNr:SCREEN-VALUE  IN FRAME DEFAULT-FRAME).

   cDirection = "P". /* Previous */ 
   RUN getAppserverData(OUTPUT lok). 
   RUN RefreshQuery.
   APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikkSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikkSok C-Win
ON CHOOSE OF btnButikkSok IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEF VAR cReturnValues   AS CHAR NO-UNDO.

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Butiker;Butik;KortNavn;ButNamn"
                     ,"WHERE TRUE"
                      ,""                                                  
                      ,"Butik",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF bOk AND cReturnValues NE "" THEN DO:
      ASSIGN 
          FI-ButikkNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ENTRY(1,cReturnValues,"|")
          iButikkNr = INTEGER(FI-ButikkNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME).
      cDirection = "C". /* Next */ 
      RUN getAppserverData(OUTPUT lok). 
      RUN RefreshQuery.
    END.
    ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCurrent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCurrent C-Win
ON CHOOSE OF btnCurrent IN FRAME DEFAULT-FRAME /* Idag */
DO:
  fi-date = TODAY. 
  fi-date:SCREEN-VALUE = string(fi-date).
  iButikknr = INT(FI-ButikkNr:SCREEN-VALUE  IN FRAME DEFAULT-FRAME).
  
  cDirection = "C". /* Next */ 
  RUN getAppserverData(OUTPUT lok). 
  RUN RefreshQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfi-Date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfi-Date C-Win
ON CHOOSE OF btnfi-Date IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN dCal.w (fi-Date:HANDLE).

  fi-date = DATE(FI-Date:SCREEN-VALUE).

  cDirection = "P". /* Previous */ 
  RUN getAppserverData(OUTPUT lok). 
  RUN RefreshQuery.
  APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnForward
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnForward C-Win
ON CHOOSE OF btnForward IN FRAME DEFAULT-FRAME /* > */
DO:
  fi-date = fi-date + 1. 
  fi-date:SCREEN-VALUE = string(fi-date).
  iButikknr = INT(FI-ButikkNr:SCREEN-VALUE  IN FRAME DEFAULT-FRAME).

  cDirection = "N". /* Next */ 
  RUN getAppserverData(OUTPUT lok). 
  RUN RefreshQuery.
  APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-Date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-Date C-Win
ON MOUSE-SELECT-CLICK OF fi-Date IN FRAME DEFAULT-FRAME
DO:
  MESSAGE 'test-1'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  DEFINE VARIABLE dDato AS DATE        NO-UNDO.
  DEFINE VARIABLE cRetValue AS CHARACTER   NO-UNDO.
  
  dDato = fi-Date.
  RUN w-kalender.w (INPUT-OUTPUT dDato, "").
  cRetValue = RETURN-VALUE.
  DYNAMIC-FUNCTION('SetStatus', 'popupform',TRUE)   NO-ERROR.
  RUN __sethdlPopUpFrameProc (THIS-PROCEDURE) NO-ERROR.
  RUN ResetMenu('CONFIRMMENU_ONLINE').

MESSAGE 'test-2'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF cRetValue = "OK" THEN DO:
      /* sätter datumet till en dag tillbaka för att använda knappen som har resten av logiken */
      fi-date = dDato - 1.
      APPLY "CHOOSE" TO btnForward.
      RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-Date C-Win
ON VALUE-CHANGED OF fi-Date IN FRAME DEFAULT-FRAME
DO:
        /* sätter datumet till en dag tillbaka för att använda knappen som har resten av logiken */
      fi-date = DATE(FI-Date:SCREEN-VALUE) - 1.
      APPLY "CHOOSE" TO btnForward.
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
  DISPLAY fi-Date FI-butikkNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnBackward UltraTabFolder btnButikkSok btnfi-Date btnCurrent 
         btnForward 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAppserverData C-Win 
PROCEDURE GetAppserverData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER lConnected AS LOGICAL NO-UNDO. 
DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE cLayout AS CHAR NO-UNDO. 
DEFINE VARIABLE cPingHost AS CHARACTER   NO-UNDO.
DEF VAR cReturnParam    AS CHAR NO-UNDO.

  SESSION:SET-WAIT-STATE("general").
  RUN onlinedatads.p (iButikknr,
                      cDirection,
                      cBrukerId,
                      INPUT-OUTPUT fi-date,
                      OUTPUT cEnabledFolders, 
                      OUTPUT DATASET-HANDLE hDataSet
                      ).
/*   MESSAGE 'ibutikkNr' iButikkNr SKIP             */
/*           'cDir' cDirection SKIP                 */
/*           'cBrukerid' cBrukerId SKIP             */
/*           fi-Date SKIP                           */
/*           'cEnabledFolders' cEnabledFolders SKIP */
/*           VALID-HANDLE(hDataSet)                 */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
  lConnected = YES.
  SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQueryHandles C-Win 
PROCEDURE GetQueryHandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hDataSet AS HANDLE NO-UNDO. 
DEFINE OUTPUT PARAMETER hDataQuery AS HANDLE EXTENT NO-UNDO. 

DEFINE VARIABLE iNumBuffers AS INT NO-UNDO. 
DEFINE VARIABLE cTableName AS CHAR NO-UNDO. 
DEFINE VARIABLE cTableLabel AS CHAR NO-UNDO. 
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. 


    EXTENT(hDataQuery) = hDataSet:NUM-BUFFERS. 
    
    DO iNumBuffers = 1 TO EXTENT(hDataQuery):
        ASSIGN 
            cTableName  = hDataSet:GET-BUFFER-HANDLE(iNumBuffers):NAME
            cTableLabel = hDataSet:GET-BUFFER-HANDLE(iNumBuffers):SERIALIZE-NAME 
            hBuffer     = hDataSet:GET-BUFFER-HANDLE(iNumBuffers) NO-ERROR. 

        IF NOT ERROR-STATUS:ERROR THEN
        DO:
            CREATE QUERY hDataQuery[iNumBuffers]. 
            hDataQuery[iNumBuffers]:SET-BUFFERS   (hDataSet:GET-BUFFER-HANDLE(iNumBuffers)).
            hDataQuery[iNumBuffers]:QUERY-PREPARE ("for each " + cTableName).
            hDataQuery[iNumBuffers]:PRIVATE-DATA = IF cTableLabel NE ? THEN cTableLabel ELSE cTableName. 
        END. 
    END.

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
  DEFINE VARIABLE cFolderTabLabels AS CHAR NO-UNDO. 
  DEFINE VARIABLE cFolderTabLabelsTMP AS CHAR NO-UNDO. 
  DEF VAR cLang  AS CHAR NO-UNDO.

{syspara.i 5 1 1 iCl INT}

RUN enable_UI.

/* DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?). */

DO WITH FRAME {&FRAME-NAME}:

  cBrukerId = USERID('Skotex').
  FIND Bruker NO-LOCK WHERE 
      Bruker.BrukerId = cBrukerId NO-ERROR.
  IF AVAILABLE Bruker 
      AND Bruker.ButikkNr > 0 
      AND CAN-FIND(Butiker WHERE Butiker.Butik = Bruker.ButikkNr) 
    THEN
      iButikkNr = Bruker.butikkNr.
   
  IF iButikkNr = 0 THEN iButikknr = iCl.

  cConnectionString  = "standard". /* getParameter (100, 13, 16). */
  
  fi-date = TODAY. 
  RUN getAppserverData(OUTPUT lok). 
  IF NOT lok THEN RETURN "NOCONN".

/*   oDevExDate = NEW JBoxDevExDateEdit(THIS-PROCEDURE,fi-Date:HANDLE). */
/*   oDevExDate:RegisterWithJukeBox(YES).                               */

  cFolderTabLabelsTMP = "Avdeling,Hovedgrupper,Varegrupper,Leverandør,Selgere,Timesalg,Varesalg,Butikker,Budsjett D/U,Budsjett M/Å".

  cLang = DYNAMIC-FUNCTION('LANGUAGE') NO-ERROR.
  IF ERROR-STATUS:ERROR OR cLang = ? THEN cLang = "NO".
  IF cLang <> "NO" THEN
      cFolderTabLabelsTMP = "Avdelning,Huvudgrupper,Varugrupper,Leverantör,Säljare,Timmefsg,Varufsg,Butiker,Budget D/V,Budget M/Å".
  DO iCnt = 1 TO NUM-ENTRIES(cEnabledFolders):
      IF ENTRY(iCnt,cEnabledFolders) = "1" THEN
          cFolderTabLabels = cFolderTabLabels + (IF cFolderTabLabels <> "" THEN "," ELSE "") + ENTRY(iCnt,cFolderTabLabelsTMP).
  END.
  RUN GetQueryHandles (INPUT hDataSet, OUTPUT hDataQuery). 
  EXTENT(hBrowser) = EXTENT(hDataQuery).
/*   DEFINE VARIABLE OSKTabFolder  AS osk.OSKTabFolder.       */
/*   OSKTabFolder = NEW osk.OSKTabFolder(EXTENT(hDataQuery)). */
/*   OSKTabFolder:hCall = THIS-PROCEDURE.                     */
/*   MESSAGE "1"                                              */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                   */
  MESSAGE "1" EXTENT(hBrowser)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  oUltraTabFolder = NEW uc.JBoxUltraTabFolder(THIS-PROCEDURE,UltraTabFolder:HANDLE).
  MESSAGE "2"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  oUltraTabFolder:RegisterWithJukeBox(YES). /* For communication with JBox objects, resize etc. YES: Visible */
  MESSAGE "3"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  oUltraTabFolder:hCall = THIS-PROCEDURE.
  MESSAGE "4"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  oUltraTabFolder:giInitNumFolders = EXTENT(hDataQuery).

  MESSAGE EXTENT(hDataQuery)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  DO iCnt = 1 TO EXTENT(hDataQuery):
      hBrowser[iCnt] = NEW osk.OSKGrid(hDataQuery[iCnt]).            
      hBrowser[iCnt]:toplevel = FALSE.
      hBrowser[iCnt]:FormBorderStyle = System.Windows.Forms.FormBorderStyle:NONE.
      hBrowser[iCnt]:Dock = System.Windows.Forms.DockStyle:BOTTOM.
      hBrowser[iCnt]:bringToFront(). 
      hBrowser[iCnt]:PerformAutoResizeColumns().
      hBrowser[iCnt]:ResizeAllColumns().
      hBrowser[iCnt]:show(). 
/*       OSKTabFolder:AddObjectToPage(hBrowser[iCnt],iCnt,ENTRY(iCnt,cFolderTabLabels)). */
      oUltraTabFolder:AddObjectToPage(hBrowser[iCnt],iCnt,ENTRY(iCnt,cFolderTabLabels)).
  END.
END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,200,0,0).

FIND LAST TransLogg NO-LOCK WHERE
      TransLogg.Butik = iButikkNr AND 
      TransLogg.Dato <= TODAY USE-INDEX ButDatoTid NO-ERROR.
IF AVAILABLE TransLogg THEN FI-Date = TransLogg.Dato.
  ELSE fi-date = TODAY. 
fi-date:SCREEN-VALUE = STRING(fi-date).
FI-ButikkNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(iButikkNr).
cDirection = "P". /* Previous */ 
RUN getAppserverData(OUTPUT lok). 
RUN RefreshQuery.

APPLY "ENTRY" TO FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshQuery C-Win 
PROCEDURE RefreshQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO iCnt = 1 TO EXTENT(hDataQuery):
    hBrowser[iCnt]:RefreshQuery().            
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TextChangedDateEdit C-Win 
PROCEDURE TextChangedDateEdit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cCurrDateFormat AS CHAR NO-UNDO.
/*
  cCurrDateFormat = SESSION:DATE-FORMAT.
  SESSION:DATE-FORMAT = "dmy".
  fi-Date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = oDevExDate:dateEdit1:TEXT NO-ERROR. 
  ASSIGN fi-date.
  SESSION:DATE-FORMAT = cCurrDateFormat.
  DISPLAY fi-date. /* For å aktivere datoformat hvis det er byttet. */

  fi-date = DATE(FI-Date:SCREEN-VALUE) - 1.
  MESSAGE 
      fi-date
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  APPLY "CHOOSE" TO btnForward.
  RETURN NO-APPLY.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedRecord C-Win 
PROCEDURE ValueChangedRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN SUPER.

  MESSAGE 'Gurre'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumFolders C-Win 
FUNCTION getNumFolders RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: UltraTabFolder:InitializeComponent will ask for this value
    Notes:  
------------------------------------------------------------------------------*/

  RETURN EXTENT(hDataQuery).   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

