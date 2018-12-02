&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hServer    AS HANDLE     NO-UNDO.
DEFINE VARIABLE cConnect   AS CHARACTER INIT "-S xoverforbong" NO-UNDO.

DEFINE VARIABLE iMinuter   AS INTEGER  INIT 1   NO-UNDO. /* minuter */
DEFINE VARIABLE iTick      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iHPix         AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWPix         AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE tt_msg NO-UNDO
    FIELD radnr AS INTE
    FIELD txt   AS CHAR FORMAT "x(30)" LABEL "Status behandling (sista 10)"
    INDEX radnr IS PRIMARY UNIQUE radnr DESCENDING.
DEFINE BUFFER buftt_msg FOR tt_msg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Bmsg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_msg

/* Definitions for BROWSE Bmsg                                          */
&Scoped-define FIELDS-IN-QUERY-Bmsg tt_msg.txt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bmsg   
&Scoped-define SELF-NAME Bmsg
&Scoped-define QUERY-STRING-Bmsg FOR EACH tt_msg
&Scoped-define OPEN-QUERY-Bmsg OPEN QUERY {&SELF-NAME} FOR EACH tt_msg.
&Scoped-define TABLES-IN-QUERY-Bmsg tt_msg
&Scoped-define FIRST-TABLE-IN-QUERY-Bmsg tt_msg


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Bmsg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-StartStop IMAGE-1 Bmsg 
&Scoped-Define DISPLAYED-OBJECTS FI-Status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD datafinns C-Win 
FUNCTION datafinns RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD socketconnect C-Win 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startup C-Win 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE PSTimer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPSTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-StartStop  NO-FOCUS
     LABEL "Start/Stopp server" 
     SIZE 25 BY 1.14.

DEFINE VARIABLE FI-Status AS CHARACTER FORMAT "X(256)":U INITIAL "Initiering pågår ..." 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icon/redlight.jpg":U
     SIZE 32 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bmsg FOR 
      tt_msg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bmsg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bmsg C-Win _FREEFORM
  QUERY Bmsg DISPLAY
      tt_msg.txt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 9.05 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-StartStop AT ROW 14.33 COL 26
     Bmsg AT ROW 4.81 COL 1.6
     FI-Status AT ROW 15.76 COL 1.2 NO-LABEL
     IMAGE-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 51.2 BY 15.76.


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
         TITLE              = "Transaksjons-/Statistikkoppdatering"
         HEIGHT             = 15.76
         WIDTH              = 51.2
         MAX-HEIGHT         = 21.24
         MAX-WIDTH          = 76.2
         VIRTUAL-HEIGHT     = 21.24
         VIRTUAL-WIDTH      = 76.2
         MAX-BUTTON         = no
         RESIZE             = no
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB Bmsg IMAGE-1 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Status IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bmsg
/* Query rebuild information for BROWSE Bmsg
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_msg.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Bmsg */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME PSTimer ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.48
       COLUMN          = 37
       HEIGHT          = 2.86
       WIDTH           = 13
       HIDDEN          = yes
       SENSITIVE       = yes.
/* PSTimer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Transaksjons-/Statistikkoppdatering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transaksjons-/Statistikkoppdatering */
DO:
  /* This event will close the window and terminate the procedure.  */
    IF chPSTimer:ENABLED = TRUE THEN DO:
        MESSAGE "Stoppe serveren för att kunna stänga fönstret."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartStop C-Win
ON CHOOSE OF B-StartStop IN FRAME DEFAULT-FRAME /* Start/Stopp server */
DO:
    RUN StartStopServer(NOT chPSTimer:ENABLED).
/*     DO WITH FRAME {&FRAME-NAME}:                                */
/*         ASSIGN FI-Oppstart:SCREEN-VALUE  = FI-Oppstart          */
/*                FI-Intervall:SCREEN-VALUE = STRING(FI-Intervall) */
/*                FI-FirstTid:SCREEN-VALUE  = FI-FirstTid          */
/*                FI-LastTid:SCREEN-VALUE   = FI-LastTid           */
/*                .                                                */
/*     END.                                                        */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 C-Win
ON MOUSE-SELECT-DBLCLICK OF IMAGE-1 IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = IF SELF:PRIVATE-DATA = "MINI" THEN iWPix ELSE 160
     {&WINDOW-NAME}:HEIGHT-PIXELS = IF SELF:PRIVATE-DATA = "MINI" THEN iHPix ELSE  72
     SELF:PRIVATE-DATA = STRING(SELF:PRIVATE-DATA = "MINI","STOR/MINI").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PSTimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PSTimer C-Win OCX.Tick
PROCEDURE PSTimer.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

    RUN Behandladata.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bmsg
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(chPSTimer) THEN
        RELEASE OBJECT chPSTimer   NO-ERROR.
    IF VALID-HANDLE(PSTimer) THEN
        DELETE OBJECT PSTimer   NO-ERROR.
    ASSIGN chPSTimer  = ?.
    RUN disable_UI.
    QUIT.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN initTT.
  RUN enable_UI.

  B-StartStop:SENSITIVE = FALSE.
  DO WITH FRAME {&FRAME-NAME}:
      IF socketconnect() = TRUE THEN DO WITH FRAME {&FRAME-NAME}:
          FI-Status:SCREEN-VALUE = "Programmet kjører allerede (kontroller)".
      END.
      ELSE DO:
          startup().
          datafinns().
          B-StartStop:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
          IF CAN-DO(SESSION:PARAMETER,"AUTOSTART") THEN DO:
              ASSIGN chPSTimer:ENABLED  = TRUE.
              IMAGE-1:LOAD-IMAGE(".\icon\greenlight.jpg").
          END.
      END.
  END.
  ASSIGN iWPix = {&WINDOW-NAME}:WIDTH-PIXELS 
         iHPix = {&WINDOW-NAME}:HEIGHT-PIXELS.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Behandladata C-Win 
PROCEDURE Behandladata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF datafinns() THEN DO WITH FRAME {&FRAME-NAME}:
        IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg") NO-ERROR.
        ASSIGN chPSTimer:ENABLED = FALSE.
        RUN batchxoverforbong.p.
        ASSIGN chPSTimer:ENABLED = TRUE.
        IMAGE-1:LOAD-IMAGE(IF NOT chPSTimer:ENABLED THEN ".\icon\redlight.jpg" ELSE ".\icon\greenlight.jpg").
    END.
    datafinns().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectHandler C-Win 
PROCEDURE ConnectHandler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip_hSocket AS HANDLE NO-UNDO. /* handle to the client socket that just connected */
 ip_hSocket:SET-READ-RESPONSE-PROCEDURE("ReadMessage":U,THIS-PROCEDURE).
RETURN. /* all done */

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

OCXFile = SEARCH( "wbatchoverforbong.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chPSTimer = PSTimer:COM-HANDLE
    UIB_S = chPSTimer:LoadControls( OCXFile, "PSTimer":U)
    PSTimer:NAME = "PSTimer":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wbatchoverforbong.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY FI-Status 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-StartStop IMAGE-1 Bmsg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Filloggmsg C-Win 
PROCEDURE Filloggmsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cMsg AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
   DO ii = 1 TO 10:
       FIND tt_msg WHERE tt_msg.radnr = ii.
       IF ii < 10 THEN DO:
           FIND buftt_msg WHERE buftt_msg.radnr = ii + 1. 
           ASSIGN tt_msg.txt = buftt_msg.txt.
       END.
       ELSE
           tt_msg.txt = cMsg.
   END.
   {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chPSTimer = chPSTimer:PSTimer
           chPSTimer:ENABLED  = FALSE
           chPSTimer:INTERVAL = iMinuter * 60 * 1000
           .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTT C-Win 
PROCEDURE initTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DO ii = 1 TO 10:
        CREATE tt_msg.
        ASSIGN tt_msg.radnr = ii.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NesteTick C-Win 
PROCEDURE NesteTick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadMessage C-Win 
PROCEDURE ReadMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartStopServer C-Win 
PROCEDURE StartStopServer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lStart AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iSek   AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF lStart = TRUE THEN DO:
        RUN Behandladata.
        ASSIGN chPSTimer:ENABLED = TRUE.
    END.
    ELSE DO:
        ASSIGN chPSTimer:ENABLED = FALSE.
    END.
  END.
  IMAGE-1:LOAD-IMAGE(IF NOT chPSTimer:ENABLED THEN ".\icon\redlight.jpg" ELSE ".\icon\greenlight.jpg").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION datafinns C-Win 
FUNCTION datafinns RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lDataFinns AS LOGICAL     NO-UNDO.
  FOR FIRST Filer NO-LOCK WHERE Filer.Innlest   = TRUE AND 
                                Filer.Oppdatert = TRUE AND               
                                Filer.Overfort  = FALSE:
      lDataFinns = TRUE.
  END.
  FI-Status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(lDataFinns,"Det finnes data for behandling/").
  RETURN lDataFinns.
     /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION socketconnect C-Win 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 CREATE SOCKET hServer NO-ERROR.
/*  hServer:CONNECT("-H ken1lap3 -S 9101":U). */
 hServer:CONNECT(cConnect) NO-ERROR.
 IF hServer:CONNECTED() THEN DO:
     /* Detta är ett serverprogram */
     /* Om vi får kontakt så måste vi avsluta. Vi kan inte starta 2 instanser */
     hServer:DISCONNECT() NO-ERROR.
     DELETE OBJECT hServer NO-ERROR.
     RETURN TRUE.
 END.
 ELSE
     RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startup C-Win 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lOkEnable AS LOGICAL    NO-UNDO.
 CREATE SERVER-SOCKET hServer. /* create the server socket and store the handle in hServer */

 /* ensure that the server can received connection requests. In this example, the server
    is using the localmachine and a service of 1234. Obviously the host and service
    can be replaced with any machine or service that you need */
 ASSIGN lOkEnable = hServer:ENABLE-CONNECTIONS(cConnect) NO-ERROR.

 IF lOkEnable THEN DO:
     /* when a client connects to this server, run the procedure "ConnectHandler" in THIS-PROCEDURE */
     hServer:SET-CONNECT-PROCEDURE("ConnectHandler":U,THIS-PROCEDURE).
 END.
 ELSE RETURN FALSE.
 RETURN TRUE. /* all's ok !*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

