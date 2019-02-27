&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

def var wBAtchHandle as handle no-undo.
DEF VAR cTekst       AS CHAR   NO-UNDO.

DEFINE VARIABLE hServer AS HANDLE     NO-UNDO.
DEFINE VARIABLE cConnect AS CHARACTER INIT "-S batch" NO-UNDO.

DEFINE VARIABLE lIconExist      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cGreenIcon      AS CHARACTER INIT ".\icon\bullet_green.ico"  NO-UNDO.
DEFINE VARIABLE cYellowIcon     AS CHARACTER INIT ".\icon\bullet_yellow.ico"    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS FI-Batch FI-Tekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD socketconnect wWin 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startup wWin 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Batch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 101 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Statusmelding fra batchserver" 
      VIEW-AS TEXT 
     SIZE 101 BY .62
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Batch AT ROW 2.19 COL 2.6 NO-LABEL
     FI-Tekst AT ROW 1.48 COL 2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.8 BY 3.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Batch - oppdaterer lagertransaskjoner"
         HEIGHT             = 3.19
         WIDTH              = 104.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-Batch IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Tekst IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Batch - oppdaterer lagertransaskjoner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Batch - oppdaterer lagertransaskjoner */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
  IF socketconnect() = TRUE THEN DO:
      QUIT.
  END.
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectHandler wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cTekst AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      cTekst = PROGRAM-NAME(1) + "," + 
      PROGRAM-NAME(2) + "," + 
      PROGRAM-NAME(3) + "," + 
      PROGRAM-NAME(4) + "," + 
      PROGRAM-NAME(5) + "," + 
      PROGRAM-NAME(6) + "," + 
      PROGRAM-NAME(7) + "," + 
      PROGRAM-NAME(8) + "," + 
      PROGRAM-NAME(9)
      .
  run StoppBatch.
  IF cTekst  MATCHES "*w-modul*" THEN. /* Gjør ingenting */
  ELSE QUIT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY FI-Batch FI-Tekst 
      WITH FRAME fMain IN WINDOW wWin.
  VIEW FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/
    hServer:DISABLE-CONNECTIONS() NO-ERROR.
    DELETE OBJECT hServer NO-ERROR.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/*   IF socketconnect() = TRUE THEN DO:                    */
/*       MESSAGE "Programmet kjører allerede (kontroller)" */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*       APPLY "CLOSE" TO THIS-PROCEDURE.                  */
/*       RETURN.                                           */
/*   END.                                                  */
  IF SEARCH(cGreenIcon) <> ? AND SEARCH(cYellowIcon) <> ? THEN
      lIconExist = TRUE.

  RUN SUPER.
  RUN VisIcon (1).
  startup().
  /* Code placed here will execute AFTER standard behavior.    */
  RUN StartBatch ("bt-2oppdlagertrans.p", "BATCH", output wBatchHandle).      


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Melding1 wWin 
PROCEDURE Melding1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wTekst as char no-undo.
  
  display
    wTekst @ FI-Batch
  with frame fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadMessage wWin 
PROCEDURE ReadMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR lv_memData       AS MEMPTR NO-UNDO. /* storage area for the message */                                                     */
/*  DEF VAR lv_memMessage AS MEMPTR NO-UNDO. /* storage area for the message */                                                        */
/*  DEFINE VARIABLE cPostnr AS CHARACTER  NO-UNDO.                                                                                     */
/*  DEFINE VARIABLE lv_cData AS CHAR   NO-UNDO. /* the actual message */                                                               */
/*  DEFINE VARIABLE lv_hSocket AS HANDLE NO-UNDO. /* a handle to client sockets */                                                     */
/*  DEFINE VARIABLE iSize AS INTEGER    NO-UNDO.                                                                                       */
/*  DEF VAR lv_iMessageSize AS INT NO-UNDO.                                                                                            */
/*  DEFINE VARIABLE cSubRutin AS CHARACTER   NO-UNDO.                                                                                  */
/*  DEFINE VARIABLE cParameter AS CHARACTER   NO-UNDO.                                                                                 */
/*  DEFINE VARIABLE cReturnValue AS CHARACTER   NO-UNDO.                                                                               */
/* iSize = 4.                                                                                                                          */
/*                                                                                                                                     */
/*  IF NOT SELF:CONNECTED() THEN RETURN.                                                                                               */
/*                                                                                                                                     */
/*  SET-SIZE(lv_memData) = iSize. /* all the messages in this demo are 4 bytes long. You have to know the message size in advance */   */
/*                                                                                                                                     */
/*  SELF:READ(lv_memData,1,iSize,READ-AVAILABLE) NO-ERROR. /* read the message from the client and put it into memory */               */
/*                                                                                                                                     */
/* /*  SET-SIZE(lv_memData) = 4. /* all the messages in this demo are 4 bytes long. You have to know the message size in advance */ */ */
/* /*                                                                                                                               */ */
/* /*  SELF:READ(lv_memData,1,4,READ-AVAILABLE) NO-ERROR. /* read the message from the client and put it into memory */             */ */
/*                                                                                                                                     */
/*  IF ERROR-STATUS:ERROR THEN RETURN. /* ooops */                                                                                     */
/*                                                                                                                                     */
/*  ASSIGN lv_iMessageSize = GET-LONG(lv_memData,1).                                                                                   */
/*                                                                                                                                     */
/*  SET-SIZE(lv_memData) = 0.                                                                                                          */
/*  SET-SIZE(lv_memData) = lv_iMessageSize.                                                                                            */
/*                                                                                                                                     */
/*  SELF:READ(lv_memData,1,lv_iMessageSize,READ-AVAILABLE) NO-ERROR. /* read the message from the client and put it into memory */     */
/*                                                                                                                                     */
/*  IF ERROR-STATUS:ERROR THEN RETURN. /* ooops */                                                                                     */
/*                                                                                                                                     */
/*  ASSIGN lv_cData = GET-STRING(lv_memData,1,lv_iMessageSize). /* convert the memory address into the message string */               */
/*  ASSIGN cSubRutin  = ENTRY(1,lv_cData,CHR(1))                                                                                       */
/*         cParameter = ENTRY(3,lv_cData,CHR(1)).                                                                                      */
/*                                                                                                                                     */
/*  RUN VALUE(cSubRutin) (cParameter, OUTPUT cReturnValue) NO-ERROR.                                                                   */
/*                                                                                                                                     */
/*  IF ENTRY(1,cReturnValue) = "FEL" THEN                                                                                              */
/*      ASSIGN ENTRY(2,lv_cData,CHR(1)) = "FEL"                                                                                        */
/*             ENTRY(3,lv_cData,CHR(1)) = ENTRY(2,cReturnValue).                                                                       */
/*  ELSE                                                                                                                               */
/*      ASSIGN ENTRY(3,lv_cData,CHR(1)) = cReturnvalue.                                                                                */
/*  SET-SIZE(lv_memMessage) = 4 + LENGTH(lv_cData).                                                                                    */
/*  PUT-LONG(lv_memMessage,1) = LENGTH(lv_cData).                                                                                      */
/*  PUT-STRING(lv_memMessage,5,LENGTH(lv_cData)) = lv_cData.                                                                           */
/*  ASSIGN lv_hSocket = SESSION:FIRST-SOCKET. /* get the first connected client socket. */                                             */
/*  DO WHILE VALID-HANDLE(lv_hSocket): /* loop through all connected client sockets */                                                 */
/*   IF lv_hSocket:CONNECTED()                                                                                                         */
/*      THEN lv_hSocket:WRITE(lv_memMessage,1,GET-SIZE(lv_memMessage)). /* write the received message to the client */                 */
/*   ASSIGN lv_hSocket = lv_hSocket:NEXT-SIBLING. /* move on to the next connected client socket */                                    */
/*  END. /* do while */                                                                                                                */
/*  SET-SIZE(lv_memData) = 0. /* always do this - set memptr size to zero, otherwise you start eating memory ... */                    */
/*  SET-SIZE(lv_memMessage) = 0.                                                                                                       */

 RETURN. /* all's done */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartBatch wWin 
PROCEDURE StartBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wProcName AS CHAR   NO-UNDO.
  DEF INPUT  PARAMETER wProdID   AS CHAR   NO-UNDO.
  DEF OUTPUT parameter wHandle   AS HANDLE NO-UNDO.

  ASSIGN wHandle = SESSION:FIRST-PROCEDURE.

  DO WHILE VALID-HANDLE(wHandle):
     IF wHandle:FILE-NAME EQ wProcName AND
          wHandle:PRIVATE-DATA EQ wProdID THEN LEAVE.

     ASSIGN wHandle = wHandle:NEXT-SIBLING.
  END.

  IF NOT VALID-HANDLE(wHandle) THEN
  DO:
    RUN VALUE(wProcName) PERSISTENT SET wHandle (this-procedure).
    ASSIGN wHandle:PRIVATE-DATA = wProdID.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StoppBatch wWin 
PROCEDURE StoppBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wHandle as handle.

  ASSIGN wHandle = SESSION:FIRST-PROCEDURE.

  BLOKKEN:
  DO WHILE VALID-HANDLE(wHandle):
     IF wHandle:FILE-NAME EQ "bt-2oppdlagertrans.p" then
       leave BLOKKEN.

     ASSIGN wHandle = wHandle:NEXT-SIBLING.
  END. /* BLOKKEN */
  if valid-handle(wHandle) then delete procedure wHandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisIcon wWin 
PROCEDURE VisIcon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iTyp AS INTEGER     NO-UNDO.
   IF NOT lIconExist THEN
       RETURN.
   IF iTyp = 1 THEN /* paus */
       {&WINDOW-NAME}:LOAD-ICON(cGreenIcon) NO-ERROR.
   ELSE IF iTyp = 2 THEN /* paus */
       {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION socketconnect wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startup wWin 
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

