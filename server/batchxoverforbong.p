&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE hServer  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cConnect AS CHARACTER INIT "-S xoverforbong" NO-UNDO.
DEFINE VARIABLE h_dfiler AS HANDLE      NO-UNDO.
DEFINE VARIABLE hSource  AS HANDLE      NO-UNDO.
DEFINE BUFFER bufFiler FOR Filer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-socketconnect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD socketconnect Procedure 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startup Procedure 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* IF socketconnect() = TRUE THEN DO:                  */
/*   MESSAGE "Programmet kjører allerede (kontroller)" */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*   APPLY "CLOSE" TO THIS-PROCEDURE.                  */
/*   QUIT.                                             */
/* END.                                                */
/* startup().                                          */
RUN dfiler.w PERSISTENT SET h_dfiler.
ASSIGN hSource = SOURCE-PROCEDURE NO-ERROR.
RUN FilLoop.
/* hServer:DISABLE-CONNECTIONS() NO-ERROR. */
/* DELETE OBJECT hServer NO-ERROR.         */
IF VALID-HANDLE(h_dfiler) THEN
    DELETE PROCEDURE h_dfiler.
/* QUIT. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ConnectHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectHandler Procedure 
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

&ENDIF

&IF DEFINED(EXCLUDE-FilLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilLoop Procedure 
PROCEDURE FilLoop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntfiler AS INTEGER     NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  REPEAT:
    iAntfiler = 0.
    ii = 0.
    FOR EACH bufFiler NO-LOCK WHERE bufFiler.Innlest   = TRUE AND
                                    bufFiler.Oppdatert = TRUE AND
                                    bufFiler.Overfort  = FALSE BY bufFiler.FilId:
        iAntfiler = iAntfiler + 1.
        RUN Overfordatasett (bufFiler.filid).
        RUN SetFilOverfort IN h_dfiler (INPUT bufFiler.FilId).
    END.
    IF VALID-HANDLE(hSource) THEN
        RUN Filloggmsg IN hSource (SUBSTR(STRING(TIME,"HH:MM:SS"),1,5) + " BEHANDLADE FILER: " + STRING(iAntfiler)) NO-ERROR.
    FOR FIRST bufFiler NO-LOCK WHERE bufFiler.Innlest   = TRUE AND 
                                     bufFiler.Oppdatert = TRUE AND               
                                     bufFiler.Overfort  = FALSE:
        ii = 1.
        LEAVE.
    END.
    IF ii = 0 THEN
        LEAVE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Overfordatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Overfordatasett Procedure 
PROCEDURE Overfordatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER plFilId      AS DEC       NO-UNDO.
    DEFINE        VARIABLE  piAntLinjer  AS INTEGER   NO-UNDO.
    DEFINE        VARIABLE  pcErrorListe AS CHARACTER NO-UNDO.
    DEFINE        VARIABLE  lDatafinnes  AS LOGICAL   NO-UNDO.
    DEFINE        VARIABLE  piStart      AS INTEGER     NO-UNDO.
    DEFINE BUFFER bufDatasett FOR Datasett.
    FOR EACH DataSett NO-LOCK WHERE DataSett.FilId = plFilId AND
                                    DataSett.SettStatus >= 2 AND 
                                    DataSett.SettStatus <= 8 AND
                                    DataSett.Behandlet  >= 3 AND
                                    DataSett.Behandlet  <= 4:
/*         IF DataSett.Behandlet < 2 OR DataSett.Behandlet >= 5 THEN */
/*         DO:                                                       */
/*             NEXT OPPDATERDATASETT.                                */
/*         END.                                                      */

        /* Starter overføring av bonger. */
        ASSIGN lDatafinnes = TRUE.
        ASSIGN piStart = TIME.
        RUN xoverforbong.p (Datasett.DataSettId, 
                            THIS-PROCEDURE, /* h_Telleverk, */
                            h_dfiler, 
                            OUTPUT piAntLinjer).
        IF RETURN-VALUE <> "" THEN
        DO:
            RUN NyFilLogg IN h_dfiler (INPUT plFilId, RETURN-VALUE).
        END.
        FIND bufDatasett WHERE ROWID(bufDatasett) = ROWID(Datasett) NO-LOCK.
        IF bufDatasett.Behandlet = 5 THEN DO:
            RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                           " - Datasett overført: " + 
                               string(Datasett.DataSettId) + 
                           " Tidsbruk: " + 
                           STRING(TIME - piStart,"HH:MM:SS") + 
                           " Antall poster: " +
                           STRING(piAntLinjer) + 
                           ".").
        END.
    END.
/*     IF lDatafinnes = FALSE THEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReadMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadMessage Procedure 
PROCEDURE ReadMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Telleverk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Telleverk Procedure 
PROCEDURE Telleverk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: xoveforbong sänder telleverk till GUI-vindu
         Vi gör en dummy      
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDummy AS CHARACTER   NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-socketconnect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION socketconnect Procedure 
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

&ENDIF

&IF DEFINED(EXCLUDE-startup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startup Procedure 
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

&ENDIF

