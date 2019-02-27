&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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

DEF VAR pv_hClientSocket AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-StartUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartUp Procedure 
FUNCTION StartUp RETURNS LOGICAL
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

SESSION:ADD-SUPER-PROCEDURE(THIS-PROCEDURE:HANDLE).

Startup().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PostMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostMessage Procedure 
PROCEDURE PostMessage :
DEF INPUT PARAMETER ip_cMessage AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ip_cChannel AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ip_cData    AS CHAR NO-UNDO.
 DEF VAR lv_memData    AS MEMPTR NO-UNDO. /* storage area for the message */
DEF VAR lv_memWeb AS MEMPTR NO-UNDO. /* storage area for the message */
DEFINE VARIABLE lv_cMessage AS CHARACTER  NO-UNDO.
 DEF VAR lv_hMessageBuffer AS MEMPTR NO-UNDO.
 
 DEF VAR lv_cWeb AS CHAR NO-UNDO.

 IF NOT pv_hClientSocket:CONNECTED() THEN RETURN.

 ASSIGN lv_cWeb = ip_cData.
 
/*  SET-SIZE(lv_hMessageBuffer) = LENGTH(lv_cMessage). */

 IF LENGTH(lv_cWeb) GT 0 THEN
 DO:
  SET-SIZE(lv_memWeb) = LENGTH(lv_cWeb).
  PUT-STRING(lv_memWeb,1,LENGTH(lv_cWeb)) = lv_cWeb.
  
  IF pv_hClientSocket:CONNECTED()
     THEN pv_hClientSocket:WRITE(lv_memWeb,1,GET-SIZE(lv_memWeb)). /* write the received message to the client */
  
/*   MESSAGE pv_hClientSocket:CONNECTED()   */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  SET-SIZE(lv_memWeb) = 0.
 END.

 READKEY PAUSE 0.
 
/*  pv_hClientSocket:DISCONNECT(). */







/*  PUT-LONG(lv_hMessageBuffer,1) = LENGTH(lv_cMessage).                     */
/*  PUT-STRING(lv_hMessageBuffer,5,LENGTH(lv_cMessage)) = lv_cMessage.       */
/*                                                                           */
/*  pv_hClientSocket:WRITE(lv_hMessageBuffer,1,GET-SIZE(lv_hMessageBuffer)). */
/*                                                                           */
/*  SET-SIZE(lv_hMessageBuffer) = 0.                                         */
 
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
 DEF VAR lv_memData AS MEMPTR NO-UNDO. /* storage area for the message */

 DEF VAR lv_cMessage AS CHAR   NO-UNDO. /* the actual message */
 DEF VAR lv_cChannel AS CHAR   NO-UNDO. /* the Channel */
 DEF VAR lv_cData    AS CHAR   NO-UNDO. /* the message data */
 
 DEF VAR lv_iMessageSize AS INT NO-UNDO.
 DEFINE VARIABLE cSocketData AS CHARACTER  NO-UNDO.

/* MESSAGE "READ"                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
 IF NOT pv_hClientSocket:CONNECTED() THEN RETURN.
IF pv_hClientSocket:GET-BYTES-AVAILABLE() EQ 0 THEN RETURN.

SET-SIZE(lv_memData) = pv_hClientSocket:GET-BYTES-AVAILABLE(). 


 pv_hClientSocket:READ(lv_memData,1,GET-SIZE(lv_memData),READ-AVAILABLE) NO-ERROR.

 IF ERROR-STATUS:ERROR THEN DO: 
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = "":U.
     SET-SIZE(lv_memData) = 0.
     RETURN. /* ooops */
 END.

 ASSIGN cSocketData = GET-STRING(lv_memData,1,GET-SIZE(lv_memData)) /* convert the memory address into the message string */
        cSocketData = TRIM(cSocketData).
/* MESSAGE LENGTH(cSocketdata) cSocketdata SKIP ASC(SUBSTR(cSocketdata,1,1)) SKIP ASC(SUBSTR(cSocketdata,2,1)) */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                      */
 SET-SIZE(lv_memData) = 0.

 IF ERROR-STATUS:ERROR THEN RETURN. /* ooops */
 
 PUBLISH "ITEM" (cSocketData).

 RETURN. /* all's done */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-StartUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartUp Procedure 
FUNCTION StartUp RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 CREATE SOCKET pv_hClientSocket NO-ERROR.

/*  pv_hClientSocket:CONNECT("-H ken1lap3 -S 9101":U). */
 pv_hClientSocket:CONNECT("-H 127.0.0.1 -S 4701":U).
 pv_hClientSocket:SET-READ-RESPONSE-PROCEDURE("ReadMessage",THIS-PROCEDURE).
 
 RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

