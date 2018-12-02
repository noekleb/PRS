DEF VAR cFilNavn AS CHAR NO-UNDO.

ASSIGN cFilNavn = ''.

RUN sendEMailButikk (cFilNavn). 
PROCEDURE sendEMailButikk:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER icFiler   AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER pcSubject AS CHARACTER NO-UNDO.

 DEFINE VARIABLE cCLMailfra AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cSubject   AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE iCL        AS INTEGER NO-UNDO.

 DEFINE VARIABLE lMailOK AS logi NO-UNDO.
 DEFINE VARIABLE cMessage AS CHAR NO-UNDO.
 DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cMailTo   AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cFiler    AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE ii        AS INTEGER     NO-UNDO.
 DEFINE VARIABLE cFrom     AS CHARACTER NO-UNDO.
 
 DEFINE BUFFER mailButiker FOR Butiker.

 {syspara.i 5 1 1 iCL INT}

 FIND mailButiker NO-LOCK WHERE
   mailButiker.Butik = iCL NO-ERROR.
 /* Butikken må finnes. */
 IF NOT AVAILABLE mailButiker THEN 
   RETURN.
   
 /* ePostadresse må være satt på butikken. */
 IF mailButiker.ePostAdresse = '' THEN 
   RETURN. 

 {syspara.i 50 50 1  cMailhub }
 {syspara.i 50 50 2  cDoAUTH  }
 {syspara.i 50 50 3  cAuthType}
 {syspara.i 50 50 4  cUser    }
 {syspara.i 50 50 5  cPassword}
 {syspara.i 50 50 31 cMailTo}
 {syspar2.i 50 50 31 cEmailCC }
 IF cDoAUTH = "0" THEN
     ASSIGN cDoAUTH   = "FALSE"
            cAuthType = ""
            cUser     = ""
            cPassword = "".
 ELSE
     cDoAUTH = "TRUE".
IF pcSubject <> ''
  THEN cSubject = pcSubject.
 ELSE 
   ASSIGN 
   cSubject = "Eksport kontantsalg dampbakeriet " + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS").
    
 /* Trekker ut bare filnavn */   
 DO ii = 1 TO NUM-ENTRIES(icFiler):
    cFiler = cFiler + (IF cFiler <> "" THEN "," ELSE "") + 
    ENTRY(NUM-ENTRIES(ENTRY(ii,icFiler),"\"),ENTRY(ii,icFiler),"\").
 END.


MESSAGE 'Sending av eMail' SKIP
    'cMailhub' cMailhub SKIP
    'cMailTo' cMailTo SKIP
    'mailButiker.ePostAdresse' mailButiker.ePostAdresse SKIP
    'cEmailCC' cEmailCC SKIP
    'cFiler' cFiler SKIP
    'icFiler' icFiler SKIP
    'cSubject' cSubject SKIP
    'cDoAUTH' cDoAUTH SKIP
    'cAuthType' cAuthType SKIP
    'cUser' cUser SKIP
    'cPassword' cPassword
  VIEW-AS ALERT-BOX.


/*
 RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   mailButiker.ePostAdresse,
        /*EmailCC    */   cEmailCC,
        /*Attachments*/   cFiler,
        /*LocalFiles */   icFiler,
        /*Subject    */   cSubject,
        /*Body       */   "",
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   cDoAUTH,
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
/*         IF lMailOk = FALSE THEN DO:                     */
/*             MESSAGE "Sending avbrutt med melding:" SKIP */
/*                     cMessage                            */
/*                     VIEW-AS ALERT-BOX.                  */
/*         END.                                            */

*/
END PROCEDURE.
