/*===============================================================*/
/*                   Coe Press Equipment, Corp.                  */
/* smtpmail.p           Created: 03/27/01  Author: Paul C. Keary */
/*                                                 plk@cpec.com  */
/* Credits:                                                      */
/* This program was adapted from a combination of the work of    */
/* Scott Auge and distributed as part of his mailsdk             */
/* Scott had laid the groundwork for sending an email with       */
/* attachments. But his original code did not verify the servers */
/* response to make sure the data was being sent properly.       */
/*                                                               */
/* Mario Paranhos @ United Systems, Inc (770)449-9696            */
/* Mario's original work smtpmail.p was a big help in developing */
/* an understanding of how SMTP works.  His program was very     */
/* elegant in the way it deals with Sockets however Scott's work */
/* had done much more in the area of including attachments.      */
/*                                                               */
/* Disclaimer: This program is not fully tested especially with  */
/* regard to the Reading of the Server Responses, and encoding   */
/* of the MIME message body . Also the logging and server params */
/* are not completely developed.                                 */
/*===============================================================*/
/* Desc: Opens up an SMTP connection and sends an email message  */
/*       to multiple recipients.                                 */
/* SMTP Protocols taken from RFC821                              */
/* MIME Protocols taken from RFC1521                             */
/* Parameters:                                                   */
/* EmailFrom - email address of user originating the email, the  */
/*             SMTP server should require that this user is real */
/* EmailTo,CC - list of email addresses separated by semicolons  */
/*             or commas (All semicolons will be replaced by     */
/*             commas so don't include stuff like Smith, John    */
/* Attachments - Comma separated list of attachment descriptions */
/*             Format looks like:                                */
/* file[:type=<mimetype>][:charset=<charset>][:filetype=<filetype] */
/* LocalFiles  - comma separated list of filenames to the files  */
/*             described in Attachments.  The filenames in this  */
/*             parameter must either be in the Progress path or  */
/*             have the entire path specified.  In contrast to   */
/*             filenames in the Attachments are the filenames to */
/*             show for the Attachments.                         */
/* Subject     - No explaination necessary                       */
/* Body        - Actual text of the message can be whatever you  */
/*             want as long as you include the correct Header    */
/*             information in the MIMEHeader.                    */
/*             If you are just sending plaintext you can leave   */
/*             the MIMEHeader blank, this is default setting     */
/*             If you wanted to send HTML you might use:         */
/*             type=text/html:charset=us-ascii:filetype=ascii    */
/* MIMEHeader [type=<mimetype>][:charset=<chrset>][:filetype=<type>] */
/*                                                               */
/* oSuccessful - Yes the message was succesfully generated       */
/*               No there was some error that prevented message  */
/*                  from being successful generated.             */
/*               ? there may have been a problem with a recipient*/
/*                 or format of the email.                       */
/*===============================================================*/

DEF INPUT PARAMETER EmailFrom           AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailTo             AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailCC             AS CHAR NO-UNDO.
DEF INPUT PARAMETER Attachments         AS CHAR NO-UNDO.
DEF INPUT PARAMETER LocalFiles          AS CHAR NO-UNDO.
DEF INPUT PARAMETER Subject             AS CHAR NO-UNDO.
DEF INPUT PARAMETER Body                AS CHAR NO-UNDO.
DEF INPUT PARAMETER MIMEHeader          AS CHAR NO-UNDO.

DEF OUTPUT PARAMETER oSuccessful        AS LOGICAL NO-UNDO.


/* Configure These Parameters per your specific needs */
DEF VAR loglevel                        AS INTEGER NO-UNDO.
DEF VAR LogFile                         AS CHARACTER NO-UNDO.
DEF VAR EncodeDirectory                 AS CHARACTER NO-UNDO.
DEF VAR Mailhub                         AS CHARACTER NO-UNDO.
DEF VAR timezone                        AS CHARACTER NO-UNDO.

/****************************************************************/
/* Only Log attachments if debug level = 1                      */
/****************************************************************/
loglevel = 3.  /* Minimal logging */
EncodeDirectory = SESSION:TEMP-DIR.
LogFile = SESSION:TEMP-DIR + "socketemail.log".
Mailhub = "coeexch".

/* Make sure EncodeDirectory ends in a /  */
IF SUBSTRING(EncodeDirectory,LENGTH(EncodeDirectory),1) <> "/" THEN
  EncodeDirectory = EncodeDirectory + "/".

/* Determine which timezone we are in so that the Mail will have the
   correct SENT Time */
&IF OPSYS = "UNIX" 
&THEN 
  INPUT THROUGH date +%Z NO-ECHO.
  SET timezone.
  INPUT CLOSE.
&ELSE timezone = "EST".
&ENDIF


DEF VAR cLocalFile                      AS CHARACTER NO-UNDO.
DEF VAR cBinaryFile                     AS CHARACTER NO-UNDO.

/* Used to communicate with SMTP Socket */
DEF VAR hSocket           AS HANDLE NO-UNDO.
DEF VAR mData             AS MEMPTR NO-UNDO.
DEF VAR smtpcmd           AS CHARACTER FORMAT "x(90)" NO-UNDO.
DEF VAR ServResponse      AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEF VAR ServerCode        AS INTEGER NO-UNDO.

DEF VAR idx               AS INTEGER  NO-UNDO.

/* Used to Build MIME Message Body */
DEF VAR cTempValue                      AS CHARACTER NO-UNDO.
DEF VAR cBoundary                       AS CHARACTER NO-UNDO.
DEF VAR cMimeType                       AS CHARACTER NO-UNDO.
DEF VAR cCharSet                        AS CHARACTER NO-UNDO.
DEF VAR cFileType                       AS CHARACTER NO-UNDO.
DEF VAR cFile                           AS CHARACTER NO-UNDO.
DEF VAR cImportData                     AS CHARACTER NO-UNDO.

DEF VAR cMonth                          AS CHARACTER NO-UNDO 
         INIT "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

DEFINE STREAM sLogfile.
&GLOBAL-DEFINE Stream STREAM sLogFile 
&GLOBAL-DEFINE LOGGER PUT {&Stream} UNFORMATTED TODAY " " STRING (TIME, "hh:mm:ss") " "

/* No Point in doing anything if EmailFrom and EmailTo Not Known */
IF EmailFrom = "" OR
   EmailTo = "" THEN RETURN.

OUTPUT {&Stream} TO VALUE(LogFile) UNBUFFERED APPEND.
IF loglevel <= 2 THEN
  {&Logger} "Socket email started" SKIP
          "Input Parameters" SKIP
          "EmailFrom = " EmailFrom SKIP
          "EmailTo = " EmailTo SKIP
          "EmailCC = " EmailCC SKIP
          "Attachments = " Attachments SKIP
          "LocalFiles = " LocalFiles SKIP
          "Subject = " Subject SKIP
          "Body = " Body SKIP
          "MIMEHeader = " MIMEHeader SKIP.
ELSE {&Logger} "Send Mail From " EmailFrom " to " EmailTo SKIP.

/****************************************************************/
/* Create the socket, log in the log file that it was succes-   */
/* ful or not.                                                  */
/****************************************************************/

CREATE SOCKET hSocket.
hSocket:SET-SOCKET-OPTION ("TCP-NODELAY", "FALSE").
hSocket:SET-SOCKET-OPTION ("SO-LINGER", "FALSE").
hSocket:CONNECT("-H " + MailHub + " -S smtp") NO-ERROR.

IF hSocket:CONNECTED() = FALSE THEN DO:
  {&LOGGER} "Unable to Connect to " + Mailhub + "~n".
  RUN CleanUp.
  RETURN.
END.  /* Cannot make a connection to mail server */

IF loglevel <= 2 THEN {&LOGGER} "Socket Connection Established.~n" .

RUN GetResponse.
IF loglevel <= 2 THEN {&LOGGER} ServResponse "~n".
IF ServerCode <> 220 THEN DO:
  RUN Cleanup.
  RETURN.
END.

/****************************************************************/
/* Begin sending message                                        */
/****************************************************************/
smtpcmd = "HELO " + Mailhub + " ~n".
RUN WriteData.
RUN GetResponse.
IF ServerCode <> 250 THEN DO:
  RUN Cleanup.
  RETURN.
END.

smtpcmd = "MAIL FROM: " + EmailFrom + "~n".
RUN WriteData.
RUN GetResponse.
IF ServerCode <> 250 THEN DO:
  RUN Cleanup.
  RETURN.
END.

EmailTo = REPLACE(EmailTo,";",",").
EmailCC = REPLACE(EmailCC,";",",").

/****************************************************************/
/* Loop through all To's                                        */
/****************************************************************/
IF EmailTo <> "" THEN
DO idx = 1 TO NUM-ENTRIES(EmailTo):
  smtpcmd = "RCPT TO: " + ENTRY(idx,EmailTo) + "~n".
  RUN WriteData.
  RUN GetResponse.
  IF ServerCode >= 500 AND
     ServerCode < 600 THEN oSuccessful = ?.
END.

/****************************************************************/
/* Loop through all CC's                                        */
/****************************************************************/
IF EmailCC <> "" THEN
DO idx = 1 TO NUM-ENTRIES(EmailCC):
  smtpcmd = "RCPT TO: " + ENTRY(idx,EmailCC) + "~n".
  RUN WriteData.
  RUN GetResponse.
  IF ServerCode >= 500 AND
     ServerCode < 600 THEN oSuccessful = ?.
END.


/* Build Email Header */

smtpcmd = "DATA ~n".
RUN WriteData.
RUN GetResponse.
IF ServerCode <> 354 THEN DO:
  RUN Cleanup.
  RETURN.
END.

smtpcmd = "From: " + EmailFrom + " ~n".

/****************************************************************/
/* Loop through all To's                                        */
/****************************************************************/
IF EmailTo <> "" THEN DO idx = 1 TO NUM-ENTRIES(EmailTo):
  smtpcmd = smtpcmd + "To: " + ENTRY(idx,EmailTo) + " ~n".
END.

/****************************************************************/
/* Loop through all CC's                                        */
/****************************************************************/
IF EmailCC <> "" THEN DO idx = 1 TO NUM-ENTRIES(EmailCC):
  smtpcmd = smtpcmd + "Cc: " + ENTRY(idx,EmailCC) + " ~n".
END.

smtpcmd = smtpcmd + "Subject: " + Subject + "~n".
RUN WriteData.

/* Sample format    Date: 27 March 2001 10:30:00 EST */
smtpcmd = "Date: " + STRING(DAY(TODAY)) + " " +
               ENTRY(MONTH(TODAY),cMonth) + " " +
               STRING(YEAR(TODAY),"9999") + " " +
               STRING(TIME,"hh:mm:ss") + " " + 
/*                timezone + */
               "~n".
RUN WriteData.

/* Done building Email Header, Now do the body of the message */


/** Set up a boundary value if we have file attachments **/
/** Create multi mime header for email **/
IF Attachments <> "" THEN DO:
  cBoundary = "MAIL_BOUNDARY".
  smtpcmd = "MIME-Version: 1.0~n" +
         'Content-type: multipart/mixed; boundary="' + 
         cBoundary + '"~n' + "Content-Transfer-Encoding: 7bit~n".
  RUN WriteData.
  smtpcmd = "This is a multi-part MIME Encoded message~n~n" +
                 "--" + cBoundary + "~n".
  RUN WriteData.
END.

/** Do we have a MIME Type for this messsage **/
IF MIMEHeader <> "" THEN DO:
  RUN ParseParm(MIMEHeader).
  smtpcmd = IF Attachments <> "" THEN "Mime-Version: 1.0~n"
                                          ELSE "".
  smtpcmd = smtpcmd + "Content-Type: " + cMimeType +
                 "; charset=" + cCharSet + "~n" +
                 "Content-Transfer-Encoding: 7bit~n".
  RUN WriteData.
END.

/****************************************************************/
/* Output the Message                                           */
/****************************************************************/
  smtpcmd = "~n".
  RUN WriteData.
  IF LENGTH(Body) < 500 THEN DO:
    smtpcmd = Body.
    RUN WriteData.
  END.
  ELSE DO:
    DO idx = 1 TO LENGTH(Body) BY 499:
      cTempValue = IF LENGTH(Body) > idx + 499
      THEN SUBSTRING(Body,idx,499)
      ELSE SUBSTRING(Body,idx, LENGTH(Body) - idx + 1).
      smtpcmd = cTempValue.
      RUN WriteData.
    END.
  END.
  smtpcmd = "~n".
  RUN WriteData.

/****************************************************************/
/* Process any files attachments.                               */
/****************************************************************/
/* LocalFiles holds comma separated list of files that are in the
   Progress path or contain an fullpath
   Attachments holds colon separated list of parameters of 
   to use in sending file. The 1st parameter is the name of file to
   use in generating a temporary file, the remaining parameters are
   all optional: Type=text/plain   Charset=US-ASCII   FileType=ASCII
*/

DO idx = 1 TO NUM-ENTRIES(LocalFiles):
  ASSIGN
  cFile = ENTRY(1,ENTRY(idx,Attachments),":")
  cLocalFile = ENTRY(idx,LocalFiles).

  IF SEARCH(cLocalFile) = ? THEN NEXT.

  RUN ParseParm(ENTRY(idx,Attachments)).
  IF cFileType = "Binary" THEN DO:
    cBinaryFile = cLocalFile.
    cLocalFile  = EncodeDirectory + cFile + "." + 
                  STRING(RANDOM(1,99999),"99999").
    RUN utils/base64encode.p(cBinaryFile, cLocalFile).
  END.

  /** set up the mime header **/
  /* Content-Type: <mimetype>; charset=<charset> */
      
  smtpcmd = "~n--" + cBoundary + "~n" +
                 "Content-type: " + cMimeType + "; ".
  IF cFileType <> "Binary" THEN
    smtpcmd = smtpcmd + cCharSet.

  smtpcmd = smtpcmd + '~n        name="' + cFile + '"~n'.
  RUN Writedata.

  IF cFileType = "Binary" THEN DO:
    smtpcmd = 'Content-Transfer-Encoding: base64~n'.
    RUN WriteData.
  END.

  smtpcmd = 'Content-Disposition: attachment;~n' +
                 '        filename="' + cFile + '"~n~n'.
  RUN WriteData.

  /** now do the file **/
  INPUT FROM VALUE(cLocalFile) NO-ECHO.
    ASSIGN
    smtpcmd = ""
    idx = 1.
    REPEAT:
      IMPORT UNFORMATTED cImportData.
      smtpcmd = smtpcmd + cImportData + "~n".
      IF idx modulo 5 = 0 THEN DO:
        RUN WriteData.
        smtpcmd = "".
      END.
      idx = idx + 1.
    END.

    /** Send the last line if not a multiple of 10 **/
    IF smtpcmd <> "" THEN DO:
      RUN WriteData.
    END.
  INPUT CLOSE.

  IF cFileType = "Binary" THEN DO:
  /** if we have a "Binary" file then try to delete the encoded version **/
    OS-DELETE VALUE(cLocalFile) NO-ERROR.
  END.
END. /** process each file attachment **/

IF Attachments <> "" THEN DO:
  smtpcmd = '~n--' + cBoundary + '--~n~n'.
  RUN WriteData.
END.

/****************************************************************/
/* Finish off SMTP                                              */
/****************************************************************/
smtpcmd = ".~n".
RUN Writedata.
RUN GetResponse.
IF ServerCode <> 250 THEN DO:
  RUN Cleanup.
  RETURN.
END.

smtpcmd = "QUIT~n".
RUN WriteData.
RUN GetResponse.

RUN Cleanup.
IF oSuccessful = NO THEN oSuccessful = YES.


PROCEDURE Cleanup.
  {&Logger} "End SMTP Session" SKIP.
  OUTPUT {&Stream} CLOSE.

  IF hSocket:CONNECTED() THEN hSocket:DISCONNECT() NO-ERROR.
  DELETE OBJECT hSocket.
END.

PROCEDURE WriteData:
  IF SMTPCMD = "" THEN RETURN.

  IF loglevel <= 2 THEN {&LOGGER} "SMTP:" SMTPCMD "~n".

  SET-SIZE(mdata) = LENGTH(SMTPCMD) + 1.
  PUT-STRING(mData,1) = SMTPCMD.
  hSocket:Write(mdata, 1, LENGTH(SMTPCMD)).
  SET-SIZE(mData) = 0.

END.

PROCEDURE GetResponse:
  DEF VAR i                             AS INTEGER INIT 1 NO-UNDO.
  /****************************************************************/
  /* Sometimes we may need to wait a little bit for the server to */
  /* respond so we loop a bit till we get something interesting.  */
  /* If we dont, we just bail out of here.                        */
  /****************************************************************/
  DO WHILE hSocket:GET-BYTES-AVAILABLE() = 0:
    IF 100 MOD i = 0 AND loglevel = 1 THEN
      {&LOGGER} "~nBytes on socket: " hSocket:GET-BYTES-AVAILABLE() "~n".
    i = i + 1.
    IF i = 10000 THEN LEAVE.
  END.

  IF hSocket:GET-BYTES-AVAILABLE() = 0 THEN DO:
    IF loglevel <= 2 THEN {&LOGGER} "RESP: Nothing to Read~n".
    RETURN.
  END.

  /****************************************************************/
  /* We discover we have something to read from the socket, lets  */
  /* see what we got!  Pop it into raw memory, then put it into   */
  /* a 4GL varible.                                               */
  /****************************************************************/

  /* Rewrote this code using a read-handler , there is no point in
     looping aimlessly just to wait for the socket to fill up */

  i = hSocket:GET-BYTES-AVAILABLE().
  IF i > 0 THEN DO:
    SET-SIZE(mData) = i + 1.
    hSocket:READ(mData, 1, i , READ-EXACT-NUM).
    ServResponse = GET-STRING(mData,1).
    SET-SIZE(mData) = 0.
    serverCode = INTEGER(ENTRY(1, ServResponse, " ")) NO-ERROR.
    IF loglevel <= 2 THEN {&LOGGER} "RESP:" ServResponse "~n".
  END.
END. /* PROCEDURE GetResponse */

/*******************************************************************/
/** Parse mime type and char set out of header                    **/
/** If nothing there, return the default                          **/
/*******************************************************************/
PROCEDURE ParseParm:
DEF INPUT PARAMETER cString             AS CHARACTER NO-UNDO.

DEF VAR c                               AS CHARACTER NO-UNDO.
DEF VAR i                               AS INTEGER  NO-UNDO.

ASSIGN
cMimeType = "text/plain"
cCharSet  = "US-ASCII"
cFileType = "ASCII".

DO i = 1 TO NUM-ENTRIES(cString,":"):
  c = ENTRY(i,cString,":").
  CASE ENTRY(1,c,"="):
    WHEN "Type" THEN DO:
      cMimeType = ENTRY(2,c,"=").
    END.
    WHEN "CharSet" THEN DO:
      cCharSet = ENTRY(2,c,"=").
    END.
    WHEN "FileType" THEN DO:
      cFileType = ENTRY(2,c,"=").
    END.
  END CASE.
END.
END PROCEDURE. /** ParseParm **/
