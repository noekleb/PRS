/* Wrapper procedure for smtpmail5_7a

   Created: 24.01.06 by Brynjar Hasle
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cMailTo       AS CHAR NO-UNDO.
DEF VAR cStatus       AS CHAR NO-UNDO.
DEF VAR cMailFrom     AS CHAR NO-UNDO.
DEF VAR cSubject      AS CHAR NO-UNDO.
DEF VAR cBccTo        AS CHAR NO-UNDO.
DEF VAR cCopyTo       AS CHAR NO-UNDO.
DEF VAR cBody         AS CHAR NO-UNDO.
DEF VAR cMailServer   AS CHAR NO-UNDO.
DEF VAR cAttachments  AS CHAR NO-UNDO. /* list of files */
DEF VAR cLocalFiles   AS CHAR NO-UNDO.
DEF VAR cReplyTo      AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR cContentType  AS CHAR NO-UNDO. /*i.e. content-type:charset=iso8859-1*/
DEF VAR cBodyType     AS CHAR NO-UNDO.
DEF VAR iImportance   AS INT  NO-UNDO.
DEF VAR bAuthenticate AS LOG  NO-UNDO.
DEF VAR cAuthType     AS CHAR NO-UNDO.
DEF VAR cUser         AS CHAR NO-UNDO.
DEF VAR cPwd          AS CHAR NO-UNDO.
DEF VAR cVersion      AS CHAR NO-UNDO.
DEF VAR hSmtpMail     AS HANDLE NO-UNDO.
DEF VAR bDummy        AS LOG  NO-UNDO.
DEF VAR cDummy        AS CHAR NO-UNDO.

ASSIGN cMailServer   = ENTRY(1,icParam,"|")
       cMailTo       = ENTRY(2,icParam,"|")
       cMailFrom     = ENTRY(3,icParam,"|")
       cCopyTo       = ENTRY(4,icParam,"|")
       cBccTo        = ENTRY(5,icParam,"|")
       cAttachments  = ENTRY(6,icParam,"|")
       cLocalFiles   = ENTRY(7,icParam,"|")
       cSubject      = ENTRY(8,icParam,"|")
       cBody         = ENTRY(9,icParam,"|")
       cContentType  = ENTRY(10,icParam,"|") /* MIMEHeader */
       cBodyType     = ENTRY(11,icParam,"|") /* Valid values include 0 to 3, 1 = HIGH; 3 = Low */
       iImportance   = INTEGER(ENTRY(12,icParam,"|"))
       bAuthenticate = LOGICAL(ENTRY(13,icParam,"|"))
       cAuthType     = ENTRY(14,icParam,"|")
       cUser         = ENTRY(15,icParam,"|")
       cPwd          = ENTRY(16,icParam,"|")
       cVersion      = ENTRY(17,icParam,"|")
       NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Error in parameters for smtpmail. " + PROGRAM-NAME(1) + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
  RETURN.
END.


DO ix = 1 TO NUM-ENTRIES(cBccTo):
  cCopyTo = cCopyTo + ENTRY(ix,cBccTo) + "^B,".
END.
cCopyTo = TRIM(cCopyTo,",").

IF cContentType = "" THEN cContentType = "content-type:charset=" + SESSION:CPINTERNAL.
IF cBodyType = "" THEN cBodyType = "text".
IF cVersion = "" THEN cVersion = "smtpmailv5_8a.p".

ocReturn = "test".
RUN VALUE(cVersion) PERSIST SET hSmtpMail (
    INPUT cMailServer,
    INPUT cMailTo,
    INPUT cMailFrom,
    INPUT cCopyTo,
    INPUT "",
    INPUT cAttachments,
    INPUT cSubject,
    INPUT cBody,
    INPUT cContentType,
    INPUT cBodyType,
    INPUT iImportance,

    INPUT bAuthenticate,
    INPUT cAuthType,
    INPUT cUser,
    INPUT cPwd,

    OUTPUT obOk,
    OUTPUT ocReturn
    ).

IF VALID-HANDLE(hSmtpMail) THEN
  DELETE PROCEDURE hSmtpMail.

obOk = ocReturn = "".

IF cContentType = "B64ENCODED" THEN
  RUN VALUE(cVersion) (
      INPUT cMailServer,
      INPUT "RemoveEncodedFiles",
      INPUT "",
      INPUT "",
      INPUT "",
      INPUT "",
      INPUT "",
      INPUT "",
      INPUT "",
      INPUT "",
      INPUT iImportance,

      INPUT NO,
      INPUT "",
      INPUT "",
      INPUT "",

      OUTPUT bDummy,
      OUTPUT cDummy
      ) NO-ERROR.
