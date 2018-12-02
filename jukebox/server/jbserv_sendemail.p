/* Wrapper procedure for smtpmail5_7a

   Created: 24.01.06 by Brynjar Hasle
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cMailTo     AS CHAR     NO-UNDO.
DEF VAR cStatus     AS CHAR     NO-UNDO.
DEF VAR cMailFrom   AS CHAR     NO-UNDO.
DEF VAR cTopic      AS CHAR     NO-UNDO.
DEF VAR cBccTo      AS CHAR     NO-UNDO.
DEF VAR cCopyTo     AS CHAR     NO-UNDO.
DEF VAR cBody       AS CHAR     NO-UNDO.
DEF VAR cMailServer AS CHAR     NO-UNDO.
DEF VAR cAttachment AS CHAR     NO-UNDO. /* list of files */
DEF VAR cReplyTo    AS CHAR     NO-UNDO.
DEF VAR ix          AS INT      NO-UNDO.
DEF VAR hSmtpMail   AS HANDLE   NO-UNDO.
def var cContentType as char no-undo. /*i.e. 
content-type:charset=iso8859-1*/

ASSIGN cMailServer = ENTRY(1,icParam,"|")
       cMailFrom   = ENTRY(2,icParam,"|")
       cMailTo     = ENTRY(3,icParam,"|")
       cTopic      = ENTRY(4,icParam,"|")
       cBody       = ENTRY(5,icParam,"|")
       NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Error in parameters for smtpmail. " + PROGRAM-NAME(1).
  RETURN.
END.
IF NUM-ENTRIES(icParam,"|") > 5 THEN
  cCopyTo     = ENTRY(6,icParam,"|").
IF NUM-ENTRIES(icParam,"|") > 6 THEN
  cCopyTo     = ENTRY(7,icParam,"|").
IF NUM-ENTRIES(icParam,"|") > 7 THEN
  cBccTo      = ENTRY(8,icParam,"|").
IF NUM-ENTRIES(icParam,"|") > 8 THEN
  cReplyTo    = ENTRY(9,icParam,"|").
IF NUM-ENTRIES(icParam,"|") > 9 THEN
  cAttachment = ENTRY(10,icParam,"|").
IF NUM-ENTRIES(icParam,"|") > 10 THEN
  cContentType = ENTRY(11,icParam,"|").

DO ix = 1 TO NUM-ENTRIES(cBccTo):
  cCopyTo = cCopyTo + ENTRY(ix,cBccTo) + "^B,".
END.
cCopyTo = TRIM(cCopyTo,",").

IF cContentType = "" THEN   
  cContentType = "content-type:charset=" + SESSION:CPINTERNAL.

/* RUN smtpmailv5_7a.p ( */
/* RUN smtpmailv5_8a.p ( */
RUN smtpmailv5_8c.p PERSIST SET hSmtpMail (
    INPUT cMailServer,
    INPUT cMailTo,
    INPUT cMailFrom,
    INPUT cCopyTo,
    INPUT "",
    INPUT cAttachment,
    INPUT cTopic,
    INPUT cBody,
    INPUT cContentType,
    INPUT "text",
    INPUT 1,

    INPUT NO,
    INPUT "",
    INPUT "",
    INPUT "",

/*     INPUT YES,                    */
/*     INPUT "base64",               */
/*     INPUT "brynjar@chemistry.no", */
/*     INPUT "abcdefg",              */

    OUTPUT obOk,
    OUTPUT ocReturn
    ).

DELETE PROCEDURE hSmtpMail NO-ERROR.

IF ocReturn = "" THEN obOk = TRUE.

