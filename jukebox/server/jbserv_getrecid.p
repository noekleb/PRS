DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBuffer           AS CHAR NO-UNDO.
DEF INPUT  PARAM icCriteria         AS CHAR NO-UNDO.
DEF OUTPUT PARAM orRecId            AS RECID NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR bOK        AS LOG NO-UNDO.

{incl/validatesession.i}

CREATE BUFFER hBuffer FOR TABLE icBuffer.

IF NOT icCriteria MATCHES "*WHERE*" THEN
  bOK = hBuffer:FIND-BY-ROWID(TO-ROWID(icCriteria),NO-LOCK) NO-ERROR.
ELSE
  bOK = hBuffer:FIND-FIRST(icCriteria,NO-LOCK) NO-ERROR.

IF bOk THEN
  orRecId = hBuffer:RECID.
ELSE
  ocReturn = "Failed to retrieve recid for buffer " + icBuffer + ", rowid: " + icCriteria.

DELETE OBJECT hBuffer.

