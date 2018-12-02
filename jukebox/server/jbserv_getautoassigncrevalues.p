/* Fetch values that should automatically be assigned on create in jbserv_servertrans.p */

DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCreateAutoAssign AS CHAR NO-UNDO INIT "cCreatedBy,dCreated".
DEF OUTPUT PARAM ocCreateValues     AS CHAR NO-UNDO INIT "unknown|TODAY".

DEF VAR hBuffLoginSession AS HANDLE NO-UNDO.
DEF VAR bOK               AS LOG    NO-UNDO.

CREATE BUFFER hBuffLoginSession FOR TABLE "JBoxLoginSession" NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN DO:
  bOk = hBuffLoginSession:FIND-FIRST("WHERE cSessionId = '" + icSessionId + "'",NO-LOCK) NO-ERROR.
  IF bOK THEN
    ocCreateValues = hBuffLoginSession:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|TODAY".
  DELETE OBJECT hBuffLoginSession NO-ERROR.
END.

