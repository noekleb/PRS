DEF INPUT  PARAM icVarebehlinjeRowid AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.

DEF VAR hVBLbuffer AS HANDLE NO-UNDO.
DEF VAR bOK        AS LOG    NO-UNDO.

hVBLbuffer = BUFFER VarebehLinje:HANDLE.

bOK = hVBLbuffer:FIND-BY-ROWID(TO-ROWID(icVarebehlinjeRowid),EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
IF bOk AND hVBLbuffer:BUFFER-FIELD(icFields):DATA-TYPE = "DECIMAL" THEN DO:
  hVBLbuffer:BUFFER-FIELD(icFields):BUFFER-VALUE = DEC(icValues).
  RUN Varebehlinje_kalkuler.p (hVBLbuffer,icFields).
END.
ELSE 
  ocReturn = "Varebehlinje ikke tilgj. for oppdatering".

