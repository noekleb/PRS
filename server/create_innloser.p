DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR cInnloserId  AS CHARACTER NO-UNDO.

DEF BUFFER bInnloser FOR Innloser.

ASSIGN cInnloserId = entry(1,icValues,'|').

/*
FIND FIRST bInnloser NO-LOCK WHERE
  bInnloser.InnloserId = cInnloserId AND 
  ROWID(bInnloser) <> TO-ROWID(icRowid) NO-ERROR.

IF AVAIL bInnloser THEN
    ocReturn = "Innløser med innløserid '" + cInnloserId + "' er opprettet tidligere. Angi et innløserid som ikke er brukt fra før".
*/