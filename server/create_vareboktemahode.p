DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iVbTemeNr  AS INT NO-UNDO.

DEF BUFFER bVarebokTemaHode FOR VarebokTemaHode.

FOR EACH bVarebokTemaHode NO-LOCK
    BY bVarebokTemaHode.VbTemeNr DESC:
  iVbTemeNr = bVarebokTemaHode.VbTemeNr.
  LEAVE.
END.
iVbTemeNr = iVbTemeNr + 1.

hBuffer:BUFFER-FIELD("VbTemeNr"):BUFFER-VALUE = iVbTemeNr.

