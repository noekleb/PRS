DEF INPUT  PARAM icPartnerId     AS CHAR NO-UNDO.
DEF INPUT  PARAM icPassPhrase    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG  NO-UNDO.

DEF VAR ix      AS INT NO-UNDO.
DEF VAR cResult AS CHAR NO-UNDO.

DO ix = 0 TO LENGTH(icPartnerId) - 1:
  cResult = cResult + CHR(INT(SUBSTR(icPassPhrase,1 + ix * 7,5)) / MAX(INT(SUBSTR(icPassPhrase,6 + ix * 7,2)),1)).
END.

obOk = cResult = SUBSTR(icPartnerId,1,3).



