DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR oiWeek AS INT NO-UNDO.

IF idDate NE ? THEN DO:
  RUN weeknum.p (idDate,OUTPUT oiWeek).
  IF oiWeek NE ? THEN
    ocValue = STRING(oiWeek).
END.
