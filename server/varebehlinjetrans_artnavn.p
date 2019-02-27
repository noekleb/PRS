DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
IF AVAIL VarebehLinjeTrans THEN DO:
  FIND FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN
    ocValue = VarebehLinje.Beskr.
END.
