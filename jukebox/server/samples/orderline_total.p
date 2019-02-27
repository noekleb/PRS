DEF INPUT PARAM  irOrderLine  AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocTotal AS CHAR  NO-UNDO.

FIND OrderLine WHERE ROWID(OrderLine) = irOrderLine NO-LOCK NO-ERROR.
IF AVAIL OrderLine THEN DO:
  IF OrderLine.Price NE 0 THEN
    ocTotal = STRING(OrderLine.Price * Qty).
  ELSE DO:
    FIND FIRST ITEM OF OrderLine NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
      ocTotal = STRING(ITEM.Price * Qty).
  END.

END.
