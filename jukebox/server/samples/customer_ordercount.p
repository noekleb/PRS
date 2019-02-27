DEF INPUT PARAM  ipCustNum    AS INT NO-UNDO.
DEF INPUT PARAM  icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocOrderCount AS CHAR NO-UNDO.

DEF VAR ix AS INT NO-UNDO.
FOR EACH Order FIELDS() NO-LOCK
    WHERE Order.CustNum = ipCustNum
      AND Order.OrderStatus NE "Shipped":
  ix = ix + 1.
END.

ocOrderCount = STRING(ix).
