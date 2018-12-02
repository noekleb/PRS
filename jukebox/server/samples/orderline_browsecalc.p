/* In order to speed up processing of calculated fields it can be crucial
   to place the procedures in a single persistent procedure that is loaded
   once before processing the records.
   To invoke:
   
   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
   
   Usage in browse (the name must not end on .p) :
   
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
   
   (vs, when calling a .p each time - : 
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total.p|Total"
   )
------------------------------------------------------------------------*/   
define variable hMyResultSet as handle no-undo.
function setResultSetBuffer RETURNS LOGICAL (INPUT hResultSetBuffer AS HANDLE).
  hMyResultSet = hResultSetBuffer.
end function.

PROCEDURE orderline_price:
  DEF INPUT PARAM  irOrderLine  AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocPrice      AS CHAR  NO-UNDO.

  FIND OrderLine WHERE ROWID(OrderLine) = irOrderLine NO-LOCK NO-ERROR.
  IF AVAIL OrderLine THEN DO:
    IF OrderLine.Price NE 0 THEN
      ocPrice = STRING(OrderLine.Price).
    ELSE DO:
      FIND FIRST ITEM OF OrderLine NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
        ocPrice = STRING(ITEM.Price).
    END.
  END.
END PROCEDURE.

PROCEDURE orderline_total:
  DEF INPUT PARAM  irOrderLine  AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocTotal AS CHAR  NO-UNDO.
  
  FIND OrderLine WHERE ROWID(OrderLine) = irOrderLine NO-LOCK NO-ERROR.
  IF AVAIL OrderLine THEN DO:
    IF OrderLine.Price NE 0 THEN
      ocTotal = STRING(OrderLine.Price * OrderLine.Qty).
    ELSE DO:
      FIND FIRST ITEM OF OrderLine NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
        ocTotal = STRING(ITEM.Price * OrderLine.Qty).
    END.
  
  END.

END PROCEDURE.
