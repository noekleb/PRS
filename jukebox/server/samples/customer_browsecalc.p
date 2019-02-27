/* In order to speed up processing of calculated fields it can be crucial
   to place the procedures in a persistent procedure that is loaded
   once before processing the records.
   To invoke:
   
   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","customer_browsecalc.p").
   
   Usage in browse (the name must not end on .p) :
   
   + ";+OrderTotal|DECIMAL|->><>>><>>9.99|CustOrderTotal(CustNum)|Order Total"
   
   (vs, when calling a .p each time - : 
   + ";+OrderTotal|DECIMAL|->><>>><>>9.99|CustOrderTotal.p(CustNum)|Order Total"
   )
------------------------------------------------------------------------*/   

PROCEDURE CustOrderTotal:
  DEF INPUT PARAM  iiCustNum    AS INT  NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  DEF VAR fTotal AS DECIMAL NO-UNDO.

  FOR EACH Order NO-LOCK
      WHERE Order.CustNum = iiCustNum
      ,EACH OrderLine NO-LOCK
            OF Order 
     :
    fTotal = fTotal + OrderLine.Qty * OrderLine.Price.

  END.
  ocReturn = STRING(fTotal). 

  IF fTotal < 1000 THEN ocReturn = "skiprow".
END PROCEDURE.


PROCEDURE FirstItem:
  DEF INPUT  PARAM irOrder AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  ocReturn = "test". /* Always string value. Use SKIPROW to exclude row from result set */
END PROCEDURE.