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

PROCEDURE order_itemlist:
  DEF INPUT PARAM  iiOrderNum   AS INT  NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  DEF VAR ix AS INT NO-UNDO.

  FOR EACH OrderLine NO-LOCK
      WHERE OrderLine.Ordernum = iiOrderNum
     ,FIRST ITEM NO-LOCK
            OF OrderLine
     :
    ocReturn = ocReturn + ITEM.ItemName + " (" + STRING(OrderLine.Qty) + ")" + CHR(10).
  END.
  ocReturn = TRIM(ocReturn,CHR(10)).  
END PROCEDURE.



PROCEDURE OrderTotal:
  DEF INPUT  PARAM ipOrdernum AS integer NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  DEF VAR fTot AS DEC NO-UNDO.

  FOR EACH OrderLine NO-LOCK
      WHERE OrderLine.Ordernum = ipOrdernum
     ,FIRST ITEM NO-LOCK
            OF OrderLine
     :
    fTot = fTot + OrderLine.Qty * Item.Price.
  END.
  ocReturn = STRING(fTot).  
END PROCEDURE.