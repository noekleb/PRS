/* You can use a static temp-table on the server and transfer the handle to it like you would do with a dynamic.
   There are orders not assigned to salesrep in the db. Hence the extra loop.
-----------------------------------------------------------------*/

DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR iOrderStatusKey     AS INT  NO-UNDO.
DEF VAR iSalesRepKey        AS INT  NO-UNDO INIT 1000.  /* <- to make sure that order statuses and salesreps get unique node indexes */
DEF VAR iCurrOrderStatusKey AS INT  NO-UNDO.
DEF VAR iOrderSalesrepCount AS INT  NO-UNDO.
DEF VAR cSalesRep           AS CHAR NO-UNDO.
DEF VAR dFrom               AS DATE NO-UNDO.
DEF VAR dTo                 AS DATE NO-UNDO.

ASSIGN cSalesRep = ENTRY(1,icParam,"|")
       dFrom     = DATE(ENTRY(2,icParam,"|"))
       dTo       = DATE(ENTRY(3,icParam,"|"))
       .

DEF BUFFER bOrder FOR Order.

DEF TEMP-TABLE ttOrderTotals
    FIELD iNodeIndex   AS INT
    FIELD iParentIdx   AS INT
    FIELD cNodeLabel   AS CHAR
    FIELD iLevel       AS INT
    FIELD SalesRep     AS CHAR
    FIELD OrderStatus  AS CHAR
    FIELD iStatusCount AS INT
    INDEX idxParentNode iParentIdx cNodeLabel
    INDEX idxLabel cNodeLabel.
hTempTable = BUFFER ttOrderTotals:HANDLE:TABLE-HANDLE.

FOR EACH Salesrep WHERE (IF cSalesRep NE "" THEN Salesrep.Region = cSalesRep ELSE TRUE) NO-LOCK:
  RUN GetOrders (Salesrep.Salesrep,Salesrep.Repname).
END.

IF cSalesRep = "" THEN
  FOR EACH bOrder NO-LOCK
      WHERE NOT CAN-FIND(FIRST Salesrep OF bOrder)
      BREAK BY bOrder.Salesrep:
    IF FIRST-OF(Salesrep) THEN 
      RUN GetOrders(bOrder.Salesrep,bOrder.Salesrep).
  END.

DELETE OBJECT hTempTable.

PROCEDURE GetOrders:
  DEF INPUT PARAM icSalesRep AS CHAR NO-UNDO.
  DEF INPUT PARAM icRepName  AS CHAR NO-UNDO.

  iOrderSalesrepCount = 0.
  FOR EACH Order NO-LOCK
      WHERE Order.Salesrep = icSalesRep
        AND (IF dFrom NE ? THEN Order.OrderDate GE dFrom ELSE TRUE) 
        AND (IF dto   NE ? THEN Order.OrderDate LE dto   ELSE TRUE) 
      BREAK BY OrderStatus:
    iOrderSalesrepCount = iOrderSalesrepCount + 1.
    IF LAST-OF(OrderStatus) THEN DO:
      FIND FIRST ttOrderTotals
           WHERE ttOrderTotals.cNodeLabel = Order.OrderStatus NO-ERROR.
      IF NOT AVAIL ttOrderTotals THEN DO:
        CREATE ttOrderTotals.
        ASSIGN iOrderStatusKey          = iOrderStatusKey + 1
               ttOrderTotals.iNodeIndex = iOrderStatusKey
               ttOrderTotals.cNodeLabel = Order.OrderStatus
               .
      END.
      
      ASSIGN ttOrderTotals.iStatusCount = ttOrderTotals.iStatusCount + iOrderSalesrepCount
             iCurrOrderStatusKey        = ttOrderTotals.iNodeIndex
             .

      FIND FIRST ttOrderTotals
           WHERE ttOrderTotals.iParentIdx = iCurrOrderStatusKey 
             AND ttOrderTotals.Salesrep   = icSalesrep NO-ERROR.
      IF NOT AVAIL ttOrderTotals THEN DO:
        CREATE ttOrderTotals.
        ASSIGN iSalesRepKey              = iSalesRepKey + 1
               ttOrderTotals.iNodeIndex  = iSalesRepKey
               ttOrderTotals.cNodeLabel  = icRepName + " (" + STRING(iOrderSalesrepCount) + ")"
               ttOrderTotals.iParentIdx  = iCurrOrderStatusKey
               ttOrderTotals.SalesRep    = icSalesRep
               ttOrderTotals.OrderStatus = Order.OrderStatus
               iOrderSalesrepCount       = 0
               .
      END.
    END.
  END.

END PROCEDURE.
