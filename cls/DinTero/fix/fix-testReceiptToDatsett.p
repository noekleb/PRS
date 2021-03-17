{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEF VAR pcReceipt AS LONGCHAR NO-UNDO. 
DEF VAR cReceiptFile AS CHAR NO-UNDO.

/* Preparer variablen til å holde på UTF-8 data. */
FIX-CODEPAGE(pcReceipt) = 'UTF-8'.

/* TEST TEST Henter foreløpig data fra fil. Den skal komme inn via en variabel. */
ASSIGN 
  cReceiptFile = 'cls\DinTero\Test\PostReceiptForDiscountResponse_75899_10%.json'
  .

/* TEST TEST Legger inn kvitteringen i en LongChar. */
/* Longchar variabelen skal senere komme som en input parameter. */
COPY-LOB FROM FILE cReceiptFile TO OBJECT pcReceipt.



RUN cls\dintero\receiptToDataset.p (pcReceipt, OUTPUT DATASET dsreceipts).
