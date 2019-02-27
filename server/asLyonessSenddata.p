DEFINE OUTPUT PARAMETER DATASET-HANDLE hDset.
DEFINE VARIABLE lcData AS LONGCHAR     NO-UNDO.

DEFINE TEMP-TABLE tt_Dealer NO-UNDO SERIALIZE-NAME "EJournal"
    FIELD DealerID AS CHAR 
    FIELD PeriodBegin AS DATETIME
    FIELD PeriodEnd   AS DATETIME
    INDEX did IS PRIMARY UNIQUE DealerID.
DEFINE TEMP-TABLE tt_transaction NO-UNDO SERIALIZE-NAME "Transaction"
    FIELD DealerID AS CHAR SERIALIZE-HIDDEN
    FIELD Strekkode       AS CHAR SERIALIZE-NAME "CardNumber"
    FIELD b_id     AS DECI DECIMALS 0 SERIALIZE-NAME "InvoiceNumber"
    FIELD InvoiceDate      AS DATETIME
    FIELD InvoiceAmount    AS DECI
    FIELD InvoiceCurrency  AS CHAR
    FIELD SegmentNr        AS INTE
    INDEX bid IS PRIMARY DealerID.

/* CREATE DATASET hDset.                                                                                   */
/* hDset:ADD-BUFFER( BUFFER tt_Dealer:HANDLE).                                                             */
/* hDset:ADD-BUFFER( BUFFER tt_transaction:HANDLE).                                                        */
/*                                                                                                         */
/* hDset:ADD-RELATION(BUFFER tt_Dealer:HANDLE,BUFFER tt_transaction:HANDLE,"DealerID,DealerID",TRUE,TRUE). */
/*  */
DEFINE DATASET DSET FOR tt_Dealer, tt_transaction 
    DATA-RELATION tt_Dealer FOR tt_Dealer,
      tt_transaction RELATION-FIELDS(DealerID,DealerID) NESTED .

CREATE tt_Dealer.
ASSIGN tt_dealer.dealerid = "85792"
    tt_dealer.PeriodBegin = NOW - 10000
    tt_dealer.PeriodEnd = NOW.


CREATE tt_transaction.
ASSIGN
    tt_transaction.DealerID        = "85792"
    tt_transaction.Strekkode       = "7710007007379"
    tt_transaction.b_id            = 2010031136000054
    tt_transaction.InvoiceDate     = NOW - 5000
    tt_transaction.InvoiceAmount   = 500
    tt_transaction.InvoiceCurrency = "NOK"
    tt_transaction.SegmentNr       = 1.

CREATE tt_transaction.
ASSIGN
    tt_transaction.DealerID        = "85792"
    tt_transaction.Strekkode       = "7710007007379"
    tt_transaction.b_id            = 2010031136000055
    tt_transaction.InvoiceDate     = NOW - 4000
    tt_transaction.InvoiceAmount   = 400
    tt_transaction.InvoiceCurrency = "NOK"
    tt_transaction.SegmentNr       = 1.

hDset = DATASET DSET:HANDLE.
    

