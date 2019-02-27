DEF VAR cLongChar AS LONGCHAR NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.

DEF VAR StockDataSet AS HANDLE NO-UNDO.

{asGetPRSLager.i}

CREATE DATASET StockDataSet.
ASSIGN
/*     StockDataSet:SERIALIZE-HIDDEN = TRUE */
StockDataSet:SERIALIZE-NAME   = "Stock".
StockDataSet:ADD-BUFFER(TEMP-TABLE tt_stockbutiker:DEFAULT-BUFFER-HANDLE).
StockDataSet:ADD-BUFFER(TEMP-TABLE tt_stock:DEFAULT-BUFFER-HANDLE).
StockDataSet:ADD-RELATION(BUFFER tt_stockbutiker:HANDLE, BUFFER tt_stock:HANDLE,"butik,butik").

RUN asGetPrsLager.p (OUTPUT cLongChar, OUTPUT bOk, OUTPUT ocReturn).

StockDataSet:READ-JSON ("longchar", cLongchar,"EMPTY").
StockDataSet:WRITE-JSON("file",'konv\ttStock' + 
                                REPLACE(STRING(TODAY),'/','') + 
                                REPLACE(STRING(TIME,"HH:MM:SS"),':','') +
                                '.json',YES).

MESSAGE bOk SKIP
        ocReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
