DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(60)" NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN
    cfil   = 'konv\order_status_report12_12_2018_15_19.csv'
    cUtFil = 'konv\eMailLst.csv'
    .

INPUT STREAM Inn FROM VALUE(cFil).
OUTPUT STREAM Ut TO VALUE(cUtFil).
REPEAT :
    IMPORT STREAM inn UNFORMATTED
        cRecord.
    /*
    DISPLAY
        ENTRY(1,cRecord,';') FORMAT "x(30)"
        ENTRY(2,cRecord,';') FORMAT "x(30)"
    WITH WIDTH 350.
    */
    PUT STREAM Ut UNFORMATTED 
        ENTRY(2,cRecord,';') ';'.

END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
