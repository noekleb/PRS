DEFINE VARIABLE cStr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cc AS CHARACTER  NO-UNDO.
INPUT FROM c:\tmp\pbkj.skv.
REPEAT:
    IMPORT UNFORMATTED cStr.
    FIND KjedensButikker WHERE kjedenr = 1    AND
         regionnr    = int(ENTRY(2,cStr,";")) AND
         distriktnr  = int(ENTRY(3,cStr,";")) AND
         butikknr    = int(ENTRY(4,cStr,";")) NO-LOCK NO-ERROR.
    IF NOT AVAIL kjedensbutikker THEN DO:
        CREATE kjedensbutikker.
     ASSIGN 
            KjedeNr      = 1
            RegionNr     = int(ENTRY(2,cStr,";"))
            DistriktNr   = int(ENTRY(3,cStr,";"))
            ButikkNr     = int(ENTRY(4,cStr,";"))
            Firmanavn    = ENTRY(5,cStr,";")
            ButikkNavn   = ENTRY(5,cStr,";")
            DriftsFormId = int(ENTRY(7,cStr,";"))
            BeliggenhetId = 1
            DriftsTypeId = 1
            Medlemsstatus = 1.
     cc = cc + ENTRY(4,cStr,";") + ",".
    END.
END.
INPUT CLOSE.
OUTPUT TO "CLIPBOARD".
PUT UNFORMATTED cc SKIP.
OUTPUT CLOSE.

/* 5104,5115,5148,5160,5305,5317,5502,25102,25145,25160,25165,25378,25392,65285, */
