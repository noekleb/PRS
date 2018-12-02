/* 
    Räknar igenom alla beställningar där inleverans har startats eller slutförts.
    (BestStat > 4)
    Vid negativ inleverans (BestStat = 7) görs en multiplikation med -1, makulert och
    overlevert är här = 0.
*/
DEFINE VARIABLE iTotInnLevert AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOverLevert   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMakulert     AS INTEGER    NO-UNDO.
FOR EACH BestHode WHERE BestHode.BestStat > 4:
    ASSIGN iTotInnLevert = 0
           iMakulert     = 0
           iOverLevert   = 0.
    FOR EACH BestLevert OF BestHode NO-LOCK.
        IF BestLevert.Avskrevet = FALSE THEN
                ASSIGN iTotInnLevert = iTotInnLevert + BestLevert.Levert.
        ELSE
            ASSIGN iMakulert = iMakulert + BestLevert.Rest.
    END.
    FOR EACH BestLevert OF BestHode WHERE BestLevert.Rest < 0
          BREAK BY BestLevert.Butik
                BY BestLevert.Storl
                BY BestLevert.Rest:
         IF FIRST-OF(BestLevert.Storl) THEN
            ASSIGN iOverLevert   = iOverLevert + ABS(BestLevert.Rest).
    END.
    ASSIGN BestHode.TotInnLev   = iTotInnLevert * IF BestHode.BestStat = 7 THEN -1 ELSE 1
           BestHode.TotMakulert = iMakulert 
           BestHode.TotOverLev  = iOverLevert.
END.

