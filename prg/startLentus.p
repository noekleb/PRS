DEFINE VARIABLE cPFfil AS CHARACTER  NO-UNDO.
IF NOT CONNECTED("skotex") THEN DO:
    IF SESSION:PARAMETER BEGINS "PF" THEN DO:
        ASSIGN cPFfil = ENTRY(2,SESSION:PARAMETER,"=").
        IF SEARCH(cPffil) <> ? THEN DO:
            RUN connectDb.p (cPFfil).
        END.
        ELSE
            QUIT.
    END.
END.

IF CONNECTED("skotex") AND CONNECTED("data") THEN
    RUN kupongdata_2_lentus.p NO-ERROR.
QUIT.
