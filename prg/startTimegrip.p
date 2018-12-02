DEFINE VARIABLE cPFfil AS CHARACTER  NO-UNDO.
IF NOT CONNECTED("skotex") THEN DO:
    IF SESSION:PARAMETER BEGINS "PF" THEN DO:
        ASSIGN cPFfil = ENTRY(2,SESSION:PARAMETER,"=").
        IF SEARCH(cPffil) <> ? THEN
            RUN connectDb.p (cPFfil).
        ELSE
            RETURN.
    END.
END.
IF NOT CONNECTED("skotex") THEN
    RETURN.
RUN artstat2timegrip.p NO-ERROR.
