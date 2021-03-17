DEF VAR iOrdretype AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR iantstk AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF buffer bufPkSdlLinje FOR PkSdlLinje.

FOR EACH PkSdlhode NO-LOCK WHERE 
    /*PkSdlHode.PkSdlNr = "127798" AND*/
    PkSdlHode.PkSdlStatus = 10,
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK,
    FIRST PkSdlPris OF PkSdlHode NO-LOCK:

    IF NOT CAN-DO('10,20,40',STRING(PkSdlLinje.ButikkNr)) THEN
        NEXT.

    IF NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 THEN
    DO:
        iOrdreType = INT(ENTRY(2,ENTRY(1,PkSdlHode.MeldingFraLev,CHR(10)),' ')).
    END.
    ELSE ASSIGN
        iOrdreType = 0
        .
    IF iOrdretype = 0 AND PkSdlHode.PkSdlOpphav = 1 THEN
    DO:

        /*IF NOT PkSdlLinje.Beskr BEGINS 'VAREPRØVE*' THEN NEXT.*/
    END.

    IF PkSdlLinje.Beskr BEGINS 'vareprøve' THEN
    DO:
        iAnt = iant + 1.

        FOR EACH bufPkSdlLinje OF PkSdlHode NO-LOCK:
            iAntStk = iAntStk + PkSdlLinje.Antall.
        END.
        DISPLAY
            PkSdlHode.SendtDato
            PkSdlHode.pkSdlId
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlStatus
            PkSdlHode.PkSdlOpphav
            iOrdreType
            PkSdlLinje.butikkNr
            PkSdlLinje.Beskr
            PkSdlPris.Rab1%
            ENTRY(1,PkSdlHode.MeldingFraLev,CHR(10)) WHEN NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 FORMAT "x(20)"
            ENTRY(2,PkSdlHode.MeldingFraLev,CHR(10)) WHEN NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 FORMAT "x(20)"
            ENTRY(3,PkSdlHode.MeldingFraLev,CHR(10)) WHEN NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 FORMAT "x(20)"
        WITH WIDTH 350.
    END.
END.
MESSAGE iAnt SKIP
    iantStk
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
