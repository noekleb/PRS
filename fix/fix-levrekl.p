CURRENT-WINDOW:WIDTH = 250.

DEF VAR wVVAreKost LIKE Lager.VVareKost NO-UNDO.
DEF VAR pi AS INT NO-UNDO.

DEF BUFFER bBongLinje FOR BongLinje.

DEF STREAM Ut.

OUTPUT STREAM ut TO VALUE("jf-reklam2.txt") NO-ECHO.

FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.TransDato > 01/01/2002 AND
    BongLinje.TransDato <= 12/13/2002 AND
    BongLinje.TTId = 4:

    pi = pi + 1.

    FIND FIRST bBongLinje NO-LOCK WHERE
        bBongLinje.ButikkNr  = BongLinje.ButikkNr AND
        bBongLinje.GruppeNr  = BongLinje.GruppeNr AND
        bBongLinje.KasseNr   = BongLinje.KasseNr AND
        bBongLinje.TransDato = BongLinje.TransDato AND
        bBongLinje.BongNr    = BongLinje.BongNr AND
        bBongLinje.TTId      <> 4 AND
        RECID(bBongLinje)    <> RECID(BongLinje) 
        NO-ERROR.


    FIND TransLogg NO-LOCK WHERE
        TransLogg.Butik   = BongLinje.ButikkNr AND
        TransLogg.TransNr = Bonglinje.TransNr AND
        TransLogg.SeqNr   = Bonglinje.SeqNr NO-ERROR.

    IF NOT AVAILABLE bBongLinje THEN
    DO:
        FIND LAger NO-LOCK WHERE
            LAger.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
            Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
        IF AVAILABLE LAger THEN
            wVVAreKost = Lager.VVAreKost.
        ELSE
            wVVAreKost = 0.

        PAUSE 0 BEFORE-HIDE.
        DISPLAY stream ut
            pi
            "*" WHEN AVAILABLE Translogg
            BongLinje.ButikkNr
            BongLinje.KasseNr
            BongLinje.BongNr
            BongLinje.TransDato
            BongLinje.Antall (TOTAL)
            BongLinje.LinjeSum (TOTAL)
            wVVAreKost (TOTAL)
            WITH WIDTH 248
            .
    END.
END.

OUTPUT STREAM ut CLOSE.
