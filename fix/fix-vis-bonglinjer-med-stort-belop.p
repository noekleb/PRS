/* fix-vis-bonglinjer-med-stort-belop.p */
FOR EACH BongHode NO-LOCK WHERE
    BongHode.ButikkNr = 78691 AND
    BongHode.GruppeNr = 1 AND
    BongHode.KasseNr > 0 AND
    BongHode.Dato >= 01/01/2004 AND 
    BongHOde.DAto <= 11/30/2004:

    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.TTID = 62:

        IF bonglinje.linjesum > 9999 THEN
        DO:
            PAUSE 0.
            DISPLAY
                BongLinje.ButikkNR
                BongLinje.BongNr
                BongLinje.TTId
                BongLinje.LinjeSum
                BongLinje.BongTekst
                .
            OUTPUT TO VALUE("feilbelop.txt") APPEND.
            PUT UNFORMATTED 
                BongLinje.ButikkNr " "
                BongLinje.BongNr " "
                BongLinje.KasseNr " "
                BongLinje.Dato " "
                BongLinje.TTId   " "
                BongLinje.LinjeSum  " "
                BongLinje.BongTekst
                SKIP.
            OUTPUT CLOSE.
        END.
    END.
END.


