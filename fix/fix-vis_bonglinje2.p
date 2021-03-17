CURRENT-WINDOW:WIDTH = 350.
FOR EACH BongLinje WHERE 
    BongNr = 107109:
    DISPLAY
        BongLinje.ButikkNr
        BongLinje.BongNr
        BongLinje.TTId
        BongLinje.TBId
        BongLinje.BongTekst
        BongLinje.Antall
        BongLinje.LinjeSum
        BongLinje.LinjeRab
        BongLinje.subtotalRab
        BongLinje.MvaKr
    WITH WIDTH 350.
END.
