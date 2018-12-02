CURRENT-WINDOW:WIDTH = 350.
FOR EACH BongHode NO-LOCK WHERE 
    BongHode.ButikkNr = 10 AND 
    BongHode.GruppeNr = 1 AND
    Bonghode.KasseNr = 3 AND 
    BongHode.Dato >= 01/01/2017 AND
    CAN-FIND(FIRST BongLinje WHERE
           BongLinje.B_Id = BongHode.B_Id AND
           BongLinje.Makulert = FALSE AND
           CAN-DO("026",STRING(BongLinje.TTId,"999"))):

    DISPLAY
        Bonghode.butikknr
        BongHode.Dato
        BongHode.KasseNr
        BongHode.BongNr
    WITH WIDTH 350.
END.
