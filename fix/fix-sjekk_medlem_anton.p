CURRENT-WINDOW:WIDTH = 450.

FOR EACH Butiker NO-LOCK WHERE butiker.apningsdato <= TODAY:

FOR EACH BongHode WHERE 
    BongHode.ButikkNr = Butiker.Butik AND
    BongHode.Dato >= 01/01/2012 AND 
    bonghode.medlemskort <> '' AND 
    BongHode.Makulert = 0 /*AND BongHode.KundeNr = 17400021*/,
    EACH BongLinje WHERE BongLinje.B_Id = BongHode.B_Id,
    EACH TransLogg WHERE 
        TransLogg.Butik   = BongLinje.ButikkNr AND
        TransLogg.TransNr = BongLinje.TransNr AND
        TransLogg.SeqNr   = BongLinje.SeqNr:

    /*
    FIND Medlem NO-LOCK WHERE
        Medlem.MedlemsNr = TransLogg.MedlemsNr.
    FIND Kunde NO-LOCK WHERE 
        Kunde.KundeNr = TransLogg.KundNr.
    */
    /*
    DISPLAY
        BongHode.BongNR
        BongHode.KundeNr
        BongHode.MedlemsNr
        BongHode.MedlemNavn
        BongHode.MedlemsKort
        BongHode.KundeKort
        BongLinje.BongNr
        BongLinje.LinjeNr
        BongLinje.LinjeRab (TOTAL)
        BongLinje.LinjeSum  (TOTAL)
        (BongLinje.LinjeSum  - BongLinje.LinjeRab) (TOTAL)
        BongLinje.BongTekst
        TransLogg.MedlemsNr
        TransLogg.KundNr

        /*
        TransLogg.BongId
        TransLogg.Butik
        TransLogg.TransNr
        TransLogg.SeqNr
        Medlem.ForNAvn + ' ' + Medlem.EtterNavn FORMAT "x(30)"
        Kunde.Navn
        */
        WITH WIDTH 450
        .
    */
   TransLogg.MedlemsNr = BongHode.MedlemsNr. 
END.

END. /* BUTIKER */

