CURRENT-WINDOW:WIDTH = 300.

FOR EACH Butiker NO-LOCK  WHERE 
    Butiker.KundeNr > 0:

    FOR EACH FakturaHode EXCLUSIVE-LOCK WHERE
        FakturaHode.KundeNr = Butiker.KundeNr AND 
        FakturaHode.Dato    >= 11/01/2013 and
        FakturaHode.Totalt = 0:

        FOR EACH FakturaLinje OF FakturaHode EXCLUSIVE-LOCK:

            IF AVAILABLE ArtBas THEN RELEASE Artbas.
            IF AVAILABLE ArtPris THEN RELEASE ArtPris.
            
            IF FakturaLinje.ArtikkelNr > 0 THEN
                FIND ArtBas NO-LOCK WHERE 
                    ArtBas.ArtikkelNr = FakturaLinje.ArtikkelNr NO-ERROR.
            IF AVAILABLE ArtBas THEN
                FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
                    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN
                FIND FIRST ArtPris NO-LOCK WHERE 
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
           
            
            IF FakturaLinje.NettoPris = 0 THEN
            ASSIGN
                FakturaLinje.NettoPris     = ArtPris.Innkjopspris[1]
                FakturaLinje.Pris          = ArtPris.Innkjopspris[1]
                FakturaLinje.MvaKr         = 0
                FakturaLinje.NettoLinjeSum = FakturaLinje.NettoPris * abs(FakturaLinje.Antall)
                FakturaLinje.Linjesum      = FakturaLinje.NettoPris * abs(FakturaLinje.Antall) + FakturaLinje.MvaKr
                FakturaLinje.DbKr          = 0
                FakturaLinje.Db%           = 0
                .
           
           
            DISPLAY
                FakturaHode.Faktura_Id
                FakturaHode.KundeNr
                FakturaHode.Navn FORMAT "x(10)"
                FakturAHode.Dato
                FakturaHode.Totalt
                '|'
                FakturaHode.FakturertDato
                FakturaHode.FakturaNr
                '|'
                FakturaLinje.Pris
                FakturaLinje.NettoPris
                FakturaLinje.MvaKr
                FakturaLinje.NettoLinjeSum
                FakturaLinje.Linjesum
                FakturaLinje.Mva%
                FakturaLinje.DbKr
                FakturaLinje.Db% 
                FakturaLinje.Db% 
                WITH WIDTH 300.
           
           
            END.

        /* Påfører og beregner rabatt. Rabatt pr. linje tildeles automatisk når totalrabatt <> 0 blir satt. */
        RUN update_fakturahode.p (FakturaHode.faktura_Id,"KalkulerTotaler","",1).

    END.
END.
