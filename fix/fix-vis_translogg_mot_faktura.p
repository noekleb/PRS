DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE('Konv\FakturaUttrekk' + REPLACE(STRING(TODAY),'/','') + '.csv').

CURRENT-WINDOW:WIDTH = 350.

PUT STREAM Ut UNFORMATTED
        'Butik;'
        'OvButik;'
        'B_Id;'
        'Dato;'
        'bongId;'
        'BongLinjeNr;'
        'TransNr;'
        'Beskr;'
        'LevKod;'
        'LevFargKod;'
        'Storl;'
        'KundNr;'
        'Antall;'
        'Pris;'
        'SumPris;'
        'VVareKost;'
        'Sumvarekost;'
        'FakturaLinje.Antall;'
        'FakturaLinje.LinjeSum;'
        'FakturaHode.FakturaNr;'
        'FakturaHode.Dato;'
        'FakturaHode.totalt'
    SKIP.

FOR EACH TransLogg NO-LOCK WHERE 
    TransLogg.Dato >= 01/01/2018 AND 
    TransLogg.Dato <= 12/31/2018 AND 
    TransLogg.Butik = 16 AND 
    TransLogg.TTId = 6 AND 
    Translogg.OvButik = 20:

    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    FIND BongHode NO-LOCK WHERE 
        BongHode.ButikkNr = TransLogg.Butik AND 
        BongHode.GruppeNr = 1 AND 
        BongHode.KasseNr = TransLogg.KassaNr AND 
        BongHode.Dato    = TransLogg.Dato AND 
        BongHode.BongNr  = TransLogg.BongId
        NO-ERROR.

    IF AVAILABLE FakturaHode THEN 
        RELEASE FakturaHode.
    IF AVAILABLE FakturaLinje THEN 
        RELEASE FakturaLinje.

    FIND FIRST FakturaLinje NO-LOCK WHERE 
        FakturaLinje.b_Id = BongHode.b_Id AND 
        FakturaLinje.BongLinjeNr = TransLogg.BongLinjeNr NO-ERROR.
    IF AVAILABLE FakturaLinje THEN
        FIND FakturaHode NO-LOCK WHERE 
            FakturaHode.Faktura_Id = FakturaLinje.Faktura_Id NO-ERROR.

    PUT STREAM Ut UNFORMATTED
            TransLogg.Butik ';'
            TransLogg.OvButik ';'
            BongHode.B_Id ';'
            TransLogg.Dato ';'
            TransLogg.bongId ';'
            TransLogg.BongLinjeNr ';'
            TransLogg.TransNr ';'
            ArtBas.Beskr ';'
            ArtBas.LevKod ';'
            ArtBas.LevFargKod ';'
            Translogg.Storl ';'
            Translogg.KundNr ';'
            Translogg.Antall ';'
            TransLogg.Pris ';'
            (TransLogg.Antall * TransLogg.Pris) ';'
            TransLogg.VVareKost ';'
            (TransLogg.Antall * TransLogg.VVareKost) ';'
            FakturaLinje.Antall ';'
            FakturaLinje.LinjeSum ';'
            FakturaHode.FakturaNr ';'
            FakturaHode.Dato ';'
            FakturaHode.totalt
        SKIP.
END.
OUTPUT Stream Ut CLOSE.
