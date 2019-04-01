CURRENT-WINDOW:WIDTH = 350.

OUTPUT TO VALUE('pakkseddelpriser_feil_artikkel20.txt').
FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 20 AND 
    PkSdlHode.RegistrertDato >= 01/01/2017,
    EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK:

    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
        PkSdlLinje.LevKod = PkSdlPris.LevKod AND
        PkSdlLinje.LevFargKod = PkSdlPris.LevFargKod AND 
        PkSdlLinje.Beskr = PkSdlPris.Beskr        
        NO-ERROR.

    IF (AVAILABLE PkSdlLinje AND (PkSdlLinje.ArtikkelNr <> PkSdlPris.ArtikkelNr))THEN
    DO:  
        DISPLAY
            PkSdlHode.RegistrertDato
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlStatus
            PkSdlLinje.butikkNr
            '|'
            PkSdlLinje.Beskr
            PkSdlLinje.LevKod
            PkSdlLinje.LevFargKod
            PkSdlLinje.ArtikkelNr
            '|'
            PkSdlPris.ArtikkelNr
            PkSdlPris.Beskr
            PkSdlPris.LevKod
            PkSdlPris.LevFargKod
            AVAILABLE PkSdlLinje
            '*' WHEN (PkSdlLinje.ArtikkelNr <> PkSdlPris.ArtikkelNr)
        WITH WIDTH 350.
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr.
    END.

END.
