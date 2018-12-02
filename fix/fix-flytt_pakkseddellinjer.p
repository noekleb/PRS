FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10:

    FOR EACH PkSdlPris WHERE 
        PkSdlPris.PkSdlId = PkSdlHode.PkSdlId AND
        PkSdlPris.ArtikkelNr =  9849329:

        PkSdlPris.ArtikkelNr = 9849554.
    END.

    FOR EACH PkSdlLinje WHERE 
        PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId AND 
        PkSdlLinje.ArtikkelNr = 9849329:

        PkSdlLinje.ArtikkelNr = 9849554.
    END.

END.
