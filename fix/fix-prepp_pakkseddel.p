FIND PkSdlHode WHERE PkSdlId = 100104.
PkSdlHode.PkSdlOpphav = 3.
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
    FOR EACH ArtPris WHERE 
        ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
        ArtPris.ProfilNr = 16:
        DELETE ArtPris.
    END.
END.
