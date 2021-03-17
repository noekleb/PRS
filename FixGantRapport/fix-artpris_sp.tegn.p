FOR EACH ArtPris WHERE 
    ArtPris.ArtikkelNr = 9833576 AND 
    ArtPris.ProfilNr = 1:

    DISPLAY
        ArtPris.Db%[1]   
        ArtPris.MvaKr[1] 
        ArtPris.DbKr[1]  
        .
    FOR EACH HPrisKo OF ArtBas:
        DELETE HPrisKo.
    END.
END.
