CURRENT-WINDOW:WIDTH = 300.

DEF BUFFER bArtPris FOR ArtPris.

FOR EACH ArtPris WHERE 
    ArtPris.ProfilNr = 1 AND
    /*ArtPris.ArtikkelNr = 40099026 AND*/
    ArtPris.InnkjopsPris[1] = 1 NO-LOCK:

    FIND ArtBas OF ArtPris NO-LOCK.

    FIND bArtPris NO-LOCK WHERE
        bArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bArtPris.ProfilNr = 1 NO-ERROR.

    DISPLAY
        ArtPris.ArtikkelNr
        ArtBas.LevKod
        ArtBas.Beskr
        ArtPris.ProfilNr
        ArtPris.InnkjopsPris[1]
        ArtPris.Db%[1]
        bArtPris.Db%[1]
        ArtPris.Db%[1] - bArtPris.Db%[1]
        ArtPris.Pris[1]
        ArtPris.EDato
        ArtPris.RegistrertDato
    WITH WIDTH 300.
END.
