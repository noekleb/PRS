CURRENT-WINDOW:WIDTH = 350.

DEF BUFFER bufArtPris FOR ArtPris.

FOR EACH PkSdlHode NO-LOCK, 
    EACH PkSdlMottak NO-LOCK WHERE 
         PkSdlMottak.PkSdlId    = PkSdlHode.PkSdlId AND
         PkSdlMottak.MotTattDato = 11/09/2016,
    EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE
         PkSdlLinje.ButikkNr = 16,
    FIRST ArtPris EXCLUSIVE-LOCK WHERE
        ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
    ArtPris.ProfilNr = PkSdlLinje.butikkNr:

    FIND bufArtPris NO-LOCK WHERE
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr = 1 NO-ERROR.
   
    DISPLAY
        PkSdlHode.PkSdlNr
        PksdlMottak.MotTattDato
        PkSdlLinje.butikkNr
        PkSdlLinje.ArtikkelNr
        PkSdlLinje.Kode
        ArtPris.InnkjopsPris[1]
        ArtPris.Rab1%[1]
        ArtPris.Pris[1]
        '|'
        bufArtPris.InnkjopsPris[1]
        bufArtPris.Rab1%[1]
        bufArtPris.Pris[1]
    WITH WIDTH 350.
   

    /* Fiker kalkylen */
    /*
    BUFFER-COPY bufArtPris
        EXCEPT ProfilNr
        TO ArtPris.
    */
END.
