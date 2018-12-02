FOR EACH Artlag WHERE
    Artlag.ArtikkelNr = 0 
    /*
    Artlag.Vg = 1 AND
    ArtLAg.LopNr = 4010 AND
    ArtLag.ArtikkelNr = 0*/ :

    FIND ArtBas NO-LOCK WHERE
        ArtBAs.Vg    = Artlag.Vg AND
        ArtBAs.LopNr = Artlag.LopNr NO-ERROR.

    IF AVAILABLE ArtBas THEN
        ArtLag.ArtikkelNr = ArtBas.ArtikkelNr.


    /*
    DISPLAY
        Artlag.Vg
        Artlag.LopNr
        Artlag.ArtikkelNR
        (IF AVAILABLE ArtBAs
           THEN ArtBas.ArtikkelNr
           ELSE 0)
        .
    */
END.
