CURRENT-WINDOW:WIDTH = 350.
FOR EACH Translogg WHERE 
    butik = 15 AND
    ttid  = 1 AND
    Dato >= 01/01/2017:
    FIND ArtBas WHERE
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    FIND StrKonv NO-LOCK WHERE
        StrKonv.Storl = TransLogg.Storl NO-ERROR.
    /*     
    DISPLAY
        Translogg.Dato
        butik 
        ttid
        Translogg.Ordreforslag
        Translogg.Antall
        AVAILABLE ArtBas 
        ArtBas.Pakke WHEN AVAILABLE ArtBas         
        ArtBas.OPris WHEN AVAILABLE ArtBAs
        ArtBAs.Pant WHEN AVAILABLE ArtBas
        AVAILABLE StrKonv
        '|'
        ArtBas.KjedeVare WHEN AVAILABLE ArtBas
        ArtBas.Grunnsortiment WHEN AVAILABLE ArtBas
    WITH WIDTH 350.
    */

    Translogg.Ordreforslag = FALSE.
    IF AVAILABLE ArtBas THEN
        ASSIGN 
        ArtBas.Grunnsortiment = TRUE
        ArtBas.KjedeVare = TRUE
        .

END.
