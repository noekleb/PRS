CURRENT-WINDOW:WIDTH = 350.
FOR EACH Translogg WHERE Postert = FALSE:
    IF translogg.Ovbutik = 0 THEN
    DO:
        DISPLAY
            Translogg.Dato
            Translogg.ttid
            translogg.butik
            translogg.ovbutik
            Translogg.ArtikkelNr
            TransLogg.Bongtekst
            TransLogg.BAtchNr
            WITH WIDTH 350.
        /*DELETE translogg.*/
        Translogg.Ovbut = 16.
    END.

END.
