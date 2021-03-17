ASSIGN
    CURRENT-WINDOW:WIDTH = 200.

TRANSLOOP:
FOR EACH TransLogg NO-LOCK WHERE
    TransLogg.BatchNr = 5356 AND
    TransLogg.Dato    = 05/01/2000 AND
    Translogg.Butik   = 6 AND
    TransLogg.Kassa   = 2:

    find TelleLinje exclusive-lock where
      TelleLinje.TelleNr    = 6001 and
      TelleLinje.ArtikkelNr = TransLogg.ArtikkelNr and
      TelleLinje.Butik      = TransLogg.Butik and
      TelleLinje.Storl      = TransLogg.Storl no-error.
    IF NOT AVAILABLE TelleLinje THEN
        NEXT TRANSLOOP.

/*
    DISPLAY 
        TransLogg.BatchNr
        TransLogg.ArtikkelNr
        TransLogg.TTId
        TransLogg.Dato
        TransLogg.Butik
        TransLogg.Kassa
        TransLogg.Vg
        TransLogg.LopNr
        TransLogg.Storl
        TransLogg.Antall
        TelleLinje.AntallPar
        TelleLinje.TelleNr
        (TransLogg.Antall + TelleLinje.AntallPar) COLUMN-LABEL "KorrAnt"
    with WIDTH 198.
*/
END. /* TRANSLOOP */
