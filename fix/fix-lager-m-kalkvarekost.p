CURRENT-WINDOW:WIDTH = 200.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = 167 NO-ERROR.

FOR EACH Tellelinje WHERE
    TelleLinje.TelleNr = 1:


    FIND ArtBas NO-LOCK where
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr.
    FIND ArtPris OF ArtBas where
        ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.

    ASSIGN
        TelleLinje.OpptVerdi = TelleLinje.AntallTalt * ArtPris.VareKost[1]
        .

/*     DISPLAY                                               */
/*         TelleLinje.Butik                                  */
/*         TelleLinje.AntallTalt                             */
/*         TelleLinje.VVAreKost                              */
/*         Tellelinje.OpptVerdi                              */
/*         ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] */
/*         WITH WIDTH 198                                    */
/*         .                                                 */
END.
