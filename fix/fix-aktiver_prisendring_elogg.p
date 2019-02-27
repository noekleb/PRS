CURRENT-WINDOW:WIDTH = 350.
FOR EACH KampanjeLinje NO-LOCK WHERE
      KampanjeId = 83:
    FIND ArtBas EXCLUSIVE-LOCK WHERE 
        Artbas.ArtikkelNr = KampanjeLinje.ArtikkelNr.
    /*
    DISPLAY
        KampanjeLinje.KampanjeId
        KampanjeLinje.ArtikkelNr
        ArtBas.Beskr FORMAT "x(40)"
    WITH WIDTH 350.
    */
    ASSIGN 
        ArtBas.EDato = TODAY
        ArtBas.ETid  = TIME
        .

END.
