CURRENT-WINDOW:WIDTH = 300.
FOR EACH KampanjeLinje WHERE 
    CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = Kampanjelinje.ArtikkelNr):

    IF NOT CAN-DO("11",STRING(KampanjeLinje.KampanjeId)) THEN
        NEXT.

    FIND KampanjeHode OF KampanjeLinje NO-LOCK.
    FIND ArtBas WHERE 
        ArtBas.ArtikkelNr = KampanjeLinje.Artikkelnr NO-LOCK.
    FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 16.

    DISPLAY
        KampanjeLinje.KampanjeId
        KampanjeHode.AvslagType
        KampanjeHode.NormalPris
        Kampanjelinje.ArtikkelNr
        ArtBas.Beskr
        Kampanjelinje.Pris[2]
        KampanjeHode.Kamp%
        ArtPris.Pris[1]
        
        ((Kampanjelinje.Pris[2] - ArtPris.Pris[1]) * 100) / ArtPris.Pris[1] 

        Kampanjelinje.Varekost
        ArtPris.VareKost[1]
        KampanjeLinje.MvaKr
        Kampanjelinje.Mva%
    WITH WIDTH 300.

    KampanjeLinje.Pris = ArtPris.Pris[1] - ((ArtPris.Pris[1] * 30) / 100).

   
END.
