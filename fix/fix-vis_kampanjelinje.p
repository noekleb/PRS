
CURRENT-WINDOW:WIDTH = 300.

FOR EACH KampanjeLinje WHERE 
    Kampanjelinje.KampanjeId = 14:

    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr.

    DISPLAY
        KampanjeLinje.ArtikkelNr
        ArtBas.Beskr
        KampanjeLinje.VG
        KampanjeLinje.LopNr 
        Behandlet 
        Feilkode 
        KampanjeId 
        Mva% 
        MvaKr 
        KampanjeLinje.Pris[1] 
        KampanjeLinje.Pris[2] 
        ProfilNr 
        VareKost 
    WITH WIDTH 300.
END.
