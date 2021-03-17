FOR EACH ArtPris WHERE
    InnkjopsPris[1] = 0 AND
    InnkjopsPris[2] = 0 AND
    VareKost[1] = 0 AND
    VareKost[2] = 0 AND
    Pris[1] = 0 AND
    Pris[2] = 0:

    DELETE ArtPris.
    /*
    DISPLAY
        ArtikkelNr
        ProfilNr
        InnkjopsPris[1]  
        InnkjopsPris[2]  
        VareKost[1] 
        VareKost[2] 
        Pris[1]     
        Pris[2]     
        .
     */
END.
