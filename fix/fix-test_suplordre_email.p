FOR EACH PlListeHode WHERE 
    PlListeHode.PlListeId = 145,
    EACH PlListeLinje OF PlListeHode:

    PlListeHode.PlListeStatus = 1.
    
    /*
    DISPLAY
        PlListeHode.PlListeId
        plListeHode.PlListeStatus 
        plListeHode.PlLType       
        plListeHode.FraButikkNr   
        PlListeLinje.ArtikkelNr.
    */ 
END.
RUN generer_send_Pl_mail.p ('16').

