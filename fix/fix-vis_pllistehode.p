CURRENT-WINDOW:WIDTH = 350.
FOR EACH PlListeHode EXCLUSIVE-LOCK WHERE 
    PlListeHode.PlListeId >= 14932 AND 
    PlListeHode.PlListeId <= 14951 AND
    PlListeHode.PlListeStatus >= 20 AND
    /*PlListeHode.DatoPlukket = ? AND*/
    PlListeHode.SendtPda >= 01/01/2017
    /*
    PlListeHode.SendtPda    = ? AND 
    PlListeHode.DatoPlukket = ? 
    RegistrertDato >= 12/17/2006 , 
    EACH PlListeLinje OF PlListeHode NO-LOCK */:
    DISPLAY
    PlListeHode.PlListeId
    PlListeHode.PlListeStatus
    PlListeHode.FraButikkNr
    PlListeHode.TilbutikkNr
    PlListeHode.PrioPlukket
    PlListeHode.SendtPda
    PlListeHode.DatoPlukket
    PlListeHode.OverfortDato
    PlListEHode.RegistrertDato
    
    /*
    PlListeLinje.PlLinjeNr
    PlListeLinje.ArtikkelNr
    PlListeLinje.Antall
    PlListeLinje.AntallPlukket
    */
    WITH WIDTH 350.
    
    ASSIGN
        PlListeHode.PlListeStatus = 10
        PlListeHode.DatoPlukket = ?
        PlListeHode.TidPlukket = 0
        PlListeHode.OverfortDato = ?
        PlListeHode.AntallPlukket = 0
        PlListeHode.BuntNr = 0
        .
END.
