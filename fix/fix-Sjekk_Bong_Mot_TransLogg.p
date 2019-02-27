CURRENT-WINDOW:WIDTH = 300.
FOR EACH BongHode WHERE 
    BongHode.ButikkNr = 1 AND 
    BongHode.Dato >= 10/21/2012 , 
    EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      CAN-DO("1,2,3,4,10,119",STRING(BongLinje.TTID)) AND
     BongLinje.Makulert = FALSE AND 
     NOT CAN-FIND(TransLogg NO-LOCK WHERE
        TransLogg.Butik    = BongLinje.ButikkNR AND
        TransLogg.TransNr  = BongLinje.TransNr AND
        TransLogg.SeqNr    = BongLinje.SeqNr):


    DISPLAY
        BongLinje.Makulert
        BongLinje.TTID
        BongHode.ButikkNr
        BongLinje.TransNr
        BongLinje.SeqNr
        BongHode.Dato
        BongHode.SelgerNr
        WITH WIDTH 300.
    /*
    - Sett tilbake bongstatus
    - Sett tilbake selgerid
    - Sett tilbake oppd.flagg på datasett.
    
    */
END.
