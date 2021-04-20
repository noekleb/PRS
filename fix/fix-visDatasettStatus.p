DEF VAR iant AS INT NO-UNDO.
CURRENT-WINDOW:WIDTH = 350.

FOR EACH DataSett EXCLUSIVE-LOCK WHERE 
    DataSett.ButikkNr = 15 AND 
    DataSett.Dato = 08/07/2020, 
    FIRST BongHode OF DataSett NO-LOCK
    /*
    
    DataSett.SettStatus >= 2 AND 
    DataSett.SettStatus <= 8 AND
    DataSett.Behandlet  >= 3 AND
    DataSett.Behandlet  <= 4 AND 
    */
    
    /*
    DataSEtt.FilId      >= 3842698 AND 
    DataSett.FilId      <= 3842702
    DataSett.FilId      > 3843613
    */
    
    /*
    DataSett.SettNr     >= 9 AND 
    DataSett.SettNr     <= 15
    */
    :
    
    iant = iant + 1.
       
    DISPLAY
      DataSett.ButikkNr FORMAT ">>>>>9" LABEL "Butikknummer"
      DataSett.GruppeNr FORMAT ">9" LABEL "Gruppenummer"
      DataSett.KasseNr FORMAT ">>9" LABEL "KasseNr"
      DataSett.DataSettId FORMAT ">>>>>>>>>>>>>9" LABEL "DatasettId"
      DataSett.Dato FORMAT "99/99/99" LABEL "Dato"
      DataSett.STRING(Tid,"HH:MM:SS")
      DataSett.SettNr FORMAT ">>>9"
      DataSett.SettStatus FORMAT "9" LABEL "SettStatus"
      DataSett.FilId FORMAT ">>>>>>>>>>>>9" LABEL "FilId"
      DataSett.FilType FORMAT ">9" LABEL "FilType"
      DataSett.Behandlet FORMAT ">9" LABEL "Behandlet status"
      DataSett.AntallLinjer FORMAT "->,>>>,>>9" LABEL "Antall linjer"
      DataSett.pfFlagg FORMAT ">9" LABEL "Status ProfitBase"
    WITH WIDTH 350.
    FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK /*WHERE
        BongHode.BongStatus >= 5 AND
        BongHode.BongStatus <= 6*/:

        DISPLAY
            BongHode.butikkNr
            BongHode.GruppeNr
            BongHode.KasseNr
            BongHode.Dato
            BongHode.BongNr
            BongHode.BongStatus
        .
        
      FOR EACH BongLinje NO-LOCK WHERE 
          BongLinje.B_Id = BongHode.B_id:

           DISPLAY
             BongHode.ButikkNr
             BongHode.Dato
             BongHode.BongNr
             bonglinje.ttid
             bonglinje.tbid
             BongLinje.ArtikkelNr
           WITH WIDTH 350.
       END. 
        
    END.

END.


MESSAGE iant
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
