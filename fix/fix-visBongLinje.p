CURRENT-WINDOW:WIDTH = 350.
FOR EACH BongLinje NO-LOCK WHERE 
    BongLinje.ButikkNr = 2 AND 
    BongLinje.GruppeNr = 1 AND 
    BongLinje.KasseNr >= 1 AND 
    BongLinje.Dato = 04/28/2020 AND 
    BongLinje.TTId = 108 USE-INDEX BongLinje,
    FIRST BongHode NO-LOCK WHERE 
        BongHode.B_Id = BongLinje.B_Id
    :

    FIND FIRST Kas_Rap WHERE 
         Kas_Rap.Dato = BongLinje.Dato AND 
         Kas_Rap.Butik = BongLinje.butik AND 
         Kas_Rap.Kasse = BongLinje.KasseNr AND 
         kas_rap.KassererNr = INT(BongHode.KassererNr) NO-ERROR.

    FIND FIRST KassererOppgj NO-LOCK WHERE 
        KassererOppgj.ButikkNr = BongLinje.Butik AND 
        KassererOppgj.Dato = bongLinje.Dato AND 
        KassererOppgj.KassererNr = 0 NO-ERROR.

    DISPLAY
        BongLinje.ButikkNr
        BongLinje.GruppeNr
        BongLinje.KasseNr
        BongLinje.Dato
        BongLinje.TTId
        KassererOppgj.OpptaltVeksel WHEN AVAILABLE KassererOppgj
        KassererOppgj.OpptaltInnVeksel WHEN AVAILABLE KassererOppgj       
        '|'
        Kas_Rap.VekselBeholdning WHEN AVAILABLE Kas_Rap
    WITH WIDTH 350.

END.
