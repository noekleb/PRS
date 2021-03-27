CURRENT-WINDOW:WIDTH = 450.
CURRENT-WINDOW:HEIGHT = 50.
FOR EACH BongLinje NO-LOCK /*WHERE 
    BongLinje.ButikkNr = 2 AND 
    BongLinje.GruppeNr = 1 AND 
    BongLinje.KasseNr >= 1 AND 
    BongLinje.Dato = 04/28/2020 AND 
    BongLinje.TTId = 108 USE-INDEX BongLinje */ ,
    FIRST BongHode NO-LOCK WHERE 
        BongHode.B_Id = BongLinje.B_Id
    BREAK BY BongLinje.ButikkNr
          BY BongLinje.Kassenr
          BY BongLinje.Dato
          BY BongLinje.BongNr:

    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = BongHode.Butik NO-ERROR.

    FIND FIRST Kas_Rap WHERE 
         Kas_Rap.Dato = BongLinje.Dato AND 
         Kas_Rap.Butikk = BongLinje.butik AND 
         Kas_Rap.Kasse = BongLinje.KasseNr AND 
         kas_rap.KassererNr = INT(BongHode.KassererNr) NO-ERROR.

    FIND FIRST KassererOppgj NO-LOCK WHERE 
        KassererOppgj.ButikkNr = BongLinje.Butik AND 
        KassererOppgj.Dato = bongLinje.Dato AND 
        KassererOppgj.KassererNr = 0 NO-ERROR.

    FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = DEC(BongLinje.ArtikkelNr) AND 
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        
    DISPLAY
        /* BongHode.B_Id */
        BongLinje.ButikkNr
        /* BongLinje.GruppeNr */
        BongLinje.KasseNr
        BongLinje.Dato
        BongLinje.BongNr FORMAT ">>>>>9"          
        BongLinje.LinjeNr
        BongLinje.TTId COLUMN-LABEL 'TTID'
        Bonglinje.TBId COLUMN-LABEL 'TBID'
        BongLinje.ArtikkelNr     
        BongLinje.Strekkode      
        /*
        BongLinje.VareGr         
        BongLinje.LopeNr         
        */
        BongLinje.Storrelse  FORMAT "x(6)"     
        BongLinje.BongTekst  FORMAT "x(10)"     
        BongLinje.VareGruppeNavn FORMAT "x(10)" 
        BongLinje.VVarekost FORMAT "->>>>9"     
        ArtPris.Pris[1] WHEN AVAILABLE ArtPris
        BongLinje.Mva%  FORMAT "->>>9"          
        /*
        BongLinje.FeilKode       
        BongLinje.NotatKode      
        BongLinje.RefNr          
        BongLinje.RefTekst  FORMAT "x(10)"      
        */
        BongLinje.Antall (TOTAL BY BongLinje.BongNr)               
        BongLinje.LinjeSum FORMAT "->>>>9" (TOTAL BY BongLinje.BongNr)      
        BongLinje.LinjeRab FORMAT "->>>9"  (TOTAL BY BongLinje.BongNr)           
        BongLinje.BongPris FORMAT "->>>>9"           
        BongLinje.MvaKr  (TOTAL BY BongLinje.BongNr)               
        /**BongLinje.MvaGr */
        /*BongLinje.MvaGruppeNavn  FORMAT "x(10)" */

        /*
        KassererOppgj.OpptaltVeksel WHEN AVAILABLE KassererOppgj
        KassererOppgj.OpptaltInnVeksel WHEN AVAILABLE KassererOppgj       
        '|'
        Kas_Rap.VekselBeholdning WHEN AVAILABLE Kas_Rap
        */
    WITH WIDTH 550.

END.
