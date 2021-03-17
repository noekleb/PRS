CURRENT-WINDOW:WIDTH = 300.

FOR EACH ButikkForsalj:
    DELETE ButikkForsalj.
END.
FOR EACH Forsalj:
    DELETE Forsalj.
END.

FOR EACH InfoPOS.Kasserer NO-LOCK:
    /*
    DISPLAY
    InfoPOS.Kasserer.butnr          
    InfoPOS.Kasserer.kassnr      
    InfoPOS.Kasserer.ansnr       
    InfoPOS.Kasserer.ansdato     
    InfoPOS.Kasserer.etternavn   
    InfoPOS.Kasserer.fornavn     
    InfoPOS.Kasserer.adresse     
    InfoPOS.Kasserer.postnr      
    InfoPOS.Kasserer.kasstekst   
    InfoPOS.Kasserer.passord     
    InfoPOS.Kasserer.fdato       
    InfoPOS.Kasserer.girabatt    
    InfoPOS.Kasserer.endrepris   
    InfoPOS.Kasserer.taretur     
    InfoPOS.Kasserer.slettidl    
    InfoPOS.Kasserer.slettbong   
    InfoPOS.Kasserer.slettforst  
    InfoPOS.Kasserer.bongpaavent 
    InfoPOS.Kasserer.gikreditt   
    InfoPOS.Kasserer.sperret     
    InfoPOS.Kasserer.teamnr      
    InfoPOS.Kasserer.telefon1    
    InfoPOS.Kasserer.telefon2    
    WITH WIDTH 300.
    */
    IF NOT CAN-FIND(FIRST Forsalj WHERE
                    Forsalj.ForsNr = int(string(InfoPOS.Kasserer.butnr) + STRING(InfoPOS.Kasserer.kassnr,'999'))) THEN
    DO:
        CREATE forsalj.
    END.
    ELSE FIND FIRST Forsalj EXCLUSIVE-LOCK WHERE
                    Forsalj.ForsNr = int(string(InfoPOS.Kasserer.butnr) + STRING(InfoPOS.Kasserer.kassnr,'999')).

    ASSIGN
        Forsalj.ForsNr         = int(string(InfoPOS.Kasserer.butnr) + STRING(InfoPOS.Kasserer.kassnr,'999'))
        Forsalj.ForsaljAktiv   = NOT InfoPOS.Kasserer.sperret
        Forsalj.ButikkNr       = InfoPOS.Kasserer.butnr          
        Forsalj.FoAnstNr       = InfoPOS.Kasserer.ansnr       
        Forsalj.RegistrertDato = InfoPOS.Kasserer.ansdato             
        Forsalj.FoNamn         = InfoPOS.Kasserer.fornavn + ' ' + InfoPOS.Kasserer.etternavn   
        Forsalj.FoAdr          = InfoPOS.Kasserer.adresse     
        Forsalj.FoPoNr         = string(InfoPOS.Kasserer.postnr)      
        Forsalj.navnikasse     = InfoPOS.Kasserer.kasstekst   
        Forsalj.Passord        = int(InfoPOS.Kasserer.passord)     
        Forsalj.FodtDato       = InfoPOS.Kasserer.fdato       
        Forsalj.Rabatt         = InfoPOS.Kasserer.girabatt    
        Forsalj.Prisendring    = InfoPOS.Kasserer.endrepris   
        Forsalj.Retur          = InfoPOS.Kasserer.taretur     
        Forsalj.SlettTidligere = InfoPOS.Kasserer.slettidl    
        Forsalj.SlettBong      = InfoPOS.Kasserer.slettbong   
        Forsalj.SletteForste   = InfoPOS.Kasserer.slettforst  
        /*InfoPOS.Kasserer.bongpaavent*/ 
        /*InfoPOS.Kasserer.gikreditt*/   
        /*InfoPOS.Kasserer.teamnr*/      
        Forsalj.FoTel          = InfoPOS.Kasserer.telefon1 + (IF TRIM(InfoPOS.Kasserer.telefon2) = '' THEN '' ELSE '/') +
                                 TRIM(InfoPOS.Kasserer.telefon2)
        .
    FIND FIRST ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.ForsNr     = Forsalj.ForsNr AND
        ButikkForsalj.Butik      = Forsalj.ButikkNr AND 
        ButikkForsalj.KassererId = InfoPOS.Kasserer.kassnr NO-ERROR.
    IF NOT AVAILABLE ButikkForsalj THEN
    DO:
        CREATE ButikkForsalj.
        ASSIGN
            ButikkForsalj.ForsNr     = Forsalj.ForsNr
            ButikkForsalj.Butik      = Forsalj.ButikkNr
            ButikkForsalj.KassererId = InfoPOS.Kasserer.kassnr
            .
    END.

END.
