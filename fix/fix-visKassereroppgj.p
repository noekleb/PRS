CURRENT-WINDOW:WIDTH = 350.
FOR EACH KassererOppgj NO-LOCK WHERE 
        ButikkNr = 6  AND 
        dato = 04/25/2020:
    
    DISPLAY
        /* Idx */
        KassererOppgj.ButikkNr                COLUMN-label 'ButikkNr'               
        KassererOppgj.Dato                    column-label 'Dato'    
        KassererOppgj.KassererNr              COLUMN-LABEL 'KassererNr'
        KassererOppgj.z_Nummer                COLUMN-LABEL 'z_nummer'
        /* Idx slutt */
        KassererOppgj.SelgerNr
        KassererOppgj.SelgerId
        /* SelgerSlutt */
        KassererOppgj.OpptaltVeksel           column-label 'OpptaltVeksel'           
        KassererOppgj.OpptaltKontanter        column-label 'OpptaltKontanter'        
        KassererOppgj.OpptaltSjekk            column-label 'OpptaltSjekk'            
        KassererOppgj.OpptaltReserve          column-label 'OpptaltReserve'          
        KassererOppgj.OpptaltGavekort         column-label 'OpptaltGavekort'         
        KassererOppgj.OpptaltTilgode          column-label 'OpptaltTilgode'          
        KassererOppgj.OpptaltGaveKortAndre    column-label 'OpptaltGaveKortAndre'    
        KassererOppgj.OpptaltGavekortUtlevert column-label 'OpptaltGavekortUtlevert' 
        KassererOppgj.OpptaltTilgodeAndre     column-label 'OpptaltTilgodeAndre'     
        KassererOppgj.OpptaltTilgodeUtlevert  column-label 'OpptaltTilgodeUtlevert'  
        KassererOppgj.OpptaltInnVeksel        column-label 'OpptaltInnVeksel'        
        KassererOppgj.OpptaltLevertBank       column-label 'OpptaltLevertBank'       
        /*
        KassererOppgj.OpptaltValuta           column-label 'OpptaltValuta'           
        KassererOppgj.OpptaltBilag            column-label 'OpptaltBilag'            
        KassererOppgj.OpptaltFinansiering     column-label 'OpptaltFinansiering'     
        KassererOppgj.OpptaltUtbetalt         column-label 'OpptaltUtbetalt'         
        KassererOppgj.OpptaltKupong           column-label 'OpptaltKupong'           
        */
    WITH WIDTH 350.
END.
