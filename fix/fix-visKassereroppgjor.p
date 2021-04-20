CURRENT-WINDOW:WIDTH = 350.
FOR EACH KassererOppgj NO-LOCK WHERE 
        ButikkNr = 11 AND 
        dato = 05/28/2020:
    
    DISPLAY
        KassererOppgj.ButikkNr                COLUMN-label 'ButikkNr'               
        KassererOppgj.Dato                    column-label 'Dato'  
        KassererOppgj.KassererNr              COLUMN-LABEL 'KassererNr'
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
        KassererOppgj.OpptaltValuta           column-label 'OpptaltValuta'           
        KassererOppgj.OpptaltLevertBank       column-label 'OpptaltLevertBank'       
        KassererOppgj.OpptaltBilag            column-label 'OpptaltBilag'            
        KassererOppgj.OpptaltFinansiering     column-label 'OpptaltFinansiering'     
        KassererOppgj.OpptaltUtbetalt         column-label 'OpptaltUtbetalt'         
        KassererOppgj.OpptaltKupong           column-label 'OpptaltKupong'  
        KassererOppgj.RegistrertDato
        STRING(KassererOppgj.RegistrertTid,"HH:MM:SS")
        KassererOppgj.EDato
        STRING(KassererOppgj.ETid,"HH:MM:SS")
    WITH WIDTH 350.
END.

