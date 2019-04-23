DEF VAR cSkriver AS CHAR NO-UNDO.

ASSIGN 
    cSkriver = 'Snagit 2018'
    cSkriver = 'novaPDF 10'
    cSkriver = 'EPSON91C161 (XP-850 Series)'
    .
    
FOR EACH Butiker:
    ASSIGN 
        Butiker.RapPrinter = cSkriver
        Butiker.FakturaSkriver = cSkriver
        .
END.
