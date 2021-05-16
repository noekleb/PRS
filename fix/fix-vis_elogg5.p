CURRENT-WINDOW:WIDTH = 350.
FOR EACH ELogg NO-LOCK WHERE 
    ELogg.TabellNavn     = 'KOrdreHode' AND
    ELogg.EksterntSystem = "WEBBUT":
    
    DISPLAY
    ELogg
    WITH WIDTH 350.
END.
