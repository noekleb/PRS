/* 9826442 */
CURRENT-WINDOW:WIDTH = 300.
FOR EACH ELogg NO-LOCK WHERE 
    ELogg.TabellNavn = 'Artbas' AND 
    ELogg.EksterntSystem = 'WEBBUT':
    DISPLAY 
        ELogg.TabellNAvn
        ELogg.EksterntSystem
        NUM-ENTRIES(ELogg.Verdier,CHR(1))
        ELogg.Verdier
    WITH WIDTH 300.
   
END.


