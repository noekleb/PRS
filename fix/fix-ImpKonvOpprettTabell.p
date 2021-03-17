DEF BUFFER bImpKonv FOR ImpKonv.
CURRENT-WINDOW:WIDTH = 350.
FOR EACH ImpKonv EXCLUSIVE-LOCK WHERE 
    ImpKonv.EDB-System = 'GANT Global' AND
    ImpKonv.Tabell     = "Regnskapsavd"
    :

    CREATE bImpKonv.
    BUFFER-COPY ImpKonv
        EXCEPT Tabell
        TO bImpKonv
        ASSIGN 
            bImpKonv.Tabell = "VareflytAvdeling"
            .
    DISPLAY
        ImpKonv
    WITH WIDTH 350.

END.
