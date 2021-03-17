CURRENT-WINDOW:WIDTH = 350.

FOR EACH Tilgode NO-LOCK WHERE 
    Tilgode.BruktDato >= 01/01/2020 AND 
    Tilgode.BruktTid >= 0 AND
    Tilgode.BruktButNr > 0:

    IF Tilgode.BruktButNr = Tilgode.ButNr THEN 
        NEXT.

    FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = Tilgode.BruktSelgerNr NO-ERROR.
    DISPLAY
        /*
        tilgode.identType
        Tilgode.Dato
        Tilgode.Modus
        tilgode.Dato
        STRING(tilgode.Tid,"HH:MM:SS")
        tilgode.KasseNr
        tilgode.KassNr
        Tilgode.BongNr
        '|'
        */ 
        Tilgode.BruktDato COLUMN-LABEL 'Brukt'
        STRING(Tilgode.BruktTid,"HH:MM:SS") COLUMN-LABEL 'Tid'
        Tilgode.BruktButNr COLUMN-LABEL 'Butikk'
        Tilgode.BruktBongNr FORMAT ">>>>>9" COLUMN-LABEL 'BongNr'
        Tilgode.BruktKasseNr
        Tilgode.BruktSelgerNr
        Selger.Navn WHEN AVAILABLE Selger
        tilgode.IdentNr FORMAT "x(30)"
        Tilgode.Belop
        Tilgode.butNr COLUMN-LABEL 'Opprettet i'
        Tilgode.RegistrertDato COLUMN-LABEL 'Registrert'
        STRING(tilgode.RegistrertTid,"HH:MM:SS") COLUMN-LABEL 'Tid'

    WITH WIDTH 350.
END.
