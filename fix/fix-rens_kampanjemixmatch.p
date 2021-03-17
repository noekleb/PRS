CURRENT-WINDOW:WIDTH = 200.
FOR EACH KampanjeMixMatch EXCLUSIVE-LOCK WHERE
    KampanjeMixMatch.KampEierId > 1:

    DISPLAY
        KampanjeMixMatch.KampId
        KampanjeMixMatch.KampEierId
        KampanjeMixMatch.KampNavn
        KampanjeMixMatch.KampKlar
        WITH WIDTH 200.


    FOR EACH KampanjeTilbArtikkel WHERE
        KampanjeTilbArtikkel.KampId = KampanjeMixMatch.KampId:
        DELETE 
            KampanjeTilbArtikkel.
    END.

    FOR EACH KampanjeButikker WHERE
        KampanjeButikker.KampId = KampanjeMixMatch.KampId:
        DELETE
            KampanjeButikker.
    END.

    FOR EACH KampanjeButMottak WHERE
        KampanjeButMottak.KampId = KampanjeMixMatch.KampId:
        DELETE
            KampanjeButMottak.
    END.    
    
    FOR EACH KampanjeTilbud WHERE
        KampanjeTilbud.KampId = KampanjeMixMatch.KampId:
        DELETE
            KampanjeTilbud.
    END.    
    
    DELETE KampanjeMixMatch.
END.
