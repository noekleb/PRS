TRIGGER PROCEDURE FOR DELETE OF KampanjeMixMatch.

FOR EACH KampanjeTilbud OF KampanjeMixMatch:
    FOR EACH KampanjeTilbArtikkel OF KampanjeTilbud:
        DELETE KampanjeTilbArtikkel.
    END.
    FOR EACH KampTilbTemplate OF KampanjeTilbud:
        DELETE KampTilbTemplate.
    END.
    DELETE KampanjeTilbud.
END.
FOR EACH KampanjeButikk OF KampanjeMixMatch:
    DELETE KampanjeButikk.
END.
FOR EACH KampanjeButMottak OF KampanjeMixMatch:
    DELETE KampanjeButMottak.
END.

