/* fix-SlettKampanje.p */
FOR EACH KampanjeHode:
    FOR EACH Kampanjelinje OF KampanjeHode:
        DELETE kampanjelinje.
    END.
    FOR EACH KampanjeButKobling  WHERE 
        KampanjeButKobling.KampanjeId = KampanjeHode.KampanjeId:
        DELETE KampanjeButKobling.
    END.
    DELETE KampanjeHode.
END.
FOR EACH KampanjeMixMatch:
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
END.
