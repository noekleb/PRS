TRIGGER PROCEDURE FOR DELETE OF Lager.

    DEF BUFFER trgArtLag FOR ArtLag.

    FOR EACH trgArtLag WHERE
        trgArtLag.ArtikkelNr = Lager.ArtikkelNr AND
        trgArtLag.Butik      = Lager.Butik:
      DELETE trgArtLag.
    END.
