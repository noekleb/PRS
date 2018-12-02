FOR EACH ArtBas WHERE ArtBAs.ArtikkelNr <= 9999 AND
    ArtBAs.Opris = TRUE:

    FIND VArGr NO-LOCK WHERE
        varGr.Vg = INT(ArtBAs.ArtikkelNr) NO-ERROR.
    IF AVAILABLE VarGr THEN       
    DO: 
        DISPLAY
            ArtBAs.ArtikkelNr
            ArtBAs.Vg
            ArtBAs.OPris
            .
        FOR EACH ARtPris OF ArtBAs:
            DELETE ArtPris.
        END.
        FOR EACH Lager OF ArtBas:
            DELETE Lager.
        END.
        FOR EACH ArtLag WHERE
            ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:
            DELETE ArtLag.
        END.
        FOR EACH Strekkode OF ArtBAs:
            DELETE Strekkode.
        END.
        DELETE ArtBAs.
    END.

END.
