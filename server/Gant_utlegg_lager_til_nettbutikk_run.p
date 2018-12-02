DEF STREAM Inn.

FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.OPris             = FALSE AND
    ArtBas.Lager             = TRUE AND 
    ArtBas.WebButikkArtikkel = TRUE:
    FOR EACH Lager OF ArtBAs EXCLUSIVE-LOCK:
        ASSIGN
            Lager.ETid = TIME.
    END.
END.

QUIT.
