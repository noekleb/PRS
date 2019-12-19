DELETE FROM prisko.
FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.Tilbud = TRUE:
    ArtPris.tilbud = FALSE.
END.
