FOR EACH Prisko:
    DELETE PrisKo.
END.
FOR EACH ArtPris WHERE 
    ArtPris.Tilbud = TRUE:
    ArtPris.tilbud = FALSE.
END.
