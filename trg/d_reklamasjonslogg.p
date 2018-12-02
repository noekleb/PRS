TRIGGER PROCEDURE FOR DELETE OF ReklamasjonsLogg.

/* Sletter alle reklamasjonslinjene til reklamasjonen */
FOR EACH Reklamasjonslinje OF Reklamasjonslogg:
    DELETE Reklamasjonslinje.
END.


