TRIGGER PROCEDURE FOR DELETE OF Filer.

/* Sletter buffer for innlest fil. */
FOR EACH Fillinjer OF Filer:
    DELETE FilLinjer.
END.

/* Nullstiller loggen for filen */
FOR EACH FilLogg OF Filer:
    DELETE FilLogg.
END.


