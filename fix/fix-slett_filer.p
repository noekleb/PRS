FOR EACH Filer WHERE dato >= 03/01/2013:
    FOR EACH Fillinje OF Filer:
        DELETE Fillinje.
    END.
    DELETE Filer.
END.
