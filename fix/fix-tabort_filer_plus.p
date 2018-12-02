FOR EACH filer WHERE filer.filid >= 52454 AND filnavn BEGINS "journal.001" AND
                         filer.filid <= 52859:
    IF filer.filid = 52778 OR filer.filid = 52784 THEN
        NEXT.
    FOR EACH fillinjer WHERE fillinjer.filid = filer.filid:
        DELETE fillinjer.
    END.
    FOR EACH fillogg WHERE fillogg.filid = filer.filid:
        DELETE fillogg.
    END.
    DELETE filer.
END.


