CURRENT-WINDOW:WIDTH = 200.

DEF STREAM UtFil.

OUTPUT STREAM UtFil TO VALUE("Kjedestruktur.txt") NO-ECHO.
FOR EACH Butiker no-lock:
    FIND KjedensButikker WHERE
        kjedensbutikker.ButikkNr = Butiker.butik NO-ERROR.

    IF AVAILABLE Kjedensbutikker THEN
    DO:
        FIND Kjede NO-LOCK WHERE
            kjede.KjedeNr = KjedensButikker.KjedeNr NO-ERROR.
        FIND KjedeRegion OF Kjedensbutikker NO-ERROR.
        FIND KjedeDistrikt OF Kjedensbutikker NO-ERROR.
    END.

    PAUSE 0.
    DISPLAY
        Butiker.Butik
        Butiker.ButNamn
        (IF AVAILABLE kjedensbutikker
           THEN kjedensbutikker.KjedeNr
           ELSE 0)
        (IF AVAILABLE kjeden
           THEN kjede.KjedeNavn
           ELSE "*Ukjent") COLUMN-LABEL "Kjede"
        (IF AVAILABLE kjedensbutikker
           THEN kjedensbutikker.RegionNr
           ELSE 0)
        (IF AVAILABLE kjedeRegion
           THEN KjedeRegion.RegionNavn
           ELSE "*Ukjent") COLUMN-LABEL "Region"
        (IF AVAILABLE kjedensbutikker
           THEN kjedensbutikker.DistriktNr
           ELSE 0)
        (IF AVAILABLE kjedeDistrikt
           THEN Kjededistrikt.DistriktNavn
           ELSE "*Ukjent") COLUMN-LABEL "Distrikt"
        WITH WIDTH 198.

    PUT STREAM UtFil UNFORMATTED
        Butiker.Butik ";"
        Butiker.ButNamn ";"
        (IF AVAILABLE kjedensbutikker
           THEN kjedensbutikker.KjedeNr
           ELSE 0) ";"
        (IF AVAILABLE kjeden
           THEN kjede.KjedeNavn
           ELSE "*Ukjent") ";"
        (IF AVAILABLE kjedensbutikker
           THEN kjedensbutikker.RegionNr
           ELSE 0) ";"
        (IF AVAILABLE kjedeRegion
           THEN KjedeRegion.RegionNavn
           ELSE "*Ukjent") ";"
        (IF AVAILABLE kjedensbutikker
           THEN kjedensbutikker.DistriktNr
           ELSE 0) ";"
        (IF AVAILABLE kjedeDistrikt
           THEN Kjededistrikt.DistriktNavn
           ELSE "*Ukjent")
        SKIP.
END.
OUTPUT STREAM UtFil CLOSE.
