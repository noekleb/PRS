OUTPUT TO VALUE("ImpKonvUTV2.csv").

EXPORT DELIMITER ";"
    "EDato"
    "ETid"
    "BrukerID"
    "RDato"
    "RTid"
    "RAv"
    "EDB-System"
    "Tabell"
    "InterntId"
    "EksterntId"
    "Merknad"
    .
FOR EACH ImpKonv NO-LOCK:
    EXPORT DELIMITER ";"
        ImpKonv
        .
END.
