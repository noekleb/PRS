DEF VAR cTekst AS CHAR FORMAT "x(40)".

FOR EACH Kasse NO-LOCK:
    IF Kasse.ElJournalKatalog <> "" THEN
        cTekst = Kasse.ElJournalKatalog.
    ELSE 
        cTekst = Kasse.KvitteringKatalog.
    OS-CREATE-DIR VALUE(cTekst) NO-ERROR.
END.
