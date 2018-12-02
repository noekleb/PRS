OUTPUT TO VALUE('c:\tmp\butikkfordeling.csv').
DEF VAR iLoop AS INT NO-UNDO.
DO iLoop = 1 TO 3:
    FOR EACH Avdeling NO-LOCK:
        PUT UNFORMATTED
            (IF iLoop = 1 THEN 'Liten' ELSE IF iLoop = 2 THEN 'Medium' ELSE 'Stor') ';'
            Avdeling.AvdelingNr ';'
            Avdeling.AvdelingNavn
            SKIP.
    END.
END.

