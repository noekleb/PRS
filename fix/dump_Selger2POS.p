DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
OUTPUT TO ".\kom\ut\posdump.txt".
FOR EACH Selger WHERE Selger.selgernr < 99999999 NO-LOCK:
    IF (TRIM(Selger.NavnIKasse) <> "" OR TRIM(Selger.Navn) <> "") THEN DO:
        IF CAN-FIND(butiker WHERE butiker.butik = Selger.butikknr) THEN DO:
            cString = "SELGER" + ";" + STRING(selger.selgernr) + ";" +
                                       (IF TRIM(Selger.NavnIKasse) <> "" THEN Selger.NavnIKasse ELSE ENTRY(1,TRIM(Selger.Navn)," ")) + ";" +
                                      STRING(Selger.Butikknr).
            PUT UNFORMATTED cString SKIP.
        END.
    END.
END.
