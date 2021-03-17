    DEFINE VARIABLE cstring AS CHARACTER  NO-UNDO.
    OUTPUT TO "CLIPBOARD".
 FOR EACH selger NO-LOCK.
     IF Selger.selgernr < 99999999 AND (TRIM(Selger.NavnIKasse) <> "" OR TRIM(Selger.Navn) <> "") THEN DO:
         IF CAN-FIND(butiker WHERE butiker.butik = Selger.butikknr) THEN DO:
             cString = "SELGER" + ";" + STRING(selger.selgernr) + ";" +
                                        (IF TRIM(Selger.NavnIKasse) <> "" THEN Selger.NavnIKasse ELSE ENTRY(1,TRIM(Selger.Navn)," ")) + ";" +
                                       STRING(Selger.Butikknr).
             PUT UNFORMATTED cString SKIP.
         END.
     END.
 END.
