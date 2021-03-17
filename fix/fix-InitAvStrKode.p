DEF VAR piStrKode AS INT NO-UNDO.

FOR EACH ArtLag:
    IF NOT CAN-find(StrKonv WHERE
      StrKonv.Storl = ArtLag.Storl) THEN
    DO:
        FIND LAST StrKonv NO-LOCK USE-INDEX StrKode NO-ERROR.
        IF AVAILABLE StrKonv THEN
            piStrKode = StrKonv.StrKode + 1.
        ELSE
            piStrKode = 1.
        CREATE StrKonv.
        ASSIGN
            StrKonv.Storl   = ArtLAg.Storl
            StrKonv.StrKode = piStrKode
            StrKonv.Merknad = "Initiert automatisk"
            ArtLAg.StrKode  = piStrKode
            .
    END.
END.

FOR EACH StrTStr:
    IF NOT CAN-find(StrKonv WHERE
      StrKonv.Storl = StrTStr.SoStorl) THEN
    DO:
        FIND LAST StrKonv NO-LOCK USE-INDEX StrKode NO-ERROR.
        IF AVAILABLE StrKonv THEN
            piStrKode = StrKonv.StrKode + 1.
        ELSE
            piStrKode = 1.
        CREATE StrKonv.
        ASSIGN
            StrKonv.Storl   = StrTStr.SoStorl
            StrKonv.StrKode = piStrKode
            StrKonv.Merknad = "Initiert automatisk"
            .
    END.
END.
