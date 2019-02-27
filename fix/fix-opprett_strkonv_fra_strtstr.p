DEF VAR iStrKode AS INT NO-UNDO.

FIND LAST StrKonv USE-INDEX StrKode.
ASSIGN
    iStrKode = StrKonv.StrKode + 1.
CURRENT-WINDOW:WIDTH = 250.
FOR EACH StrTStr NO-LOCK WHERE
    NOT CAN-FIND(StrKonv where
                 StrKonv.Storl = StrTStr.SoStorl):
    DISPLAY StrTStr
        WITH WIDTH 250.
    CREATE StrKonv.
    ASSIGN
        StrKonv.StrKode = iStrKode
        StrKonv.Storl   = StrTStr.SoStorl
        iStrKode        = iStrKode + 1.
END.
