
DEF VAR piLoop AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.
FOR EACH StrKonv WHERE
    StrKonv.Merknad BEGINS "VPI"
    BY StrKonv.StrKode DESCENDING:
    PAUSE 0.
    DISPLAY
        StrKonv.StrKode FORMAT ">>>999"
        StrKonv.Storl
        StrKonv.Merknad
        WITH WIDTH 200.
    DELETE StrKonv.
    piLoop = piLoop + 1.
END.

DISPLAY piLoop.
