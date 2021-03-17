FOR EACH VarGr NO-LOCK:
    FIND HuvGr OF VarGr NO-LOCK.

    FOR EACH ArtBas OF VarGr NO-LOCK:
        IF ArtBas.Hg <> HuvGr.Hg THEN
        DISPLAY
            VarGr.Vg
            HuvGr.Hg
            ArtBas.ARtikkelNr
            ArtBas.Vg
            ArtBas.Hg
            .
    END.
END.
