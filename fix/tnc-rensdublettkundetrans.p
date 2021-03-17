
CURRENT-WINDOW:WIDTH = 200.
                          
DEF BUFFER bkundetrans FOR kundetrans.

    FOR EACH KundeTrans EXCLUSIVE-LOCK WHERE
        KundeTrans.Butik < 20 AND
        KundeTrans.BongLinje = 0:
        FIND FIRST bKundeTrans NO-LOCK WHERE
            bKundeTrans.KundeNr   = kundetrans.kundenr AND
            bKundeTrans.Butik     = kundetrans.butik AND
            bKundeTrans.KassaNr   = kundetrans.kassanr AND
            bKundeTrans.Dato      = kundetrans.dato      AND
            bKundeTrans.BongId    = kundetrans.BongId AND
            bKundeTrans.BongLinje > 0 and
            bKundeTrans.Vg        = KundeTrans.Vg AND
            bKundeTrans.LopNr     = KundeTrans.LopNr AND
            bKundeTrans.Storl     = KundeTrans.Storl AND
            recid(bKundeTrans)    <> recid(KundeTrans) NO-ERROR.


        PAUSE 0.
        DISPLAY
            KundeTrans.KundeNr
            KundeTrans.Butik
            KundeTrans.KassaNr
            KundeTrans.Dato
            KundeTrans.BongId
            KundeTrans.BongLinje
            "*" WHEN AVAILABLE bKundeTrans
            WITH WIDTH 198.
        
        /* Bort med dubletten. */
        IF AVAILABLE bKundeTrans THEN
            DELETE KundeTrans.
    END.
