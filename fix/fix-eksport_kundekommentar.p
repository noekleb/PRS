CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR  NO-UNDO.

ASSIGN
    cFilNavn = 'Myhre_kundekommentar16042015.csv'.

DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

PUT STREAM Ut UNFORMATTED
    'KundeNr;KommentarId;SeqNr;Kommentar'
    SKIP.
FOR EACH KundeKommentar NO-LOCK:
    /*
    DISPLAY
        KundeKommentar.KundeNr
        KundeKommentar.KommentarId
        KundeKommentar.KundeKommentar FORMAT "x(200)"
        LOOKUP(CHR(10),KundeKommentar.KundeKommentar) 
        LOOKUP(CHR(13),KundeKommentar.KundeKommentar) 
        LENGTH(KundeKommentar.KundeKommentar) 
    WITH WIDTH 300.
    */
    IF LENGTH(KundeKommentar.KundeKommentar) <= 70 THEN
    DO:
        PUT STREAM Ut UNFORMATTED
            KundeKommentar.KundeNr ';'
            KundeKommentar.KommentarId ';'
            '1;'
            QUOTER(KundeKommentar.KundeKommentar)
        SKIP.
    END.
    ELSE IF LENGTH(KundeKommentar.KundeKommentar) > 70 AND
            LENGTH(KundeKommentar.KundeKommentar) <= 140
         THEN
    DO:
        PUT STREAM Ut UNFORMATTED
            KundeKommentar.KundeNr ';'
            KundeKommentar.KommentarId ';'
            '1;'
            QUOTER(SUBSTRING(KundeKommentar.KundeKommentar,1,70)) ';'
        SKIP.
        PUT STREAM Ut UNFORMATTED
            KundeKommentar.KundeNr ';'
            KundeKommentar.KommentarId ';'
            '2;'
            QUOTER(SUBSTRING(KundeKommentar.KundeKommentar,71))
        SKIP.
    END.
    ELSE
    DO:
        PUT STREAM Ut UNFORMATTED
            KundeKommentar.KundeNr ';'
            KundeKommentar.KommentarId ';'
            '1;'
            QUOTER(SUBSTRING(KundeKommentar.KundeKommentar,1,70)) ';'
        SKIP.
        PUT STREAM Ut UNFORMATTED
            KundeKommentar.KundeNr ';'
            KundeKommentar.KommentarId ';'
            '2;'
            QUOTER(SUBSTRING(KundeKommentar.KundeKommentar,71,70)) ';'
        SKIP.
        PUT STREAM Ut UNFORMATTED
            KundeKommentar.KundeNr ';'
            KundeKommentar.KommentarId ';'
            '3;'
            QUOTER(SUBSTRING(KundeKommentar.KundeKommentar,141))
        SKIP.
    END.
END.

OUTPUT STREAM Ut CLOSE.

