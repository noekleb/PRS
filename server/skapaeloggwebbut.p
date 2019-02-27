DEFINE VARIABLE dDatum AS DATE        NO-UNDO.

DEFINE VARIABLE iButik AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE tt_artbas NO-UNDO
    FIELD artikkelnr AS DECI
    INDEX art IS PRIMARY UNIQUE artikkelnr.

iButik = 16.

DO dDatum = TODAY - 1 TO TODAY:
    FOR EACH translogg WHERE translogg.butik = iButik AND translogg.dato = dDatum NO-LOCK USE-INDEX ButDatoTid:
        FIND tt_artbas WHERE tt_artbas.artikkelnr = translogg.artikkelnr NO-ERROR.
        IF NOT AVAIL tt_artbas THEN DO:
            CREATE tt_artbas.
            ASSIGN tt_artbas.artikkelnr = translogg.artikkelnr.
        END.
    END.
END.

FOR EACH tt_artbas:
       FIND ELogg WHERE 
            ELogg.TabellNavn     = "ArtBas" AND
            ELogg.EksterntSystem = "POS"    AND
            ELogg.Verdier        = STRING(tt_artbas.artikkelnr) NO-ERROR NO-WAIT.
       IF LOCKED ELogg THEN
           NEXT.
       IF NOT AVAIL Elogg THEN DO:
       CREATE Elogg.
       ASSIGN ELogg.TabellNavn     = "ArtBas"
              ELogg.EksterntSystem = "WEBBUT"   
              ELogg.Verdier        = STRING(tt_artbas.artikkelnr).
       END.
       ASSIGN ELogg.EndringsType = 1
              ELogg.Behandlet    = FALSE.
       IF AVAILABLE ELogg THEN RELEASE ELogg.
END.
