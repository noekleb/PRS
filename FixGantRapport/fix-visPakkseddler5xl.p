DEF VAR cOrgRecord AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR cRecord    AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.


DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    ibutNr     = 16
    cFil       = 'konv\VMOT' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.' + STRING(iButNr)
    cOrgRecord = "&ArtikkelNr;&Qty;'&Dato &Tid';&ButNr;&Kode;System;;;1;&ButNr;4;&OrdreNr;&LevNr;&Modell"
    .

OUTPUT STREAM Ut TO VALUE(cFil).
FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PksdlStatus = 20,
    FIRST PkSdlMottak OF PkSdlHode NO-LOCK WHERE 
        PkSdlMottak.MottattDato >= 01/01/2019,
    EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK WHERE 
        /*PkSdlLinje.ArtikkelNr = 9842435 AND*/
    PkSdlLinje.butikkNr = 16,
    FIRST Strekkode NO-LOCK WHERE 
        Strekkode.Kode = PkSdlLinje.Kode,
    FIRST ArtBas OF Strekkode NO-LOCK
    :

    IF PkSdlHode.PkSdlNr = '194075' THEN
        NEXT.

    FIND StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
    IF (Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr) THEN
    DO:
        /*
        DISPLAY
            PkSdlHode.PkSdlNr
            PkSdlLinje.LinjeNr
            PkSdlLinje.butikkNr
            PkSdlMottak.MottattDato
            PkSdlLinje.LevKod
            PkSdlLinje.LevFargKod
            StrKonv.Storl
            PkSdlLinje.Antall
            PkSdlHode.EDato
            PkSdlLinje.ArtikkelNr
            PkSdlLinje.Kode
            Strekkode.ArtikkelNr
            (Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr)
        WITH WIDTH 350.
        */

        ASSIGN 
            cRecord = cOrgRecord
            cRecord = REPLACE(cRecord,'&Dato',STRING(PkSdlMottak.MottattDato))
            cRecord = REPLACE(cRecord,'&Tid',STRING(PkSdlMottak.MottattTid,"HH:MM:SS"))
            cRecord = REPLACE(cRecord,'&ButNr',STRING(iButNr))
            cRecord = REPLACE(cRecord,'&LevNr',STRING(PkSdlLinje.LevNr))
            cRecord = REPLACE(cRecord,'&Qty',STRING(PkSdlLinje.Antall))
            cRecord = REPLACE(cRecord,'&Artikkelnr',STRING(Strekkode.ArtikkelNr))
            cRecord = REPLACE(cRecord,'&Kode',Strekkode.Kode)
            cRecord = REPLACE(cRecord,'&OrdreNr','999999')
            cRecord = REPLACE(cRecord,'&Modell',ArtBas.LevKod)
            .

        PUT STREAM Ut UNFORMATTED
            cRecord
            SKIP.
    END.
END.
OUTPUT STREAM Ut CLOSE.
