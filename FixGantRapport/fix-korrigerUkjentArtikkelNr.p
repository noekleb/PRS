DEF VAR cOrgRecord AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR cRecord    AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.


CURRENT-WINDOW:WIDTH = 350.
DEF BUFFER bufArtBas FOR ArtBas.

FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PksdlStatus = 15,
    FIRST PkSdlMottak OF PkSdlHode NO-LOCK,
    EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK,
    FIRST Strekkode NO-LOCK WHERE 
        Strekkode.Kode = PkSdlLinje.Kode,
    FIRST ArtBas OF Strekkode NO-LOCK,
    FIRST PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK WHERE 
        PkSdlPris.LevKod = PkSdlLinje.LevKod AND
        PkSdlPris.LevFargKod = PkSdlLinje.LevFargKod AND 
        PkSdlPris.Beskr      = PkSdlLinje.Beskr
    :

    FIND bufArtBas WHERE 
        BufArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
    FIND StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
    IF (PkSdlPris.ArtikkelNr <> PkSdlLinje.ArtikkelNr) THEN
    DO:

        DISPLAY
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlStatus
            PkSdlLinje.LinjeNr
            PkSdlLinje.butikkNr
            PkSdlMottak.MottattDato
            PkSdlLinje.LevKod
            PkSdlLinje.LevFargKod
            StrKonv.Storl
            PkSdlLinje.Antall
            PkSdlHode.EDato
            PkSdlLinje.Kode
            PkSdlLinje.ArtikkelNr
            Strekkode.ArtikkelNr
            (Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr)
            PkSdlPris.ArtikkelNr
            (PkSdlPris.ArtikkelNr <> PkSdlLinje.ArtikkelNr)
        WITH WIDTH 350.

        ASSIGN
            PkSdlPris.ArtikkelNr  = Strekkode.ArtikkelNr
            PkSdlHode.PkSdlStatus = 10
            .

    END.
END.
