DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR itelleNr AS INT NO-UNDO.
DEF VAR ibutNr AS INT NO-UNDO.
DEF VAR cEan AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR iAntTalt AS INT NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR iLinjeNr AS INT NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cInnfil = 'konv\KorrigerTellingBut40_01022019.csv'
    iTelleNr = 28975
    iButNr = 40
    .

FIND TelleHode NO-LOCK WHERE 
    TelleHode.TelleNr = iTelleNr NO-ERROR.

INPUT STREAM Inn FROM VALUE(cInnFil).
OUTPUT STREAM Ut TO VALUE('konv\KorrigerTellingBut40_01022019.log').
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.
    iLinjeNr = iLinjeNr + 1.

    IF iLinjeNr = 1 THEN 
        NEXT.

    ASSIGN 
        cEan = TRIM(ENTRY(18,cRecord,';'))
        iAntTalt = INT(ENTRY(9,cRecord,';'))
        .

    IF AVAILABLE ArtBas THEN 
        RELEASE ArtBas.
    IF AVAILABLE StrKonv THEN
        RELEASE StrKonv.
    IF AVAILABLE telleLinje THEN
        RELEASE TelleLinje.

    FIND strekkode NO-LOCK WHERE 
        Strekkode.Kode = cEan NO-ERROR.
    IF AVAILABLE Strekkode THEN
    DO:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        FIND StrKonv NO-LOCK WHERE 
            StrKonv.strKode = Strekkode.StrKode NO-ERROR.
    END.

    IF AVAILABLE StrKonv AND AVAILABLE Strekkode THEN
        FIND TelleLinje EXCLUSIVE-LOCK WHERE 
            TelleLinje.TelleNr = TelleHode.TelleNr AND 
            TelleLinje.ArtikkelNr = Strekkode.ArtikkelNr AND
            TelleLinje.Butik = iButNr AND 
            TelleLinje.Storl = StrKonv.Storl NO-ERROR.

    DISPLAY STREAM Ut
        TelleHode.TelleNr
        cEan
        TelleLinje.Beskr WHEN AVAILABLE TelleLinje
        iAntTalt
        '|'
        TelleLinje.AntallTalt WHEN AVAILABLE TelleLinje
        (IF AVAILABLE TelleLinje THEN INT((iAntTalt - TelleLinje.AntallTalt)) ELSE 0)
    WITH WIDTH 350.

    IF AVAILABLE TelleLinje THEN
    DO:
        ASSIGN
            TelleLinje.AntallTalt    = iAntTalt
            TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * TelleLinje.VVareKost
            TelleLinje.AntallDiff    = TelleLinje.AntallPar - TelleLinje.AntallTalt
            TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
            .                          
    END.

END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.


