CURRENT-WINDOW:WIDTH = 250.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cEAN     AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cBeskr   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR cLevKod  AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cLevFargKod AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR dKj_inkj_pris AS DEC NO-UNDO.
DEF VAR cRecord  AS CHAR NO-UNDO.
DEF VAR dVarebokNr AS DEC NO-UNDO.
DEF VAR lDec AS DEC NO-UNDO.

ASSIGN
    cFilNavn = "Q:\Appdir\Sport1HK\Strtyper12082008\Hardvare messe- feil størrelse-størrelsekode.csv"
    dVarebokNr = 9000008
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn).

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cRecord.

    ASSIGN
        cEAN = ENTRY(1,cRecord,';')
        .

    ASSIGN
        lDec = DEC(cEAN) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
    DO:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = dec(cEAN) NO-ERROR.

        IF AVAILABLE ArtBas THEN DO:

            FIND VarebokLinje NO-LOCK WHERE
                VarebokLinje.VareBokNr = dVareBokNr AND
                VareBokLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.

            FIND FIRST EkstVPILEv NO-LOCK WHERE
                EkstVPILev.LevNr = VareBokLinje.LevNr AND
                NOT EkstVPILev.KortNavn BEGINS "XON" NO-ERROR.
            IF AVAILABLE EkstVPILEv THEN
                FIND VPIArtBas NO-LOCK WHERE
                VPIArtBas.EkstVPILEvNr = EkstVPILEv.EkstVPILEvNr AND
                VPIArtBas.VareNr = string(VarebokLinje.ArtikkelNr) NO-ERROR.
            DISPLAY
                cEAN
                ArtBas.ArtikkelNr
                Vareboklinje.LevKod
                Vareboklinje.Beskr
                Vareboklinje.LevFargKod
                VareBokLinje.ArtikkelNr
                EkstVPILEv.EkstVPILEvNr WHEN AVAILABLE EkstVPILev
                VPIArtBas.VareNr WHEN AVAILABLE VPIArtBas
                VPIArtBas.StrType WHEN AVAILABLE VPIArtBas 
                ArtBas.StrType  COLUMN-LABEL "ArtBas StrType"
                WITH WIDTH 250.

        END.
    END.


END.

INPUT STREAM Inn CLOSE.
