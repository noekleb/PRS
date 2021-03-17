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
    cFilNavn = "Q:\Appdir\Sport1HK\BrittElin12082008\KOPI av Restlager BD til sport 1 april 08 VPI (2) Med kjedens innkjøpspris.csv"
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn).

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cRecord.

    ASSIGN
        cEAN          = ENTRY(4,cRecord,';')
        cLevKod       = ENTRY(3,cRecord,';')
        cBeskr        = ENTRY(5,cRecord,';')
        cLevFargKod   = ENTRY(7,cRecord,';')
        dKj_inkj_pris = dec(ENTRY(37,cRecord,';'))
        .

    ASSIGN
        lDec = DEC(cEAN) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    FIND strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode EXCLUSIVE-LOCK NO-ERROR.


    IF AVAILABLE ArtBas THEN DO:
        PAUSE 0.
        DISPLAY
            cEAN
            cLevKod
            cBeskr
            cLevFargKod
            dKj_inkj_pris
            AVAILABLE Strekkode
            ArtBas.ArtikkelNr
            WITH WIDTH 250.


        
        /* Oppdaterer artikkel */
        ASSIGN
            ArtBas.KjedeInnkPris = dKj_inkj_pris
            ArtBas.KjedeRab%     = (ArtBas.Katalogpris - ArtBas.KjedeInnkPris) / ArtBas.KatalogPris * 100
            ArtBas.KjedeRab%     = IF ArtBas.KjedeRab% = ? THEN 0 ELSE ArtBas.KjedeRab%. 
        
    END.


END.

INPUT STREAM Inn CLOSE.
