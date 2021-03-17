CURRENT-WINDOW:WIDTH = 250.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cEAN     AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cBeskr   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR cLevKod  AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cLevFargKod AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR dKj_inkj_pris AS DEC NO-UNDO.
DEF VAR cRecord  AS CHAR NO-UNDO.
DEF VAR dVarebokNr AS DEC NO-UNDO.

ASSIGN
    cFilNavn = "Q:\Appdir\Sport1HK\BjornRune11082008\NYTT 1 Tilbudsark Sport 1 tekstil-sko vår-sommer 2009_ jotunheim (2).csv"
    dVarebokNr = 9000007.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn).

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cRecord.

    ASSIGN
        cEAN          = ENTRY(4,cRecord,';')
        cLevKod       = ENTRY(3,cRecord,';')
        cBeskr        = ENTRY(5,cRecord,';')
        cLevFargKod   = ENTRY(7,cRecord,';')
        dKj_inkj_pris = dec(ENTRY(13,cRecord,';'))
        .

    FIND strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode.
    IF AVAILABLE ArtBas THEN
        FIND VareBokLinje WHERE
          VareBokLinje.VareBokNr  = dVareBokNr AND
          VareBokLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.


    IF /*cBeskr = "Manhattan ls Softsh." AND*/ AVAILABLE Strekkode THEN DO:
        PAUSE 0.
        DISPLAY
            cEAN
            cLevKod
            cBeskr
            cLevFargKod
            dKj_inkj_pris
            AVAILABLE Strekkode
            ArtBas.ArtikkelNr
            VareBokLinje.ArtikkelNr
            VareBokLinje.KjedeInnkPris 
            WITH WIDTH 250.

        /* Oppdaterer vareboklinjen */
        ASSIGN
            VarebokLinje.KjedeInnkPris = dKj_inkj_pris
            VarebokLinje.KjedeRab%     = (VarebokLinje.InnkjopsPris - VarebokLinje.KjedeInnkPris) / VarebokLinje.InnkjopsPris * 100. 
    END.


END.

INPUT STREAM Inn CLOSE.
