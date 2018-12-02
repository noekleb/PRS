DEF VAR cFilNavn          AS CHAR NO-UNDO.
DEF VAR lKjedeRab%        AS DEC NO-UNDO.        
DEF VAR lKjedeInnkPris    AS DEC NO-UNDO.  
DEF VAR lKjedeSupRab%     AS DEC NO-UNDO.        
DEF VAR lKjedeSupInnkPris AS DEC NO-UNDO.
DEF VAR cLinje            AS CHAR NO-UNDO.
DEF VAR lVareBokNr        AS DEC NO-UNDO.
DEF VAR cKode             AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN
    lVareBokNr = 9000013
    cFilNavn   = 'varebok_kjederab.csv'.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
REPEAT:
    IMPORT STREAM Inn UNFORMATTED 
        cLinje.
    IF NUM-ENTRIES(cLinje,';') < 3 THEN
        NEXT.
    ASSIGN
        cKode             = ENTRY(1,cLinje,';')
        lKjedeInnkPris    = DEC(ENTRY(2,cLinje,';'))
        lKjedeSupInnkPris = DEC(ENTRY(3,cLinje,';'))
        .
    FIND Strekkode NO-LOCK WHERE 
        Strekkode.Kode = cKode NO-ERROR.
    DO TRANSACTION:
        IF AVAILABLE Strekkode THEN
            FIND VareBokLinje EXCLUSIVE-LOCK WHERE 
              VareBokLinje.VareBokNr = lVareBokNr AND
              VareBokLinje.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR. 

        IF AVAILABLE VarebokLinje THEN
        OPPDATER:
        DO:
            IF (VareBokLinje.KjedeRab% <> 0 OR 
                VareBokLinje.KjedeSupRab% <> 0) THEN
                LEAVE OPPDATER.
            ELSE DO:
                ASSIGN
                    lKjedeRab%        = (VarebokLinje.InnkjopsPris - lKjedeInnkPris) / VarebokLinje.InnkjopsPris * 100
                    lKjedeSupRab%     = (VarebokLinje.InnkjopsPris - lKjedeSupInnkPris) / VarebokLinje.InnkjopsPris * 100
                    .

                /* Oppdaterer vareboklinje */
                ASSIGN
                    VarebokLinje.KjedeInnkPris    = lKjedeInnkPris
                    VarebokLinje.KjedeSupInnkPris = lKjedeSupInnkPris
                    VarebokLinje.KjedeRab%        = lKjedeRab%
                    VarebokLinje.KjedeSupRab%     = lKjedeSupRab%
                    .

                /*
                DISPLAY
                    VarebokLinje.ArtikkelNr
                    cKode FORMAT "x(20)"            
                    VarebokLinje.InnkjopsPris
                    lKjedeInnkPris
                    lKjedeRab%
                    lKjedeSupInnkPris
                    lKjedeSupRab%
                    WITH WIDTH 250
                */    .
            END.
        END. /* OPPDATER */
    END.

END.
