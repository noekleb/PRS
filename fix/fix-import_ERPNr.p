DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje   AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cEAN     AS CHAR NO-UNDO.
DEF VAR cErpNr   AS CHAR NO-UNDO.
DEF VAR iant     AS DEC  NO-UNDO.
DEF VAR iAntUkjent AS DEC NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.

ASSIGN
    cFilNavn = 'C:\appdir\tn\EAN_ERP_Nummer.csv'
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    ASSIGN
        cEAN   = trim(ENTRY(1,cLinje,';'))
        cERPNr = trim(ENTRY(2,cLinje,';'))
        iAnt = iant + 1 NO-ERROR.

    IF cEAN <> '' THEN
    DO:
        FIND Strekkode EXCLUSIVE-LOCK WHERE
            Strekkode.Kode = cEAN NO-ERROR.
        IF AVAILABLE Strekkode THEN
            Strekkode.ERPNr = cERPNr.
        ELSE iAntUkjent = iAntUkjent + 1.
        /*
        DISPLAY
            cFilNavn
            cEAN
            cERPNr
            Strekkode.ERPNr WHEN AVAILABLE Strekkode
            'UKJENT' WHEN NOT AVAILABLE strekkode
            WITH WIDTH 300.
        */
    END.
END.

INPUT STREAM Inn CLOSE.

MESSAGE 'Antall linjer:' iAnt  SKIP 'Antall ukjente' iAntukjent
    VIEW-AS ALERT-BOX INFO BUTTONS OK.




