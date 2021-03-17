DEF VAR cKode AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.

DEF STREAM Inn.

INPUT STREAM inn FROM VALUE('c:\home\lindbak\ankommet\GNVpi_tn2.csv').
REPEAT:
    iAnt = iant + 1.
    IMPORT STREAM Inn UNFORMATTED
        cLinje.
    IF iant <= 2 THEN
        NEXT.

    FIND StrekKode WHERE
        Strekkode.Kode = cKode NO-ERROR.

    cKode = ENTRY(2,cLinje,';').
    /*
    DISPLAY
        cKode
        Strekkode.Kode WHEN AVAILABLE Strekkode
        .
    */
    IF AVAILABLE Strekkode THEN
        DELETE strekkode.
END.
INPUT STREAM Inn CLOSE.
