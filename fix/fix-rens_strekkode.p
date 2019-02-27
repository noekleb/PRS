DEF VAR cFil AS CHAR  NO-UNDO.
DEF VAR cKode AS CHAR FORMAT "x(24)" NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.

ASSIGN
    cFil = 'c:\appdir\tn\GNVpi.csv'
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFil).
REPEAT:
    iant = iant + 1.
    IF iAnt <= 2 THEN 
        NEXT.
    IMPORT STREAM Inn UNFORMATTED 
        cLinje.

    cKode = ENTRY(2,cLinje,';').

    FIND Strekkode EXCLUSIVE-LOCK WHERE
        Strekkode.Kode = cKode NO-ERROR.

    /*
    DISPLAY
        cKode
        (IF AVAILABLE Strekkode THEN Strekkode.ArtikkelNr ELSE 0) FORMAT ">>>>>>>>>>>>>9"
        .
    */

    IF AVAILABLE Strekkode THEN
        DELETE Strekkode.
END.
INPUT STREAM inn CLOSE.
