DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.
DEF VAR cPkSdlNr AS CHAR FORMAT "x(20)" NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cUtfil = 'C:\Appdir\Se\konv\PakkseddelListeOutletUT.csv'
    cFil   = 'C:\Appdir\Se\konv\PakkseddelListeOutlet.csv'
    .

INPUT STREAM Inn FROM VALUE(cFil).
OUTPUT STREAM Ut TO VALUE(cUtFil).
PUT STREAM Ut 
    'PkSdlNr;Status;ButikkNr;MottattDato'
    SKIP.
REPEAT:
    IMPORT STREAM Inn DELIMITER ';'
        cPkSdlNr
        .

    IF AVAILABLE PkSdlLinje THEN 
        RELEASE PkSdlLinje.
    FIND LAST PkSdlHode NO-LOCK WHERE 
        PkSdlHode.PkSdlNr = cPkSdlNr NO-ERROR.
    IF AVAILABLE PkSdlHode THEN
        FIND FIRST PkSdlLinje OF PkSdlHode NO-ERROR.
    IF AVAILABLE PkSdlHode THEN
        FIND FIRST PkSdlMottak OF PkSdlhode NO-ERROR.

    /*
    DISPLAY
        cPkSdlNr
        PkSdlHode.PkSdlNr WHEN AVAILABLE PkSdlHode 
        (IF AVAILABLE PkSdlHode THEN STRING(PkSdlHode.PkSdlStatus) ELSE '*Ukjent pksdl')
        PksdlLinje.butikkNr WHEN AVAILABLE PkSdlLinje
        PksdlMottak.MottattDato WHEN AVAILABLE PkSdlMottak
    WITH WIDTH 350.
    */

    PUT STREAM Ut UNFORMATTED
        cPkSdlNr ';'
        (IF AVAILABLE PkSdlHode THEN STRING(PkSdlHode.PkSdlStatus) ELSE '*Ukjent pksdl') ';'
        (IF AVAILABLE PksdlLinje THEN STRING(PksdlLinje.butikkNr) ELSE '') ';'
        (IF AVAILABLE PkSdlMottak THEN STRING(PksdlMottak.MottattDato) ELSE '')
        SKIP.
END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.

