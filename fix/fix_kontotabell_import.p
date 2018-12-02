CURRENT-WINDOW:WIDTH = 250.

DEF VAR cFil   AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR FORMAT "x(50)" NO-UNDO.

DEF STREAM Inn.

ASSIGN
    cFil = 'C:\Polygon\PRS Dokumentasjon\Import-Eksport filer\SIE Export\Sverige_Kontoplan_Normal_2011_ver1.csv'.

INPUT STREAM Inn FROM VALUE(cFil) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    IF NUM-ENTRIES(cLinje,';') < 7 THEN
        NEXT.

    IF (entry(2,cLinje,';') = 'XX' OR
        entry(5,cLinje,';') = 'XX')
        THEN
    DO:
        IF entry(2,cLinje,';') = 'XX' AND 
           NOT CAN-FIND(KontoTabell WHERE
                        KontoTabell.KontoNr = INT(entry(3,cLinje,';'))) THEN
        DO:
            CREATE KontoTabell.
            ASSIGN
                KontoTabell.KontoNr   = INT(entry(3,cLinje,';'))
                KontoTabell.KontoNavn = entry(4,cLinje,';')
                .
        END.
        IF entry(5,cLinje,';') = 'XX' AND 
           NOT CAN-FIND(KontoTabell WHERE
                        KontoTabell.KontoNr = INT(entry(6,cLinje,';'))) THEN
        DO:
            CREATE KontoTabell.
            ASSIGN
                KontoTabell.KontoNr   = INT(entry(6,cLinje,';'))
                KontoTabell.KontoNavn = entry(7,cLinje,';')
                .
        END.
    END.
END.

INPUT STREAM Inn CLOSE.
