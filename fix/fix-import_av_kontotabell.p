CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje   AS CHAR NO-UNDO.

DEF VAR iKonto1 AS INT  NO-UNDO.
DEF VAR cNavn1  AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR iKonto2 AS INT  NO-UNDO.
DEF VAR cNavn2  AS CHAR FORMAT "x(40)" NO-UNDO.

ASSIGN
    cFilNavn = 'Sverige_Kontoplan_Normal_2011_ver1.csv'
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn).
REPEAT :
    IMPORT STREAM Inn UNFORMATTED cLinje.

    ASSIGN
        iKonto1 = 0
        iKonto2 = 0
        cNavn1  = ''
        cNavn2  = ''
        .
    IF NUM-ENTRIES(cLinje,';') >= 4 AND LENGTH(ENTRY(3,cLinje,';')) = 4 THEN
    DO:
        ASSIGN
        iKonto1 = INT(TRIM(ENTRY(3,cLinje,';')))
        cNavn1  = TRIM(ENTRY(4,cLinje,';'))
        NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND iKonto1 > 0 THEN
        DO:
            FIND KontoTabell EXCLUSIVE-LOCK WHERE
                KontoTabell.KontoNr = iKonto1 NO-ERROR.
            IF NOT AVAILABLE KontoTabell THEN
            DO:
                CREATE KontoTabell.
                ASSIGN
                    KontoTabell.KontoNr = iKonto1.
            END.
            ASSIGN
                KontoTabell.KontoNavn = cNavn1.
        END.
    END.
    IF NUM-ENTRIES(cLinje,';') >= 7 AND LENGTH(ENTRY(6,cLinje,';')) = 4 THEN
    DO:
        ASSIGN
        iKonto2 = INT(TRIM(ENTRY(6,cLinje,';')))
        cNavn2  = TRIM(ENTRY(7,cLinje,';'))
        NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND iKonto2 > 0 THEN
        DO:
            FIND KontoTabell EXCLUSIVE-LOCK WHERE
                KontoTabell.KontoNr = iKonto2 NO-ERROR.
            IF NOT AVAILABLE KontoTabell THEN
            DO:
                CREATE KontoTabell.
                ASSIGN
                    KontoTabell.KontoNr = iKonto2.
            END.
            ASSIGN
                KontoTabell.KontoNavn = cNavn2.
        END.
    END.
    DISPLAY
        iKonto1 
        cNavn1  
        iKonto2 
        cNavn2  
        WITH WIDTH 300.
END.
INPUT STREAM Inn CLOSE.
