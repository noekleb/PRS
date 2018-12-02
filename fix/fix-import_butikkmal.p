DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR iInt AS INT NO-UNDO.

DEF STREAM Inn.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cFil = 'C:\NSoft\Polygon\PRS\WorkPlace\SportNorgeButikkfordeling25062018.csv'
    .

INPUT STREAM Inn FROM VALUE(cfil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.
    
    ASSIGN 
        iInt = INT(ENTRY(1,cRecord,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    FIND ButMalHode EXCLUSIVE-LOCK WHERE
        ButMalHode.ButMalNr = INT(ENTRY(1,cRecord,';')) NO-ERROR.
    IF NOT AVAILABLE ButMalHode THEN
    DO:
        CREATE butMalHode.
        ASSIGN
            ButMalHode.butMalNr = INT(ENTRY(1,cRecord,';'))
            .
    END.
    ASSIGN
        ButMalHode.butMalNavn = ENTRY(2,cRecord,';')
        .
    FIND ButMalLinje EXCLUSIVE-LOCK WHERE 
        ButMalLinje.butMalNr   = ButMalHode.ButMalNr AND
        ButMalLinje.AvdelingNr = INT(ENTRY(3,cRecord,';')) NO-ERROR.
    IF NOT AVAILABLE butMalLinje THEN
    DO:
        CREATE butMalLinje.
        ASSIGN 
            ButMalLinje.ButMalNr = ButMalHode.butMalNr
            ButMalLinje.AvdelingNr = INT(ENTRY(3,cRecord,';')).
    END.
    ASSIGN
        ButMalLinje.Fordeling% = INT(ENTRY(5,cRecord,';'))
        .
END.
INPUT STREAM Inn CLOSE.
