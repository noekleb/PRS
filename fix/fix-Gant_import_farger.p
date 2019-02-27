DEF VAR cLinje AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR iFarg LIKE Farg.Farg NO-UNDO.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE('konv\Fargekoder03022017.csv').
REPEAT:
    IMPORT STREAM Inn UNFORMATTED 
        cLinje.

    FIND FIRST Farg NO-LOCK WHERE
        Farg.KFarge = TRIM(ENTRY(1,cLinje,';')) NO-ERROR.
    IF NOT AVAILABLE Farg THEN
    DO:
        FIND LAST Farg.
        IF AVAILABLE Farg THEN
            iFarg = Farg.Farg + 1.
        ELSE 
            ifarg = 1.
        CREATE Farg.
        ASSIGN
            Farg.Farg     = iFarg
            Farg.KFarge   = TRIM(ENTRY(1,cLinje,';'))
            Farg.FarBeskr = TRIM(ENTRY(1,cLinje,';')) + ' ' + 
                            TRIM(ENTRY(2,cLinje,';'))
            .
    END.

    DISPLAY
        farg.farg FORMAT ">>>>>9"
        farg.KFarge
        farg.FarBeskr
    WITH WIDTH 250.
END.
INPUT STREAM Inn CLOSE.
