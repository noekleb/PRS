DEF VAR cRecord AS CHAR NO-UNDO.
DEF VAR cPkSdlNr AS CHAR NO-UNDO.
DEF VAR cVareType AS CHAR NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Inn.

ASSIGN 
    cFil = 'konv\PksdlVaretyper12122019.csv'
    .

INPUT STREAM Inn FROM VALUE(cFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cRecord.
    ASSIGN 
        cVaretype = TRIM(ENTRY(5,cRecord,';'))
        cPkSdlNr = TRIM(ENTRY(6,cRecord,';'))
        .
    

    IF cVareType <> '' AND cPkSdlNr <> '' THEN
    DO:
        FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
            PkSdlHode.PkSdlNr = cPkSdlNr:
            ASSIGN 
                PkSdlHode.VareType = cVareType
                .
        END.
        DISPLAY
            cPkSdlNr FORMAT "x(10)"
            cVareType FORMAT "x(30)"
            cRecord FORMAT "x(150)"
        WITH WIDTH 350.
    END.

END.
INPUT STREAM Inn CLOSE.
