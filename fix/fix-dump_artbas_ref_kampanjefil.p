DEF VAR iLevNr AS INT NO-UNDO.
DEF VAR cLevKod AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.

DEF STREAM Inn.

INPUT STREAM Inn FROM 'c:\appdir\tn\dumpPRSkamp.csv'.
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.
    ASSIGN
        iLevNr  = INT(ENTRY(2,cLinje,';'))
        cLevKod = ENTRY(3,cLinje,';')
        .

    FIND FIRST ArtBAs WHERE 
        ArtBas.LevNr = iLevNr AND
        ArtBas.LevKod = cLevKod NO-ERROR.
    IF NOT AVAILABLE ArtBAs THEN
        NEXT.

    /*
    DISPLAY
        iLevNr
        cLevKod
        ArtBas.ArtikkelNr WHEN AVAILABLE ArtBas
    WITH WIDTH 250.
    */

    OUTPUT TO 'artbas.d' APPEND.
        EXPORT ArtBas.
    OUTPUT CLOSE.

    OUTPUT TO 'artpris.d' APPEND.
        FOR EACH ArtPris OF ArtBas:
            EXPORT ArtPris.
        END.
    OUTPUT CLOSE.

    OUTPUT TO 'strekkode.d' APPEND.
        FOR EACH Strekkode OF ArtBas:
            EXPORT Strekkode.
        END.
    OUTPUT CLOSE.
END.
INPUT STREAM INn CLOSE.
