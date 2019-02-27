DEF STREAM InnFil.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR piLevNr AS INT.
DEF VAR pcLevNavn AS CHAR NO-UNDO.

ASSIGN
    cFilNavn = "splev091003-1155.rpt".

INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.

REPEAT :
    IMPORT stream InnFil delimiter ";"
        piLevNr
        pcLevNavn
        .
    FIND LevBAs NO-LOCK WHERE
        LevBas.LevNr = piLevNr NO-ERROR.
    IF NOT AVAILABLE LEvBAs THEN
    DO:
        CREATE LEvBAs.
        ASSIGN
            LevBas.LevNr = piLevNr
            LevBas.LevNamn = pcLevNavn.
    END.
    DISPLAY
        piLevNr
        pcLevNavn
        LEvBas.LEvNr WHEN AVAILABLE LEvBas
        LEvBas.LEvnamn WHEN AVAILABLE LEvBas
        .
END.
