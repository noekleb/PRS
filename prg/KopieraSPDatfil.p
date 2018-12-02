DEFINE VARIABLE cc      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cSrcFil AS CHARACTER FORMAT "x(30)"  NO-UNDO.
DEFINE VARIABLE c3      AS CHARACTER FORMAT "x(30)"  NO-UNDO.

DEFINE VARIABLE cSenast    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNyasteFil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNyasteFullFil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTilDir    AS CHARACTER  NO-UNDO.

/* Letar efter filen som styr vart vi skall kopiera -param "c:\home\abc\xyz\kopiera_til_dir" */
/* Den skall innehålla ett dir: c:\home\abc\xyz */
IF SESSION:PARAMETER = "" THEN
    QUIT.
IF SEARCH(SESSION:PARAMETER) = ? THEN DO:
    QUIT.
END.
ELSE DO:
    INPUT FROM VALUE(SESSION:PARAMETER).
    IMPORT UNFORMATTED cTilDir.
    INPUT CLOSE.
    cTilDir = RIGHT-TRIM(cTilDir,"\") + "\".
END.
/* Om filen finns har vi kopierat tidigare */
/* Filen skall innehålla senaste kopierade */
IF SEARCH(cTilDir + "Sista_kopiering.txt") = ? THEN DO:
    cSenast = "".
END.
ELSE DO:
    INPUT FROM VALUE(cTilDir + "Sista_kopiering.txt").
    IMPORT UNFORMATTED cSenast.
    INPUT CLOSE.
    IF cSenast = ? THEN
        cSenast = "".
    ELSE
        cSenast = TRIM(cSenast).
END.

INPUT FROM OS-DIR(".") NO-ECHO /* NO-ATTR-LIST */ .
REPEAT:
    SET cc  cSrcFil  c3.
    IF cc MATCHES "S7_*.dat" THEN DO:
        FILE-INFO:FILE-NAME = cSrcFil.
        IF FILE-INFO:FILE-SIZE = 0 THEN
            NEXT.
        IF cc > cNyasteFil THEN
            ASSIGN cNyasteFil = cc
                   cNyasteFullFil = cSrcFil.
    END.
END.
INPUT CLOSE.
IF cNyasteFil <> "" AND cNyasteFil > cSenast THEN DO:
    IF SEARCH(cTilDir + cNyasteFil) <> ? THEN DO:
        OUTPUT TO VALUE(cTilDir + "Sista_kopiering.txt").
        PUT UNFORMATTED cNyasteFil SKIP.
        OUTPUT CLOSE.
    END.
    ELSE DO:
        OS-COPY VALUE(cNyasteFullFil) VALUE(cTilDir + "TMP" + cNyasteFil).
        OS-RENAME VALUE(cTilDir + "TMP" + cNyasteFil) VALUE(cTilDir + cNyasteFil).
        OUTPUT TO VALUE(cTilDir + "Sista_kopiering.txt").
        PUT UNFORMATTED cNyasteFil SKIP.
        OUTPUT CLOSE.
    END.
END.
QUIT.

