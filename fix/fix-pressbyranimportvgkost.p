/* Setter KostProsent for varegrupper */

DEF VAR cFilNAvn AS CHAR NO-UNDO.
DEF VAR piVg     AS INT  NO-UNDO.
DEF VAR pl%      AS DEC  NO-UNDO.

DEF STREAM inn.

ASSIGN
    cFilNavn = "C:\Appdir\SkoTex\DB%GruppeFraMd.txt"
    .

INPUT STREAM inn FROM VALUE(cFilNavn) NO-ECHO.

BLOKKEN:
REPEAT:
    IMPORT STREAM inn DELIMITER ";"
        piVg
        pl%
        .

    FIND VarGr EXCLUSIVE-LOCK WHERE
        VarGr.Vg = piVg NO-ERROR.
    IF AVAILABLE VarGr AND pl% > 0 THEN
    DO:
        ASSIGN
            VarGr.Kost_Proc = 100 - pl%
            VarGr.Kost_Proc = IF VarGr.Kost_Proc = 0
                                THEN 65.0
                                ELSE VarGr.Kost_Proc
            .
    END.

END. /* BLOKKEN */

INPUT STREAM inn CLOSE.
