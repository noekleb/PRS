DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFiler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSista AS CHARACTER  NO-UNDO.
/* cDir = "c:\appdir\91d\7Eleven\rap". */
cDir = "E:\7-Eleven\Priskontroll\Resultat_Ny".
INPUT FROM OS-DIR(cDir) NO-ECHO.
REPEAT:
    SET cfil.
    IF cFil MATCHES "*.xls" AND length(ENTRY(1,cFil,".")) = 3 THEN DO:
        ASSIGN cFiler = cFiler + (IF cFiler <> "" THEN "," ELSE "") + cFil.
/*         DISP cfil. */
    icount = icount + 1.
    END.
/*     IF icount = 3 THEN */
/*     LEAVE.             */
END.
INPUT CLOSE.
OUTPUT TO "CLIPBOARD".
DO iCount = 1 TO NUM-ENTRIES(cFiler):
    INPUT FROM VALUE(cDir + "\" + ENTRY(iCount,cFiler)).
    cSista = "".
    REPEAT:
        IMPORT UNFORMATTED cStr.
        IF cStr BEGINS "Antal" THEN
            LEAVE.
        cSista = cStr.
    END.
    INPUT CLOSE.
    PUT UNFORMATTED cSista SKIP.
END.
OUTPUT CLOSE.
MESSAGE cFiler
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


