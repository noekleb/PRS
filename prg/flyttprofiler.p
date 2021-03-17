DEFINE INPUT  PARAMETER ipScannKatalog   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipFlyttkommando  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipFlyttaDennaFil AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cFil      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFullFil  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAttr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iTst      AS INTEGER    NO-UNDO.

IF ipFlyttaDennaFil = "" THEN DO:
    INPUT FROM OS-DIR(ipScannKatalog).
    REPEAT:
       IMPORT cFil cFullFil cAttr.
       IF cAttr = "F" AND NUM-ENTRIES(cFil,".") = 2 AND LENGTH(ENTRY(2,cFil,".")) = 5 THEN DO:
           ASSIGN iTst = INT(ENTRY(2,cFil,".")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               NEXT.
           cKommando = ipFlyttkommando + " " + cFullFil + " " + cFil.
           OS-COMMAND SILENT VALUE(cKommando).
       END.

    END.
    INPUT CLOSE.
END.
ELSE DO:
    IF SEARCH(ipFlyttaDennaFil) <> ? THEN DO:
        ASSIGN cFil = ENTRY(NUM-ENTRIES(ipFlyttaDennaFil,"\"),ipFlyttaDennaFil,"\")
               cKommando = ipFlyttkommando + " " + ipFlyttaDennaFil + " " + cFil.
        OS-COMMAND SILENT VALUE(cKommando).
    END.
END.
