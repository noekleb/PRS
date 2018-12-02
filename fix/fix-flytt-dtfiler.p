DEFINE VARIABLE cFilNavn   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInputdir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOutputDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRiktigaFilnamnet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButDirFraFilnavn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHitSkallFilen AS CHARACTER  NO-UNDO.
ASSIGN cInputdir = "I:\DT-filer 2004\".
INPUT FROM OS-DIR(cInputdir) NO-ECHO.
REPEAT:
    SET cFilNavn FORMAT "X(50)".
     IF NUM-ENTRIES(cFilNavn,"-") = 2 AND cFilNavn BEGINS "000" THEN DO:
         ASSIGN cRiktigaFilnamnet = ENTRY(2,cFilNavn,"-")
                cButDirFraFilnavn = REPLACE(ENTRY(1,cRiktigaFilnamnet,"."),"dtbr","").
                cHitSkallFilen    = "F:\Home\Pressbyran\Ankommet\" + cButDirFraFilnavn.
         OS-CREATE-DIR VALUE(cHitSkallFilen).
         OS-RENAME VALUE(cInputdir + cFilNavn) VALUE(cHitSkallFilen + "\" + cRiktigaFilnamnet).
      END.
END.

