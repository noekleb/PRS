DEF VAR cFilNavn   AS CHAR NO-UNDO.
DEF VAR cNyFilNavn AS CHAR NO-UNDO.
DEF VAR iLoop      AS INT  NO-UNDO.
DEF VAR cKatalog   AS CHAR NO-UNDO.
DEF VAR cLinje     AS CHAR NO-UNDO.
DEF VAR cKode      AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.

ASSIGN
    cKatalog = "C:\Home\pressbyran\ankommet\"
    .

DEF STREAM Inn.
DEF STREAM Ut.

DO iLoop = 9 TO 28:
    ASSIGN 
        cFilNavn   = "PB_200402&1.dat"
        cNyFilNavn = "PB_200402&1New.dat"
        cFilNavn   = cKatalog + SUBSTITUTE(cFilNavn,STRING(iLoop,"99"))
        cNyFilNavn = cKatalog + SUBSTITUTE(cNyFilNavn,STRING(iLoop,"99"))
        .

    IF SEARCH(cFilNavn) <> ? THEN
    KONVERTERFIL:
    DO:
        PAUSE 0.
        DISPLAY cFilNavn FORMAT "x(80)"
            SEARCH(cFilNavn)
            WITH DOWN WIDTH 198.
        DOWN 1.

        INPUT STREAM  Inn FROM VALUE(cFilNavn) NO-ECHO.
        OUTPUT STREAM Ut  TO VALUE(cNyFilNavn) NO-ECHO.

        LESLINJE:
        REPEAT:
            IMPORT STREAM Inn UNFORMATTED cLinje.
            ASSIGN
                cKode = ENTRY(1,cLinje,"|")
                .
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = cKode NO-ERROR.
            IF AVAILABLE Strekkode THEN
                FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
            /* Det var to linjer som ikke finner match. De skipper vi. Artikkelen */
            /* finnes, men med en annen strekkode.                                */
            /* Gjelder linje 1 og 3 i filen som konverteres. I Alle filer.        */
            IF AVAILABLE Strekkode THEN
            DO:
                IF AVAILABLE Strekkode THEN
                    PUT STREAM Ut UNFORMATTED 
                      trim(string(Strekkode.ArtikkelNr,">>>>>>>>>>>>9")) + 
                      "|" cLinje "|"
                      (IF AVAILABLE ArtBas 
                         THEN trim(string(ArtBas.LinkVareNr,">>>>>>>>>>>>9"))
                         ELSE "0")
                      SKIP.
                ELSE
                    PUT STREAM Ut UNFORMATTED "0|" cLinje "|0" SKIP.
            END.

        END. /* LESLINJE */

        OUTPUT STREAM Ut CLOSE.
        INPUT STREAM Inn CLOSE.
    END. /*KONVERTERFIL */
END.
