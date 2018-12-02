CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR iAnt     AS INT  NO-UNDO.
DEF VAR iAnt2    AS INT NO-UNDO.

DEF TEMP-TABLE bufArtPris LIKE ArtPris.

ASSIGN
    cFilNavn = 'c:\home\lindbak\ankommet\artpris.d'.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

LOOPEN:
REPEAT:
    iAnt = iant + 1.
    /*IF iAnt > 10 THEN LEAVE LOOPEN.*/

    CREATE bufArtPris.
    IMPORT STREAM Inn bufArtPris.

    FIND ArtPris EXCLUSIVE-LOCK WHERE
        ArtPris.ArtikkelNr = bufArtPris.ArtikkelNr AND
        ArtPris.ProfilNr   = 1 NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
        NEXT.

    IF (ArtPris.VareKost[1] = 0 OR ArtPris.Pris[1] = 0) THEN
    DO:
        BUFFER-COPY bufArtPris 
            EXCEPT ProfilNr
            TO ArtPris.
        iAnt2 = iAnt2 + 1.
        DISPLAY
            iAnt
            iAnt2
            bufArtPris.ArtikkelNr
            bufArtPris.VareKost[1]
            bufArtPris.Pris[1]
            ArtPris.ArtikkelNr WHEN AVAILABLE ArtPris
            ArtPris.VareKost[1] WHEN AVAILABLE ArtPris
            ArtPris.Pris[1] WHEN AVAILABLE ArtPris
            WITH WIDTH 300.
    END.
END. /* LOOPEN */

INPUT STREAM Inn CLOSE.


