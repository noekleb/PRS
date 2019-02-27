DEF VAR cLevListe AS CHAR NO-UNDO.
DEF VAR piLoop    AS INT  NO-UNDO.

ASSIGN
    cLevListe = "1001,1006,1018,1011"
    .

DO piLoop = 1 TO NUM-ENTRIES(cLevListe):
    FOR EACH ArtBas WHERE
        ArtBas.LevNr = int(ENTRY(piLoop,cLevListe)):

        FIND FIRST ArtPris OF ArtBAs.
        ASSIGN
            ArtBAs.ForhRab%    = 20
            ArtBas.SupRab%     = 10
            ArtBas.KatalogPris = ArtPris.Pris[1]
            ArtBAs.KjedeInnkPris = ArtPris.InnkjopsPris[1]
            .
    END.

END.


