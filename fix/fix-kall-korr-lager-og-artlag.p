
DEF VAR wLager  AS INT NO-UNDO.    
DEF VAR wAntall AS INT NO-UNDO.

/* IF SEARCH("Lagerkorr.txt") <> ? THEN  */
/*     OS-DELETE VALUE("Lagerkorr.txt"). */

PUBLISH 'infoDisp' ("Korr. av artikler ute av synk.").

FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.OPris = FALSE AND
    ArtBas.Lager = TRUE:
    DO:
        PUBLISH 'infoDisp' (string(ArtBas.ArtikkelNr) + " " + 
                            Artbas.beskr).
        /*RUN fix-lager-og-artlag.p (ArtBas.ArtikkelNR).*/
        RUN korrigerArtlag_fra_translogg.p (ArtBas.ArtikkelNR).

        PAUSE 0.
    END.
END.

