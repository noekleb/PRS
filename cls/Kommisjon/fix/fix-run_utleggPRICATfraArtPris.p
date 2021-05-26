
FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.ProfilNr = 100,
    EACH ArtBAs OF ArtPris EXCLUSIVE-LOCK:
    ArtBAs.EDato = TODAY - 2.
    ArtBas.ETid  = TIME.
    Artpris.EDato = ArtBAs.EDato.
    ArtPRis.ETid = ArtBas.ETid.
END.
RUN cls\kommisjon\run_utleggPRICATfraArtPris.p.
/* RUN cls\kommisjon\utleggPRICATfraArtPris.p. */

