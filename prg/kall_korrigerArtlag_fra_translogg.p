def input parameter cArtikkelNrLst as char no-undo.

def var piLoop  as int no-undo.

do piLoop = 1 to num-entries(cArtikkelNrLst):
  FOR EACH ArtBas NO-LOCK WHERE
    Artbas.artikkelnr = dec(entry(piLoop,cArtikkelNrLst)):

    run korrigerArtlag_fra_translogg.p (ArtBas.ArtikkelNr).
  END.
END.
