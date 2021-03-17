TRIGGER PROCEDURE FOR CREATE OF SkoTex.ArtBas.

DEF VAR trgArtikkelNr LIKE artbas.artikkelnr NO-UNDO.

ASSIGN
  ArtBas.Storrelser = TRUE.

LOOPEN:
DO WHILE TRUE:
    RUN trg/genArtikkelnr.p (OUTPUT trgArtikkelNr).
    ASSIGN
      SkoTex.ArtBas.ArtikkelNr     = trgArtikkelNr
      SkoTex.ArtBas.RegistrertDato = TODAY
      SkoTex.ArtBas.RegistrertTid  = TIME
      SkoTex.ArtBas.RegistrertAV   = USERID("skotex") /* NO-ERROR*/
      Skotex.ArtBas.Kjedevare      = FALSE
      SkoTex.ArtBas.LinkVareAnt    = 1
      .
    LEAVE LOOPEN.
END.





