DEF VAR cTekst AS CHAR INITIAL '187,199,1199,1440,1450' NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

DO piLoop = 1 TO NUM-ENTRIES(cTekst):
  FIND ArtBAs WHERE
      ArtBas.ArtikkelNr = DEC(ENTRY(piLoop,cTekst)).
  ArtBas.ArtSlag = 5.
END.
