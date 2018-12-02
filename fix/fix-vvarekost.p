CURRENT-WINDOW:WIDTH = 300.
DEF VAR lArtikkelNR AS DEC NO-UNDO.

lArtikkelNr = 8463336.

FOR EACH Lager WHERE
  Lager.ArtikkelNr = lArtikkelNr:
  FIND FIRST ArtBas OF Lager NO-LOCK NO-ERROR.
  FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.

  IF Lager.VVareKost = ? OR Lager.VVareKost = 0 THEN
    Lager.VVareKost = ArtPris.VareKost[1].
  DISPLAY
    Lager.VVAreKost
    .
END.
