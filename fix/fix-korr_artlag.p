current-window:width = 300.

for each ArtBAs where ArtBas.ArtikkelNr = 1831291:
  run fix-lager-og-artlag.p (ArtBAs.ArtikkelNr).
end.
