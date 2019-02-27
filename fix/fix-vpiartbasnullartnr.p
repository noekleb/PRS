for each VPIArtBas where VPIArtBas.EkstVPILevNR = 100 and
                         VPIArtBAs.ArtikkelNr > 0:
  if not can-find(artBas where
  ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNR) then
  VPIArtBas.ArtikkelNr = 0.
end.
