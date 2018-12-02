For each ArtBas where ArtBas.ArtSlag > 0:
  ArtBas.ArtSlag = 0.
End.
For each ArtBAs where ArtBas.OPris = false:
  if ArtBas.Lager = false then ArtBas.Lager = true.
End.
For each ArtBas where ArtBas.Sasong = 0:
  ArtBas.Sasong = 112.
End.
For each EkstVPILev no-lock:
  for each VPIArtBas:
      if VPIArtBas.Lager = false then VPIArtBas.Lager = true.
      if VPIArtBas.ArtSlag > 0 then VPIArtBas.ArtSlag = 0.
      if VPIArtBas.SaSong = 0 then VPIArtBas.Sasong = 112.
  end.
End.
