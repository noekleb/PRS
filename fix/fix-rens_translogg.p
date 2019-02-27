Display 'ArtSlag'. Pause 0.
For each ArtBas where ArtBas.ArtSlag > 0:
  ArtBas.ArtSlag = 0.
End.
Display 'Lagerfix'. Pause 0.
For each Lager where Lager.VVareKost = ?:
  Lager.VVareKost = 0.
End.
Display 'TransLogg'. Pause 0.
For each Translogg where TransLogg.Dato >= 01/01/01 and TransLogg.VVareKost = ?:
  TransLogg.VVareKost = 0.
End. 
Display 'Lager'. Pause 0.
For each ArtBAs where ArtBas.OPris = false:
  if ArtBas.Lager = false then ArtBas.Lager = true.
End.
Display 'Sesong'. Pause 0.
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
Def  buffer bKundeKort for Kundekort.
Display 'Kundekort'. Pause 0.
For each KundeKort where  length(KundeKort.KortNr) = 6  and 
  not can-find(bKundeKort where bKundeKort.KortNr = left-trim(KundeKort.KortNr,'0')):
  KundeKort.KortNr = left-trim(KundeKort.KortNr,'0').
End.
display 'ELogg'. pause 0.
for each ELogg where EksterntSystem matches '*korr*':
  delete ELogg.
end.


