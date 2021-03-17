output to value("artbas.log").
  export "Eksport av artbas startet." today string(time,"HH:MM:SS").
output close.

def stream Data1.
def stream Data2.
def stream Data3.

output stream Data1 to value("artbas.d").
output stream Data2 to value("artpris.d").
output stream Data3 to value("Strekkode.d").
for each ArtBas no-lock where     
     ArtBas.WebButikkArtikkel = True:
  export stream Data1 ArtBas.
  for each ArtPris of ArtBAs no-lock:
    export stream Data2 ArtPris.
  end.
  for each Strekkode of ArtBas no-lock:
    export stream Data3 Strekkode.
  end.
end.
output stream Data3 close.
output stream Data2 close.
output stream Data1 close.

output to value("artbas.log") append.
  export "Eksport av Dagsoppgjor ferdig." today string(time,"HH:MM:SS").
output close.

