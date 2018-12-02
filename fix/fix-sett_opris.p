def stream Inn.

def var cTekst as char no-undo.

input stream inn from value('opris.txt') no-echo.
repeat:

  import stream inn cTekst.
  
  if available ArtBas then release Artbas.
  
  find strekkode no-lock where
    Strekkode.Kode = cTekst no-error.
  if available Strekkode then find ArtBas of Strekkode.
  
  if available ArtBas then artbas.opris = true.
  
  display cTekst
  strekkode.Kode when available Strekkode.
end.

input stream inn close.
