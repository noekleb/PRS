current-window:width = 178.

def var x as int.
for each besthode no-lock:
  if BestHode.ArtikkelNr < 4000 then
    next.
    
  find artbas of besthode no-lock.
  find first artpris of artbas exclusive-lock.
  find first bestPris of BestHode no-lock.

  OUTPUT TO "SkoTeddi.txt" APPEND.
  export artbas.artikkelnr.
  OUTPUT CLOSE.
  
  if artpris.pris[1] = 0 then
    do:
    x = x + 1 .
      display 
        artbas.artikkelnr 
        artbas.vg 
        artbas.lopnr 
        artbas.registrertdato
        artpris.pris 
        
        BestPris.Pris
        
      with width 176.
      RUN SettArtPris.
    end.
end.
display x.

PROCEDURE SettArtPris:
  ASSIGN
    ArtPris.Tilbud          = false
    ArtPris.VareKost[1]     = BestPris.VareKost 
    ArtPris.MvaKr[1]        = BestPris.MvaKr 
    ArtPris.LevNr           = ArtBas.LevNr
    ArtPris.EuroManuel      = false
    ArtPris.ValPris[1]      = BestPris.ValPris 
    ArtPris.Rab1Kr[1]       = BestPris.Rab1Kr 
    ArtPris.Rab1%[1]        = BestPris.Rab1% 
    ArtPris.Rab2Kr[1]       = BestPris.Rab2Kr 
    ArtPris.Rab2%[1]        = BestPris.Rab2% 
    ArtPris.Frakt[1]        = BestPris.Frakt 
    ArtPris.Frakt%[1]       = BestPris.Frakt% 
    ArtPris.DivKostKr[1]    = BestPris.DivKostKr 
    ArtPris.DivKost%[1]     = BestPris.DivKost% 
    ArtPris.Rab3Kr[1]       = BestPris.Rab3Kr 
    ArtPris.Rab3%[1]        = BestPris.Rab3% 
    ArtPris.DBKr[1]         = BestPris.DBKr 
    ArtPris.DB%[1]          = BestPris.DB% 
    ArtPris.EuroPris[1]     = BestPris.EuroPris 
    ArtPris.InnkjopsPris[1] = BestPris.InnkjopsPris 
    ArtPris.Mva%[1]         = BestPris.Mva% 
    ArtPris.Pris[1]         = BestPris.Pris.

END PROCEDURE.
