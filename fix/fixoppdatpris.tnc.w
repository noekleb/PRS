find ArtBas where
  artbas.vg = 4 and lopnr = 638.
  
  

find VarGr of ArtBas.
find Moms of VarGr.  

display 
  ArtBas.ArtikkelNr 
  artBas.Vg 
  artBas.LopNr 
  artBas.Beskr
  Moms.MomsProc
with side-labels 1 column.
  
update ArtBas.ValPris.

for each Butiker:
   find first ArtLag where
     ArtLag.Butik = Butiker.Butik and
     ArtLag.Vg    = ArtBas.Vg     and
     ArtLag.LopNr = ArtBas.LopNr no-error.
    if available ArtLag then
      do:
/*
      update ArtLAg.NormPris = 391
             ArtLag.Pris     = 995
             ArtLag.ReaPris  = 699
             ArtLag.reatom   = 991230  
             Artlag.reafom   = 981230.  
*/             
      display ArtLag.Butik 
             ArtLAg.NormPris
             ArtLag.Pris     
             ArtLag.ReaPris  
             ArtLag.reatom     
             Artlag.reafom.
      end.  

end.
