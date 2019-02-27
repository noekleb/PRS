current-window:width = 180.

def buffer clLager for LAger.

def var wCl as int no-undo.
{syspara.i 5 1 1 wCl int}

def stream Logg.

/* 
fix-ovfeil.p 10/1-00
Retter opp feil som har oppstått som følge av feilaktig opprettede 
transaksjoner i w-gridlager.w.
*/

def temp-table OvArtikkler
  field ArtikkelNr like ArtBas.ArtikkelNr.

output stream Logg to value("fix-ovfeil.log").
  
for each Batch no-lock where
  Batch.Beskrivelse begins "Overføringer:":
  for each TransLogg of BatchLogg no-lock where
    TransLogg.Postert = true:
    if not can-find(OvArtikkler where
                     OvArtikkler.ArtikkelNr = TransLogg.ArtikkelNr) then
      BLOKK:
      do:
        find clLager no-lock where
          clLager.ArtikkelNr = TransLogg.ArtikkelNr and
          clLager.Butik      = wCl no-error.
        for each LAger where
          LAger.ArtikkelNr = TransLogg.ArtikkelNr and
          LAger.Butik      <> wCl:
          if Lager.VVareKost = 0 then
            do:
              LAger.VVAreKost = clLager.VVareKost.
              create OvArtikkler.
              assign
              OvArtikkler.ArtikkelNr = TransLogg.ArtikkelNr.          
              leave BLOKK.
            end.
        end.
      end.
  end.    
end.  


TEMP:
for each OvArtikkler /*where ovArtikkler.ArtikkelNr = 17*/:
  find ArtBas no-lock where
    ArtBas.ArtikkelNr = OvArtikkler.ArtikkelNr no-error.
  if not available ArtBas then
    next TEMP.
    
  display stream Logg 
    OvArtikkler.ArtikkelNr
    ArtBas.Vg
    ArtBas.LopNr
  with width 178.
  
  find clLager no-lock where
    clLager.ArtikkelNr = OvArtikkler.ArtikkelNr and
    clLager.Butik      = wCl no-error.
  if available clLager then
    do:
      for each LAger where
        LAger.ArtikkelNr = OvArtikkler.ArtikkelNr and
        LAger.Butik      <> wCl:
        if Lager.VVareKost = 0 then
          LAger.VVAreKost = clLager.VVareKost.
/*
        display stream Logg 
          ArtBas.Vg column-label "Vg"
          ArtBas.LopNr column-label "LopNr"
          LAger.ArtikkelNr column-label "ArtNR"
          Lager.Butik column-label "But"
          Lager.Lagant column-label "Ant"
          Lager.VVareKost column-label "VVKst"
        with width 178.
*/        
      end.
    
    end.
end. /* TEMP */
output stream Logg close.
