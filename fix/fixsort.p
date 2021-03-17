def var wStrListe as char format "x(80)" no-undo.
def var wAntListe as char format "x(80)" no-undo.
def var wSeqNr    as int                 no-undo.
def var wStrTypeId as int no-undo.

def buffer bArtBas for ArtBas.

assign
  current-window:width = 181.

for each artbas exclusive-lock where
  ArtBas.Sort > 0
  break by ArtBas.Sort with frame Ost width 120:
  
  if first-of(ArtBas.Sort) and
     ArtBas.Sort <> 0 then
    do:
      assign 
        wStrListe = ""
        wSeqNr    = 0.
      
      find sortiment of artbas no-lock no-error.
      if available Sortiment then
        do:
          /* Oppretter StrType Hode */
          find StrType no-lock where
            StrType.StrTypeId = Sortiment.Sort + 800 no-error.
          if not available StrType then
            do:
              create StrType.
              assign
                StrType.StrTypeId = Sortiment.Sort + 800
                StrType.Beskrivelse = Sortiment.SoBeskr
                wStrTypeId = StrType.StrTypeId.
            end.
          find bArtBas where
            recid(bArtBas) = recid(ArtBAs).
          assign
            bArtBAs.StrTypeId = StrType.StrTypeId.
                   
          for each SortAnt of Sortiment no-lock :
            assign
              wSeqNr    = wSeqNr + 1
              wStrListe = wStrListe + 
                          (if wStrListe = ""
                            then ""
                            else ";") +
                          SortAnt.SoStorl
              wAntListe = wAntListe + 
                          (if wAntListe = ""
                            then ""
                            else ";") +
                          string(SortAnt.SoAnt).
                          
             /* Oppretter StrTStr */
             find StrTStr no-lock where
               StrTStr.StrTypeId = StrType.StrTypeId and
               StrTStr.SeqNr     = wSeqNr no-error.
             if not available StrTStr then
               do:
                 create StrTStr.
                 assign
                   StrTStr.StrTypeId = StrType.StrTypeId
                   StrTStr.SeqNr     = wSeqNr
                   StrTStr.SoStorl   = SortAnt.SoStorl.
               end.
          end.
        end.  
        display ArtBas.sort sortiment.sobeskr wStrListe with frame Ost Down.
    end.
  assign 
    artbas.strtypeid = wStrTypeId.
end.  
