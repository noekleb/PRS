def var iKopierNyeArtikler as int no-undo.
assign
    iKopierNyeArtikler = 1
    .
def buffer bVPIArtBas for VPIArtBas.
        
for each VPIArtBas no-lock where
   VPIArtBas.EkstVPILEvNr >= 10000 and
   VPIArtBas.LEvNr <= 11999:
 
  
      /* Kopierer til VPI register 1 før det sendes. */
      IF (iKopierNyeArtikler > 0 AND VPIArtBas.EkstVPILevNr <> iKopierNyeArtikler) THEN
        DO: 
          IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr) THEN
            RUN vpiartbas_kopier_vpi_til_vpi.p (VPIArtBas.EkstVPILevNr,iKopierNyeArtikler,VPIArtBas.ArtikkelNr).
        END.
  
end.  
