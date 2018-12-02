CURRENT-WINDOW:WIDTH = 250.
FOR EACH VPIArtBAs WHERE StrType < 2:
    IF TRIM(VPIArtBAs.EkstStrTypeNavn) <> '' THEN
        FIND StrType NO-LOCK WHERE
          StrType.Beskrivelse = trim(VPIArtBAs.EkstStrTypeNavn) NO-ERROR.
    FIND ArtBAs NO-LOCK WHERE
        ArtBas.ArtikkelNR = VPIArtBas.ArtikkelNr NO-ERROR.

    /*
    DISPLAY
        VPIArtBAs.EkstVPILEvNr
        VPIArtBAs.ArtikkelNr
        VPIArtBAs.Beskr
        VPIArtBAs.StrtypeId
        StrType.StrTypeId WHEN AVAILABLE StrType column-label 'EKSTERNT'
        VPIArtBAs.Pakke
        VPIArtBAs.EkstStrTypeNavn
        VPIArtBAs.RegistrertDato
        ArtBas.ArtikkelNR
        ArtBas.StrTypeId
        CAN-FIND(StrType WHERE StrType.StrTypeId = ArtBAs.StrTypeId)
        WITH WIDTH 250.
  */      
  IF ArtBas.StrTypeId > 2 THEN
      VPIArtBAs.StrTypeId = ArtBas.StrTypeId.
  ELSE 
      VPIArtBAs.STrTypeId = 2.
END.
