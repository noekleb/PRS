FOR EACH ArtBAs WHERE ArtikkelNr = 0:
  FOR EACH Strekkode OF ArtBas:
    DELETE STrekkode.
  END.
  FOR EACH ArtPris OF ArtBAs:
    DELETE ArtPris.
  END.
  DELETE ArtBas.
END.

FOR EACH VPIArtBAs WHERE ArtikkelNr = 0:
  FOR EACH VPIStrekkode OF VPIArtBas:
    DELETE VPISTrekkode.
  END.
  FOR EACH VPIArtPris OF VPIArtBAs:
    DELETE VPIArtPris.
  END.
  DELETE VPIArtBas.
END.
FOR EACH Strekkode WHERE 
    Strekkode.ArtikkelNr = 0:
    DELETE Strekkode.
END.
FOR EACH VPIStrekkode WHERE 
    VPIStrekkode.VareNr = '':
    DELETE VPIStrekkode.
END.
FOR EACH VPIStrekkode WHERE 
    VPIStrekkode.VareNr = '0':
    DELETE VPIStrekkode.
END.

