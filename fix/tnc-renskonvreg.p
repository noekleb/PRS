   
FOR EACH kONVrEG WHERE
    KonvReg.EDB-System = "RESTPAR" and
    KonvReg.Tabell     = "" AND
    NOT CAN-FIND(ArtBAs WHERE
                 ArtBas.ArtikkelNr = DEC(KonvReg.EkstId)):
     
  DISPLAY
      KonvReg.EDB-System
      KonvReg.Tabell
      KonvReg.EkstId
      KonvReg.InterntId
      .
  DELETE KonvReg.
    /*
    
    KonvReg.EkstId     = string(ArtBas.ArtikkelNr)) then
    */
END.
