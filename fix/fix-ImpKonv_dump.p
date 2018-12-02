OUTPUT TO VALUE('PRSMappingTotNyDump.csv').
FOR EACH impKonv NO-LOCK:
  PUT UNFORMATTED
    ImpKonv.EDB-System  ';'
    ImpKonv.Tabell  ';'    
    ImpKonv.EksterntId  ';'
    ImpKonv.Merknad  ';'
    ImpKonv.InterntId  
    SKIP.
END.

OUTPUT CLOSE.
