current-window:width = 300.

for each ArtBas where 
  ArtBas.StrTypeId >= 2 and
  ArtBAs.STrTypeId <= 3 and 
  can-find(first BestHode of ArtBas):
    /*
    display
      ArtBas.ArtikkelNr
      ArtBAs.StrTypeId
      .
    */

    /* Korr av strekkoder */
    for each STrekkode of ArtBas where
      Strekkode.StrKode = 264:
      assign
        Strekkode.StrKode = 1
        .
    end.
    
    /* Korr av artlag */
    for each ArtLag exclusive-lock where
      ArtLag.ArtikkelNr = ArtBAs.ArtikkelNr and
      ArtLag.StrKode = 264:
      
      assign
        ArtLAg.StrKode = 1
        ArtLag.Storl = ' 1'
        .
    end.
    
    for each BestHode of ArtBas where BestHode.StrTypeId = 3:
      assign
        BestHode.StrTypeId = 2.
        
      for each BestSort of BestHode:
        /*
        display
          BestSort.BestNr
          BestSort.SortId
          BestSort.StrInterval
          BestSort.Fordeling
          BestSort.Storrelser
          with width 300.
        */  
      end.
      
      for each BestSTr of BestHode where
        BestSTr.Storl = 'OS':
        assign
          BestStr.Storl = ' 1'.
      end.
    end.
    
    for each Translogg where
      Translogg.ArtikkelNr = ArtBas.ArtikkelNr and
      Translogg.Storl = 'OS':
      
      assign
        Translogg.Storl = ' 1'.
      
    end.
end.
