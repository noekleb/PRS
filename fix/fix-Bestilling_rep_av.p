current-window:width = 250.
for each Ordre 
  /*where
  Ordre.OrdreNr = 12600032 */:
  for each BestHode of Ordre where
    /*BestHode.BestNr = 12647635 and*/
    BestHode.StrTypeId = 2:
    
    find first BestSort of BestHode where BestSort.Fri = true.
  
    for each FriButik where
      Fributik.BestNr = BestHode.BestNr and
      FriButik.FriAntal[1] = 0:
  
      find BestStr no-lock where
        BestStr.BestNr = BestHode.BestNr and
        BestSTr.butik  = FriButik.butik and
        trim(BestStr.Storl) = trim(BestSort.Storrelser) and
        BestStr.BestStat = FriButik.BestStat no-error. 
  
      if available BestStr then FriButik.FriAntal[1] = BestStr.Bestilt.
      /*
      display
      BestSort.BestNr
      BestSort.Storrelser
      Fributik.BestNr
      Fributik.Butik
      Fributik.BestStat
      Fributik.FriAntal[1]
      Fributik.FriAntal[2]
      Fributik.TotAntal
      BestStr.Bestilt when available BestStr
      with width 250.
      */
      
    end.    
  end.
end.  
