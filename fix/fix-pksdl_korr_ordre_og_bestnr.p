current-window:width = 250.
for each PkSdlHode no-lock /*where
  PkSdlhode.PkSdlNr = "648"*/:
  
  display
  PkSdlHode.PkSdlId
  PkSdlHode.PkSdlNr
  PkSdlHode.SendtDato
  PkSdlHode.EkstId
  PkSdlHode.Cl
  with width 250.
  
  for each PkSdlLinje of PkSdlHode exclusive-lock:
    find first StrKonv no-lock where
      StrKonv.StrKode = PkSdlLinje.StrKode no-error. 
    find first BestHode where
      BestHode.ArtikkelNr = PkSdlLinje.ArtikkelNr /*and
      BestHode.LevDato    = PkSdlHode.SendtDato*/  and
      BestHode.EkstId     = PkSdlHode.EkstId and
      BestHode.BestStat  <= 4 and
      (can-find(first BestStr no-lock where
                BestStr.BestNr    = BestHode.BestNr and
                BestStr.Butik     = PkSdlLinje.ButikkNr and
                trim(BestStr.Storl)     = trim(StrKonv.Storl) and
                BestStr.BestStat  = BestHode.BestStat))
      no-error.
    
    if available BestHode then
    do:
      if (PkSdlLinje.OrdreNr = 0 and PkSdlLinje.BestNr = 0) then
        assign
        PkSdlLinje.OrdreNR = BestHode.OrdreNr
        PkSdlLinje.BestNr  = BestHode.BestNr
        .
    end.
    /*
    display
    PkSdlLinje.LevNr
    PkSdlLinje.ArtikkelNr
    PkSdlLinje.LevKod
    PkSdlLinje.Beskr
    PkSdlLinje.LevFargKod
    PkSdlLinje.OrdreNR
    PkSdlLinje.BestNr
    /*
    PkSdlLinje.ButikkNr
    PkSdlLinje.StrKode
    StrKonv.StrKode        when available StrKonv
    */
    BestHode.CL            when available BestHode
    BestHode.OrdreNr       when available BestHode
    BestHode.BestNr        when available BestHode
    /*BestHode.ArtikkelNr    when available BestHode*/
    BestHode.LevDato       when available BestHode
    BestHode.BestStat      when available BestHode
    BestHode.EkstId        when available BestHode
    with width 250.
    */
    
  end.  
end.  
