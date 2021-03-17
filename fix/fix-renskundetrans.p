def var piKundeNr as int no-undo.
/*
Sletter alle gavekortstransaksjoner som ikke skal ligge på kunden.
*/
  
PUBLISH 'infoDisp' ("Sletter dobbelposterte gavekort.").
for each Kunde no-lock:
  for each KundeBetTrans of Kunde where
      KundeBetTrans.TTId = 134 exclusive-lock:
      
     find BongHode no-lock where
     BongHode.ButikkNr = KundeBetTrans.Butik and
     BongHode.GruppeNr = 1 and
     BongHode.KasseNr  = KundeBetTrans.KassaNr and
     BongHode.Dato     = KundeBetTrans.Dato and
     BongHode.BongNr   = KundeBetTrans.BongId.
  
     if BongHode.KundeNr <> KundeBetTrans.KundeNr then
     do:
       /*
       display
         KundeBetTrans.TTId
         KundeBetTrans.Butik
         KundeBetTrans.KassaNr
         kundeBetTrans.Dato
         KundeBetTrans.BongId
         KundeBetTrans.KortNr
         BongHode.BongNr
         BongHode.Dato 
         Kunde.KundeNr
         BongHode.KundeNr
         BongHode.KundeKort       
         with width 248.
       */
       /* DØDEN */
       delete kundeBetTrans.
     end.     
    
  
  end.
end.
