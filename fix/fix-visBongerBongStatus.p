CURRENT-WINDOW:WIDTH = 350.

FOR EACH Kasse NO-LOCK WHERE 
  Kasse.ButikkNr = 2 AND 
  Kasse.GruppeNr = 1 AND 
  Kasse.KasseNr <= 90,
  EACH bongHode NO-LOCK WHERE 
      BongHode.ButikkNr = Kasse.butikkNr AND 
      BongHode.GruppeNr = Kasse.GruppeNr AND          
      BongHode.KasseNr = Kasse.KasseNr AND
      BongHode.Dato >= 08/07/2020 AND 
      BongHode.BongNr = 13328:

   DISPLAY
     BongHode.ButikkNr
     BongHode.Dato
     BongHode.BongNr
     BongHode.BongStatus
     /*
     bonglinje.ttid
     bonglinje.tbid
     BongLinje.ArtikkelNr
     */
   WITH WIDTH 350.
END.
