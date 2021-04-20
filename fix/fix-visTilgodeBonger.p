CURRENT-WINDOW:WIDTH = 350.

FOR EACH Kasse NO-LOCK WHERE 
  Kasse.ButikkNr = 2 AND 
  Kasse.GruppeNr = 1 AND 
  Kasse.KasseNr <= 90,
  EACH bongHode NO-LOCK WHERE 
      BongHode.ButikkNr = Kasse.butikkNr AND 
      BongHode.GruppeNr = Kasse.GruppeNr AND          
      BongHode.KasseNr = Kasse.KasseNr AND
      BongHode.Dato >= 07/25/2020 AND
      CAN-FIND(FIRST BongLinje WHERE 
               BongLinje.B_Id = BongHode.B_Id AND 
               BongLinje.TTId = 69),
  EACH BongLinje NO-LOCK WHERE 
      BongLinje.ButikkNr = Kasse.ButikkNr AND 
      BongLinje.GruppeNr = Kasse.GruppeNr AND 
      BongLinje.KasseNr = Kasse.KasseNr AND
      BongLinje.Dato = BongHode.Dato AND 
      BongLinje.TTId = 69 /* AND 
      BongLinje.ArtikkelNr = '9002'*/:

   DISPLAY
     BongHode.ButikkNr
     BongHode.Dato
     BongHode.BongNr
     bonglinje.ttid
     bonglinje.tbid
     BongLinje.ArtikkelNr
   WITH WIDTH 350.
END.
