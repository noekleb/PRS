
run ByttButikk (261, 309).
run ByttButikk (323, 353).
run ByttButikk (324, 354).
run ByttButikk (325, 355).
run ByttButikk (326, 356).

procedure ByttButikk:
  def input parameter iButNr1 as int no-undo.
  def input parameter iButNr2 as int no-undo.

  /*261	Anton Sport Moss AS	*iButNr2*/
  display "KOrdreHode Butik iButNr1" with frame g.
  for each KOrdreHode where
    KOrdreHode.ButikkNr = iButNr1:
    KOrdreHode.ButikkNr = iButNr2. 
  end.   
  display "FakturaHode Butik iButNr1" with frame g.
  for each FakturaHode where
    FakturaHode.ButikkNr = iButNr1:
    FakturaHode.ButikkNr = iButNr2. 
  end.  
  display "KundeTrans Butik iButNr1" with frame g.
  for each KundeTrans where
    KundeTrans.Butik = iButNr1:
    KundeTrans.Butik = iButNr2. 
  end.  
  display "MedTrans Butik iButNr1" with frame g.
  for each MedTrans where
    MedTrans.Butik = iButNr1:
    MedTrans.Butik = iButNr2. 
  end.  
  display "Medlem Butik iButNr1" with frame g.
  for each Medlem where
    Medlem.Butik = iButNr1:
    Medlem.Butik = iButNr2. 
  end.  
  display "Kunde Butik iButNr1" with frame g.
  for each Kunde where
    Kunde.Butik = iButNr1:
    Kunde.Butik = iButNr2. 
  end.  
end procedure.
