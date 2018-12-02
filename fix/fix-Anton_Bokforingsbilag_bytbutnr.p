
run ByttButikk (261, 309).
run ByttButikk (323, 353).
run ByttButikk (324, 354).
run ByttButikk (325, 355).
run ByttButikk (326, 356).

procedure ByttButikk:
  def input parameter iButNr1 as int no-undo.
  def input parameter iButNr2 as int no-undo.

  for each Akt_Rapp where
    Akt_Rapp.Butik = iButNr1:
    Akt_Rapp.Butik = iButNr2. 
  end.  
  for each dags_rap where
    dags_rap.Butik = iButNr1:
    dags_rap.Butik = iButNr2. 
  end.  
  for each kas_rap where
    kas_rap.Butik = iButNr1:
    kas_rap.Butik = iButNr2. 
  end.  
  for each Kort_Spes where
    Kort_Spes.Butik = iButNr1:
    Kort_Spes.Butik = iButNr2. 
  end.  
  for each BokforingsBilag where
    BokforingsBilag.ButikkNr = iButNr1:
    BokforingsBilag.ButikkNr = iButNr2. 
  end.  
  for each KassererDag where
    KassererDag.ButikkNr = iButNr1:
    KassererDag.ButikkNr = iButNr2. 
  end.  
  for each KassererKontanter where
    KassererKontanter.ButikkNr = iButNr1:
    KassererKontanter.ButikkNr = iButNr2. 
  end.  
  for each KassererOppgj where
    KassererOppgj.ButikkNr = iButNr1:
    KassererOppgj.ButikkNr = iButNr2. 
  end.  
  for each KassererValuta where
    KassererValuta.ButikkNr = iButNr1:
    KassererValuta.ButikkNr = iButNr2. 
  end.  
   
end procedure.

