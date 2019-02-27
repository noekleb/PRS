
run ByttButikk (261, 309).
run ByttButikk (323, 353).
run ByttButikk (324, 354).
run ByttButikk (325, 355).
run ByttButikk (326, 356).

procedure ByttButikk:
  def input parameter iButNr1 as int no-undo.
  def input parameter iButNr2 as int no-undo.
  
  display "BongHode".
  for each BongHode where
    BongHode.ButikkNr = iButNr1:
    BongHode.ButikkNr = iButNr2.
    for each BongLinje where
      BongLinje.B_Id = BongHode.B_Id:
        BongLinje.ButikkNR = iButNr2.
      end. 
  end.  
  pause 0.
  display "Datasett".
  for each Datasett where
    Datasett.ButikkNr = iButNr1:
    Datasett.ButikkNr = iButNr2. 
  end.     
end procedure.

