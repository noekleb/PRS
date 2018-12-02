current-window:width = 200.


def buffer bbonglinje for bonglinje.

def var iAnt as int no-undo.

for each Kunde no-lock:

/* NB:
Dette mågjøres for flere betalingstyper. 
50 - Kontant
58 - Bank
Mulig også ennå flere. SJekk.
*/

for each BongHode no-lock where
  BongHode.KundeNr = Kunde.KundeNr
  :
  if can-find(First BongLinje no-lock where
  BongLinje.b_id = BongHode.B_Id and BongLinje.ttid = 65) and
  can-find(first BongLinje where BongLinje.B_Id = BongHode.B_Id and
  BongLinje.TTId = 50) then
  do:
    find first BongLinje where BongLinje.B_Id = BongHode.B_Id and
      BongLinje.TTId = 50 no-error.
    iant = iAnt + 1.
  
    find first KundeBetTrans where
        KundeBetTrans.Dato           = BongLinje.TransDato and
        KundeBetTrans.BongId         = BongLinje.BongNr and
        KundeBetTrans.BongLinjeNr    = BongLinje.LinjeNr and
        KundeBetTrans.betButik       = BongLinje.ButikkNr and
        KundeBetTrans.betKassaNr     = BongLinje.KasseNr and
        KundeBetTrans.betBongId      = BongLinje.BongNr no-error.
        
    if available KundeBetTrans then
        KundeBetTrans.MotPostert = false.
        
        
     display
    iAnt
    BongHode.DAto
    BongHode.BongNr
    BongHode.KundeNr
    available BongLinje
    available KundeBetTrans
    KundeBetTrans.TTId when available KundeBetTrans
    KundeBetTrans.Belop when Available KundeBetTrans
    KundeBetTrans.MotPostert when available KundeBetTrans
    with width 198 .
  end.
end.


end.
