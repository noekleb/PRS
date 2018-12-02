current-window:width  = 200.

for each Kunde no-lock :
for each KundeBetTrans where
KundebetTrans.KundeNr = Kunde.KundeNr:

find first BongHode no-lock where
BongHode.ButikkNr = KundeBetTrans.Butik and
BongHode.KasseNr  = KundeBetTrans.KassaNr and
BongHode.BongNr   = KundeBetTrans.BongId no-error.

if available BongHode  and BongHode.KundeNr <> KundeBetTrans.KundeNr
then
do:
delete kundebettrans.

end.
end.
end.
