find besthode no-lock where BestHode.BestNr = 31.
display besthode.BestNr BestHode.Beskrivelse BestHode.OrdreNR BestHode.BestStat.
run bytbeststatus.p (recid(BestHode), "+",2).
