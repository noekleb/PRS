/* 13/12/00 COPY assignment */

FOR EACH TransLogg NO-LOCK WHERE
  TransLogg.BatchNr = 1099:
  
  
  IF CAN-DO("1,3,10",STRING(TransLogg.TTId)) THEN
DO:
  CREATE KundeTrans.
  ASSIGN
    KundeTrans.BatchNr        = TransLogg.BatchNr
    KundeTrans.Butik          = TransLogg.Butik
    KundeTrans.TransNr        = TransLogg.TransNr
    KundeTrans.ForsNr         = TransLogg.ForsNr
    KundeTrans.TTId           = TransLogg.TTId
    KundeTrans.TBId           = TransLogg.TBId
    KundeTrans.ArtikkelNr     = TransLogg.ArtikkelNr
    KundeTrans.LevNr          = TransLogg.LevNr
    KundeTrans.BongId         = TransLogg.BongId
    KundeTrans.BongLinjeNr    = TransLogg.BongLinjeNr
    KundeTrans.KassaNr        = TransLogg.KassaNr
    KundeTrans.Vg             = TransLogg.Vg
    KundeTrans.LopNr          = TransLogg.LopNr
    KundeTrans.Storl          = TransLogg.Storl
    KundeTrans.Antall         = TransLogg.Antall
    KundeTrans.Pris           = TransLogg.Pris
    KundeTrans.RabKr          = TransLogg.RabKr.

  ASSIGN
    KundeTrans.Mva            = TransLogg.Mva
    KundeTrans.Dato           = TransLogg.Dato
    KundeTrans.Tid            = TransLogg.Tid
    KundeTrans.SeqNr          = TransLogg.SeqNr
    KundeTrans.VVarekost      = TransLogg.VVarekost
    KundeTrans.SattVVareKost  = TransLogg.SattVVareKost
    KundeTrans.KundeNr        = 1
    KundeTrans.KortNr         = TransLogg.KortNr.
END.
END.
