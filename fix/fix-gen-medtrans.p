/* 13/12/00 COPY assignment */

FOR EACH TransLogg NO-LOCK WHERE
  TransLogg.BatchNr = 1099:
  
  
  IF CAN-DO("1,3,10",STRING(TransLogg.TTId)) THEN
DO:
  CREATE MedTrans.
  ASSIGN
    MedTrans.BatchNr        = TransLogg.BatchNr
    MedTrans.Butik          = TransLogg.Butik
    MedTrans.TransNr        = TransLogg.TransNr
    MedTrans.ForsNr         = TransLogg.ForsNr
    MedTrans.TTId           = TransLogg.TTId
    MedTrans.TBId           = TransLogg.TBId
    MedTrans.ArtikkelNr     = TransLogg.ArtikkelNr
    MedTrans.LevNr          = TransLogg.LevNr
    MedTrans.BongId         = TransLogg.BongId
    MedTrans.BongLinjeNr    = TransLogg.BongLinjeNr
    MedTrans.KassaNr        = TransLogg.KassaNr
    MedTrans.Vg             = TransLogg.Vg
    MedTrans.LopNr          = TransLogg.LopNr
    MedTrans.Storl          = TransLogg.Storl
    MedTrans.Antall         = TransLogg.Antall
    MedTrans.Pris           = TransLogg.Pris
    MedTrans.RabKr          = TransLogg.RabKr.

  ASSIGN
    MedTrans.Mva            = TransLogg.Mva
    MedTrans.Dato           = TransLogg.Dato
    MedTrans.Tid            = TransLogg.Tid
    MedTrans.SeqNr          = TransLogg.SeqNr
    MedTrans.VVarekost      = TransLogg.VVarekost
    MedTrans.SattVVareKost  = TransLogg.SattVVareKost
    MedTrans.MedlemsNr      = 1
    MedTrans.KortNr         = TransLogg.KortNr.
END.
END.
