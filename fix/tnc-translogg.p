CURRENT-WINDOW:WIDTH = 200.

FIND medlem 200000545 NO-LOCK.

FOR EACH MedTrans OF Medlem NO-LOCK:
    FIND TransLogg NO-LOCK WHERE
        TransLogg.Butik    = MedTrans.Butik AND
        TransLogg.TransNr  = MedTrans.TransNr AND
        TransLogg.SeqNr    = MEdTrans.SeqNr NO-ERROR.
    DISPLAY
        MedTrans.MedlemsNr
        MedTrans.Vg
        MedTrans.LopNr
        MedTrans.Antall
        MedTrans.Pris
        WITH WIDTH 198.
    DISPLAY
        TransLogg.Butik  
        TransLogg.TransNr 
        TransLogg.SeqNr
        TransLogg.MedlemsNr
        TransLogg.Kund
        WITH WIDTH 198.

END.

