
CURRENT-WINDOW:WIDTH = 200.
                          
FOR EACH MEdlem WHERE 
    MEdlem.MEdlemsNr = 200001522 and
    Medlem.KundeNR <> 0 NO-LOCK:
    FOR EACH MedTrans OF medlem NO-LOCK:
       IF NOT CAN-FIND(FIRST KundeTrans WHERE
                       KundeTrans.KundeNr   = Medlem.KundeNR and
                       KundeTrans.Butik     = MEdTrans.Butik AND
                       KundeTrans.TransNr   = MEdTrans.TransNR AND
                       KundeTrans.SeqNr     = MEdTrans.SeqNr 
                       /*
                       KundeTrans.KundeNr   = Medlem.KundeNR and
                       KundeTrans.Butik     = MEdTrans.Butik AND
                       KundeTrans.KassaNr   = MEdTrans.KassaNR AND
                       KundeTrans.Dato      = MEdTrans.DAto AND
                       KundeTrans.BongId    = MEdTrans.BongId AND
                       KundeTrans.Vg        = MEdTrans.Vg AND
                       KundeTrans.LopNr     = MEdTrans.LopNr
                       */)
                       THEN
       DO:
           DISPLAY
           MEdTrans.MEdlemsNr 
           MEdlem.KundeNr
           MEdTrans.Butik
           Medtrans.KAssa
           MEdtrans.Dato
           MEdTrans.BongId
           MEdTrans.Vg
           MEdTrans.LopNr
           MEdTrans.Storl
           MEdTrans.Antall
           MEdTrans.Pris
           WITH WIDTH 198
           .

          
          /* Salgstransaksjonene */
          DO ON ERROR UNDO, LEAVE:
              IF AVAILABLE KundeTrans THEN
                  RELEASE KundeTrans.
              CREATE KundeTrans.
              BUFFER-COPY MedTrans TO KundeTrans
                  ASSIGN
                  KundeTrans.KundeNr = MEdlem.KundeNr
                  NO-ERROR
                  .
              IF ERROR-STATUS:ERROR THEN
              DO:
                  IF AVAILABLE KundeTrans THEN
                      DELETE KundeTrans.
              END.

          END.
          
       END.
    END.
END.

