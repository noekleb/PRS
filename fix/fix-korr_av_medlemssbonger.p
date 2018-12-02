CURRENT-WINDOW:WIDTH = 250.

DEFINE VARIABLE dDato AS DATE        NO-UNDO.
FOR EACH butiker NO-LOCK:
    FOR EACH kasse WHERE kasse.butik = butiker.butik NO-LOCK:
         DO dDato = DATE(1,1,2010) TO DATE(10,2,2010):
             FOR EACH bonghode WHERE bonghode.butikknr = butiker.butik AND
                                     bonghode.gruppenr = 1 AND
                                     bonghode.kassenr  = kasse.kassenr AND
                                     bonghode.dato     = dDato NO-LOCK:
                 IF BongHode.MedlemsKort = "" AND BongHode.MedlemsNr > 0 THEN
                 DO:
                     FIND MedlemsKort NO-LOCK WHERE
                         MedlemsKort.KortNr = string(BongHode.MedlemsNr)
                     
                     DISP 
                     bonghode.butikknr bonghode.dato bonghode.bongnr bonghode.medlemskort bonghode.medlemsnr
                     WITH WIDTH 250.
                 END.
             END.
         END.
    END.
END.

