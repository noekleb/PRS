DEF VAR X AS DEC.
FOR EACH MEdtrans NO-LOCK WHERE
    MedTrans.MedlemsNr = 200002249:

    X = X + (MEdTrans.Pris - MedTrans.Mva - MedTrans.RabKr - MEdTrans.SubTotalRab) * 
            (IF MedTrans.Antall < 0
               THEN -1
               ELSE 1).

    DISPLAY
        (MEdTrans.Pris - MedTrans.Mva - MedTrans.RabKr - MEdTrans.SubTotalRab)
        X
        .
END.
