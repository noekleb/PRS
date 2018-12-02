CURRENT-WINDOW:WIDTH = 200.

FOR EACH MedTrans /*WHERE
    MedTrans.KortNR = "1414" */
    /*
    BREAK BY MedTrans.KortNr
          BY MedTrans.MedlemsNr
    */
    :

    FIND FIRST MedlemsKort NO-LOCK WHERE
        MedlemsKort.KortNr = MedTrans.KortNr NO-ERROR. 

    IF AVAILABLE MedlemsKort THEN
        ASSIGN
        MedTrans.MedlemsNr = Medlemskort.MedlemsNr.

    DISPLAY
        AVAILABLE MedlemsKort
        MedlemsKort.KortNr
        MEdlemsKort.MedlemsNr
        "*"
        MedTrans.KortNr
        MEdTrans.MEdlemsNr
        MEdTrans.Dato
        MedTrans.Vg
        MedTrans.LopNr
        MEdTrans.Storl
        MedTrans.TransNr
        MedTrans.SeqNr
        WITH WIDTH 198.

END.
