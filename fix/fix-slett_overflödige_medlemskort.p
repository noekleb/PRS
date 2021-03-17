DEF BUFFER bMedlemsKort FOR Medlemskort.

CURRENT-WINDOW:WIDTH = 250.

FOR EACH MedlemsKort NO-LOCK WHERE
    MedlemsKort.RegistrertDato = TODAY:
    FOR EACH bMedlemsKort EXCLUSIVE-LOCK WHERE
        bMedlemsKort.MedlemsNr = MedlemsKort.MedlemsNr:

        IF bMedlemsKort.KortNr <> MedlemsKort.KortNr AND 
            NOT CAN-FIND(FIRST MedTrans WHERE MedTrans.KortNr = bMedlemsKort.KortNr) THEN
        DO:
            DELETE bMedlemsKort.
            /*
            DISPLAY
                MedlemsKort.KortNr
                MedlemsKort.RegistrertDato
                bMedlemsKort.KortNr
                bMedlemsKort.RegistrertDato
                '*' WHEN CAN-FIND(FIRST MedTrans WHERE MedTrans.KortNr = bMedlemsKort.KortNr)
                WITH WIDTH 250
                .
            */
        END.
    END.
END.
