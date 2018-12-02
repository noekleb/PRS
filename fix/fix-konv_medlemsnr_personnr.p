/* fix-konv_medlemsnr_personnr.p */

DEF VAR lMedlemsNr AS DEC NO-UNDO.

FOR EACH Medlem WHERE 
    Medlem.MedlemsNr < 9999999999:

    IF NOT CAN-DO('10,11',STRING(LENGTH(Medlem.PersonNr))) THEN
        NEXT.

    ASSIGN
        lMedlemsNr = Medlem.MedlemsNr.
    RUN konverter_medlemsnr_personnr.p (INPUT-OUTPUT lMedlemsNr).
END.
