FOR EACH Medlem WHERE
    MEdlem.Etternavn BEGINS "But/Kasse":
    DISPLAY
        fornavn
        etternavn
        medlemsnr.
    PAUSE 0.

    FOR EACH medlemsaldo WHERE Medlemsaldo.MedlemsNr = Medlem.MEdlemsNr:
        DELETE MEdlemsaldo.
    END.
    FOR EACH MedlemsKort OF Medlem:
        DELETE Medlemskort.
    END.
    FOR EACH MEdTrans OF Medlem:
        DELETE Medtrans.
    END.
    DELETE MEdlem.
END.
