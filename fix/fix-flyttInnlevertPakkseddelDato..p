FOR EACH PkSdlHode:
    PkSdlHode.SendtDato = 04/29/2020.
    FOR EACH PkSdlMottak:
        PkSdlMottak.MottattDato = PkSdlHode.SendtDato.
    END.
    /* NB NB NB Her flyttes ALLE */
    FOR EACH VareTrans WHERE 
        VareTrans.Transaksjonstype = 5:
        VareTrans.Dato = PkSdlHode.SendtDato.
    END.
END.
