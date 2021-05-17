 CURRENT-WINDOW:WIDTH = 350.
FOR EACH Bokforingsbilag NO-LOCK,
    EACH Bonghode NO-LOCK WHERE 
        BongHode.butikkNr = BokforingsBilag.ButikkNr AND 
        BongHode.Dato = BokforingsBilag.OmsetningsDato:
    DISPLAY
        BokforingsBilag.BokforingsId
        Bokforingsbilag.ButikkNr
        Bokforingsbilag.Aar
        Bokforingsbilag.BokforingsNr
        '|'
        Bokforingsbilag.OmsetningsDato
        Bokforingsbilag.EODMottatt
        BokforingsBilag.GodkjentFlagg
        BokforingsBilag.SendtRegnskap
        '|'
        Bokforingsbilag.GodkjentDato
        Bokforingsbilag.EODDato
        '|'
        BongHode.ButikkNr
        BongHode.Dato
    WITH WIDTH 350. 

END.
