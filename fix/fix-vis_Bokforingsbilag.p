 CURRENT-WINDOW:WIDTH = 350.
FOR EACH Bokforingsbilag NO-LOCK:
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
    WITH WIDTH 350. 

END.
