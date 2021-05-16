DEF VAR ibutNr AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    dDato = 05/28/2020
    ibutNr = 11
    .

FIND FIRST BokforingsBilag NO-LOCK WHERE 
    Bokforingsbilag.OmsetningsDato = dDato AND 
    Bokforingsbilag.ButikkNr = iButNr NO-ERROR.
FOR EACH Bokforingsvisning NO-LOCK WHERE 
    BokforingsVisning.BokforingsId = bokforingsBilag.bokforingsId:
    
    DISPLAY
        Bokforingsbilag.OmsetningsDato 
        bokforingsBilag.ButikkNr
    WITH WIDTH 350.
    DISPLAY
        BokforingsVisning
    WITH WIDTH 350.
END.
