CURRENT-WINDOW:WIDTH = 350.

FOR EACH BokforingsKorrBilag WHERE 
    BokforingsKorrBilag.TTId = 134,
    FIRST BokforingsBilag NO-LOCK WHERE 
        BokforingsBilag.OmsetningsDato = TODAY - 5 AND 
        BokForingsBilag.BokforingsId = BokforingsKorrBilag.BokforingsId:
    
    DISPLAY
        BokforingsBilag.ButikkNR
        BokforingsKorrBilag.TTId
        BokforingsKorrBilag.TbId
        BokforingsKorrBilag.Merknad
        BokforingsKorrBilag.Belop
        BokforingsKorrBilag.KontoNr
        BokforingsKorrBilag.DatoTid
    WITH WIDTH 350.
    
    /*UPDATE KontoNr.*/
END.
