FOR EACH Bokforingsbilag WHERE 
    Bokforingsbilag.ButikkNr = 10 AND 
    BokforingsBilag.Omsetningsdato = 09/13/2016:

    ASSIGN 
        EODDato = OmsetningsDato
        EODMottatt = TRUE 
        EODDatoTidMottatt = NOW
        .
    ASSIGN 
        EODDato = ?
        EODMottatt = FALSE 
        EODDatoTidMottatt = ?
        .
END.
