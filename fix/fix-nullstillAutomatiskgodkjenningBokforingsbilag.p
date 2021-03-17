FOR EACH Bokforingsbilag EXCLUSIVE-LOCK WHERE 
    Bokforingsbilag.ButikkNr = 2 AND 
    BokforingsBilag.Omsetningsdato >= 09/05/2020:

    /*
    FOR EACH BokforingsKorrBilag OF BokforingsBilag EXCLUSIVE-LOCK:
        IF BokforingsKorrBilag.Merknad BEGINS 'Automatisk godkjent' OR 
           BokforingsKorrBilag.Merknad BEGINS 'Atomatisk øresavrunding' THEN
            DELETE BokforingsKorrBilag.        
    END.
    */

    ASSIGN 
        BokforingsBilag.GodkjentFlagg = FALSE
        .
END.
