CURRENT-WINDOW:WIDTH = 350.
FOR EACH EODKasse WHERE 
    EODKasse.butikkNr = 10 AND 
    EODKasse.EODDato = 09/13/2016:
    DISPLAY
        EODKasse
    WITH WIDTH 350.
    /*DELETE EODKasse.*/

END.
