CURRENT-WINDOW:WIDTH = 350.
FOR EACH Medlem EXCLUSIVE-LOCK WHERE 
    Medlem.eksterntMedlemsNr > '' AND 
    Medlem.eksterntMedlemsNr <> ? AND 
    NOT Medlem.eksterntMedlemsNr BEGINS 'M-' AND 
    Medlem.EDato >= TODAY - 30:

    DISPLAY
        Medlem.MedlemsNr
        Medlem.forNavn
        Medlem.Etternavn
        Medlem.EksterntMedlemsNr
        Medlem.Kilde
        Medlem.Mobiltlf
    WITH WIDTH 350.
    /*
    UPDATE 
        Medlem.MobilTlf
        medlem.EksterntMedlemsNr FORMAT "x(60)"
        .
    */
    eTid = TIME.
    Kilde = 'Dintero'.
END.
