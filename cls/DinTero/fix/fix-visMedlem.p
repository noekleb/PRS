
CURRENT-WINDOW:WIDTH = 350.
FOR EACH Medlem EXCLUSIVE-LOCK WHERE 
    Medlem.EDato = TODAY
    /*Medlem.EksterntMedlemsNr = '10019'*/
    /*Medlem.EksterntMEdlemsNr = '83c49362-10cb-4781-8592-c6c18546ae9f'*/
    :

    DISPLAY
        Medlem.MedlemsNr
        Medlem.EksterntMedlemsNr FORMAT "x(50)"
        Medlem.forNavn FORMAT "x(30)"
        Medlem.Etternavn
        Medlem.Medtype
        Medlem.ePostAdresse
        Medlem.MobilTlf
        Medlem.MKlubBId
        Medlem.MedGruppe
        Medlem.Kjonn
        Medlem.Kilde
        Medlem.EDato
        STRING(Medlem.ETid,"HH:MM:SS")
    WITH WIDTH 350.

  /* UPDATE  Medlem.forNavn Etternavn Medlem.EksterntMedlemsNr. */
END.
