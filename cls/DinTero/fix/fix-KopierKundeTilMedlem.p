DEF VAR wKortNummer AS CHAR NO-UNDO.
DEF VAR wTekst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH Kunde WHERE 
    Kunde.ePostadresse > '' AND 
    Kunde.mobiltlf > '':


    /*
    DISPLAY
        Kunde.KundeNr
        Kunde.ePostAdresse
        Kunde.Mobiltlf
        CAN-FIND(FIRST Medlem WHERE 
                    Medlem.KundeNr = Kunde.KundeNr)    
        WITH WIDTH 350.
    */

    IF NOT CAN-FIND(FIRST Medlem WHERE 
                    Medlem.KundeNr = Kunde.KundeNr) THEN
    DO:
        CREATE Medlem.
        ASSIGN 
            Medlem.Medtype          = 1
            Medlem.MedGruppe        = 3 /* Kunde */            
            Medlem.HovedMedlemFlagg = true
            Medlem.HovedMedlemsNr   = 0.0
            Medlem.RegKode          = "Kunde"
            Medlem.ButikkNr         = Kunde.ButikkNr

            Medlem.ForNavn          = ENTRY(1,Kunde.Navn,' ')
            Medlem.EtterNavn        = REPLACE(Kunde.Navn,Medlem.ForNavn,'')
            Medlem.Adresse1         = Kunde.Adresse1
            Medlem.Adresse2         = Kunde.Adresse2
            Medlem.PostNr           = Kunde.PostNr
            Medlem.Telefon          = Kunde.KontTelefon
            Medlem.Telefaks         = Kunde.KontTelefaks
            Medlem.MobilTlf         = Kunde.KontMobilTlf
            Medlem.Land             = Kunde.Land
            Medlem.ButikkNr         = Kunde.ButikkNr
            Medlem.BydelsNr         = Kunde.BydelsNr
            Medlem.ePostAdresse     = Kunde.KontE-Post
            Medlem.KundeNr          = Kunde.KundeNr
            .
        ASSIGN 
            wKortNummer = STRING(Medlem.MedlemsNr)
            .

        IF wKortNummer = "" THEN DO:
              FIND LAST Medlemskort WHERE Medlemskort.Kortnr > (STRING(Medlem.ButikkNr) + "000000") AND 
                                          Medlemskort.Kortnr < (STRING(Medlem.ButikkNr) + "999999") AND 
                                          LENGTH(Medlemskort.Kortnr) = LENGTH(STRING(Medlem.ButikkNr) + "999999") NO-LOCK NO-ERROR.
              ASSIGN wTekst = IF AVAIL Medlemskort THEN
                  STRING(DECI(Medlemskort.Kortnr) + 1) ELSE 
                      STRING(Medlem.ButikkNr) + "000001".
              REPEAT:
                  RUN sjekkomkorterunikt.p (INPUT wTekst).
                  IF RETURN-VALUE = "" THEN
                  DO:
                    CREATE MedlemsKort.
                    ASSIGN MedlemsKort.MedlemsNr = Medlem.MedlemsNr
                           MedlemsKort.KortNr    = wTekst
                           MedlemsKort.AktivertDato = TODAY
                           MedlemsKort.UtgarDato    = TODAY + (365).
                      LEAVE.
                  END.
                  ELSE
                      ASSIGN wTekst = STRING(DECI(wTekst) + 1).
              END.
          END.

          ELSE
          KORTNUMMER:
          DO:
              FIND FIRST MedlemsKort WHERE
                   MEdlemsKort.KortNr    = wKortNummer no-error.
              IF NOT AVAILABLE MedlemsKort THEN
                CREATE MedlemsKort.
              ASSIGN
                  MedlemsKort.MedlemsNr = Medlem.MedlemsNr
                  MedlemsKort.KortNr    = wKortNummer
                  MedlemsKort.AktivertDato = TODAY
                  MedlemsKort.UtgarDato    = TODAY + (365)
                  MedlemsKort.Innehaver    = Medlem.Fornavn + " " +
                                             Medlem.EtterNavn
                  .
          END. /* KORTNUMMER */
    END.
END.
