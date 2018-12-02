CURRENT-WINDOW:WIDTH = 300.

DEF BUFFER bMedlemsKort FOR MedlemsKort.
DEF BUFFER bMedlem      FOR Medlem.

/* Renser nye tabeller. */
FOR EACH MedKjop:
    DELETE MedKjop.
END.
FOR EACH MedRabSjekk:
    DELETE MedRabSjekk.
END.
FOR EACH MedRabReskontr:
    DELETE MedRabReskontr.
END.

FOR EACH Medlem EXCLUSIVE-LOCK WHERE
    Medlem.KundeNr = 0,
    EACH MedlemsKor OF Medlem EXCLUSIVE-LOCK:

    FIND KundeKort EXCLUSIVE-LOCK WHERE
        KundeKort.KortNr = medlemsKort.KortNr NO-ERROR.
    IF AVAILABLE KundeKort THEN
        FIND Kunde OF KundeKort EXCLUSIVE-LOCK NO-ERROR.
    FIND bMedlemskort WHERE 
         bMedlemskort.KortNr = '983' + MedlemsKort.KortNr NO-ERROR.
    IF AVAILABLE bMedlemsKort THEN
        FIND bMedlem OF bMedlemskort NO-ERROR.

    /*
    DISPLAY
        Medlem.MedlemsNr
        Medlem.ForNavn FORMAT "x(10)"
        Medlem.Etternavn FORMAT "x(10)"
        Medlem.KundeNr
        MedlemsKort.KortNr WHEN AVAILABLE MedlemsKort
        MedlemsKort.RegistrertDato WHEN AVAILABLE Medlemskort
        KundeKort.KortNr WHEN AVAILABLE KundeKort
        Kunde.Navn COLUMN-LABEL 'KundeNavn' WHEN AVAILABLE Kunde
        Kunde.KundeNr COLUMN-LABEL 'KundeNr' WHEN AVAILABLE Kunde

        bMedlemsKort.KortNr WHEN AVAILABLE bMedlemskort
        bMedlemsKort.MedlemsNr WHEN AVAILABL bMedlemsKort
        bMedlem.MedlemsNr WHEN AVAILABLE bMedlem
        WITH WIDTH 300.
    PAUSE 0 BEFORE-HIDE.
    */
    IF AVAILABLE bMedlemsKort THEN
        FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
          TransLogg.MedlemsNr = Medlem.MedlemsNr:
          IF TransLogg.MedlemsNr <> bMedlemsKort.MedlemsNr THEN
              TransLogg.MedlemsNr = bMedlemsKort.MedlemsNr.
          /*
          DISPLAY
            TransLogg.Dato
            TransLogg.MedlemsNr
            TransLogg.KortNr
            TransLogg.KundNr
            WITH WIDTH 300.
          */
        END.
    /* Sletter medlemmet. */
    RUN slettMEdlem.p (Medlem.MedlemsNr).

END.

DISPLAY 'Sletter medlemsstatistikk'. pause 0.
FOR EACH StLinje WHERE StLinje.StTypeId = 'Medlem':
    DELETE StLinje.
END.

FOR EACH Medlem WHERE Medlem.KundeNr > 0 AND
    NOT CAN-FIND(Kunde WHERE Kunde.KundeNr = Medlem.KundeNr):
    /* Sletter medlemmet. */
    RUN slettMEdlem.p (Medlem.MedlemsNr).        .
END.
DISPLAY 'Sletter medlem'. pause 0.

/* Denne må kjøres med forsiktighet. Kjør alltid blokken etter også */ 
FOR EACH Medlem:
    FIND Kunde WHERE 
        Kunde.KundeNr = Medlem.KundeNr.
    IF Kunde.Navn <> (Medlem.fornavn + " " + Medlem.Etternavn) THEN 
        /* Sletter medlemmet. */
        RUN slettMEdlem.p (Medlem.MedlemsNr).        .
END.
DEF VAR cKKort AS CHAR NO-UNDO.
DEFINE VARIABLE bOk    AS LOG       NO-UNDO.
DEFINE VARIABLE cMsgs  AS CHARACTER NO-UNDO.
DEF VAR dMedlemsNr     AS DEC NO-UNDO.


FOR EACH Kunde WHERE NOT CAN-FIND(FIRST Medlem WHERE Medlem.KundeNr = kunde.KundeNr):
    FIND FIRST KundeKort OF Kunde.
    cKKort = KundeKort.KortNr.
    DELETE KundeKort.



    /* Bare medlem skal opprettes. */
    RUN genkundeMedlem.p (Kunde.Butik,
                          Kunde.GruppeId,
                          INPUT-OUTPUT Kunde.KundeNr,
                          OUTPUT dMedlemsNr,
                          OUTPUT bOk,
                          OUTPUT cMsgs).
    /* Kundekortene legges opp på samme kunde, men unike medlemskort */
    /* legges på separate medlemmer.                              */
    RUN genkundekort_og_medlem.p (Kunde.ButikkNr,
                                  Kunde.KundeNr,
                                  dMedlemsNr,
                                  ckKort,
                                  ckKort,
                                  999,
                                  OUTPUT bOk,
                                  OUTPUT cMsgs).

END.


FOR EACH Kunde NO-LOCK,
    FIRST KundeKort OF Kunde NO-LOCK,
    EACH translogg WHERE TransLogg.KundNr = Kunde.KundeNr AND
         NOT can-find(Medlem WHERE Medlem.MedlemsNr = TransLogg.MedlemsNr):
        
        FIND FIRST MEdlem NO-LOCK WHERE
            Medlem.KundeNR = Kunde.KundeNr NO-ERROR.

        FIND FIRST MedlemsKort NO-LOCK WHERE
            MedlemsKort.KortNr = '983' + string(KundeKort.KortNr,'999999') NO-ERROR. 


        ASSIGN
            TransLogg.MedlemsNr = Medlem.MedlemsNr
            TransLogg.Korttype = 2
            .
END.

/* En dobbelsjekk */
FOR EACH BongHode WHERE
    BongHode.MedlemsNr > 0 AND
    NOT CAN-FIND(Medlem WHERE Medlem.MedlemsNr = BongHode.MedlemsNr):

    FIND Kunde WHERE Kunde.KundeNr = BongHode.KundeNr NO-LOCK NO-ERROR.
    FIND FIRST bMedlem WHERE bMedlem.KundeNr = Kunde.KundeNr NO-LOCK NO-ERROR.

    FIND MedlemsKort NO-LOCK WHERE
        MedlemsKort.KortNr = '983' + BongHode.KundeKort NO-ERROR.
    IF AVAILABLE MedlemsKort THEN 
        FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
    IF AVAILABLE Medlem AND AVAILABLE bMedlem THEN
    DO:
        IF bMedlem.MedlemsNr = Medlem.MedlemsNr THEN
        ASSIGN
        BongHode.MedlemsNr   = Medlem.MedlemsNr
        BongHode.MedlemsKort = '983' + BongHode.KundeKort.
    END.
END.

/* Navnerydding. */
FOR EACH Kunde, EACH Medlem OF Kunde:
    ASSIGN
        Medlem.ForNavn   = Kunde.Navn
        Medlem.EtterNavn = ''.
END.

/* Fikser statsistikk og saldo på alle medlemmene. */
FOR EACH Medlem NO-LOCK:
  DISPLAY 
      "Oppdaterer statistikk på medlem " + 
      STRING(Medlem.MedlemsNr) + " " + Medlem.ForNavn + " " + MEdlem.EtterNavn
      FORMAT "x(50)" 
      .
  PAUSE 0.
  RUN oppdatmedlemstat.p(Medlem.MedlemsNr).
  RUN beregnmedlemsaldo.p(Medlem.MedlemsNr, 0).
END.
