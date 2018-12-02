/************************************************************
    Program:  x-oppdlagertrans.i
    Created:  TN   13 Mar 99
Description:

    TN 4/6-99   Lagt inn kontroll på at varen har varekost.
    TN 8/6-99   Korrigert loging av feil ved sjekk på varekos = 0.

Last change:  TN    5 Apr 100    9:03 am
************************************************************/

    /* Slipper overføringslås på artikkelen */
    IF TransLogg.TTId = 6 THEN
      DO:
        FIND FIRST KonvReg EXCLUSIVE-LOCK WHERE
          KonvReg.EDB-System = wEDB-System AND
          KonvReg.Tabell     = wTabell   AND
          KonvReg.EkstId     = string(TransLogg.ArtikkelNr) NO-ERROR NO-WAIT.
        IF AVAILABLE KonvReg THEN
          DELETE KonvReg.
      END.

    /* Initiering */
    ASSIGN
      wVareKost  = 0
      wTotAntall = wTotAntall + 1. /* Antall poster importert */

    /* Sjekker butikk */
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = TransLogg.Butik NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
      DO:
        ASSIGN TransLogg.FeilKode = 10. /* Ukjent butikk på transen */
        RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
        NEXT OPPDAT_TRANS{&Blokk}.
      END.
      

    /* Sjekker mottagende butikk på overføringstransaksjoner */
    IF TransLogg.TTId = 6 THEN
      DO:
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = TransLogg.OvButik NO-ERROR.
        IF NOT AVAILABLE Butiker THEN
          DO:
            ASSIGN TransLogg.FeilKode = 11. /* Ukjent butikk på transen */
            RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
            NEXT OPPDAT_TRANS{&Blokk}.
          END.
      END.

    FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = TransLogg.Storl NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN
      DO:
        FIND LAST StrKonv NO-LOCK USE-INDEX StrKode.
        IF AVAILABLE StrKonv THEN
            piStrKode = StrKonv.StrKode + 1.
        ELSE
            piStrKode = 1.
        CREATE StrKonv.
        ASSIGN
            StrKonv.Storl   = Translogg.Storl
            StrKonv.StrKode = piStrKode
            StrKonv.Merknad = "Opprettet fra x-oppdlagertrans.w"
            .
      END.

    /* Info om Bestillingen */
    IF VALID-HANDLE(wProgram-Handle) THEN
      RUN ProfilInfo IN wProgram-Handle (INPUT STRING(Butiker.Butik) + " " +
                                    Butiker.ButNamn).

  /* Dobbelfind på artikkel. */
    IF TransLogg.ArtikkelNr = 0 OR NOT CAN-FIND(ArtBas WHERE
                                                ArtBas.ArtikkelNr = Translogg.ArtikkelNr) THEN
    DO:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = Translogg.Kode NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN
            FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = LEFT-TRIM(Translogg.Kode,"0") NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN
        DO:
            IF (LENGTH(TransLogg.Kode) > 5 AND LENGTH(TransLogg.Kode) < 13) THEN
            FIND Strekkode WHERE
              Strekkode.Kode = STRING(DEC(Translogg.Kode),"9999999999999") NO-ERROR.
        END.
        IF AVAILABLE Strekkode THEN
        DO:
            FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN
                ASSIGN
                TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
                TransLogg.Vg         = ArtBas.Vg
                TransLogg.LopNr      = ArtBas.LopNr
                .

        END.
        /* Sjekker match mot Vg, Varetekst og størrelse */
        ELSE DO:
            IF AVAILABLE ArtBas THEN RELEASE ArtBas.
            IF AVAILABLE StrTstr THEN RELEASE StrTStr.
            FIND FIRST ArtBas NO-LOCK WHERE
                ArtBas.Vg    = TransLogg.Vg AND
                ArtBas.Beskr BEGINS TransLogg.BongTekst NO-ERROR.
            IF AVAILABLE ArtBas THEN
            DO:
                FIND  FIRST StrTStr WHERE
                      StrTStr.StrTypeId = ArtBas.StrTypeId AND
                      StrTStr.SoStorl   = TransLogg.Storl NO-ERROR.
                IF AVAILABLE StrTStr THEN
                    TransLogg.ArtikkelNr = ArtBas.ArtikkelNr.
            END.
        END.
    END.
    /* Henter artikkelinformasjonen */
    FIND FIRST ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg    = TransLogg.Vg AND
          ArtBAs.LopNr = TransLogg.LopNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN
          DO:
            ASSIGN TransLogg.FeilKode = 3. /* Ukjent artikkelnummer */
            RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
            NEXT OPPDAT_TRANS{&Blokk}.
          END.
        ELSE DO:
          ASSIGN 
            TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
            TransLogg.FeilKode   = 3. /* Artikkelnummer oppdatert */
          RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
        END.
      END.
    
    /* Bygger varegruppe og løpenummer på transaksjonen hvis denne er endret. */
    /* Artikkelnummer gjelder som kobling mot artikkelen.                     */
    IF ArtBas.Vg <> Translogg.Vg OR ArtBas.LopNr <> Translogg.LopNr THEN
        ASSIGN
        TransLogg.Vg    = ArtBas.Vg
        TransLogg.LopNr = ArtBas.LopNr
        .
    /* Aktiverer varer i kasse hvis de ikke er aktivert fra før. */
    IF ArtBas.IKasse = FALSE AND Translogg.TTId = 5 THEN
    DO:
        FIND CURRENT ArtBas EXCLUSIVE-LOCK.
        ASSIGN
            ArtBas.IKasse = TRUE
            .
        FIND CURRENT ArtBas NO-LOCK.
    END.

    /* Er det transaksjoner av type overføring (som bare kan komme fra kassen), skal */
    /* disse ignoreres for PLU og ikke lagerstyrte varer.                            */
    IF CAN-DO("6",STRING(TransLogg.TTId)) THEN
    DO:
      IF ArtBas.Lager = FALSE OR
         ArtBas.OPris = TRUE THEN
      DO:
        ASSIGN TransLogg.FeilKode = 20. /* Overføring på ikke lagerstyrt vare. */
        RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
        NEXT OPPDAT_TRANS{&Blokk}.
      END.
    END.

    /* Henter Moms% */
    IF AVAILABLE ArtBas THEN /* Setter moms på transer med kjent artikkelnr. */
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VarGr THEN
      FIND VarGr NO-LOCK WHERE  /* Setter moms på transer med ukjent artikkelnr. */
        VarGr.Vg = TransLogg.Vg NO-ERROR.
    IF AVAILABLE VarGr THEN
      DO:
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        IF AVAILABLE Moms THEN
          wMva% = Moms.MomsProc.
        ELSE
          wMva% = 0.
      END.
    ELSE
      wMva% = 0.
    /* Bruker default MVA hvis den ikke er satt. */
    IF NOT AVAILABLE VarGr AND wMva% = 0 THEN
      wMva% = wDefMva%.

    /* Kontrollerer Kortnummer. Meldem og Kundekort. */
    IF TransLogg.KortNr <> "" THEN
    KORTSALG:
    DO:
      /* Slippet eventuell medlemspost. */
      IF AVAILABLE Medlem THEN
        RELEASE Medlem.

      /* Tester på medlemskort med og uten ledende nuller. */
      FIND FIRST Medlemskort NO-LOCK WHERE
        MedlemsKort.KortNr = TransLogg.KortNr NO-ERROR.
      IF NOT AVAILABLE MedlemsKort THEN
        FIND FIRST MedlemsKort NO-LOCK WHERE
          MedlemsKort.KortNr = STRING(INT(TransLogg.KortNr),"999999") NO-ERROR.
      IF AVAILABLE MedlemsKort THEN
      MEDLEMSKORT:
      DO:
        FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
        IF AVAILABLE Medlem THEN
        DO:
          ASSIGN
            TransLogg.MedlemsNr = Medlem.MedlemsNr
            TransLogg.KortType  = 3
            .
          /* Er medlemmet koblet til en kunde, skal også kundenummer settes i transaksjonen. */
          IF Medlem.KundeNr <> 0 THEN
            ASSIGN
              TransLogg.KundNr = Medlem.KundeNr.
        END.
      END. /* MEDLEMSKORT */

      /* Var det ikke medlemskort, sjekkes det om det var et kundekort. */
      ELSE
      KUNDEKORT:
      DO:
        FIND FIRST KundeKort NO-LOCK WHERE
          KundeKort.KortNr = TransLogg.KortNr NO-ERROR.
        IF NOT AVAILABLE KundeKort THEN
          FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KortNr = STRING(INT(TransLogg.KortNr),"999999") NO-ERROR.
        IF AVAILABLE KundeKort THEN
        DO:
          FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
          IF AVAILABLE Kunde THEN
            DO:
              ASSIGN
                TransLogg.KortType = 2
                TransLogg.kundnr   = Kunde.KundeNr
                .
            END.
        END.
      END. /* KUNDEKORT */

      /* Medlem opprettes automatisk hvis det ikke finnes og kortet ikke er */
      /* et kundekort.                                                      */
      IF (wOpprettMedlem AND         /* Flagger automatisk opprettelse. */
          NOT AVAILABLE Medlem) THEN /* Medlem finnes ikke fra før      */
      OPPRETTMEDLEM:
      DO:
        /* Er det et gyldig kundenummer, skal medlem ikke opprettes automatisk. */
        IF TransLogg.MedlemsNr = 0 AND TransLogg.KundNr <> 0 THEN 
            LEAVE OPPRETTMEDLEM.

        RUN opprettmedlem.p (?, INPUT TransLogg.KortNr, INPUT Translogg.Butik , OUTPUT wMedlemsNr).
        FIND Medlem EXCLUSIVE-LOCK WHERE
            Medlem.MedlemsNr = wMedlemsNr NO-ERROR.
        IF AVAILABLE Medlem THEN
          ASSIGN
            Medlem.EtterNavn    = "But/Kasse: " + 
                                  STRING(TransLogg.Butik) + "/" + 
                                  STRING(TransLogg.KassaNr) + " " + 
                                  STRING(TransLogg.Dato)
            TransLogg.MedlemsNr = Medlem.MedlemsNr
            TransLogg.KortType  = 3
            .
        RELEASE Medlem.
      END. /* OPPRETTMEDLEM */

      /* Hvis både medlems og kundenummer er ukjent, skal transen ikke oppdateres. */
      IF (TransLogg.KundNr    = 0 AND
          TransLogg.MedlemsNr = 0) THEN
      DO:
        ASSIGN TransLogg.FeilKode = 14. /* Ukjent kunde/medlem på transaksjonen */
        RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
        NEXT OPPDAT_TRANS{&Blokk}.
      END.
    END. /* KORTSALG */

    /* Henter lager og varekost for butikken */
    FIND Lager EXCLUSIVE-LOCK WHERE
      Lager.ArtikkelNr = TransLogg.ArtikkelNr AND
      Lager.Butik      = TransLogg.Butik NO-ERROR NO-WAIT.
    IF LOCKED Lager THEN
      DO:
        ASSIGN TransLogg.FeilKode = 4. /* Posten er låst fra en annen terminal */
        RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
        NEXT OPPDAT_TRANS{&Blokk}.
      END.
    IF NOT AVAILABLE Lager THEN
      DO:
        ASSIGN TransLogg.FeilKode = 1. /* Lagerpost finnes ikke */
        RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
        CREATE Lager.
        ASSIGN
            Lager.ArtikkelNr = TransLogg.ArtikkelNr
            Lager.Butik      = TransLogg.Butik
            .
      END.

     /* Henter ArtPris */
    FIND bButiker NO-LOCK WHERE
        bButiker.Butik = TransLogg.Butik NO-ERROR.
    IF AVAILABLE bButiker THEN
        FIND ArtPris OF ArtBas NO-LOCK WHERE
             ArtPris.ProfilNr = bButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE bButiker OR NOT AVAILABLE ArtPris THEN
    DO:
        FIND bButiker WHERE bButiker.Butik = iCl NO-ERROR.
        IF AVAILABLE bButiker THEN
            FIND ArtPris OF ArtBas NO-LOCK WHERE
                 ArtPris.ProfilNr = bButiker.ProfilNr NO-ERROR.
    END.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      
    /* Er varekost eller lagerantall 0 eller negativ, skal varekost fra kalkyle benyttes */
    /*IF Lager.VVareKost <= 0 OR Lager.Lagant <= 0 OR ArtBas.Lager = FALSE THEN*/
    /* På ikke lagerstyrte varer, skal nå varekosten fra kassen benyttes. */
    IF (Lager.VVareKost <= 0 OR Lager.Lagant <= 0) AND ArtBas.Lager = TRUE THEN
    DO:
        /* Initierer vektet varekost. */
        IF AVAILABLE ArtPris THEN
            Lager.VVareKost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1].
    END.

    /* Setter kalkulert og vektet varekost m.m.*/
    ASSIGN
        TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                   THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                   ELSE Translogg.KalkylePris
        TransLogg.Varekost     = IF AVAILABLE ArtPris
                                   THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                   ELSE TransLogg.Varekost
        TransLogg.Varekost     = IF TransLogg.Varekost = ?
                                   THEN 0
                                   ELSE TransLogg.Varekost
        TransLogg.Mva%         = (IF AVAILABLE ArtPris
                                    THEN ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                    ELSE (TransLogg.Mva / (TransLogg.Pris - TransLogg.Mva - TransLogg.RabKr)) * 100)
        Translogg.Mva%         = (IF TransLogg.Mva% = ? THEN 0 ELSE TransLogg.Mva%)
        wVareKost              = Lager.VVareKost
        wVareKost              = (IF wVareKost = ? THEN 0 ELSE wVareKost)
        .
        
    /* Disse skal ha kalkulert varekost fra normalkalkylen */
    IF CAN-DO('002,005,011',STRING(TransLogg.TTId,"999")) THEN 
      ASSIGN 
        wVareKost = ArtPris.VareKost[1]
        wVareKost = (IF wVareKost = ? THEN 0 ELSE wVareKost)
        .

    /* Har artikkelen åpen pris, skal varekost settes fra kalkylen. */
    IF ArtBas.OPris THEN
        wVareKost = 0.

    /* Sjekker om varekost er satt.                                       */
    /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
    IF wVareKost = 0 THEN /* or wBrutto% *** Skal også utføres for brutto% artikkler */
      DO:
        IF VALID-HANDLE(h_PrisKo) THEN
          /* NB: Varekost skal regnes av pris eksklusive rabatter       */
          /* Mva trekkes fra i rutinen som kalles. Fordi hvis det er    */
          /* gitt rabatt, er det feil mva. MvaKr må da beregnes pånytt. */
          RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                        INPUT TransLogg.Butik, 
                                          INPUT (Translogg.Pris - (Translogg.Pris - (Translogg.Pris / (1 + (Translogg.Mva% / 100))))), 
                                        OUTPUT wVareKost).
        IF wVareKost = 0 THEN
          DO:
            ASSIGN TransLogg.FeilKode = 11. /* Varekost = 0 */
            RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
          END.
      END.

    /* Vi setter den vektede varekosten inn i salgstransaksjonen.          */
    /* NB: Motposterte transaksjoner skal ikke ha korreksjon av vvarekost. */
    /*     Motposterte transaksjoner har feltet SattVVarekost satt.        */
    IF TransLogg.SattVVarekost = FALSE OR TransLogg.VVareKost = 0 OR TransLogg.VVarekost = ? THEN
    ASSIGN
        TransLogg.SattVVareKost = TRUE
        TransLogg.VVareKost     = wVareKost
        .

    /* info om transaksjon */
    ASSIGN
      wLestAntall = wLestAntall + 1.
    IF VALID-HANDLE(wProgram-Handle) THEN
      RUN TransInfo IN wProgram-Handle
                    (INPUT STRING(TransLogg.TransNr) + "/" +
                           string(TransLogg.SeqNr) + " Artikkel: " +
                           string(TransLogg.Vg) + "/" +
                           string(TransLogg.LopNr), 
                     INPUT "Oppdatert " + 
                           string(wLestAntall)
                    ).

    /* Posterer medlemssalg hvis medlemsnummer er satt i transaksjonen. */
    IF TransLogg.MedlemsNr <> 0 AND can-do("1,3,10",STRING(TransLogg.TTId)) THEN
    DO:
        RUN MedlemsSalg.
    END.

    /* Posterer kundesalg hvis kundenummer er satt i transaksjonen, */
    IF TransLogg.KundNr <> 0 AND can-do("1,3,10",STRING(TransLogg.TTId)) THEN
    DO:
        RUN KundeSalg.
    END.
    
    /* NonSale skal ikke behandles her. */
    IF ArtBas.Non_Sale = FALSE THEN 
    DO: 
      /* Posterer i lager hvis artikkelen har lagerstyring. */
      RUN PosterLager.
      IF RETURN-VALUE = "UNDO" THEN
        UNDO OPPDAT_TRANS{&Blokk}, NEXT OPPDAT_TRANS{&Blokk}.
      ELSE IF RETURN-VALUE = "NEXT" THEN
        NEXT OPPDAT_TRANS{&Blokk}.  

      /* Posterer i ArtLag hvis artikkelen har lagerstyring og størrelser.*/
      RUN PosterArtLag.
      IF RETURN-VALUE = "UNDO" THEN
        UNDO OPPDAT_TRANS{&Blokk}, NEXT OPPDAT_TRANS{&Blokk}.
      ELSE IF RETURN-VALUE = "NEXT" THEN
        NEXT OPPDAT_TRANS{&Blokk}.
    END.
    
    /* Posterer i statistikkene.*/
    RUN PosterStatistikk.
    IF RETURN-VALUE = "UNDO" THEN
      UNDO OPPDAT_TRANS{&Blokk}, NEXT OPPDAT_TRANS{&Blokk}.
    ELSE IF RETURN-VALUE = "NEXT" THEN
      NEXT OPPDAT_TRANS{&Blokk}.      

    /* Kvitterer ut transaksjonen                   */
    /* Nullstiller også eventuelle gamle feilkoder. */
    ASSIGN
      TransLogg.Postert     = TRUE
      TransLogg.PostertDato = TODAY
      TransLogg.PostertTid  = TIME
      TransLogg.FeilKode    = 0.

    /* info om transaksjon */
    ASSIGN
      wOppdatertAntall = wOppdatertAntall + 1.

    /* Oppretter plukkposter hvis butikken har plukklager. */
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = TransLogg.butik NO-ERROR.
    IF Butiker.Plukkbutik > 0 AND CAN-DO("1,10",STRING(TransLogg.TTId)) THEN
    DO:
      IF ArtBas.Lager = TRUE AND ArtBas.OPris = FALSE AND ArtBas.Non_Sale = FALSE THEN
        RUN Plukklager.
    END.



