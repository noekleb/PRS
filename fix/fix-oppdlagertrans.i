/************************************************************
    Program:  x-oppdlagertrans.i
    Created:  TN   13 Mar 99
Description:

    TN 4/6-99   Lagt inn kontroll på at varen har varekost.
    TN 8/6-99   Korrigert loging av feil ved sjekk på varekos = 0.

Last change:  TN    5 Apr 100    9:03 am
************************************************************/

    /* Initiering */
    ASSIGN
      wVareKost  = 0
      wTotAntall = wTotAntall + 1. /* Antall poster importert */

    /* Sjekker butikk */
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = TransLogg.Butik NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
      DO:
        NEXT OPPDAT_TRANS{&Blokk}.
      END.
      

    /* Sjekker mottagende butikk på overføringstransaksjoner */
    IF TransLogg.TTId = 6 THEN
      DO:
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = TransLogg.OvButik NO-ERROR.
        IF NOT AVAILABLE Butiker THEN
          DO:
            NEXT OPPDAT_TRANS{&Blokk}.
          END.
      END.

    /* Info om Bestillingen */
    IF VALID-HANDLE(wProgram-Handle) THEN
      RUN ProfilInfo IN wProgram-Handle (INPUT STRING(Butiker.Butik) + " " +
                                    Butiker.ButNamn).
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
            NEXT OPPDAT_TRANS{&Blokk}.
          END.
        ELSE DO:
        END.
      END.

    /* Bygger varegruppe og løpenummer på transaksjonen hvis denne er endret. */
    /* Artikkelnummer gjelder som kobling mot artikkelen.                     */
    /*
    IF ArtBas.Vg <> Translogg.Vg OR ArtBas.LopNr <> Translogg.LopNr THEN
        ASSIGN
        TransLogg.Vg    = ArtBas.Vg
        TransLogg.LopNr = ArtBas.LopNr
        .
    */
    /* Er det transaksjoner av type overføring (som bare kan komme fra kassen), skal */
    /* disse ignoreres for PLU og ikke lagerstyrte varer.                            */
    IF CAN-DO("6",STRING(TransLogg.TTId)) THEN
    DO:
      IF ArtBas.Lager = FALSE OR
         ArtBas.OPris = TRUE THEN
      DO:
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
    IF wMva% <> 0 AND YEAR(TransLogg.Dato) < 2005 THEN DO:
        ASSIGN wMva% = IF wMva% = 25 THEN 24 ELSE IF wMva% = 11 THEN 12
                       ELSE wMva%.
    END.
    /* Bruker default MVA hvis den ikke er satt. */
    IF NOT AVAILABLE VarGr AND wMva% = 0 THEN
      wMva% = wDefMva%.

    /* Henter lager og varekost for butikken */
    FIND Lager EXCLUSIVE-LOCK WHERE
      Lager.ArtikkelNr = TransLogg.ArtikkelNr AND
      Lager.Butik      = TransLogg.Butik NO-ERROR NO-WAIT.
    IF LOCKED Lager THEN
      DO:
        NEXT OPPDAT_TRANS{&Blokk}.
      END.
    IF NOT AVAILABLE Lager THEN
      DO:
/*         assign TransLogg.FeilKode = 1. /* Lagerpost finnes ikke */ */
/*         run LoggFeilITrans (input TransLogg.FeilKode).             */
        CREATE Lager.
        ASSIGN
          Lager.ArtikkelNr = TransLogg.ArtikkelNr
          Lager.Butik      = TransLogg.Butik.
      END.
    ASSIGN
      wVareKost = Lager.VVareKost.

    /* Har artikkelen åpen pris, skal varekost settes fra kalkylen. */
    IF ArtBas.OPris THEN
        wVareKost = 0.

    /* Sjekker om varekost er satt.                                       */
    /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
    IF wVareKost = 0 OR wVarekost = ? THEN /* or wBrutto% *** Skal også utføres for brutto% artikkler */
      DO:
        IF VALID-HANDLE(h_PrisKo) THEN
          RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                        INPUT TransLogg.Butik, 
                                        INPUT (Translogg.Pris - Translogg.RabKr - Translogg.Mva), 
                                        OUTPUT wVareKost).
        IF wVareKost = 0 THEN
          DO:
          END.
      END.
    IF wVarekost = ? THEN wVareKost = 0.

    /* info om transaksjon */
    ASSIGN
      wLestAntall = wLestAntall + 1.
    IF VALID-HANDLE(wProgram-Handle) THEN
      RUN TransInfo IN wProgram-Handle
                    (INPUT STRING(TransLogg.TransNr) + "/" +
                           string(TransLogg.SeqNr) + " Artikkel: " +
                           string(TransLogg.Vg) + "/" +
                           string(TransLogg.LopNr) + 
                           " Batch(" + STRING(Translogg.BatchNr) + ")", 
                     INPUT "Oppdatert " + 
                           string(wLestAntall)
                    ).

    /* Posterer i statistikkene.*/
    RUN PosterStatistikk.

    /* info om transaksjon */
    ASSIGN
      wOppdatertAntall = wOppdatertAntall + 1.

