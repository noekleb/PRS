/************************************************************
    Program:  x-oppdlagertrans.i
    Created:  TN   13 Mar 99
Description:

    TN 4/6-99   Lagt inn kontroll på at varen har varekost.
    TN 8/6-99   Korrigert loging av feil ved sjekk på varekos = 0.

Last change:  TN    6 May 100    9:39 am
************************************************************/

    /* Slipper overføringslås på artikkelen */
    if TransLogg.TTId = 6 then
      do:
        find first KonvReg exclusive-lock where
          KonvReg.EDB-System = wEDB-System and
          KonvReg.Tabell     = wTabell   and
          KonvReg.EkstId     = string(TransLogg.ArtikkelNr) no-error no-wait.
        if available KonvReg then
          delete KonvReg.
      end.

    /* Initiering */
    assign
      wVareKost  = 0
      wTotAntall = wTotAntall + 1. /* Antall poster importert */

    /* Sjekker butikk */
    find Butiker no-lock where
      Butiker.Butik = TransLogg.Butik no-error.
    if not available Butiker then
      do:
        assign TransLogg.FeilKode = 10. /* Ukjent butikk på transen */
        run LoggFeilITrans (input TransLogg.FeilKode).
        next OPPDAT_TRANS{&Blokk}.
      end.
      

    /* Sjekker mottagende butikk på overføringstransaksjoner */
    if TransLogg.TTId = 6 THEN
      DO:
        find Butiker no-lock where
          Butiker.Butik = TransLogg.OvButik no-error.
        if not available Butiker then
          do:
            assign TransLogg.FeilKode = 11. /* Ukjent butikk på transen */
            run LoggFeilITrans (input TransLogg.FeilKode).
            next OPPDAT_TRANS{&Blokk}.
          end.
      END.

    /* viser butikk som oppdateres. */  
    if first-of(TransLogg.Butik) then
      do:
        OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
        put stream LoggData unformatted 
        " Oppdaterer for butikk: " string(Butiker.Butik) + " " + Butiker.ButNamn skip. 
        OUTPUT STREAM LoggData close.        
      end.
      
    /* Info om Bestillingen */
    if valid-handle(wProgram-Handle) then
      run ProfilInfo in wProgram-Handle (input string(Butiker.Butik) + " " +
                                    Butiker.ButNamn).
    /* Henter artikkelinformasjonen */
    find first ArtBas no-lock where
      ArtBas.ArtikkelNr = TransLogg.ArtikkelNr no-error.
    if not available ArtBas then
      do:
        find ArtBas no-lock where
          ArtBas.Vg    = TransLogg.Vg and
          ArtBAs.LopNr = TransLogg.LopNr no-error.
        if not available ArtBas then
          do:
            assign TransLogg.FeilKode = 3. /* Ukjent artikkelnummer */
            run LoggFeilITrans (input TransLogg.FeilKode).
            next OPPDAT_TRANS{&Blokk}.
          end.
        else do:
          assign 
            TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
            TransLogg.FeilKode   = 3. /* Artikkelnummer oppdatert */
          run LoggFeilITrans (input TransLogg.FeilKode).
        end.
      end.

    /* Henter Moms% */
    if AVAILABLE ArtBas then
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    if AVAILABLE VarGr then
      DO:
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        if AVAILABLE Moms then
          wMva% = Moms.MomsProc.
        else
          wMva% = 0.
      END.
    ELSE
      wMva% = 0.

    /* Henter lager og varekost for butikken */
    find Lager exclusive-lock where
      Lager.ArtikkelNr = TransLogg.ArtikkelNr and
      Lager.Butik      = TransLogg.Butik no-error no-wait.
    if locked Lager then
      do:
        assign TransLogg.FeilKode = 4. /* Posten er låst fra en annen terminal */
        run LoggFeilITrans (input TransLogg.FeilKode).
        next OPPDAT_TRANS{&Blokk}.
      end.
    if not available Lager then
      do:
        assign TransLogg.FeilKode = 1. /* Lagerpost finnes ikke */
        run LoggFeilITrans (input TransLogg.FeilKode).
        create Lager.
        assign
          Lager.ArtikkelNr = TransLogg.ArtikkelNr
          Lager.Butik      = TransLogg.Butik.
      end.
    assign
      wVareKost = Lager.VVareKost.

    /* Sjekker om varekost er satt.                                       */
    /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
    if wVareKost = 0 then /* or wBrutto% *** Skal også utføres for brutto% artikkler */
      DO:
        if VALID-HANDLE(wLibHandle) then
          RUN HentVareKost in wLibHandle (INPUT ArtBas.ArtikkelNr, input TransLogg.Butik, INPUT Translogg.Pris, output wVareKost).
        if wVareKost = 0 then
          DO:
            assign TransLogg.FeilKode = 11. /* Varekost = 0 */
            run LoggFeilITrans (input TransLogg.FeilKode).
          END.
      END.

    /* Setter varekost i transaksjonen 1. gang den oppdateres.                    */
    /* Dette gj›r at varekost h†ndteres riktig ved motpostering av transaksjoner. */
    if TransLogg.SattVVareKost = FALSE then
      assign
        TransLogg.SattVVareKost = true
        TransLogg.VVareKost     = wVareKost.

    /* info om transaksjon */
    assign
      wLestAntall = wLestAntall + 1.
    if wLestAntall MODULO 10 = 0 then
      DO:
        if valid-handle(wProgram-Handle) then
          run TransInfo in wProgram-Handle
                        (input string(TransLogg.TransNr) + "/" +
                               string(TransLogg.SeqNr) + " Artikkel: " +
                               string(TransLogg.Vg) + "/" +
                               string(TransLogg.LopNr),
                         input "Oppdatert " +
                               string(wLestAntall)
                        ).
      END.

    /*
    /* Posterer i lager hvis artikkelen har lagerstyring. */
    run PosterLager.
    if return-value = "UNDO" then
      undo OPPDAT_TRANS{&Blokk}, next OPPDAT_TRANS{&Blokk}.
    else if return-value = "NEXT" then
      next OPPDAT_TRANS{&Blokk}.  
    */

    /*
    /* Posterer i ArtLag hvis artikkelen har lagerstyring og størrelser.*/
    run PosterArtLag.
    if return-value = "UNDO" then
      undo OPPDAT_TRANS{&Blokk}, next OPPDAT_TRANS{&Blokk}.
    else if return-value = "NEXT" then
      next OPPDAT_TRANS{&Blokk}.
    */

    /* Posterer i statistikkene.*/
    run PosterStatistikk.
    if return-value = "UNDO" then
      undo OPPDAT_TRANS{&Blokk}, next OPPDAT_TRANS{&Blokk}.
    else if return-value = "NEXT" then
      next OPPDAT_TRANS{&Blokk}.      

    /* Kvitterer ut transaksjonen                   */
    /* Nullstiller også eventuelle gamle feilkoder. */
    assign
      TransLogg.Postert     = true
      TransLogg.PostertDato = today
      TransLogg.PostertTid  = time
      TransLogg.FeilKode    = 0.

    /* info om transaksjon */
    assign
      wOppdatertAntall = wOppdatertAntall + 1.

