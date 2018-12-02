/* Program:     reklamasjonslinje_oppdater_lager_og_statistikk.p 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   
   
   Opprettet: 16/2-09 TN                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iBatchNr    AS INT NO-UNDO.
DEF VAR piTransNr   AS INT NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR iCL         AS INT NO-UNDO.
DEF VAR dVVAreKost  AS DEC NO-UNDO.

DEF BUFFER bButiker FOR Butiker.
DEF BUFFER clButiker FOR Butiker.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clbutiker.Butik = iCL NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    FIND ReklamasjonsLinje 
         WHERE ReklamasjonsLinje.ReklamasjonsNr = DECI(STRING(ihBuffer:BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE)) AND
               ReklamasjonsLinje.LinjeNr        = INT(STRING(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE))
         NO-LOCK NO-ERROR.
    IF AVAIL Reklamasjonslinje THEN DO:
      FIND Reklamasjonslogg OF Reklamasjonslinje NO-LOCK NO-ERROR.
      FIND ArtBas NO-LOCK where
          ArtBas.ArtikkelNr = Reklamasjonslinje.ArtikkelNr NO-ERROR.
      FIND bButiker NO-LOCK WHERE
          bButiker.Butik = Reklamasjonslinje.Butik NO-ERROR.
      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr = bbutiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN
          FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.

      IF AVAILABLE Reklamasjonslogg AND Reklamasjonslogg.OppdLager = FALSE THEN
        /* Oppretter batch og translogg. */
        OPPRETT_TRANS:
        DO TRANSACTION:
         IF Reklamasjonslinje.TTId = 4 THEN DO:
          IF iBatchNr = 0 THEN
              RUN OpprettBatch.
          IF iBatchNr = 0 or iBatchNr = ? THEN
              LEAVE OPPRETT_TRANS.

          /* Setter transaksjonsnummer  */
          find last TransLogg where
            TransLogg.Butik = Reklamasjonslinje.Butik
            use-index TransLogg no-error.
          if available TransLogg then
            piTransNr = TransLogg.TransNr + 1.
          else
            piTransNr = 1.

          ASSIGN
              dVVAreKost = Reklamasjonslinje.VVareKost.
          IF dVVarekost = 0 THEN
            DO:
              FIND Lager NO-LOCK WHERE
                Lager.ArtikkelNr = Reklamasjonslinje.ArtikkelNr AND
                Lager.Butik      = Reklamasjonslinje.Butik NO-ERROR.
              IF AVAILABLE Lager AND Lager.VVareKost > 0 AND Lager.VVareKost <> ? THEN
                  dVVareKost = Lager.VVareKost.
            END.
          IF dVVAreKost = 0 THEN
              dVVAreKost = ArtPris.Varekost[1].

          /* Oppretter TransLogg */    
          CREATE TransLogg.
          NYTRANSLOGG:
          DO WHILE TRUE ON ERROR UNDO, RETRY:
            assign 
              TransLogg.Butik        = Reklamasjonslinje.Butik
              TransLogg.TransNr      = piTransNr
              TransLogg.SeqNr        = 1
              NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                piTransNr = piTransNr + 1.
            ELSE LEAVE NYTRANSLOGG.
          END. /* NYTRANSLOGG */

          assign
            TransLogg.BatchNr      = iBatchNr
            TransLogg.TTId         = 4 /* Lagerreklamasjon */
            TransLogg.TBId         = 1
            TransLogg.ArtikkelNr   = Reklamasjonslinje.ArtikkelNr
            TransLogg.Vg           = ArtBas.VG
            TransLogg.LopNr        = ArtBas.LopNr
            TransLogg.Antall       = Reklamasjonslinje.Antall
            Translogg.Storl        = Reklamasjonslinje.Storl
            TransLogg.Pris         = dVVarekost * Reklamasjonslinje.Antall
            TransLogg.RabKr        = 0
            TransLogg.LevNr        = ArtBas.LevNr
            TransLogg.BongId       = 0
            TransLogg.BongLinjeNr  = 0
            TransLogg.KassaNr      = 0
            TransLogg.ForsNr       = Reklamasjonslogg.forsNr
            TransLogg.Plukket      = TRUE 
            TransLogg.Dato         = Reklamasjonslinje.Dato
            TransLogg.Tid          = Reklamasjonslinje.Tid
            TransLogg.SelgerNr     = 0
            TransLogg.BestNr       = 0
            TransLogg.Postert      = FALSE
            TransLogg.KortNr       = ''
            TransLogg.KundNr       = ReklamasjonsLogg.KundeNr
            Translogg.BongTekst    = ArtBas.Beskr
            TransLogg.VVareKost    = dVVarekost
            TransLogg.SattVVarekost = (IF can-do("001,003,010",string(Translogg.TTId))
                                         THEN TRUE /* Skal ikke regnes om ved opp. av statistikker. */
                                         ELSE FALSE)
            TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                       ELSE Translogg.KalkylePris
            TransLogg.Varekost     = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                       ELSE TransLogg.Varekost
            TransLogg.Mva          = Reklamasjonslinje.Mva
            Translogg.Mva          = (IF Translogg.Mva = ? THEN 0 ELSE Translogg.Mva)
            TransLogg.Mva%         = (IF AVAILABLE ArtPris
                                        THEN ArtPris.Mva%[1]
                                        ELSE TransLogg.Mva%)
            Translogg.Mva%         = (IF Translogg.Mva% = ? THEN 0 ELSE Translogg.Mva%)
            .
          END.
          FIND CURRENT ReklamasjonsLogg EXCLUSIVE-LOCK.
          ASSIGN
              ReklamasjonsLogg.OppdLager     = TRUE
              ReklamasjonsLogg.OppdLagerDato = TODAY
              Reklamasjonslogg.OppdLagerAv   = USERID('SkoTex')
              .
          FIND CURRENT ReklamasjonsLogg NO-LOCK.
        END. /* OPPRETT_TRANS */
    END.
    ELSE ocReturn = ocReturn + "Reklamasjonslinje " + STRING(ihBuffer:BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + "/" 
                             + STRING(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE) +
                               " ikke tilgj. for oppdatering av lager" + CHR(10).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

/* Flagger batchen klar for oppdatering. */
IF iBatchNr <> 0 THEN
    run batchstatus.p (iBatchNr, 2).

IF ocReturn = "" THEN obOk = TRUE.

/* --------------- Internprocedurer -----------------------*/
PROCEDURE OpprettBatch:
  /* Batch for TransLogg */
  run batchlogg.p (program-name(1),
                   "Reklamasjon fra reklam.logg " +
                   string(today) +
                   " " +
                   string(time,"HH:MM") +
                   " " +
                   userid("dictdb"),
                   output iBatchNr).

END PROCEDURE.


