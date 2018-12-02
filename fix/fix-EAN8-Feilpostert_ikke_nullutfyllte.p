CURRENT-WINDOW:WIDTH = 300.

DEF VAR cKode AS CHAR FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.
DEF VAR lVareKost AS DEC NO-UNDO.

DEFINE BUFFER bufStrekkode FOR Strekkode.
DEFINE BUFFER slettStrekkode FOR Strekkode.
DEFINE BUFFER bTransLogg FOR TransLogg.
DEFINE BUFFER bufArtBas FOR ArtBas.

FORM 
  WITH FRAME X DOWN.

   /* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
   IF iBatchNr = 0 THEN 
      RUN batchlogg.p (PROGRAM-NAME(1),
               "Varekost korr2 tobakk " +
               string(TODAY) +
               " " +
               string(TIME,"HH:MM") +
               " " +
               USERID("dictdb"),
               OUTPUT iBatchNr).


FOR EACH ArtBas NO-LOCK WHERE
  ArtBas.Hg = 15:

  FOR EACH Strekkode OF ArtBas NO-LOCK WHERE
    LENGTH(Strekkode.Kode) = 8:

    cKode = FILL('0',13 - LENGTH(Strekkode.Kode)) + Strekkode.Kode.
    FIND bufStrekkode NO-LOCK WHERE
      bufStrekkode.Kode = cKode NO-ERROR.

    IF AVAILABLE bufStrekkode AND (Strekkode.ArtikkelNr <> bufStrekkode.ArtikkelNr)THEN
    KOPI_FINNES:
    DO:
      FIND bufArtBas NO-LOCK WHERE
        bufArtBas.ArtikkelNr = bufStrekkode.ArtikkelNr NO-ERROR.
      IF AVAILABLE bufArtBas THEN 
        FIND FIRST ArtPris OF bufArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE ArtPris THEN 
        ASSIGN
        lVareKost = ArtPris.VareKost[1].
          
      /* -Flytt transaksjoner til ny artikkel 
          - Motposter på artikkel det flyttes fra.
          - Poster på artikkel det er flyttet til 
          - Sjekk varekost på transaksjonen på ny artikkel
      */
      TRANSLOGG_LOOP:
      FOR EACH TransLogg NO-LOCK WHERE
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND
        TransLogg.Kode       = Strekkode.Kode AND
        TransLogg.Dato      >= 01/01/2011:

        DISPLAY
          Strekkode.Kode
          /*cKode*/
          Strekkode.ArtikkelNr
          ArtBAs.Beskr
          bufStrekkode.Kode WHEN AVAILABLE bufStrekkode
          bufStrekkode.ArtikkelNr WHEN AVAILABLE bufStrekkode
          TransLogg.Butik
          TransLogg.Dato
          TransLogg.Kode
          Translogg.ArtikkelNr
          WITH FRAME X DOWN WIDTH 250.
        DOWN WITH FRAME X.

        FLYTTING:
        DO:
          /* Motposterer */
          LOOPEN1:
          DO iSeqNr = 2 TO 99:
              FIND FIRST bTransLogg NO-LOCK WHERE
                         bTransLogg.Butik   = TransLogg.Butik  AND
                         bTransLogg.TransNr = TransLogg.TransNr       AND
                         bTransLogg.SeqNr   = iSeqNr               
                         NO-ERROR.
              IF AVAILABLE bTransLogg THEN 
                NEXT LOOPEN1.
              ELSE DO TRANSACTION:
                  CREATE bTransLogg.
                  BUFFER-COPY TransLogg
                      EXCEPT SeqNr BatchNr
                      TO bTranslogg
                      ASSIGN
                          bTransLogg.BatchNr     = iBatchNr
                          bTransLogg.Antall      = bTransLogg.Antall * -1
                          bTransLogg.Postert     = FALSE
                          bTransLogg.PostertDato =?
                          bTransLogg.PostertTid  = 0
                          .
                  RELEASE bTransLogg.
                  LEAVE LOOPEN1.
              END.        
          END. /* LOOPEN1 */
          /* Posterer korrigert transaksjon */
          LOOPEN2:
          DO iSeqNr = 2 TO 99:
              FIND FIRST bTransLogg NO-LOCK WHERE
                         bTransLogg.Butik   = TransLogg.Butik  AND
                         bTransLogg.TransNr = TransLogg.TransNr       AND
                         bTransLogg.SeqNr   = iSeqNr               
                         NO-ERROR.
              IF AVAILABLE bTransLogg THEN 
                NEXT LOOPEN2.
              ELSE DO TRANSACTION:
                  CREATE bTransLogg.
                  BUFFER-COPY TransLogg
                      EXCEPT SeqNr BatchNr
                      TO bTranslogg
                      ASSIGN
                          bTransLogg.BatchNr     = iBatchNr
                          bTransLogg.Postert     = FALSE
                          bTransLogg.PostertDato = ? 
                          bTransLogg.PostertTid  = 0
                          bTransLogg.VVareKost   = lVareKost
                          bTransLogg.ArtikkelNr  = bufArtBas.ARtikkelNr
                          bTransLogg.Vg          = bufArtBas.Vg
                          bTransLogg.LopNr       = bufArtBas.LopNr
                          bTransLogg.LevNr       = bufArtBas.LevNr
                          .
                  RELEASE bTransLogg.
                  LEAVE LOOPEN2.
              END.        
          END. /* LOOPEN2 */

          /* -Bytt artikkelnr og annen artikkelinfo på bonglinjen. */
          KORR_BONGLINJE:
          DO TRANSACTION:
              FIND BongLinje EXCLUSIVE-LOCK WHERE
                  BongLinje.ButikkNr = TransLogg.Butik AND 
                  BongLinje.GruppeNr = 1 AND 
                  BongLinje.KasseNr  = TransLogg.KassaNr AND 
                  BongLinje.Dato     = TransLogg.Dato AND 
                  BongLinje.BongNr   = TransLogg.BongId AND
                  BongLinje.LinjeNr  = TransLogg.BongLinjeNr NO-ERROR.
              IF AVAILABLE BongLinje THEN DO: 
                ASSIGN
                  BongLinje.ArtikkelNr  = STRING(bufArtBas.ArtikkelNr)
                  BongLinje.VareGr      = bufArtBas.Vg
                  BongLinje.LopeNr      = bufArtBas.LopNr
                  BongLinje.LevNr       = bufArtBas.LevNr
                  BongLinje.VVareKost   = lVareKost.
                  RELEASE BongLinje.
              END.
          END. /* KORR_BONGLINJE */

        END. /* FLYTTING */

      END. /* TRANSLOGG_LOOP */

      /* -Slett strekkoden fra PLU artikkelen. */
      DO TRANSACTION:
          FIND slettStrekkode EXCLUSIVE-LOCK WHERE
              RECID(slettStrekkode) = RECID(STrekkode) NO-ERROR.
          IF AVAILABLE slettStrekkode THEN 
              DELETE slettSTrekkode.
      END. /* TRANSACTION */
    END. /* KOPI_FINNES */
  END. /* STREKKODE */

END. /* ARTBAS */
