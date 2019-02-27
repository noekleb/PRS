CURRENT-WINDOW:WIDTH = 132.

DEF VAR lArtikkelNr AS DEC NO-UNDO. 
DEF VAR cArtikkelnr AS CHAR NO-UNDO.
DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.


DEF BUFFER bTransLogg FOR TransLogg.

/* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
IF iBatchNr = 0 THEN 
   RUN batchlogg.p (PROGRAM-NAME(1),
            "Drivmedel_1234-9 " +
            string(TODAY) +
            " " +
            string(TIME,"HH:MM") +
            " " +
            USERID("dictdb"),
            OUTPUT iBatchNr).


FOR EACH BatchLogg NO-LOCK WHERE
    BatchLogg.BatchNr > 1295066:   /* Testkört på 1295066 därför > */

    FOR EACH TransLogg OF BatchLogg NO-LOCK TRANSACTION:

        FIND BongLinje EXCLUSIVE-LOCK WHERE
             BongLinje.ButikkNr = TransLogg.Butik AND 
             BongLinje.GruppeNr = 1 AND 
             BongLinje.KasseNr  = TransLogg.KassaNr AND 
             BongLinje.Dato     = TransLogg.Dato AND 
             BongLinje.BongNr   = TransLogg.BongId AND
             BongLinje.LinjeNr  = TransLogg.BongLinjeNr NO-ERROR.
         IF AVAILABLE BongLinje THEN 
         BONG_LINJE:
         DO:
           /* Disse skal vi ikke gjøre noe med. */
           IF NOT CAN-DO( "1,2,70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,86,87,88,89",STRING(BongLinje.HovedGr)) THEN
               LEAVE BONG_LINJE.
           ELSE 
           KORRIGER_BONGLINJE: 
           DO:
               cArtikkelNr = "9000" + string(BongLinje.HovedGr,"99").
               IF bonglinje.artikkelnr = cArtikkelnr THEN
                 LEAVE KORRIGER_BONGLINJE.
               
               lArtikkelnr = DECI(cArtikkelnr).
               FIND ArtBas NO-LOCK WHERE 
                   ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
               IF AVAILABLE ArtBas THEN
                   ASSIGN
                  BongLinje.OriginalData = "KORR=" + BongLinje.ArtikkelNr + '|' + STRING(BongLinje.VareGr) + '|' + BongLinje.Strekkode
                   BongLinje.ArtikkelNr   = STRING(ArtBas.ArtikkelNr)
                   BongLinje.VareGr       = ArtBas.Vg
                   Bonglinje.lopenr        = ArtBas.lopnr
                   BongLinje.Strekkode    = STRING(ArtBas.ArtikkelNr)
               .
               ELSE
                   LEAVE KORRIGER_BONGLINJE.
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
                   ELSE DO:
                       CREATE bTransLogg.
                       BUFFER-COPY TransLogg
                           EXCEPT SeqNr BatchNr
                           TO bTranslogg
                           ASSIGN
                               bTransLogg.BatchNr     = iBatchNr
                               bTranslogg.SeqNr       = iSeqNr
                               bTransLogg.Antall      = TransLogg.Antall * -1
                               bTransLogg.Postert     = FALSE
                               bTransLogg.PostertDato = ?
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
                   ELSE DO:
                       CREATE bTransLogg.
                       BUFFER-COPY TransLogg
                           EXCEPT SeqNr BatchNr
                           TO bTranslogg
                           ASSIGN
                               bTransLogg.BatchNr     = iBatchNr
                               bTranslogg.SeqNr       = iSeqNr
                               bTransLogg.Postert     = FALSE
                               bTransLogg.PostertDato = ? 
                               bTransLogg.PostertTid  = 0
                               bTransLogg.ArtikkelNr  = ArtBas.ArtikkelNr
                               bTransLogg.Vg          = ArtBas.Vg
                               bTransLogg.LopNr       = ArtBas.LopNr
                               bTransLogg.LevNr       = ArtBas.LevNr
                               bTransLogg.Kode        = STRING(ArtBas.ArtikkelNr)
                               .
                       RELEASE bTransLogg.
                       LEAVE LOOPEN2.
                   END.        
               END. /* LOOPEN2 */

           END. /* KORRIGER_BONGLINJE*/
           RELEASE BongLinje.
         END. /* BONG_LINJE */
    END. /*  TRANSACTION */
END.
