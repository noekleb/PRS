CURRENT-WINDOW:WIDTH = 300.
DEF VAR dDato AS DATE NO-UNDO.

DEFINE VARIABLE iBatchNr1 AS INTEGER NO-UNDO.
DEFINE VAR iBatchNr2 AS INT NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.

DEF BUFFER bTransLogg FOR TransLogg.

/* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
IF iBatchNr1 = 0 THEN 
   RUN batchlogg.p (PROGRAM-NAME(1),
            "RETUR - MOTPOSTER " +
            string(TODAY) +
            " " +
            string(TIME,"HH:MM") +
            " " +
            USERID("dictdb"),
            OUTPUT iBatchNr1).
/* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
IF iBatchNr2 = 0 THEN 
   RUN batchlogg.p (PROGRAM-NAME(1),
            "RETUR - POSTER " +
            string(TODAY) +
            " " +
            string(TIME,"HH:MM") +
            " " +
            USERID("dictdb"),
            OUTPUT iBatchNr2).

FOR EACH butiker NO-LOCK WHERE
    Butiker.Butik = 3:

  DATO_LOOP:
  DO dDato = 01/01/2012 TO TODAY:

 
  TRANSLOGG:
  FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.Dato  = dDato AND
      TransLogg.TTId  = 10 AND
      TransLogg.Butik = Butiker.Butik AND
      TransLogg.Antall < 0 AND
      TransLogg.Pris < 0:

    /* Motposterer */
    /*
    LOOPEN1:
    DO iSeqNr = 2 TO 99:
        FIND FIRST bTransLogg NO-LOCK WHERE
                   bTransLogg.Butik   = TransLogg.Butik   AND
                   bTransLogg.TransNr = TransLogg.TransNr AND
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
                    bTransLogg.BatchNr     = iBatchNr1
                    bTransLogg.SeqNr       = iSeqNr               
                    bTransLogg.Antall      = TransLogg.Antall * -1
                    /*
                    bTransLogg.Pris        = ABS(TransLogg.Pris)
                    bTransLogg.Mva         = ABS(TransLogg.Mva)
                    */
                    bTransLogg.Postert     = FALSE
                    bTransLogg.PostertDato = ?
                    bTransLogg.PostertTid  = 0
                    .
            RELEASE bTransLogg.
            LEAVE LOOPEN1.
        END.        
    END. /* LOOPEN1 */
    /* Posterer korrigert transaksjon */
    */
    LOOPEN2:
    DO iSeqNr = 2 TO 99:
        FIND FIRST bTransLogg NO-LOCK WHERE
                   bTransLogg.Butik   = TransLogg.Butik   AND
                   bTransLogg.TransNr = TransLogg.TransNr AND
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
                    bTransLogg.BatchNr     = iBatchNr2
                    bTransLogg.SeqNr       = iSeqNr               
                    bTransLogg.Postert     = FALSE
                    bTransLogg.PostertDato = ? 
                    bTransLogg.PostertTid  = 0
                    bTransLogg.Pris        = ABS(TransLogg.Pris)
                    bTransLogg.Mva         = ABS(TransLogg.Mva)
                    bTransLogg.Mva         = ROUND(((bTransLogg.Pris - bTransLogg.RabKr) * 20) / 100,2) 
                    .
            RELEASE bTransLogg.
            LEAVE LOOPEN2.
        END.        
    END. /* LOOPEN2 */
    

    /*
    DISPLAY
          TransLogg.BatchNr
          TransLogg.TTId
          TransLogg.Butik
          TransLogg.DAto
          TransLogg.Antall
          TransLogg.Pris
          TransLogg.VVareKost
    WITH WIDTH 300.
    */

  END. /* TRANSLOGG */
  END. /* Dato loop */
END. /* Butikk loop */

