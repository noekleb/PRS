CURRENT-WINDOW:WIDTH = 200.

DEF VAR pAnt AS INT NO-UNDO.
DEF VAR pcButikkListe AS CHAR NO-UNDO.
DEF VAR pdDato1       AS DATE NO-UNDO.
DEF VAR pdDato2       AS DATE NO-UNDO.
DEF VAR iBatchNr      AS INT  NO-UNDO.
DEF VAR piTransNr     AS INT  NO-UNDO.

DEF BUFFER bTransLogg FOR TransLogg.

/* Batch for TransLogg */
run batchlogg.p (program-name(1),
                 "Korr av vvarekost " +
                 string(today) +
                 " " +
                 string(time,"HH:MM") +
                 " " +
                 userid("dictdb"),
                 output iBatchNr).

ASSIGN
    pcButikkListe = ""
    pdDato1       = 01/01/2005
    pdDato2       = 12/31/2005
    .

FOR EACH Butiker NO-LOCK WHERE
    (IF pcButikkListe <> "" THEN
        CAN-DO(pcButikkListe,STRING(Butiker.Butik))
        ELSE TRUE):

  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.ButikkNr = Butiker.Butik AND
      BongLinje.GruppeNr = 1 AND
      BongLinje.KasseNr  > 0 AND
      BongLinje.Dato     >= pdDato1 AND
      BongLinje.Dato     <= pdDato2 AND
      CAN-DO("1,3,10",string(BongLinje.TTID)) AND
      abs(BongLinje.Antall) > 1:

      /* Henter transloggposten */
      FIND TransLogg EXCLUSIVE-LOCK WHERE
          TransLogg.Butik   = BongLinje.ButikkNr AND
          Translogg.TransNr = BongLinje.TransNr AND
          TransLogg.SeqNr   = BongLinje.SeqNr NO-ERROR.

      IF AVAILABLE TransLogg /*AND 
          TransLogg.VVarekost = BongLinje.VVarekost*/ THEN
      OPPRETT-KOPI:
      DO:
          /* Setter transaksjonsnummer  */
          if piTransNr = 0 then
          DO:
            find last bTransLogg where
              TransLogg.Butik = BongLinje.ButikkNr
              use-index TransLogg no-error.
            if available bTransLogg then
              piTransNr = bTransLogg.TransNr + 1.
            else
              piTransNr = 1.
          END.
        else
          piTransNr = piTransNr + 1.

        CREATE bTransLogg.
        BUFFER-COPY TransLogg 
            EXCEPT TransNr SeqNr
            TO bTransLogg
            assign bTransLogg.Butik        = BongLinje.ButikkNr
                   bTransLogg.TransNr      = piTransNr
                   bTransLogg.SeqNr        = 1
                   bTransLogg.BatchNr      = iBatchNr
            .
        ASSIGN
            bTransLogg.Postert     = FALSE
            bTransLogg.PostertDato = ?
            bTransLogg.PostertTid  = 0
            bTranslogg.Antall      = bTransLogg.Antall * -1
            .

        /* Korriger originaltrans */
        ASSIGN
            TransLogg.VVArekost   = BongLinje.VVarekost / ABS(BongLinje.Antall)
            TransLogg.Postert     = FALSE
            TransLogg.PostertDato = ?
            TransLogg.PostertTid  = 0
            .
        /* Sikrer at posten blir oppdatert. */
        run batchstatus.p (TransLogg.BatchNr, 3).

      END. /* OPPRETT-KOPI */

      DISPLAY
          Butiker.Butik
          BongLinje.Antall
          BongLinje.BongTekst
          BongLinje.VVareKost
          TransLogg.VVAreKost WHEN AVAILABLE TransLogg
          TransLogg.TransNr WHEN AVAILABLE TransLogg
          TransLogg.SeqNr WHEN AVAILABLE TransLogg
          WITH WIDTH 198.

  END.
       
END.

/* Flagger batchen klar for oppdatering. */
run batchstatus.p (iBatchNr, 2).

