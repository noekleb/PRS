/*
Alle salgs og reklamasjonstransaksjoner for artikkelen motposteres.
Kopier av transaksjonene opprettes, og varekost settes = 0.
Transaksjonene flagges for at varekost ikke er satt.
*/

CURRENT-WINDOW:WIDTH = 200.

DEF VAR pAnt AS INT NO-UNDO.
DEF VAR iBatchNr      AS INT  NO-UNDO.
DEF VAR piTransNr     AS INT  NO-UNDO.
DEF VAR pcArtListe    AS CHAR NO-UNDO.
DEF VAR piLoop        AS INT  NO-UNDO.

ASSIGN
    pcArtListe = "13513"
    .
IF pcArtListe = "" THEN
    RETURN.

DEF BUFFER bTransLogg FOR TransLogg.

/* Batch for TransLogg */
run batchlogg.p (program-name(1),
                 "Korr av vvarekost2 " +
                 string(today) +
                 " " +
                 string(time,"HH:MM") +
                 " " +
                 userid("dictdb"),
                 output iBatchNr).

ARTIKKEL:
DO piLoop = 1 TO NUM-ENTRIES(pcArtListe):
  FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
      Translogg.ArtikkelNr = dec(ENTRY(piLoop,pcArtListe)) AND
      CAN-DO("1,3,10",string(Translogg.TTID)):

      OPPRETT-KOPI:
      DO:
          /* Setter transaksjonsnummer  */
          if piTransNr = 0 then
          DO:
            find last bTransLogg where
              TransLogg.Butik = Translogg.Butik
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
            assign bTransLogg.Butik        = Translogg.Butik
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
            TransLogg.VVArekost     = 0
            Translogg.SattVVarekost = FALSE
            TransLogg.Postert       = FALSE
            TransLogg.PostertDato   = ?
            TransLogg.PostertTid    = 0
            .
        /* Sikrer at posten blir oppdatert. */
        run batchstatus.p (TransLogg.BatchNr, 3).

      END. /* OPPRETT-KOPI */

      DISPLAY
          Translogg.Butik
          Translogg.Antall
          Translogg.Bongtekst
          TransLogg.VVAreKost WHEN AVAILABLE TransLogg
          TransLogg.TransNr WHEN AVAILABLE TransLogg
          TransLogg.SeqNr WHEN AVAILABLE TransLogg
          WITH WIDTH 198.

  END.
       
END. /* ARTIKKEL */

/* Flagger batchen klar for oppdatering. */
run batchstatus.p (iBatchNr, 2).

