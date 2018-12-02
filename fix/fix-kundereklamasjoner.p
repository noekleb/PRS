/*
TN 11/11-02
Rutinen plukker frem alle reklamasjoner siden angitt dato og oppretter
en batch med motposteringer.

*/

DEF var wBatchNr      as INT           NO-UNDO.
DEF VAR wSisteBatchNr as INT NO-UNDO.
def var wTransNr      like TransLogg.TransNr no-undo.
def var wOldButik     as int no-undo.

def buffer bufTransLogg for TransLogg.

CURRENT-WINDOW:WIDTH = 270.

/* Setter batchNr */
run batchlogg.p (program-name(1),
                 "Fiks av lagerreklamasjoner - Motpostering",
                 output wBatchNr).

FIND LAST BatchLogg NO-LOCK NO-ERROR.
IF AVAILABLE BatchLogg then
  wSisteBatchNr = BatchLogg.BatchNr.
ELSE DO:
  MESSAGE "Det finnes ingen batcher registrert."
    VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
  RETURN.
END.

/* Flagger batchen under oppdatering. */
run batchstatus.p (wBatchNr, 1).

DO WITH FRAME Gurre
    WITH WIDTH 269 DOWN.
  FOR EACH BongLinje NO-LOCK WHERE
    /*BongLinje.Butik      = 3 AND */
    BongLinje.ArtikkelNr = "16669" AND
    BongLinje.Dato >= 01/02/2002 AND
    SUBSTRING(BongLinje.Originaldata,21,2) = "02" AND
    NUM-ENTRIES(BongLinje.Originaldata) = 2:

    FIND TransLogg NO-LOCK WHERE
         TransLogg.Butik   = BongLinje.Butik AND
         TransLogg.TransNr = BongLinje.TransNr AND
         TransLogg.SeqNr   = BongLinje.SeqNr.

    /* Setter transaksjonsnummer  */
    if wOldButik <> TransLogg.butik then
      DO:
        find last bufTransLogg where
          bufTransLogg.Butik = TransLogg.butik
          use-index TransLogg no-error.
        if available bufTransLogg then
          wTransNr = bufTransLogg.TransNr + 1.
        else
          wTransNr = 1.
      END.
    else
      wTransNr = wTransNr + 1.

    DISPLAY
        BongLinje.Butik
        BongLinje.Kasse
        BongLinje.Dato
        BongLinje.BongNr
        BongLinje.ArtikkelNr
        TransLogg.Butik
        TransLogg.TransNr
        TransLogg.SeqNr
        BongLinje.TTId
        BongLinje.OriginalData FORMAT "x(125)"
        WITH FRAME gurre.
    DOWN 1 WITH FRAME gurre.

    CREATE bufTransLogg.
    BUFFER-COPY TransLogg EXCEPT TransNr BatchNr TO  bufTransLogg
      assign
        bufTransLogg.TransNr     = wTransNr
        bufTransLogg.BatchNr     = wBatchNr
        bufTransLogg.Antall      = TransLogg.Antall * -1
        bufTransLogg.Postert     = false
        bufTransLogg.PostertDato = ?
        bufTransLogg.PostertTid  = 0
        bufTransLogg.FeilKode    = 0
        bufTransLogg.Plukket     = true.
    RELEASE bufTransLogg.

  END.
END.

/* Flagger batchen klar for oppdatering - 2. */
run batchstatus.p (wBatchNr, 1).

