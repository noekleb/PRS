/************************************************************
    Program:  buffer-copy-kort-translogg.p
    Created:  TN   20/3-01
Description:  Oppretter batch logg og fyller den opp med
              korttransaksjoner for motpostering.

************************************************************/

DEF var wBatchNr      as INT           NO-UNDO.
DEF VAR wSisteBatchNr as INT NO-UNDO.
def var wTransNr      like TransLogg.TransNr no-undo.
def var x             as int no-undo.
def var wOldButik     as int no-undo.
DEF VAR wSkipListe    as CHAR NO-UNDO.

def buffer bufTransLogg for TransLogg.

/* Sett inn en kommaseparert liste med de batchnummer som ikke skal kopieres. */
assign
  wSkipListe = "".

/* Setter batchNr */
run batchlogg.p (program-name(1),
                 "Fiks av medlemssalg - Motpostering",
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

MESSAGE wSisteBatchNr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

BATCH:
FOR EACH BatchLogg NO-LOCK WHERE
    BatchLogg.BatchNr >= 21477 AND
    BatchLogg.BatchNr <= 22012:
    
    pause 0.
    display BatchLogg.BatchNr x with frame g.

TRANSLOOP:
for each translogg no-lock where
  Translogg.BatchNr = BatchLogg.BAtchNr AND
  translogg.KortNr <> ""
  :

  /* Skipper transaksjonen hvis den ligger i en batch som ikke skal kopieres. */
  IF CAN-DO(wSkipListe,STRING(TransLogg.BatchNr)) then
    NEXT TRANSLOOP.

  /* Setter transaksjonsnummer  */
  if wOldButik <> TransLogg.butik then
    DO:
      find last bufTransLogg where
        bufTransLogg.Butik = TransLogg.butik
        use-index TransLogg no-error.
      if available bufTransLogg then
        wTransNR = bufTransLogg.TransNr + 1.
      else
        wTransNr = 1.
    END.
  else
    wTransNr = wTransNr + 1.

  x = x + 1.
  
  create bufTransLogg.
  
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
end. /* TRANSLOOP */
  PAUSE 0.
  DISPLAY "Ferdig batch " + STRING(BatchLogg.BAtchNr) FORMAT "x(40)" with frame g.
  IF BatchLogg.BatchNr > 22012 THEN LEAVE BATCH.
END. /* BATCH */

MESSAGE "TEST-1"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* Flagger batchen klar for oppdatering. */
run batchstatus.p (wBatchNr, 1).

MESSAGE "TEST-2"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
