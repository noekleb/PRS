/************************************************************
    Program:  buffer-copy-amundsen.p
    Created:  TN   14 Jun 99
Description:  Oppretter batch logg og fyller den opp med
              salgstranser med mer enn 1 i antall.
              DB er postert feil i statistikkene

Last change:  TN   14 Jun 99    9:31 pm
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
  wSkipListe = "1876,1877".

/* Setter batchNr */
run batchlogg.p (program-name(1),
                 "Fiks DB på åpne PLU'er - Ny batch",
                 output wBatchNr).

/*
FIND LAST BatchLogg NO-LOCK NO-ERROR.
IF AVAILABLE BatchLogg then
  wSisteBatchNr = BatchLogg.BatchNr.
ELSE DO:
  MESSAGE "Det finnes ingen batcher registrert."
    VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
  RETURN.
END.
*/

/* Flagger batchen under oppdatering. */
run batchstatus.p (wBatchNr, 1).

TRANSLOOP:
for each translogg no-lock where
  translogg.ttid = 1 and /* VAresalg */
  TransLogg.BatchNr <= wBatchNr /*wBatchNr */:

    IF TransLogg.Antall = 1 OR
        TransLogg.Antall = -1 THEN
        NEXT TRANSLOOP.
  
  FIND ArtBas NO-LOCK WHERE
      dec(ArtBas.ArtikkelNr) = dec(TransLogg.ArtikkelNr) NO-ERROR.
  IF NOT AVAILABLE ArtBAs THEN
      NEXT TRANSLOOP.
  IF ArtBas.OPris = FALSE THEN
      NEXT TRANSLOOP.

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
  pause 0.
  display x with frame g.
  
  create bufTransLogg.
  
  BUFFER-COPY TransLogg EXCEPT TransNr BatchNr TO  bufTransLogg
    assign
      bufTransLogg.TransNr     = wTransNr
      bufTransLogg.BatchNr     = wBatchNr
      bufTransLogg.Antall      = TransLogg.Antall 
      bufTransLogg.Postert     = false
      bufTransLogg.PostertDato = ?
      bufTransLogg.PostertTid  = 0
      bufTransLogg.FeilKode    = 0
      bufTransLogg.Plukket     = true
      bufTransLogg.VVAreKost   = 0
      bufTransLogg.SAttvVarekost = FALSE
      .
end. /* TRANSLOOP */

/* Flagger batchen klar for oppdatering. */
run batchstatus.p (wBatchNr, 1).
