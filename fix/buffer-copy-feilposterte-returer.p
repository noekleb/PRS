 /************************************************************
    Program:  buffer-copy-reklamasjoner.p
    Created:  TN 21/1-00
Description:  Oppretter batch logg og fyller den opp med
              reklamasjonstransaksjoner klare for omkjoring.

Last change:  TN   21 Jan 100    6:58 pm
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
                 "Fiks av feilposterte returer - Motpostering",
                 output wBatchNr).

/* Flagger batchen under oppdatering. */
run batchstatus.p (wBatchNr, 1).

BONGHODE:
FOR EACH BongHode NO-LOCK:
    BONGLINJE:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.TTId = 4:

        /* Er det en feilpostert retur? */
        IF SUBSTRING(BongLinje.Originaldata,21,2) = "02" THEN
        FIXTRANSAKSJON:
        DO:
            TRANSLOOP:
            for each translogg no-lock where
              TransLogg.Butik   = BongLinje.ButikkNr AND
              TransLogg.TransNr = BongLinje.TransNr AND
              TransLogg.SeqNr   = BongLinje.SeqNr:

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
                  bufTransLogg.Antall      = TransLogg.Antall * -1
                  bufTransLogg.Postert     = false
                  bufTransLogg.PostertDato = ?
                  bufTransLogg.PostertTid  = 0
                  bufTransLogg.FeilKode    = 0
                  bufTransLogg.Plukket     = true.
            end. /* TRANSLOOP */
        END. /* FIXTRANSAKSJON */
    END. /* BONGLINJE */
END. /* BONGHODE */

/* Flagger batchen "nesten" klar for oppdatering. */
/* Den MÅ godkjennes manuelt.                     */
run batchstatus.p (wBatchNr, 0).
