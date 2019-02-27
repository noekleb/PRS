/************************************************************
    Program:  x-motposterbatch.p
    Created:  TN   13 Jul 99
Description:

Last change:  TN   17 Jul 99   10:36 am
************************************************************/

DEF INPUT-OUTPUT PARAMETER wBatchNr LIKE BatchLogg.BatchNr.

DEF VAR wOldBatchNr as INT NO-UNDO.
DEF VAR wTransNr    as INT NO-UNDO.

DEF BUFFER bBatchLogg FOR BatchLogg.
DEF BUFFER bTransLogg FOR TransLogg.

assign
  wOldBatchNr = wBatchNr.

FIND BatchLogg NO-LOCK where
  BatchLogg.BatchNr = wOldBatchNr NO-ERROR.
IF NOT AVAILABLE BatchLogg then
  RETURN "AVBRYT".

run batchlogg.w (program-name(1),
                 "MOTPOSTERT: " + BatchLogg.Beskrivelse,
                  output wBatchNr).

/* Oppretter motpostert batch. */
DO FOR bTransLogg:

  FOR EACH TransLogg OF BatchLogg NO-LOCK:

    /* Transaksjonsnummer for butikken. */
    find last bTransLogg no-lock where
      bTransLogg.Butik = TransLogg.Butik use-index TransLogg no-error.
    if available bTransLogg then
      wTransNr = bTransLogg.TransNr + 1.
    else 
      wTransNr = 1.

    /* Sjekker at transnr er ledig */
    if can-find(bTransLogg where
                bTransLogg.Butik   = TransLogg.Butik and
                bTransLogg.TransNr = wTransNr) then
    NESTE_NR:
    do while true:
      wTransNr = wTransNr + 1.
      if can-find(bTransLogg where
                  bTransLogg.Butik   = TransLogg.Butik and
                  bTransLogg.TransNr = wTransNr) then
        next NESTE_NR.
     else
        leave NESTE_NR.
    end. /* NESTE_NR */

    CREATE bTransLogg.
    BUFFER-COPY TransLogg
                  EXCEPT TransLogg.BatchNr
                         TransLogg.TransNr
                         TransLogg.Antall
                TO bTransLogg
      assign
        bTransLogg.BatchNr     = wBatchNr
        bTransLogg.TransNr     = wTransNr
        bTransLogg.Antall      = TransLogg.Antall * -1
        bTransLogg.Postert     = false
        bTransLogg.PostertDato = ?
        bTransLogg.PostertTid  = 0
        bTransLogg.FeilKode    = 0.
  END.

  run batchstatus.p (wBatchNr, 2).

END.

