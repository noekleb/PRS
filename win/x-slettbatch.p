/************************************************************
    Program:  x-slettbatch.p
    Created:  TN   13 Jul 99
Description:

Last change:  TN   13 Jul 99    1:31 pm
************************************************************/

DEF INPUT-OUTPUT PARAMETER wBatchNr LIKE BatchLogg.BatchNr.


DEF BUFFER bBatchLogg FOR BatchLogg.
DEF BUFFER bTransLogg FOR TransLogg.

FIND BatchLogg NO-LOCK where
  BatchLogg.BatchNr = wBatchNr NO-ERROR.
IF NOT AVAILABLE BatchLogg then
  RETURN "AVBRYT".

/* Oppretter motpostert batch. */
DO TRANSACTION:
  FIND BatchLogg exclusive-LOCK where
    BatchLogg.BatchNr = wBatchNr.
  BatchLogg.Beskrivelse = "SLETTET: " + BatchLogg.Beskrivelse.

  FOR EACH TransLogg OF BatchLogg exclusive-lock:
    DELETE TransLogg.
  END.

  run batchstatus.p (wBatchNr, 4).

END.

