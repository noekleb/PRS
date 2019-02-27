/************************************************************
    Program:  batchstatus.p
    Created:  TN   13 Mar 99
Description:  Oppdaterer BatchStatus.

              run batchstatus.p (wBatchNr, 1).

              Retur i return-value om det gikk bra eller ikke.


Last change:  TN   13 Oct 99   12:47 pm
************************************************************/

DEF INPUT  PARAMETER wBatchNr    LIKE BatchLogg.BatchNr    NO-UNDO.
DEF INPUT  PARAMETER wOppdStatus LIKE BatchLogg.OppdStatus NO-UNDO.

DEF VAR wOk as LOG NO-UNDO.

DEF BUFFER bufBatchLogg FOR BatchLogg.

assign
  wOk = FALSE.

do FOR bufBatchLogg TRANSACTION:
  FIND bufBatchLogg EXCLUSIVE-LOCK where
    bufBatchLogg.BatchNr = wBatchNr NO-ERROR.
  IF AVAILABLE bufBatchLogg then
    DO:
      assign
        bufBatchLogg.OppdStatus      = wOppdStatus
        bufBatchLogg.StatusOppdatert = TODAY
        bufBatchLogg.TidOppdatert    = TIME
        bufBatchLogg.OppdatertAv     = USERID("SkoTex")
        wOk                          = TRUE.
      RELEASE bufBatchLogg.
    END.
END.

IF wOk then
  RETURN "Ok".
else
  RETURN "AVBRYT".


