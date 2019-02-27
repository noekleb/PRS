/************************************************************
    Program:  runjobb.p
    Created:  TN   25 May 98
Description:  Utfører en jobb fra jobblisten.

Last change:  TN   23 Oct 99    5:10 pm
************************************************************/


DEF INPUT        PARAMETER wJobbNr       as INT    NO-UNDO.
DEF INPUT-OUTPUT PARAMETER wDivData      as CHAR   NO-UNDO.
DEF       OUTPUT PARAMETER wStatus       as CHAR   NO-UNDO.
DEF       INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wJobbRecid  as RECID NO-UNDO.
DEF VAR wProgStatus as CHAR  NO-UNDO.

/* Setter feil som default i utgangsstatus. */
assign
  wStatus    = "FEIL,0":U
  wJobbRecid = ?.

/* Henter jobbrecord or starter eksekveringsprogram. */
JOBB:
DO:
  FIND Jobb NO-LOCK where
    Jobb.JobbNr = wJobbNr NO-ERROR.
  IF not AVAILABLE Jobb then
    DO:
      wStatus = "FEIL,1":U.
      LEAVE JOBB.
    END.
  else
    ASSIGN wJobbRecid = RECID(Jobb).

  /* Sjekker om programmet finnes. Finnes det startes det opp. */
  IF SEARCH(Jobb.EksekProgram + ".r") <> ? then
    RUN VALUE(Jobb.EksekProgram + ".r") (input STRING(Jobb.JobbNr), output wProgStatus, input wParentHandle).
  ELSE IF SEARCH(Jobb.EksekProgram + ".p") <> ? then
    RUN VALUE(Jobb.EksekProgram + ".p") (input STRING(Jobb.JobbNr), output wProgStatus, input wParentHandle).
  ELSE DO:
    wStatus = "FEIL,3".
    LEAVE JOBB.
  END.
  /* Sjekker returnkode. */
  IF wProgStatus <> "OK" then
    DO:
      wStatus = "FEIL,4".
      LEAVE JOBB.
    END.

END. /* JOBB */

/* Setter OK i utgangsstatus hvis ingen feil har oppstått. */
IF ENTRY(2,wStatus) = "0" then
  DO:
    assign
      wStatus = "OK":U.
    RUN OppdaterJobb (INPUT "OK - Generering av rapport utført").
  END.
else
  DO:
    RUN OppdaterJobb (INPUT "FEIL (" +
                            ENTRY(2,wStatus) +
                            ") - ved generering av rapport").
  END.

/* -------------- Procedyre definisjoner --------------------- */
PROCEDURE OppdaterJobb:
  DEF INPUT PARAMETER wpStatus as CHAR NO-UNDO.

  DO TRANSACTION:
    FIND Jobb exclusive-lock where
      RECID(Jobb) = wJobbRecid.
    IF AVAILABLE Jobb then
      DO:
        assign
          Jobb.JobbStatus = wpStatus
          Jobb.AntStart   = Jobb.AntStart + 1
          Jobb.FerdigDato = today
          Jobb.FerdigTid  = time.
      END.
  END. /* TRANSACTION */
END PROCEDURE.
