/************************************************************
    Program:  opprjobb.p
    Created:  TN   25 May 98
Description:  Oppretter jobbrecord.

Last change:  TN   23 Oct 99    5:08 pm
************************************************************/

DEF INPUT-OUTPUT PARAMETER wDivData      as CHAR   NO-UNDO.
DEF       OUTPUT PARAMETER wStatus       as CHAR   NO-UNDO.

DEF VAR wJobbNr as INT NO-UNDO.

/* Default feilmelding */
assign
  wStatus = "FEIL,0":U.

/* Tilodrner og sjekker at det er et gyldig jobbnummer. */
assign
  wJobbNr = INT(ENTRY(1,wDivData,";")).
IF wJobbNr = 0 then
  DO:
    wStatus = "FEIL,1":U.
    RETURN.
  END.

/* Sjekker om det finnes en slik jobb allerede. */
FIND Jobb NO-LOCK where
  Jobb.JobbNr = wJobbNr NO-ERROR.
IF available Jobb then
  DO:
    wStatus = "FEIL,2":U.
    RETURN.
  END.

/* Oppretter jobbrecorden og setter inn tilgjengelige informasjoner. */
DO TRANSACTION:
  CREATE Jobb.
  assign
    Jobb.JobbNr = wJobbNr.

  assign
    Jobb.BestillingsDato = today
    Jobb.BestillingsTid  = time
    Jobb.StartProgram    = ENTRY(2,wDivData,";")
    Jobb.EksekProgram    = ENTRY(3,wDivData,";")
    Jobb.BestiltAv       = ENTRY(6,wDivData,";")
    Jobb.StartesAv       = ENTRY(7,wDivData,";")
    Jobb.Merknad         = ENTRY(8,wDivData,";")
    Jobb.Kriterier       = ENTRY(9,wDivData,";")
    Jobb.JobbStatus      = "BESTILT".

  IF Jobb.StartesAv = "F" then /* Forgrunn */
    assign
      Jobb.StartDato       = Jobb.BestillingsDato
      Jobb.StartTid        = Jobb.BestillingsTid.
  ELSE                         /* Bakgrunn */
    assign
      Jobb.StartDato       = DATE(ENTRY(4,wDivData,";"))
      Jobb.StartTid        = int(ENTRY(5,wDivData,";")).

/*
/* DEBUG */
message "Jobbrecord:" Jobb.JobbNr skip
    "Bestilt av" Jobb.BestiltAv    skip
    "Bestillingsdato:" Jobb.BestillingsDato skip
    "Bestillingstid:" STRING(Jobb.BestillingsTid,"HH:MM:SS") skip
    "Startprogram:" Jobb.StartProgram skip
    "Esekprogram:" Jobb.EksekProgram skip
    "Startes av:" Jobb.StartesAv    skip
    "Merknad:" Jobb.Merknad      skip
    "Kriterier:" Jobb.Kriterier    skip
    "StartDato:" Jobb.StartDato    skip
    "StartTid:" STRING(Jobb.StartTid,"HH:MM:SS")
    "JobbStatus:" jobb.JobbStatus.
*/

END. /* TRANSACTION */

/* Returnerer  */
assign
  wStatus = "OK":U.






