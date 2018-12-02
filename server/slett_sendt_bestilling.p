/* Slett sendt bestilling  (og kun sendt).
   Skal bare benyttes for korrigering av en feilsituasjon 
   Parametere: Input: bestillingsnr
   
   Opprettet: 30.11.06 av BHa               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iBestNr AS INT NO-UNDO.

iBestNr = INT(icParam) NO-ERROR.

FIND BestHode WHERE BestHode.BestNr = iBestNr EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL BestHode AND BestHode.BestStat = 4 AND BestHode.BekreftetDato = ? THEN DO:
  FOR EACH VarebehBestHode EXCLUSIVE-LOCK
      WHERE VarebehBestHode.BestNr  = BestHode.BestNr:
    FOR EACH VarebehBestLinje OF VareBehBestHode EXCLUSIVE-LOCK:
      DELETE VarebehBestLinje.
    END.
    DELETE VareBehBestHode.
  END.

  FOR EACH BestLinje OF BestHode
      EXCLUSIVE-LOCK:
    DELETE BestLinje.
  END.
  FOR EACH BestPris OF BestHode
      EXCLUSIVE-LOCK:
    DELETE BestPris.
  END.
  FOR EACH BestSort OF BestHode
      EXCLUSIVE-LOCK:
    DELETE BestSort.
  END.
  FOR EACH BestStr OF BestHode
      EXCLUSIVE-LOCK:
    DELETE BestStr.
  END.
  FOR EACH BestKasse OF BestHode
      EXCLUSIVE-LOCK:
    DELETE BestKasse.
  END.

  DELETE BestHode.     
END.
ELSE IF AVAIL BestHode AND BestHode.BestStat NE 4 THEN
  ocReturn = "Programmet kan bare benyttes til å slette sendte ordre".
ELSE IF AVAIL BestHode AND BestHode.BekreftetDato NE ? THEN
  ocReturn = "Bestilling er bekreftet og kan ikke slettes".
ELSE IF NOT AVAIL BestHode THEN
  ocReturn = "Bestilling er ikke tilgjengelig for sletting" + CHR(10) + PROGRAM-NAME(1).

obOk = ocReturn = "".
