/* Sjekk om bestillingshode er for butikk
   Opprettet: 08.05.06 av BHa
-------------------------------------------------------------------*/   
DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

DEF VAR bMatch AS LOG NO-UNDO.

IF icParam = "*" THEN RETURN.   /* Butikkliste */
ELSE icParam = REPLACE(icParam,"¤",",").

FIND VarebehBestHode WHERE ROWID(VarebehBestHode) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL VarebehBestHode THEN DO:
  FOR EACH BestLinje NO-LOCK
      WHERE BestLinje.BestNr = VarebehBestHode.BestNr
        AND CAN-DO(icParam,STRING(BestLinje.Butik)):
    bMatch = YES.
  END.
END.
IF NOT bMatch THEN ocReturn = "skiprow".
