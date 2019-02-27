/* Sletting av bestilling fra suppleringsbok (varebeh).
   Kaller sletterutine for varebehbesthode 
   Dersom bestillingen ikke hører til vareh. kan ikke rutinen benyttes
   Opprettet: 21.05.06 av BHa 
-------------------------------------------------------------------------*/
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.


FIND BestHode WHERE ROWID(BestHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL BestHode THEN DO: 
  FIND FIRST VarebehBestHode NO-LOCK
       WHERE VarebehBestHode.VarebehNr = BestHode.VarebehNr
         AND VarebehBestHode.BestNr    = BestHode.BestNr
       NO-ERROR.
  IF AVAIL VarebehBestHode THEN
    RUN delval_varebehbesthode.p ("VarebehBestHode",
                                  STRING(ROWID(VarebehBestHode)),
                                  icSessionId,
                                  OUTPUT ocReturn).
  ELSE ocReturn = "Feil i sletting av bestilling:" + CHR(10) +
                  "Finner ikke tilsvarende bestilling i suppleringsbok" + CHR(10) +
                  PROGRAM-NAME(1).
END.
ELSE ocReturn = "Feil i sletting av bestilling:" + CHR(10) +
                "Bestilling ikke funnet" + CHR(10) +
                PROGRAM-NAME(1).

