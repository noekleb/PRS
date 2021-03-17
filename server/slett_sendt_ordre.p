/* Slett sendte ordre  (og kun sendte).
   Skal bare benyttes for korrigering av en feilsituasjon 
   Parametere: Input: ordrenr
   
   Opprettet: 30.11.06 av BHa               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iOrdreNr AS INT NO-UNDO.

iOrdreNr = INT(icParam) NO-ERROR.

FIND Ordre WHERE Ordre.OrdreNr = iOrdreNr EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Ordre AND Ordre.OrdreStatus = 2 THEN DO:
  FOR EACH VarebehBestHode EXCLUSIVE-LOCK
      WHERE VareBehBestHode.OrdreNr = Ordre.OrdreNr:
    FOR EACH VarebehBestLinje OF VareBehBestHode EXCLUSIVE-LOCK:
      DELETE VarebehBestLinje.
    END.
    DELETE VareBehBestHode.
  END.
  FOR EACH BestHode EXCLUSIVE-LOCK
      WHERE BestHode.OrdreNr = Ordre.OrdreNr:

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
  DELETE Ordre.
END.
ELSE IF AVAIL Ordre AND Ordre.OrdreStatus NE 2 THEN
  ocReturn = "Programmet kan bare benyttes til å slette sendte ordre".
ELSE IF NOT AVAIL Ordre THEN
  ocReturn = "Ordre er ikke tilgjengelig for sletting" + CHR(10) + PROGRAM-NAME(1).

obOk = ocReturn = "".
