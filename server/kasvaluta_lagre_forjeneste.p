/* Lagrer styrerecord, dvs prosentvis forjeneste for KasValuta
   Parameter:  
   Opprettet: 04.06.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iProfilnr    AS INT NO-UNDO.
DEF VAR fFortjeneste AS DEC NO-UNDO.

DEF BUFFER bKasValuta FOR KasValuta.

ASSIGN iProfilnr    = INTEGER(ENTRY(1,icParam,"|"))
       fFortjeneste = DECIMAL(ENTRY(2,icParam,"|"))
       NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Feil i parameter til prosedyre " + PROGRAM-NAME(1) + ": " + icParam.  
  RETURN.
END.

FIND FIRST KasValuta EXCLUSIVE-LOCK
     WHERE KasValuta.ValKod = "F%"
       AND KasValuta.ProfilNr = iProfilnr
     NO-WAIT NO-ERROR.
IF NOT AVAIL KasValuta AND NOT LOCKED(KasValuta) THEN DO:
  CREATE KasValuta.
  ASSIGN KasValuta.ValKod   = "F%"
         KasValuta.ValKurs  = fFortjeneste
         KasValuta.ProfilNr = iProfilnr
         KasValuta.ValDatum = TODAY.
END.
ELSE IF NOT LOCKED(KasValuta) THEN
  ASSIGN KasValuta.ValKurs  = fFortjeneste
         KasValuta.ValDatum = TODAY.
ELSE DO:
  ocReturn = "KasValuta tabell ikke tilgjengelig for oppdatering".
  RETURN.
END.

FOR EACH bKasValuta EXCLUSIVE-LOCK
    WHERE bKasValuta.ValNr > 0 
      AND bKasValuta.ValAktiv
      AND bKasValuta.ProfilNr = iProfilnr:
  bKasValuta.KasseValKurs = ROUND((1 - fFortjeneste / 100) * bKasValuta.ValKurs,3).
END.

obOK = ocReturn = "".
