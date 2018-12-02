/* Finn en kunde ut fra en rowid.  
   Parametere:  Rowid
   Opprettet: 30.06.05 av BHa 
   Denne skulle vært unødvendig men pga tryning med dynamisk find mot 9.1d base..                   
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

FIND Kunde WHERE ROWID(kunde) = TO-ROWID(icParam) NO-LOCK NO-ERROR.
IF AVAIL Kunde THEN
  ocReturn = STRING(KundeNr).
ELSE 
  ASSIGN obOK = FALSE
         ocReturn = "Finner ikke kunde".
