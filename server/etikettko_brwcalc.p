/* Bibliotek for kalkulerte felter, Etikettko i forbindelse med priskø 
  Opprettet: 05.03.10 av BHa
------------------------------------------------------------------------------*/  
PROCEDURE jamforpris:
  DEF INPUT  PARAM irEtikettKo  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Etikettko NO-LOCK 
       WHERE ROWID(Etikettko) = irEtikettKo
       NO-ERROR.
  IF AVAIL Etikettko THEN 
    ocValue = STRING(Etikettko.pris[1] * Etikettko.Mengde).

END PROCEDURE.

