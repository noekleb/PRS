/* Bibliotek for kalkulerte felter, artpris i forbindelse med priskø 
  Opprettet: 22.03.10 av BHa
------------------------------------------------------------------------------*/  
DEFINE VARIABLE iCurrUtskriftsnr AS INTEGER.
DEFINE BUFFER bEtikettko FOR Etikettko.

  
PROCEDURE AntEtikett:
  DEF INPUT  PARAM irEtikettko  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR iAnt AS INT NO-UNDO.
      
  FIND Etikettko NO-LOCK
       WHERE ROWID(Etikettko) = irEtikettko
       NO-ERROR.
  IF AVAIL Etikettko THEN
    FOR EACH bEtikettko NO-LOCK
        WHERE bEtikettko.Utskriftsnr = Etikettko.Utskriftsnr
        :
      iAnt = iAnt + bEtikettko.EtikettAntHylleplasser.
    END.
  ocValue = STRING(iAnt).
END.

