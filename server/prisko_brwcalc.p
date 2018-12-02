/* Bibliotek for kalkulerte felter, Prisko i forbindelse med priskø 
  Opprettet: 04.03.10 av BHa
------------------------------------------------------------------------------*/  
PROCEDURE etikettStatus:
  DEF INPUT  PARAM irPrisko     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Prisko WHERE ROWID(Prisko) = irPrisko NO-LOCK NO-ERROR.
  IF AVAIL Prisko THEN 
    CASE Prisko.EtikettStatus:
      WHEN 0 THEN ocValue = "Etikett".
      WHEN 1 THEN ocValue = "Etikett skrevet".
      WHEN 9 THEN ocValue = "Ingen etikett".
    END.
END PROCEDURE.

PROCEDURE klargjorStatus:    
  DEF INPUT  PARAM irPrisko     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Prisko WHERE ROWID(Prisko) = irPrisko NO-LOCK NO-ERROR.
  IF AVAIL Prisko THEN 
    CASE Prisko.KlargjorStatus:
      WHEN 0 THEN ocValue = "Ubehandlet".
      WHEN 1 THEN ocValue = "Behandlet".
    END.

END PROCEDURE.

PROCEDURE EndrType:    
  DEF INPUT  PARAM irPrisko     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Prisko WHERE ROWID(Prisko) = irPrisko NO-LOCK NO-ERROR.
  IF AVAIL Prisko THEN 
    CASE Prisko.Type:
      WHEN 1 THEN ocValue = "NOR".
      WHEN 2 THEN ocValue = "PÅ".
      WHEN 3 THEN ocValue = "AV".
      WHEN 4 THEN ocValue = "ETIL".
      WHEN 5 THEN ocValue = "LPÅ".
      WHEN 6 THEN ocValue = "LAV".
      OTHERWISE ocValue = STRING(Prisko.EndringsType).
    END.

END PROCEDURE.

PROCEDURE lokal_pris:    
  DEF INPUT  PARAM irPrisko     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Prisko WHERE ROWID(Prisko) = irPrisko NO-LOCK NO-ERROR.
  IF AVAIL Prisko THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr
           AND ArtPris.ProfilNr   = INTEGER(icProfilnr)
         NO-ERROR.
    IF NOT AVAIL ArtPris THEN
      FIND FIRST ArtPris NO-LOCK
           WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr
             AND ArtPris.ProfilNr   = 1
           NO-ERROR.
    IF AVAIL ArtPris THEN
      ocValue = STRING(ArtPris.Pris[1]).
  END.

END PROCEDURE.

PROCEDURE lokal_varekost:    
  DEF INPUT  PARAM irPrisko     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Prisko WHERE ROWID(Prisko) = irPrisko NO-LOCK NO-ERROR.
  IF AVAIL Prisko THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr
           AND ArtPris.ProfilNr   = INTEGER(icProfilnr)
         NO-ERROR.
    IF NOT AVAIL ArtPris THEN
      FIND FIRST ArtPris NO-LOCK
           WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr
             AND ArtPris.ProfilNr   = 1
           NO-ERROR.
    IF AVAIL ArtPris THEN
      ocValue = STRING(ArtPris.Varekost[1]).
  END.

END PROCEDURE.

PROCEDURE lokal_db%:    
  DEF INPUT  PARAM irPrisko     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Prisko WHERE ROWID(Prisko) = irPrisko NO-LOCK NO-ERROR.
  IF AVAIL Prisko THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr
           AND ArtPris.ProfilNr   = INTEGER(icProfilnr)
         NO-ERROR.
    IF NOT AVAIL ArtPris THEN
      FIND FIRST ArtPris NO-LOCK
           WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr
             AND ArtPris.ProfilNr   = 1
           NO-ERROR.
    IF AVAIL ArtPris THEN
      ocValue = STRING(ArtPris.DB%[1]).
  END.

END PROCEDURE.

