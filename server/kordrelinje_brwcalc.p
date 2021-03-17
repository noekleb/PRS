/* Bibliotek for kalkulerte felter, kundeordrelinje
  Opprettet: 04.07.06 av BHa
------------------------------------------------------------------------------*/  

PROCEDURE kordlinje_returkode:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FOR FIRST KOrdreLinje NO-LOCK 
      WHERE ROWID(KOrdreLinje) = irKOrdreLinje:

    FIND ReturkodeRegister NO-LOCK WHERE 
      ReturKodeRegister.ReturKodeId = KOrdreLinje.ReturKodeId NO-ERROR.
    IF AVAILABLE ReturKodeRegister THEN 
      ocValue = STRING(ReturKodeRegister.ReturKodeId) + ' ' + ReturKodeRegister.ReturKodeTekst.
    ELSE 
      ocValue = ''.
  END.
END PROCEDURE.

PROCEDURE kordlinje_vgrabatt%:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FOR FIRST KOrdreLinje NO-LOCK 
      WHERE ROWID(KOrdreLinje) = irKOrdreLinje
      ,FIRST ArtBas NO-LOCK
             WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.Varenr)
      ,FIRST KOrdreHode OF KOrdreLinje NO-LOCK
      :
    RUN art_vgrabatt%.p(ROWID(ArtBas),STRING(KOrdreHode.KundeNr),icSessionId,OUTPUT ocValue).
  END.
END PROCEDURE.

PROCEDURE kordrelinje_sumeksmvakr:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND KOrdreLinje NO-LOCK
       WHERE ROWID(KOrdreLinje) = irKOrdreLinje
       NO-ERROR.

  IF AVAIL KOrdreLinje THEN 
    ocValue = STRING(KOrdreLinje.NettoLinjeSum - KOrdreLinje.MvaKr).
END PROCEDURE.

PROCEDURE kordrelinje_LevKod:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND KOrdreLinje NO-LOCK
       WHERE ROWID(KOrdreLinje) = irKOrdreLinje
       NO-ERROR.

  IF AVAIL KOrdreLinje THEN 
  DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.LevKod.
      ELSE 
        ocValue = ''.
  END.
END PROCEDURE.

PROCEDURE kordrelinje_Dummy:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  ocValue = ''.
  
END PROCEDURE.

PROCEDURE kordrelinje_sumdbkr:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND KOrdreLinje NO-LOCK
       WHERE ROWID(KOrdreLinje) = irKOrdreLinje
       NO-ERROR.

  IF AVAIL KOrdreLinje THEN 
    ocValue = STRING(KOrdreLinje.DbKr * KOrdreLinje.Antall).
END PROCEDURE.

PROCEDURE kordrelinje_sumvarekost:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND KOrdreLinje NO-LOCK
       WHERE ROWID(KOrdreLinje) = irKOrdreLinje
       NO-ERROR.

  IF AVAIL KOrdreLinje THEN 
    ocValue = STRING(KOrdreLinje.Varekost * KOrdreLinje.Antall).

END PROCEDURE.

PROCEDURE kordrelinje_paa_tilbud:
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam       AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND KOrdreLinje NO-LOCK
       WHERE ROWID(KOrdreLinje) = irKOrdreLinje
       NO-ERROR.

  IF AVAIL KOrdreLinje AND KOrdreLinje.VareNr NE "" THEN DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      RUN art_paa_tilbud.p (ROWID(artbas),icParam,icSessionId,OUTPUT ocValue).
  END.

END PROCEDURE.

PROCEDURE fraktVare:    
  DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND KOrdreLinje NO-LOCK
       WHERE ROWID(KOrdreLinje) = irKOrdreLinje
       NO-ERROR.

  IF AVAIL KOrdreLinje AND KOrdreLinje.VareNr NE "" AND 
     CAN-FIND(FIRST SysPara 
              WHERE SysPara.SysHId = 150
                AND SysPara.SysGr  = 10
                AND SysPara.Parameter1 = KOrdreLinje.VareNr) THEN
   ocValue = "skiprow".
END PROCEDURE.



