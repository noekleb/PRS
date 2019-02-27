/* Bibliotek for kalkulerte felter, kasseordre hode
  Opprettet: 27.10.10 av BHa
------------------------------------------------------------------------------*/  
DEF VAR iCL           AS INT NO-UNDO.
DEF VAR iClPrisProfil AS INT NO-UNDO.
DEF VAR fAktivPris   AS DEC NO-UNDO.

PROCEDURE kasseordre_befrakter:
  DEF INPUT  PARAM irKordreHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND KOrdreHode WHERE ROWID(KOrdreHode) = irKOrdreHode NO-LOCK NO-ERROR.
  IF AVAIL KOrdreHode THEN 
    FOR EACH KOrdreLinje NO-LOCK
        OF KOrdreHode
       ,FIRST SysPara NO-LOCK
              WHERE SysPara.SysHId = 150
                AND SysPara.SysGr  = 10
                AND SysPara.Parameter1 = KOrdreLinje.VareNr
        :
      ocValue = SysPara.Parameter1.
    END.
END.

PROCEDURE kasseordre_frakt:
  DEF INPUT  PARAM irKordreHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND KOrdreHode WHERE ROWID(KOrdreHode) = irKOrdreHode NO-LOCK NO-ERROR.
  IF AVAIL KOrdreHode THEN 
    FOR EACH KOrdreLinje NO-LOCK
        OF KOrdreHode
       ,FIRST SysPara NO-LOCK
              WHERE SysPara.SysHId = 150
                AND SysPara.SysGr  = 10
                AND SysPara.Parameter1 = KOrdreLinje.VareNr
        :
      ocValue = STRING(KOrdreLinje.Pris).
    END.
END.
