/* Bibliotek for kalkulerte felter, 
  
------------------------------------------------------------------------------*/  
PROCEDURE ovbuffer_Kode:
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:
    FIND StrKonv NO-LOCK WHERE 
      StrKonv.Storl = OvBuffer.Storl NO-ERROR.
    IF AVAILABLE OvBuffer THEN 
      FIND LAST StrekKode NO-LOCK WHERE 
        StrekKode.ArtikkelNr = OvBuffer.ArtikkelNr AND 
        StrekKode.StrKode    = StrKonv.StrKode NO-ERROR.    
    ocValue = (IF AVAILABLE StrekKode THEN StrekKode.Kode ELSE ''). 
  END.
END PROCEDURE.

PROCEDURE ovbuffer_DatoTidEndret:
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:    
    ocValue = STRING(DATETIME(OvBuffer.EDato,OvBuffer.ETid),"99/99/99 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE ovbuffer_DatoTidRegistrert:
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:    
    ocValue = STRING(DATETIME(OvBuffer.RegistrertDat,OvBuffer.RegistrertTid),"99/99/99 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE ovbuffer_LevKod:
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = OvBuffer.ArtikkelNr NO-ERROR.
    ocValue = ArtBas.LevKod. 
  END.
END PROCEDURE.

PROCEDURE ovbuffer_LevFargKod:
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = OvBuffer.ArtikkelNr NO-ERROR.
    ocValue = ArtBas.LevFargKod. 
  END.
END PROCEDURE.

PROCEDURE ovbuffer_Beskr:
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = OvBuffer.ArtikkelNr NO-ERROR.
    ocValue = ArtBas.Beskr. 
  END.
END PROCEDURE.

PROCEDURE ovbuffer_TilButNavn :
  DEF INPUT  PARAM irOvbuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbuffer NO-LOCK 
      WHERE ROWID(OvBuffer) = irOvBuffer:
    FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = OvBuffer.ButikkNrTil NO-ERROR.
    ocValue = Butiker.butNamn. 
  END.
END PROCEDURE.





