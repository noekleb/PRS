/* Bibliotek for kalkulerte felter, 
  
------------------------------------------------------------------------------*/  
DEFINE VARIABLE dDatotid AS DATETIME NO-UNDO.
DEFINE VARIABLE dSum AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.

PROCEDURE bokforingsbilag_BruttoOmsetning:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.
  
  dSum = 0.
  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:
    FOR EACH Kas_Rap NO-LOCK WHERE 
      kas_rap.butikk = BokforingsBilag.ButikkNr AND 
      kas_rap.dato = BokforingsBilag.OmsetningsDato:
      dSum = dSum + kas_rap.MvaGrunnlag[1] + kas_rap.MvaGrunnlag[2] + kas_rap.MvaGrunnlag[3].  
    END.    
    IF AVAILABLE BokforingsVisning THEN  
      ocReturn = STRING(dSum).
    ELSE 
      ocReturn = ''. 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_Kassediff:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:
    FIND LAST BokforingsVisning OF Bokforingsbilag NO-LOCK WHERE 
      BokforingsVisning.Tekst BEGINS 'Kasse diff' NO-ERROR.
    IF AVAILABLE BokforingsVisning THEN  
      ocReturn = STRING(BokforingsVisning.Belop).
    ELSE 
      ocReturn = ''. 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_EndretDatoTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:
    dDatoTid = DATETIME(bokforingsbilag.eDato,bokforingsbilag.eTid * 100).
    ocReturn = STRING(dDatoTid,"99/99/9999 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_SendtDatoTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:
    dDatoTid = DATETIME(bokforingsbilag.SendtDato,bokforingsbilag.SendtTid * 100).
    ocReturn = STRING(dDatoTid,"99/99/9999 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_GodkjentDatoTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:
    dDatoTid = DATETIME(bokforingsbilag.GodkjentDato,bokforingsbilag.GodkjentTid * 100).
    ocReturn = STRING(dDatoTid,"99/99/9999 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_eTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:    
    ocReturn = STRING(bokforingsbilag.eTid,"HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_SendtTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:    
    ocReturn = STRING(bokforingsbilag.SendtTid,"HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_GodkjentTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:    
    ocReturn = STRING(bokforingsbilag.GodkjentTid,"HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE bokforingsbilag_RegistrertTid:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST bokforingsbilag NO-LOCK 
      WHERE ROWID(bokforingsbilag) = irOvBunt:    
    ocReturn = STRING(bokforingsbilag.RegistrertTid,"HH:MM:SS"). 
  END.
END PROCEDURE.











