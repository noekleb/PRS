DEFINE VARIABLE iCl AS INT NO-UNDO.
DEFINE VARIABLE iUtvidetStatus AS INTEGER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.

{syspara.i 19 9 4 iUtvidetStatus INT}
{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
  Butiker.Butik = iCl NO-ERROR.

PROCEDURE kordre_ReturFraBut: 
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN
  DO:
    ocValue = ENTRY(2,ENTRY(2,KOrdrEHode.VerkstedMerknad,CHR(10)),'.') NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      ocValue = ''.
  END.
  ELSE 
    ocValue = ''.
END PROCEDURE. 

PROCEDURE kordre_webSent: 
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN
  DO:
    ocValue = STRING(KORdreHode.webSendt,"99/99/9999 HH:MM:SS").
    IF ocValue = ? THEN ocValue = ''.
  END.
  ELSE 
    ocValue = ''.
END PROCEDURE. 

PROCEDURE kordre_webcurrentstatus:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN
    ocValue = KORdreHode.webcurrentstatus.
  ELSE 
    ocValue = ''.
END PROCEDURE. 

PROCEDURE kordre_PDF:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN
  DO: 
    IF CAN-FIND(FIRST KOrdrePostPakke OF KOrdreHode) THEN 
      ocValue = 'TRUE'.
    ELSE 
      ocValue = 'FALSE'.
  END.  
  ELSE ocValue = ''.

END PROCEDURE. 

PROCEDURE kordre_AntIkkeRet:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
    RUN kordrelinje_retur_sjekk.p(STRING(KOrdreHode.RefKOrdre_Id),?,"",OUTPUT ocValue,OUTPUT obOk).
  ELSE ocValue = ''.

END PROCEDURE. 

PROCEDURE kordre_Navn:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
    ocValue = KOrdreHode.Navn.
  ELSE 
    ocValue = ''.

END PROCEDURE. 

PROCEDURE kordre_cOpt1:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
    ocValue = KOrdreHode.cOpt1.
  ELSE 
    ocValue = ''.

END PROCEDURE. 

PROCEDURE kordre_LevAdresse:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
    ocValue = KOrdreHode.LevAdresse1.
  ELSE 
    ocValue = ''.

END PROCEDURE. 

PROCEDURE kordre_FakturaNr:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
  DO:
    FIND FakturaHode NO-LOCK WHERE 
      FakturaHode.Faktura_Id = KOrdreHode.Faktura_Id NO-ERROR.
    IF AVAILABLE FakturaHode THEN 
      ocValue = STRING(FakturaHode.FakturaNr).        
    ELSE ocValue = ''.
  END.
  ELSE ocValue = ''.

END PROCEDURE. 

PROCEDURE kordre_LevFTekst:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
  DO:
    FIND Leveringsform NO-LOCK WHERE 
      Leveringsform.LevFNr = KORdreHode.LevFNr NO-ERROR.
    IF AVAILABLE LeveringsForm THEN 
      ocValue = LeveringsForm.LevFormBeskrivelse + (IF KORdreHode.LevFNr = 8 THEN ' (' + STRING(KOrdreHode.Butik) + ')' ELSE '').        
    ELSE ocValue = 'Ukjent'.
  END.
  ELSE ocValue = 'Ukjent'.

END PROCEDURE.

PROCEDURE kordre_ShipmentSendt:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
  DO:
    ocValue = ENTRY(1,STRING(KOrdreHode.ShipMentSendt),',').
    IF ocValue = ? THEN ocValue = ''.
  END.
  ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE kordre_Dummy:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  ocValue = ENTRY(1,STRING(KOrdreHode.ShipMentSendt),',').
 
END PROCEDURE.

PROCEDURE kordre_Butikk:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
  DO:
    FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN 
      ocValue = STRING(Butiker.butik) + ' ' + Butiker.ButNamn.
    ELSE 
      ocValue = 'Ukjent'.
  END.
  ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE kordre_LevStatus:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND KOrdreHode NO-LOCK
    WHERE ROWID(KOrdreHode) = irKOrdreHode
    NO-ERROR.
  IF AVAILABLE KORdreHode THEN 
  DO:
    /* Ny utvidet status */
    IF iUtvidetStatus = 1 THEN 
    DO:
      FIND SysPara NO-LOCK WHERE 
        SysPara.SysHId = 19 AND 
        SysPara.SysGr  = 15 AND 
        SysPara.ParaNr = INT(KOrdreHode.LevStatus) NO-ERROR.
      IF AVAILABLE SysPara THEN 
        ocValue = STRING(SysPara.ParaNr) + ' ' + SysPara.Parameter1.
      ELSE 
        ocValue = 'Ukjent'.
    END.
        
    /* Gammel status. */
    ELSE 
    DO:
      FIND SysPara NO-LOCK WHERE 
        SysPara.SysHId = 19 AND 
        SysPara.SysGr  = 1 AND 
        SysPara.ParaNr  = INT(KOrdreHode.LevStatus) NO-ERROR.
      IF AVAILABLE SysPara THEN 
        ocValue = STRING(SysPara.ParaNr) + ' ' + SysPara.Parameter1.
      ELSE 
        ocValue = 'Ukjent'.            
    END.
  END.
  ELSE ocValue = ''.

END PROCEDURE.



























