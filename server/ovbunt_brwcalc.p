/* Bibliotek for kalkulerte felter, 
  
------------------------------------------------------------------------------*/  
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

PROCEDURE ovbunt_DatoTidOppdatert:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:    
    ocReturn = STRING(DATETIME(OvBunt.DatoOppdatert,OvBunt.TidOppdatert),"99/99/99 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE ovbunt_Opprettet:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:    
    ocValue = STRING(DATETIME(OvBunt.RegistrertDato,OvBunt.RegistrertTid),"99/99/99 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE ovbunt_DatotidFakturert:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:  
    FIND FakturaHode NO-LOCK WHERE 
      FakturaHode.Faktura_Id = Ovbunt.Faktura_Id NO-ERROR.
    IF AVAILABLE FakturaHode THEN   
      ocValue = STRING(DATETIME(FakturaHode.FakturertDato,FakturaHode.FakturertTid),"99/99/99 HH:MM:SS"). 
  END.
END PROCEDURE.

PROCEDURE ovbunt_FakturaNr:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:
    FIND FakturaHode NO-LOCK WHERE
      FakturaHode.Faktura_Id = OvBunt.Faktura_Id NO-ERROR.
    IF AVAILABLE FakturaHode THEN      
      ocValue = STRING(FakturaHode.FakturaNr).
    ELSE 
      ocValue = ''. 
  END.
END PROCEDURE.

PROCEDURE ovbunt_Dummy:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  ocValue = ''. 

END PROCEDURE.

PROCEDURE ovbunt_OpphavTekst:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:
    CASE OvBunt.opphav:
      WHEN  1 THEN ocValue = 'Overførings ordre register'. 
      WHEN  2 THEN ocValue = 'Fra Varemottak av bestilling'. 
      WHEN  3 THEN ocValue = '3 - Ukjent'. 
      WHEN  4 THEN ocValue = '4 - Ukjent'. 
      WHEN  5 THEN ocValue = 'Fra Nettbututikk lager'. 
      WHEN  6 THEN ocValue = 'Kundeordre plukking'. 
      WHEN  7 THEN ocValue = 'Fra pakkseddel'. 
      WHEN  8 THEN ocValue = 'Fra overføringsordre'. 
      WHEN  9 THEN ocValue = 'Fra varetrans registrering'. 
      WHEN 10 THEN ocValue = 'Reservasjon fra butikk'. /* Reservasjoner fra butikkene */
    END CASE.    
  END.
END PROCEDURE.

PROCEDURE ovbunt_BuntStatusTekst:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:
    CASE OvBunt.BuntStatus:
      WHEN  10 THEN ocValue = '10 Ny'. 
      WHEN  20 THEN ocValue = '20 Sendt eCom'. 
      WHEN  30 THEN ocValue = '30 Sendt butikk'. 
      WHEN  40 THEN ocValue = '40 Avvist eCom'. 
      WHEN  50 THEN ocValue = '50 Utlevert kunde'.
      WHEN  60 THEN ocvalue = '60 Ferdigbehandlet'. 
      OTHERWISE ocValue = ''.
    END CASE.    
  END.
END PROCEDURE.

PROCEDURE ovbunt_OvNotatFinnes:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:
    IF OvBunt.OvNotat <> '' THEN 
      ocValue = '*'.
  END.
END PROCEDURE.

PROCEDURE ovbunt_BuntStatusSkip:
  DEF INPUT  PARAM irOvBunt AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  cTekst = TRIM(REPLACE(icParam,CHR(1),',')).

MESSAGE 'cTekst:' cTekst CAN-DO(cTekst,STRING(OvBunt.BuntStatus)) SKIP 
'icParam:' icParam
VIEW-AS ALERT-BOX.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:
        
    IF (cTekst = '' OR CAN-DO(cTekst,STRING(OvBunt.BuntStatus))) THEN 
      ocReturn = ''.   
    ELSE 
      ocReturn = "SKIPROW".   
  END.
END PROCEDURE.












