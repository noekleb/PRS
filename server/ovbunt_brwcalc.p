/* Bibliotek for kalkulerte felter, 
  
------------------------------------------------------------------------------*/  

PROCEDURE ovbunt_DatoTidOppdatert:
  DEF INPUT  PARAM irOvbunt    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  FOR FIRST Ovbunt NO-LOCK 
      WHERE ROWID(OvBunt) = irOvBunt:    
    ocValue = STRING(DATETIME(OvBunt.DatoOppdatert,OvBunt.TidOppdatert),"99/99/99 HH:MM:SS"). 
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
    ocValue = STRING(DATETIME(OvBunt.FakturertDato,OvBunt.FakturertTid),"99/99/99 HH:MM:SS"). 
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
      WHEN 1 THEN ocValue = 'Overførings ordre register'. 
      WHEN 2 THEN ocValue = 'Fra Varemottak av bestilling'. 
      WHEN 3 THEN ocValue = '3 - Ukjent'. 
      WHEN 4 THEN ocValue = '4 - Ukjent'. 
      WHEN 5 THEN ocValue = 'Fra Nettbututikk lager'. 
      WHEN 6 THEN ocValue = 'Kundeordre plukking'. 
      WHEN 7 THEN ocValue = 'Fra pakkseddel'. 
      WHEN 8 THEN ocValue = 'Fra overføringsordre'. 
      WHEN 9 THEN ocValue = 'Fra varetrans registrering'. 
    END CASE.    
  END.
END PROCEDURE.



