PROCEDURE pksdl_ordrelist:
  DEF INPUT  PARAM ifPksdlId    AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FOR EACH PkSdlLinje NO-LOCK
      WHERE PkSdlLinje.PkSdlId = ifPksdlId
      :
    IF NOT CAN-DO(ocValue,STRING(PkSdlLinje.OrdreNr)) THEN 
      ocValue = ocValue + STRING(PkSdlLinje.OrdreNr) + ",".
  END.
  ocValue = TRIM(ocValue,",").
END.

PROCEDURE pksdl_ekstidlist:
  DEF INPUT  PARAM ifPksdlId    AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FOR EACH PkSdlLinje NO-LOCK
      WHERE PkSdlLinje.PkSdlId = ifPksdlId
     ,FIRST BestHode NO-LOCK
            WHERE BestHode.BestNr = PkSdlLinje.BestNr
      :
    IF NOT CAN-DO(ocValue,BestHode.EkstId) THEN 
      ocValue = ocValue + BestHode.EkstId + ",".
  END.
  ocValue = TRIM(ocValue,",").
END.
