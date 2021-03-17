/* Sjekk om det finnes en post i ProduktFamilie
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.
  
  CREATE ProduktFamMedlem.
  
  ASSIGN 
    ProduktFamMedlem.ProdFamId         = DEC(ENTRY(1,icParam,';'))
    ProduktFamMedlem.ProdFamArtikkelNr = DEC(ENTRY(2,icParam,';'))
    ProduktFamMedlem.ProdFamStrKode    = INT(ENTRY(3,icParam,';'))
  NO-ERROR.
  obOk = NOT ERROR-STATUS:ERROR.
  IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).

  /*Modifiser endringsdato for ProduktFamilie*/
  FIND ProduktFamilie OF ProduktFamMedlem EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL ProduktFamilie THEN
  DO:
    ASSIGN 
      ProduktFamilie.edato = TODAY
    NO-ERROR.
    obOk = NOT ERROR-STATUS:ERROR.
    IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
  END.
  
