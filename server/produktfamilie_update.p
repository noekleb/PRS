/* Sjekk om det finnes en post i ProduktFamilie
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.
  
  DEF VAR fProdFamId AS DEC  NO-UNDO.
  DEF VAR cFieldName AS CHAR NO-UNDO.
  DEF VAR cValue     AS CHAR NO-UNDO.
  
  ASSIGN 
    fProdFamId = DEC(ENTRY(1,icParam,';'))
    cFieldName = ENTRY(2,icParam,';')
    cValue     = ENTRY(3,icParam,';')
    obOk       = FALSE
  .
  
  FOR FIRST ProduktFamilie WHERE ProduktFamilie.ProdFamId = fProdFamId EXCLUSIVE-LOCK:
    CASE cFieldName:
      WHEN 'ProdFamPrisLinje' THEN ASSIGN ProduktFamilie.ProdFamPrisLinje = DEC(cValue).
    END CASE.
    obOk = TRUE.
  END.

