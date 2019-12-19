/* Kodrehode_manko.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE TEMP-TABLE ttKOrdrePostPakke NO-UNDO
  FIELD KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" 
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS Blob
  .

ASSIGN 
  lKOrdre_Id = DEC(ENTRY(1,icParam,'|'))
  ocReturn   = 'Fant ikke PDF filen.'
  cLogg = 'Kodrehode_getBlob' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start.' 
            ).   
IF lKOrdre_Id > 0 THEN 
DO: 
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = lKORdre_Id NO-ERROR.
  IF AVAILABLE KOrdrEHode THEN 
  DO:   
    FIND LAST KOrdrePostPakke OF KOrdreHode NO-LOCK USE-INDEX idxPostPakke NO-ERROR.
    IF AVAILABLE KOrdrePostPakke THEN 
    DO:
      BUFFER-COPY KOrdrePostPakke
        TO ttKOrdrePostPakke.
        
      ihBuffer:COPY-TEMP-TABLE (BUFFER ttKOrdrePostPakke:HANDLE,NO,NO,YES).
      ocReturn = ''.
      
     rStandardFunksjoner:SkrivTilLogg(cLogg,
                '   Blob KOrdre_Id/EkstOrdreNr: ' + STRING(KOrdreHode.KOrdre_Id) + '/' + STRING(KOrdreHode.EkstOrdreNr)  
                ).   
    END.
  END.
END.

obOk = ocReturn = "".
obOk = TRUE.

 rStandardFunksjoner:SkrivTilLogg(cLogg,
            '   Resultat: ' + STRING(obOk) + ' ' + ocReturn  
            ).   
 rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Slutt.' 
            ).   


/* **********************  Internal Procedures  *********************** */
