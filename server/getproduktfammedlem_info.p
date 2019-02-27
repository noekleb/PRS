/* Parameters:  <Value> 
   Created: 3. sept 2007 by Geir Otto Olsen
                 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR bRecordsFound        AS LOG NO-UNDO.
DEF VAR iKey                 AS INT NO-UNDO.

DEF VAR fLowPrice  AS DEC INIT  9999999 NO-UNDO.
DEF VAR fHighPrice AS DEC INIT -9999999 NO-UNDO.

ASSIGN 
  iKey          = INT(ENTRY(1,icParam,'|'))
  bRecordsFound = FALSE
.

FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.ProdFamId = iKey NO-LOCK:
  bRecordsFound = TRUE.
  FIND FIRST artpris 
    WHERE ArtPris.ProfilNr   = 1 
      AND ArtPris.ArtikkelNr = ProduktFamMedlem.ProdFamArtikkelNr 
    NO-LOCK NO-ERROR.
  IF AVAIL artpris THEN
    ASSIGN 
      fHighPrice = IF ArtPris.Pris[1] GT fHighPrice THEN ArtPris.Pris[1] ELSE fHighPrice
      fLowPrice  = IF ArtPris.Pris[1] LT fLowPrice  THEN ArtPris.Pris[1] ELSE fLowPrice
    .
END.
  IF NOT bRecordsFound THEN
    ocReturn = '0|0|0'.
  ELSE
    ocReturn = STRING(fLowPrice) + '|' + STRING(fHighPrice - fLowPrice) + '|' + STRING(fHighPrice).
  obOk = TRUE.
