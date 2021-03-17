/* pksdl_sendtimport.p

    cType1 = "Sendt;dato;Pakkseddel;id;Pk.sdl.nr;EkstId;Butikk;St;Status;Pallenr;Lokasjon;SO;Sendt outlet"
    
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery    AS HANDLE NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cRecord AS CHARACTER NO-UNDO.
DEFINE VARIABLE lModified AS LOG NO-UNDO.

DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.


{ cls\StdFunk\dsttImpFil.i }

ASSIGN
  bTest       = IF SEARCH('test.txt') = ? THEN FALSE ELSE TRUE
  bTest       = TRUE /* Ta bort linje etter prøveperiode. */
  iType       = INTEGER(ENTRY(1,icParam,'|'))
  cLogg       = ENTRY(2,icParam,'|')
  .
IF cLogg = '' THEN
  cLogg = 'pksdl_sendtimport' + REPLACE(STRING(TODAY),'/','').

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  Start pksdl_sendtimport.p.' 
                      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '    parametrene: ' + icParam
                      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '    iType: ' + STRING(iType)
                      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '    Buffer: ' + ihBuffer:NAME
                      ).
                                            
END.



CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
BUFFER_LOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  ASSIGN 
    cRecord = ihBuffer:BUFFER-FIELD("Record"):BUFFER-VALUE
    lModified = FALSE 
    .

  IF iType = 1 THEN
  TYPE1:
  DO: 
    FOR EACH bufPkSdlHode EXCLUSIVE-LOCK WHERE 
      bufPkSdlHode.PksdlNr = ENTRY(3,cRecord,';') /*AND 
      bufPkSdlHode.SendtOutLet = 0*/:

      IF INT(ENTRY(15,cRecord,';')) <> 0 THEN 
        ASSIGN bufPkSdlHode.SendtOutLet = INT(ENTRY(15,cRecord,';')) lModified = TRUE.

      IF ENTRY(11,cRecord,';') <> '' THEN   
        ASSIGN bufPkSdlHode.cPalleNr = ENTRY(11,cRecord,';') lModified = TRUE.

      IF ENTRY(12,cRecord,';') <> '' THEN
      DO: 
        IF NOT CAN-DO(bufPkSdlHode.Lokasjon,TRIM(ENTRY(12,cRecord,';'))) THEN 
          ASSIGN 
            bufPkSdlHode.Lokasjon = bufPkSdlHode.Lokasjon +
                                    (IF bufPkSdlHode.Lokasjon = '' THEN '' ELSE ',') +  
                                    TRIM(ENTRY(12,cRecord,';'))
            . 
          ASSIGN 
            lModified = TRUE
            .
      END.

      IF ENTRY(13,cRecord,';') <> '' THEN 
        ASSIGN bufPkSdlHode.Varetype = ENTRY(13,cRecord,';') lModified = TRUE.

      IF ENTRY(14,cRecord,';') <> '' THEN 
        ASSIGN bufPkSdlHode.LagerSesong = ENTRY(14,cRecord,';') lModified = TRUE.

      IF bufPkSdlHode.SendtFraLagerTilOutlet = ? THEN 
        ASSIGN bufPkSdlHode.SendtFraLagerTilOutlet = (IF bufPkSdlHode.SendtOutLet <> 0 THEN NOW ELSE ?)  lModified = TRUE.
        
      IF bTest AND lModified THEN
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  RecType: ' + STRING(iType) + 
          ' PkSdlHode: ' + STRING(bufPkSdlHode.PkSdlNr) + 
          ' PalleNr: ' + STRING(bufPkSdlHode.cPalleNr) + 
          ' Lokasjon: ' + bufPkSdlHode.Lokasjon +  
          ' LSesong: ' + bufPkSdlHode.LagerSesong +  
          ' SendtFraOutlet: ' + STRING(bufPkSdlHode.SendtOutLet) +
          ' SO:' + ENTRY(10,cRecord,';')   
          ).
      END.        
    END.
  END. /* TYPE2 */
  
  ELSE IF iType = 2 THEN
  TYPE2: 
  DO: 
    FOR EACH bufPkSdlHode EXCLUSIVE-LOCK WHERE 
      bufPkSdlHode.PksdlNr = ENTRY(6,cRecord,';') /* AND 
      bufPkSdlHode.SendtOutLet = 0 */:
        
      IF INT(ENTRY(2,cRecord,';')) <> 0 THEN 
        ASSIGN bufPkSdlHode.SendtOutLet = INT(ENTRY(2,cRecord,';')) lModified = TRUE.
        
      IF ENTRY(1,cRecord,';') <> '' THEN   
        ASSIGN bufPkSdlHode.cPalleNr = ENTRY(1,cRecord,';') lModified = TRUE.
        
      IF ENTRY(3,cRecord,';') <> '' THEN
      DO: 
        IF NOT CAN-DO(bufPkSdlHode.Lokasjon,TRIM(ENTRY(3,cRecord,';'))) THEN 
          ASSIGN 
            bufPkSdlHode.Lokasjon = bufPkSdlHode.Lokasjon +
                                    (IF bufPkSdlHode.Lokasjon = '' THEN '' ELSE ',') +  
                                    TRIM(ENTRY(3,cRecord,';'))
            . 
          ASSIGN 
            lModified = TRUE
            .
      END.
        
      IF ENTRY(5,cRecord,';') <> '' THEN 
        ASSIGN bufPkSdlHode.Varetype = ENTRY(5,cRecord,';') lModified = TRUE.

      IF ENTRY(7,cRecord,';') <> '' THEN 
        ASSIGN bufPkSdlHode.LagerSesong = ENTRY(7,cRecord,';') lModified = TRUE.
        
      IF bufPkSdlHode.SendtFraLagerTilOutlet = ? THEN 
        ASSIGN bufPkSdlHode.SendtFraLagerTilOutlet = IF bufPkSdlHode.SendtOutLet <> 0 THEN NOW ELSE ? lModified = TRUE.
        
      IF bTest AND lModified THEN
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  RecType: ' + STRING(iType) + 
          ' PkSdlHode: ' + STRING(bufPkSdlHode.PkSdlNr) + 
          ' PalleNr: ' + STRING(bufPkSdlHode.cPalleNr) + 
          ' Lokasjon: ' + bufPkSdlHode.Lokasjon + 
          ' LSesong: ' + bufPkSdlHode.LagerSesong +  
          ' SendtFraOutlet: ' + STRING(bufPkSdlHode.SendtOutLet) 
          ).
      END.        
    END.
  END. /* TYPE2 */
  hQuery:GET-NEXT().
END. /* BUFFER_LOOP */

DELETE OBJECT hQuery.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, '  Slutt.').
END.


ASSIGN
  obOK = YES
  obOk = ocReturn = ""
  .

/* **********************  Internal Procedures  *********************** */


