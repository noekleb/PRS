/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbunt */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create, delete or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEFINE VARIABLE cFelt AS CHARACTER NO-UNDO.
DEF VAR cFields        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldValues   AS CHAR  NO-UNDO.   

DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE iOrgKampanjeId      AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cBrukerId           AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iLoop               AS INTEGER NO-UNDO.
DEFINE VARIABLE lKamp%              AS DECIMAL NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

ASSIGN 
  bTest = TRUE
  cLogg = 'kampanjehode_post_update' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    Modus: ' + icAction 
  ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    FeltLOOP:' 
  ).    
DO iLoop = 1 TO NUM-ENTRIES(cFields):
  cFelt = ENTRY(iLoop,cFields).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '      Felt: ' + cFelt + ' Verdi: ' + ENTRY(iLoop,cFieldValues,'|') 
    ).    
  CASE cFelt:
    WHEN 'BrukerId' THEN 
      DO: 
        cBrukerId = ENTRY(iLoop,cFieldValues,'|').
        ASSIGN 
          ihBuffer:BUFFER-FIELD("EDato"):BUFFER-VALUE    = TODAY
          ihBuffer:BUFFER-FIELD("ETid"):BUFFER-VALUE     = TIME
          ihBuffer:BUFFER-FIELD("BrukerId"):BUFFER-VALUE = cBrukerId
          .
        IF icAction = 'CREATE' THEN 
          ASSIGN 
            ihBuffer:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE = TODAY
            ihBuffer:BUFFER-FIELD("RegistrertTid"):BUFFER-VALUE = TIME
            ihBuffer:BUFFER-FIELD("RegistrertAv"):BUFFER-VALUE = cBrukerId
            .
      END.
    WHEN 'AvslagType' THEN
      DO: 
        IF icAction = 'CREATE' THEN 
        DO:
          ihBuffer:BUFFER-FIELD("Avslagtype"):BUFFER-VALUE = ENTRY(iLoop,cFieldValues,'|').
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  CREATE Setter felt Avslagtype til: ' + ENTRY(iLoop,cFieldValues,'|') 
            ).    
        END.
      END.
    WHEN 'ProfilNr' THEN ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE = ENTRY(iLoop,cFieldValues,'|').
  END CASE.
END.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Modus: ' + icAction 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cFields: ' + cFields 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cFieldValues: ' + cFieldValues 
    ).        
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '      cBrukerId: ' + cBrukerId 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '      lKamp%: ' + STRING(lKamp%) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '      OrgKampanjeId: ' + STRING(iOrgKampanjeId) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Verdier fra buffer: ' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '      AktiveresTid.....: ' + STRING(ihBuffer:BUFFER-FIELD("AktiveresTid"):BUFFER-VALUE) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '      GyldigTilTid.....: ' + STRING(ihBuffer:BUFFER-FIELD("GyldigTilTid"):BUFFER-VALUE) 
    ).    
END.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    

ocValue = "".
RETURN.
