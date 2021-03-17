/* kordrehode_sjekksum.p 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAntall AS INTEGER NO-UNDO.
DEFINE VARIABLE lSum AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
 
ASSIGN
    bTest = TRUE 
    cLogg = 'kordrehode_sjekksum' + REPLACE(STRING(TODAY),'/','')
    .
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Parametre: ' 
      ).    
END.
    
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

  IF bTest THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Kundeordre: '  
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    KOrdre_Id: ' + STRING(ihBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE)  
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    KundeNr: ' + STRING(ihBuffer:BUFFER-FIELD("Kundenr"):BUFFER-VALUE)  
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    LevStatus: ' + ihBuffer:BUFFER-FIELD("Levstatus"):BUFFER-VALUE  
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Opphav: ' + ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE  
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    refKOrdre_Id: ' + STRING(ihBuffer:BUFFER-FIELD("RefKOrdre_Id"):BUFFER-VALUE)  
      ).
  END.    

  SUMMER:
  DO:
    FOR EACH KOrdreLinje NO-LOCK WHERE 
      KOrdreLinje.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE):
      lsum    = lSum    + KOrdreLinje.LinjeSum.
      iAntall = iAntall + KOrdreLinje.Antall.
      
      IF bTest THEN         
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      Linje: ' + STRING(ihBuffer:BUFFER-FIELD("KOrdreLinjeNr"):BUFFER-VALUE) + ' ' +  
          STRING(ihBuffer:BUFFER-FIELD("VareNr"):BUFFER-VALUE) + ' ' +  
          STRING(ihBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE) + ' ' +  
          STRING(ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE)  
          ).
    END.
  END. /* SUMMER */

  hQuery:GET-NEXT().
END.

ERROR-STATUS:ERROR = FALSE.

IF ABS(iAntall) > 0 THEN 
  ASSIGN 
    obOk     = FALSE 
    ocReturn = 'Ordrens totalsum skal være null,- ved varebytte. Se til at det for hver returnert vare er registrert en erstattningsvare (Antall = ' + STRING(iAntall) + ').' 
    .
ELSE 
  ASSIGN
    obOk     = TRUE 
    ocReturn = ''
    .
    
IF bTest THEN
DO: 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Resultat: ' + STRING(obOk) + ' ' + ocReturn 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
END.
RETURN.
   