/* ovbuffer_opprett.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iNyttLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufOvBuffer FOR OvBuffer.

ASSIGN 
    obOk        = TRUE
    iBuntNr     = INT(ENTRY(1,icParam,'|'))  
    iLinjeNr    = INT(ENTRY(2,icParam,'|'))
    iNyttLinjeNr = INT(ENTRY(3,icParam,'|')) 
    cLogg       = 'ovbuffer_kopier' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

DO TRANSACTION:
  FIND OvBuffer EXCLUSIVE-LOCK WHERE 
    OvBuffer.BuntNr = iBuntNr AND 
    OvBuffer.LinjeNr = iLinjeNr NO-ERROR.
  IF AVAILABLE OvBuffer THEN 
  DO:
    CREATE bufOvbuffer.
    BUFFER-COPY OvBuffer 
      EXCEPT LinjeNr
      TO bufOvBuffer
      ASSIGN 
        bufOvBuffer.LinjeNr = iNyttLinjeNr
        .
    ASSIGN 
      obOk = TRUE
      ocReturn = ''
      .
  END.
  ELSE 
    ASSIGN 
      obOk     = FALSE 
      ocReturn = '**Finner ikke linje det skal kopieres fra. BuntNr: ' + STRING(iBuntNr) + ' LinjeNr: ' + STRING(iLinjeNr) + '.'
      .
      
END.  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
