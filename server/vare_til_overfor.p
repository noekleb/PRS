/* vare_til_overfor.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE iOvBuntNr AS INTEGER NO-UNDO.
DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iTilbut AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.

DEFINE TEMP-TABLE ttArtikkel
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  INDEX idxArtikkel ArtikkelNr.

ASSIGN 
  bTest      = IF SEARCH('test.txt') <> ? THEN TRUE ELSE FALSE
  cLogg      = 'vare_til_overfor' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

{syspara.i 150 1 2 iButNr INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButNr NO-ERROR.

ASSIGN 
    obOk     = TRUE
    iOvBuntNr = INT(ENTRY(1,icParam,'|'))
    iButNr    =  INT(ENTRY(2,icParam,'|'))
    iTilBut   =  INT(ENTRY(3,icParam,'|'))
    .

FIND OvBunt NO-LOCK WHERE 
  OvBunt.BuntNr = iOvBuntNr NO-ERROR.
IF NOT AVAILABLE OvBunt THEN 
DO:
  ASSIGN 
    obok     = FALSE 
    ocReturn = '** Finner ikke overføring (BuntNr) ' + STRING(iOvBuntNr)
    .
  IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    ocReturn: ' + ocReturn + '. Avbryter.' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  icParam....: ' + icParam 
      ).        
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).
  END.    
  RETURN. 
END.

FIND LAST OvBuffer OF OvBunt USE-INDEX BuntLinjeNr NO-LOCK NO-ERROR.
IF AVAILABLE OvBuffer THEN 
  iLinjeNr = OvBuffer.LinjeNr + 1.
ELSE 
  iLinjeNr = 1.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  iOvBuntNr: ' + STRING(iOvBuntNr) + ' Available: ' + STRING(AVAILABLE OvBunt) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  icParam....: ' + icParam 
    ).        
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  ihBuffer:NAME: ' + ihBuffer:NAME 
    ).        
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  LinjeNr....: ' + STRING(iLinjeNr) 
    ).        
END.
  
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
LOOPEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    .
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-ERROR.
        
  IF AVAILABLE ArtBas AND NOT CAN-FIND(ttArtikkel WHERE 
                  ttArtikkel.ArtikkelNr = ArtBas.ArtikkelNr) THEN 
  DO:
    CREATE ttArtikkel.
    ASSIGN 
      ttArtikkel.ArtikkelNr = ArtBas.ArtikkelNr
      .
  END.      
        
  IF AVAILABLE ArtBas /*AND ihBuffer:BUFFER-FIELD("LagAnt"):BUFFER-VALUE > 0*/ THEN 
  DO:

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Artikkel: ' + STRING(ArtBas.ArtikkelNr) + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevKod + ' ' + ArtBas.LevFargKod 
      ).    

    FIND FIRST OvBuffer EXCLUSIVE-LOCK WHERE 
      OvBuffer.BuntNr = OvBunt.BuntNr AND 
      OvBuffer.ArtikkelNr = ArtBas.ArtikkelNr AND 
      OvBuffer.Storl = ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE AND 
      OvBuffer.ButikkNrFra = INT(iButNr) AND 
      OvBuffer.ButikkNrTil = INT(iTilbut) NO-ERROR.
    IF NOT AVAILABLE OvBuffer THEN
    DO: 
      CREATE OvBuffer.
      ASSIGN 
        OvBuffer.BuntNr      = OvBunt.BuntNr
        OvBuffer.LinjeNr     = iLinjeNr
        OvBuffer.ArtikkelNr  = ArtBas.ArtikkelNr
        OvBuffer.ButikkNrFra = INT(iButNr)
        OvBuffer.ButikkNrTil = INT(iTilbut)
        Ovbuffer.Vg          = ArtBas.Vg
        Ovbuffer.LopNr       = ArtBas.LopNr
        Ovbuffer.Merknad     = ""
        Ovbuffer.VareKost    = ihBuffer:BUFFER-FIELD("ArtLag_VVareKost"):BUFFER-VALUE
        Ovbuffer.Mva%        = 0
        OvBuffer.Storl       = ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE
        OvBuffer.TilStorl    = OvBuffer.Storl
        .
    END.
    /* Antall fra sist innhentede post vinner. */
    ASSIGN 
      OvBuffer.Antall = OvBuffer.Antall + 1 /*DEC(ihBuffer:BUFFER-FIELD("LagAnt"):BUFFER-VALUE)*/
      iLinjeNr = iLinjeNr + 1
      .
  END.      

  hQuery:GET-NEXT().
END. /* LOOPEN */

ASSIGN 
  obOK     = TRUE 
  ocReturn = ''
  .

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
