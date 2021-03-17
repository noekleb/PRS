/* artbas_setmodell.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEFINE TEMP-TABLE ttArtikkel
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD ModellFarge AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD LinjeNr AS INTEGER 
  FIELD HovedModellFarge AS LOG
  INDEX idxArtikkel ArtikkelNr
  INDEX idxModellFarge ModellFarge
  INDEX idxLinjeNr LinjeNr
  .

DEFINE BUFFER bttArtikkel FOR ttArtikkel.
DEFINE BUFFER bufArtBas FOR ArtBas.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
  bTest      = IF SEARCH('tnc.txt') <> ? THEN TRUE ELSE FALSE
  cLogg      = 'artbas_setmodell' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

ASSIGN 
    obOk     = TRUE
    .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

/* Her legges de markerte radene opp i tabellen. */
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
      iLinjeNr = iLinjeNr + 1
      ttArtikkel.ArtikkelNr  = ArtBas.ArtikkelNr
      ttArtikkel.ModellFarge = ArtBas.ModellFarge
      ttArtikkel.LinjeNr     = iLinjeNr
      .
  END.      
  hQuery:GET-NEXT().
END. /* LOOPEN */

/* Her legges alle artikler som er koblet til de markerte artiklene opp i tabellen. */
FOR EACH ttArtikkel WHERE 
  ttArtikkel.ArtikkelNr > 0:
  /* Legger til eventuelle artikler linket til hoved artikkel i modell */
  HOVEDVAREIMODELL:
  FOR EACH bufArtBas NO-LOCK WHERE
    bufArtBas.ModellFarge = ttArtikkel.ArtikkelNr:
    IF NOT CAN-FIND(bttArtikkel WHERE 
      bttArtikkel.ArtikkelNr = bufArtBas.ArtikkelNr) THEN 
    DO:
      CREATE bttArtikkel.
      ASSIGN 
        iLinjeNr = iLinjeNr + 1
        bttArtikkel.ArtikkelNr  = bufArtBas.ArtikkelNr
        bttArtikkel.ModellFarge = bufArtBas.ModellFarge
        bttArtikkel.LinjeNr     = iLinjeNr
        .
    END.      
  END. /* HOVEDVAREIMODELL */
END.

/* Hvis noen av artikklene medelm i en modell, hentes resten av artikklene i modellen også inn. */
FOR EACH ttArtikkel WHERE 
  ttArtikkel.ModellFarge > 0:
  /* Legger til eventuelle artikler linket til hoved artikkel i modell */
  HOVEDVAREIMODELL:
  FOR EACH bufArtBas NO-LOCK WHERE
    bufArtBas.ModellFarge = ttArtikkel.ModellFarge:
    IF NOT CAN-FIND(bttArtikkel WHERE 
      bttArtikkel.ArtikkelNr = bufArtBas.ArtikkelNr) THEN 
    DO:
      CREATE bttArtikkel.
      ASSIGN 
        iLinjeNr = iLinjeNr + 1
        bttArtikkel.ArtikkelNr  = bufArtBas.ArtikkelNr
        bttArtikkel.ModellFarge = bufArtBas.ModellFarge
        bttArtikkel.LinjeNr     = iLinjeNr
        .
    END.      
  END. /* HOVEDVAREIMODELL */
END.

/* Første artikkel blir ny hovedvare på den sammenslåtte modellen. */
FOR EACH ttArtikkel 
  BREAK BY ttArtikkel.LinjeNr:
  IF ttArtikkel.LinjeNr = 1 THEN 
    DO:
      ASSIGN 
        lArtikkelNr                 = ttArtikkel.ArtikkelNr
        ttArtikkel.ModellFarge      = ttArtikkel.ArtikkelNr
        ttArtikkel.HovedModellFarge = TRUE
        .
    END.
  ELSE ASSIGN 
         ttArtikkel.ModellFarge      = lArtikkelNr
         ttArtikkel.HovedModellFarge = FALSE
         .
END.

/* Oppdaterer artikklene */
FOR EACH ttArtikkel TRANSACTION:
  FIND ArtBas EXCLUSIVE-LOCK WHERE 
    ArtBas.ArtikkelNr = ttArtikkel.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    ASSIGN  
      ArtBas.ModellFarge      = ttArtikkel.ModellFarge
      ArtBas.HovedModellFarge = ttArtikkel.HovedModellFarge
      .
    RELEASE ArtBas.
  END.  
END.

ASSIGN 
  obOK     = TRUE 
  ocReturn = ''
  .

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
