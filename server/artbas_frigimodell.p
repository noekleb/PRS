/* artbas_frigimodell.p

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

iLinjeNr = 0.
/* Hvis noen av artikklene som frigis er hovedartikkel i en modell, byttes hovedartikkel i modellen. */
FOR EACH ttArtikkel:
  /* Er artikkelen hovedvare i en modell, byttes hovedvare i modellen. */
  BYTTHOVEDVAREIMODELL:
  FOR EACH bufArtBas EXCLUSIVE-LOCK WHERE
    bufArtBas.ModellFarge = ttArtikkel.ArtikkelNr:
    
    /* Den første artikkelen man finner, settes som ny hovedvare i modellen. */
    /* De resterende kobles til den artikkelen istedenfor den som frigis.    */
    IF bufArtBas.ArtikkelNr <> ttArtikkel.ArtikkelNr THEN 
    DO:
      iLinjeNr = iLinjeNr + 1.
      IF iLinjeNr = 1 THEN 
      DO:
        ASSIGN
          lArtikkelNr                = bufArtBAs.ArtikkelNr 
          bufArtBas.ModellFarge      = bufArtBas.ArtikkelNr
          bufArtBas.HovedModellFarge = TRUE
          . 
      END.
      ELSE DO:
        ASSIGN
          bufArtBas.ModellFarge      = lArtikkelNr
          bufArtBas.HovedModellFarge = FALSE
          . 
      END.   
    END.
  END. /* BYTTHOVEDVAREIMODELL */
END.

/* Artikklene frikobles. */
FOR EACH ttArtikkel 
  BREAK BY ttArtikkel.LinjeNr:
  ASSIGN 
         ttArtikkel.ModellFarge      = 0
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
