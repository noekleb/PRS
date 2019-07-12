/* Registrer 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.

DEFINE VARIABLE hQuery       AS HANDLE NO-UNDO.
DEFINE BUFFER bArtBas FOR ArtBas.

ASSIGN 
    cLogg     = 'artlag_SendTilKasse' + REPLACE(STRING(TODAY),'/','')
    cBrukerId = USERID("SkoTex")
    .

DEFINE TEMP-TABLE ttArtikkel 
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
  INDEX idxArt AS PRIMARY UNIQUE ArtikkelNr.

RUN bibl_loggDbFri.p (cLogg, 'START' + 
                 ' Bruker: ' + cBrukerId).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

    IF NOT CAN-FIND(FIRST ttArtikkel WHERE ttArtikkel.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)) THEN
    TOGGLE: 
    DO:
      FIND ArtBas WHERE 
          ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      
      IF AVAILABLE ArtBas AND NOT LOCKED ArtBas THEN
      DO:
        IF ArtBas.ModellFarge = 0 THEN
        DO:
          ASSIGN
            ArtBas.BrukerID = cBrukerId
            ArtBas.EDato    = TODAY 
            ArtBas.ETid    = TIME
            ArtBas.WebButikkArtikkel = NOT ArtBas.WebButikkArtikkel
            ArtBas.PubliserINettbutikk = IF ArtBas.WebButikkArtikkel = FALSE THEN FALSE ELSE ArtBas.PubliserINettbutikk  
            .
          FIND CURRENT ArtBas NO-LOCK NO-WAIT NO-ERROR.
          CREATE ttArtikkel.
          ASSIGN 
            ttArtikkel.ArtikkelNr = ArtBas.ArtikkelNr.
        END.
        ELSE DO:
          FOR EACH bArtBas EXCLUSIVE-LOCK WHERE bArtBas.ModellFarge = ArtBas.ModellFarge AND 
                                 bArtBas.Utgatt      = FALSE AND 
                                 bArtBas.IKasse      = TRUE:
            ASSIGN
              bArtBas.BrukerID = cBrukerId
              bArtBas.EDato    = TODAY 
              bArtBas.ETid    = TIME
              bArtBas.WebButikkArtikkel = NOT bArtBas.WebButikkArtikkel
              bArtBas.PubliserINettbutikk = IF bArtBas.WebButikkArtikkel = FALSE THEN FALSE ELSE bArtBas.PubliserINettbutikk  
              .
            CREATE ttArtikkel.
            ASSIGN 
              ttArtikkel.ArtikkelNr = bArtBas.ArtikkelNr.
          END.
        END.
      END.
    END. /* TOGGLE */ 
  IF AVAILABLE ArtBas THEN RELEASE ArtBas.
  hQuery:GET-NEXT().
END.

EMPTY TEMP-TABLE ttArtikkel.