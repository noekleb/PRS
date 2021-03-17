CURRENT-WINDOW:WIDTH = 550.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje   AS CHAR NO-UNDO.
def var cKampEierId               AS CHAR NO-UNDO.
def var cKampId                   AS CHAR NO-UNDO.
def var cKampNavn                 AS CHAR NO-UNDO.
def var cKampKlar                 AS CHAR NO-UNDO.
def var cKampTilbId               AS CHAR NO-UNDO.
def var cKamptilbNavn             AS CHAR NO-UNDO.
def var cKamptilbTypeId           AS CHAR NO-UNDO.
def var cKampTilbKvitteringsTekst AS CHAR NO-UNDO.
def var cKampTilbBelop            AS CHAR NO-UNDO.
def var cKampTilbGrenseAntall     AS CHAR NO-UNDO.
def var cHapHourId                AS CHAR NO-UNDO.
DEF VAR cKamptilbGrenseAntallBruk AS CHAR NO-UNDO.
def var cKampTilbArtSeq           AS CHAR NO-UNDO.
def var cKampTilbArtId            AS CHAR NO-UNDO.
def var cKampTilbArtBelop         AS CHAR NO-UNDO.
def var cKampTilbArtMinAntall     AS CHAR NO-UNDO.
def var cKampRabattTypeId         AS CHAR NO-UNDO. 
DEF VAR cEAN                      AS CHAR NO-UNDO.
DEF VAR lArtikkelNr               AS DEC NO-UNDO.
DEF VAR lKampTilbArtSeq AS DEC NO-UNDO.
DEF VAR iDummy AS INT NO-UNDO.


DEF STREAM Inn.

ASSIGN
    cFilNavn = 'c:\home\lindbak\ankommet\Mixkonvertering.csv'.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

LOOPEN:
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.
  ASSIGN
      iDummy = INT(ENTRY(1,cLinje,';'))
      cKampEierId               = ENTRY(1,cLinje,';')              
      cKampId                   = ENTRY(2,cLinje,';')              
      cKampNavn                 = ENTRY(3,cLinje,';')
      cKampKlar                 = ENTRY(4,cLinje,';')
      cKampTilbId               = ENTRY(5,cLinje,';')
      cKamptilbNavn             = ENTRY(6,cLinje,';')
      cKamptilbTypeId           = ENTRY(7,cLinje,';')
      cKampTilbKvitteringsTekst = ENTRY(8,cLinje,';')
      cKampTilbBelop            = ENTRY(9,cLinje,';')
      cKampTilbGrenseAntall     = ENTRY(10,cLinje,';')
      cHapHourId                = ENTRY(11,cLinje,';')
      cKamptilbGrenseAntallBruk = ENTRY(12,cLinje,';')
      cKampTilbArtSeq           = ENTRY(13,cLinje,';')
      cKampTilbArtId            = ENTRY(14,cLinje,';')
      cKampTilbArtBelop         = ENTRY(15,cLinje,';')
      cKampTilbArtMinAntall     = ENTRY(16,cLinje,';')
      cKampRabattTypeId         = ENTRY(17,cLinje,';')
      cEAN = cKampTilbArtId
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      NEXT LOOPEN.

  RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cEAN NO-ERROR.
  IF AVAILABLE Strekkode THEN
      lArtikkelNr = Strekkode.ArtikkelNr.
  ELSE NEXT LOOPEN.

  DISPLAY cLinje FORMAT "x(40)".


  /* Oppretter KampanjeMixMatch. */
  FIND KampanjeMixMatch NO-LOCK WHERE
      KampanjeMixMatch.KampId = DEC(cKampId) NO-ERROR.
  IF NOT AVAILABLE KampanjeMixMatch THEN
  DO TRANSACTION:
      CREATE KampanjeMixMatch.
      ASSIGN
          KampanjeMixMatch.KampId   = DEC(cKampId)
          KampanjeMixMatch.KampNavn = cKampNavn
          KampanjeMixMatch.KampKlar = (IF cKampKlar = 'yes' THEN TRUE ELSE FALSE)
          .
      FIND CURRENT KampanjeMixMatch NO-LOCK.
  END. /* TRANSACTION */

  FIND KampanjeTilbud NO-LOCK WHERE
      KampanjeTilbud.KampId     = KampanjeMixMatch.KampId AND
      KampanjeTilbud.KampTilbId = INT(cKampTilbId) NO-ERROR.
  IF NOT AVAILABLE KampanjeTilbud THEN
  DO TRANSACTION:
      CREATE KampanjeTilbud.
      ASSIGN
          KampanjeTilbud.KampId                   = KampanjeMixMatch.KampId 
          KampanjeTilbud.KampTilbId               = INT(cKampTilbId)
          KampanjeTilbud.KamptilbNavn             = cKamptilbNavn                         
          KampanjeTilbud.KamptilbTypeId           = INT(cKamptilbTypeId)                      
          KampanjeTilbud.KampTilbKvitteringsTekst = cKampTilbKvitteringsTekst 
          KampanjeTilbud.KampTilbBelop            = DEC(cKampTilbBelop)                       
          KampanjeTilbud.KampTilbGrenseAntall     = DEC(cKampTilbGrenseAntall)         
          KampanjeTilbud.HapHourId                = INT(cHapHourId)                               
          KampanjeTilbud.KamptilbGrenseAntallBruk = (IF cKamptilbGrenseAntallBruk = 'yes' THEN TRUE ELSE FALSE) 
          .

      FIND CURRENT KampanjeTilbud NO-LOCK.
  END. /* TRANSACTION */

  FIND FIRST KampanjeTilbArtikkel NO-LOCK WHERE
      KampanjeTilbArtikkel.KampId        = KampanjeMixMatch.KampId AND
      KampanjeTilbArtikkel.KampTilbId    = KampanjeTilbud.KampTilbId AND
      KampanjeTilbArtikkel.KampTilbArtId = lArtikkelNr NO-ERROR.
  IF NOT AVAILABLE KampanjeTilbArtikkel THEN
  DO TRANSACTION:
      FIND LAST KampanjeTilbArtikkel NO-LOCK USE-INDEX KampTilbArtSeq NO-ERROR.
      IF AVAILABLE KampanjeTilbArtikkel THEN
          lKampTilbArtSeq = KampanjeTilbArtikkel.KampTilbArtSeq + 1.
      ELSE 
          lKampTilbArtSeq = 1.

      CREATE KampanjeTilbArtikkel.
      ASSIGN
          KampanjeTilbArtikkel.KampId               = KampanjeMixMatch.KampId 
          KampanjeTilbArtikkel.KampTilbId           = KampanjeTilbud.KampTilbId 
          KampanjeTilbArtikkel.KampTilbArtId        = lArtikkelNr
          KampanjeTilbArtikkel.KampTilbArtSeq       = lKampTilbArtSeq           
          KampanjeTilbArtikkel.KampTilbArtBelop     = DEC(cKampTilbArtBelop)         
          KampanjeTilbArtikkel.KampTilbArtMinAntall = DEC(cKampTilbArtMinAntall)     
          KampanjeTilbArtikkel.KampRabattTypeId     = DEC(cKampRabattTypeId)         
          .

      FIND CURRENT KampanjeTilbArtikkel NO-LOCK.
  END. /* TRANSACTION */

  /* ButikkKobling */
  FOR EACH Butiker NO-LOCK WHERE 
      Butiker.harButikksystem = TRUE AND
      Butiker.ApningsDato <> ? AND 
      Butiker.ApningsDato >= TODAY AND
      (Butiker.NedlagtDato = ? OR Butiker.NedlagtDato < TODAY):

      FIND KampanjeButikker NO-LOCK WHERE
          KampanjeButikker.KampId = KampanjeMixMatch.KampId AND
          KampanjeButikker.Butik  = Butiker.Butik NO-ERROR.
      IF NOT AVAILABLE KampanjeButikker THEN
      DO TRANSACTION:
          CREATE KampanjeButikker.
          ASSIGN
              KampanjeButikker.KampId = KampanjeMixMatch.KampId
              KampanjeButikker.Butik  = Butiker.Butik
              .
          FIND CURRENT KampanjeButikker NO-LOCK.
      END. /* TRANSACTION */
  END.
END. /* LOOPEN */


INPUT STREAM Inn CLOSE.

/*
FOR EACH KampanjeTilbArtikkel NO-LOCK,
  KampanjeTilbud OF KampanjetilbArtikkel NO-LOCK,
  KampanjeMixMatch OF KampanjeTilbud NO-LOCK,
  KampanjeEier OF KampanjeMixMatch NO-LOCK
  BREAK BY KampanjeEier.KampEierId
        BY KampanjeMixMatch.KampId
        BY KampanjeTilbud.KampTilbId
        BY KampanjeTilbArtikkel.KampTilbArtId:
  PUT STREAM Ut
      KampanjeEier.KampEierId ';'
      KampanjeMixMatch.KampId  ';'
      KampanjeMixMatch.KampNavn ';'
      KampanjeMixMatch.KampKlar ';'
      Kampanjetilbud.KampTilbId ';'
      KampanjeTilbud.KamptilbNavn ';'
      KampanjeTilbud.KamptilbTypeId ';'
      Kampanjetilbud.KampTilbKvitteringsTekst ';'
      KampanjeTilbud.KampTilbBelop ';'
      KampanjeTilbud.KampTilbGrenseAntall ';'
      KampanjeTilbud.HapHourId ';'
      KampanjeTilbArtikkel.KampTilbArtSeq ';'
      KampanjeTilbArtikkel.KampTilbArtId ';'
      KampanjeTilbArtikkel.KampTilbArtBelop ';'
      KampanjeTilbArtikkel.KampTilbArtMinAntall ';'
      KampanjeTilbArtikkel.KampRabattTypeId
      SKIP.

  /*
  DISPLAY
    KampanjeEier.KampEierId
    /* KampanjeMixMatch */
    KampanjeMixMatch.KampId
    KampanjeMixMatch.KampNavn
    KampanjeMixMatch.KampKlar
    /* Kampanjetilbud */
    Kampanjetilbud.KampTilbId
    KampanjeTilbud.KamptilbNavn
    KampanjeTilbud.KamptilbTypeId
    Kampanjetilbud.KampTilbKvitteringsTekst
    KampanjeTilbud.KampTilbBelop
    KampanjeTilbud.KampTilbGrenseAntall
    KampanjeTilbud.HapHourId
    /* KampanjeTilbArtikkel */
    KampanjeTilbArtikkel.KampTilbArtSeq FORMAT "->>>>>>>>>>>>9"
    KampanjeTilbArtikkel.KampTilbArtId
    KampanjeTilbArtikkel.KampTilbArtBelop
    KampanjeTilbArtikkel.KampTilbArtMinAntall
    KampanjeTilbArtikkel.KampRabattTypeId
    WITH WIDTH 550.
    */
END.
OUTPUT STREAM Ut CLOSE.
*/
