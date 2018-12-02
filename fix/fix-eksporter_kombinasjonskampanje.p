CURRENT-WINDOW:WIDTH = 550.

DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF STREAM Ut.

ASSIGN
    cFilNavn = 'c:\appdir\mixmatch.csv'.

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

PUT STREAM Ut
    'KampEierId;'
    'KampId;'
    'KampNavn;'
    'KampKlar;'
    'KampTilbId;'
    'KamptilbNavn;'
    'KamptilbTypeId;'
    'KampTilbKvitteringsTekst;'
    'KampTilbBelop;'
    'KampTilbGrenseAntall;'
    'HapHourId;'
    'KampTilbArtSeq;'
    'KampTilbArtId;'
    'KampTilbArtBelop;'
    'KampTilbArtMinAntall;'
    'KampRabattTypeId' 
    SKIP.

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
