CURRENT-WINDOW:WIDTH = 550.

FOR EACH KampanjeTilbArtikkel NO-LOCK,
  KampanjeTilbud OF KampanjetilbArtikkel NO-LOCK,
  KampanjeMixMatch OF KampanjeTilbud NO-LOCK,
  KampanjeEier OF KampanjeMixMatch NO-LOCK
  BREAK BY KampanjeEier.KampEierId
        BY KampanjeMixMatch.KampId
        BY KampanjeTilbud.KampTilbId
        BY KampanjeTilbArtikkel.KampTilbArtId:
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
END.
