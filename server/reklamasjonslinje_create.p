DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iReklamasjonsnr  AS INT NO-UNDO.
DEF VAR iLinjeNr         AS INT NO-UNDO.
DEF VAR fArtikkelNr      AS DEC NO-UNDO.

DEF BUFFER bReklamasjonslinje FOR reklamasjonslinje.

ASSIGN
  iReklamasjonsNr = INT(ENTRY(LOOKUP('ReklamasjonsNr',icFields),icValues,'|'))
  fArtikkelNr     = DEC(ENTRY(LOOKUP('ArtikkelNr',icFields),icValues,'|'))
.

FIND LAST bReklamasjonslinje NO-LOCK
     WHERE bReklamasjonslinje.Reklamasjonsnr = iReklamasjonsnr NO-ERROR.

FIND ArtBas WHERE ArtBas.ArtikkelNr = fArtikkelNr NO-LOCK NO-ERROR.

IF AVAIL bReklamasjonslinje THEN
  iLinjeNr = bReklamasjonslinje.LinjeNr + 1.
ELSE iLinjeNr = 1.

ASSIGN
  hBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE = iLinjeNr
  .
FIND Reklamasjonslogg WHERE Reklamasjonslogg.ReklamasjonsNr = iReklamasjonsNr EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL ArtBas AND AVAIL Reklamasjonslogg THEN
DO:
    IF Reklamasjonslogg.levnr = 0 THEN
        ReklamasjonsLogg.levnr = ArtBas.LevNr.
    IF Reklamasjonslogg.ArtikkelNr = 0 THEN
        ASSIGN
        Reklamasjonslogg.ArtikkelNr = ArtBas.ArtikkelNr
        Reklamasjonslogg.Beskr      = ArtBas.Beskr
        Reklamasjonslogg.LevKod     = ArtBas.LevKod
        .
END.
