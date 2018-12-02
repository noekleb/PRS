DEF INPUT  PARAM irBuffer    AS ROWID NO-UNDO.
DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

DEF VAR iCl       AS INT NO-UNDO.
DEF VAR bKunTilb  AS LOG NO-UNDO.
DEF VAR iButikkNr AS INT NO-UNDO.

iButikkNr = INTEGER(ENTRY(1,icParam,"¤")).
IF NUM-ENTRIES(icParam,"¤") > 1 THEN
  bKunTilb  = LOGICAL(ENTRY(2,icParam,"¤")).

FIND Butiker NO-LOCK WHERE
     Butiker.Butik = iButikkNr NO-ERROR.

IF AVAILABLE Butiker THEN
  FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
      WHERE ROWID(ArtBas) = irBuffer:
   
    FIND FIRST ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocValue = IF bKunTilb AND NOT ArtPris.Tilbud THEN "skiprow" ELSE STRING(Artpris.Tilbud).
  END.

IF ocValue = "" THEN DO:
  {syspara.i 5 1 1 iCl INT}.
  FIND Butiker NO-LOCK WHERE
       Butiker.Butik = iCl NO-ERROR.

  IF AVAILABLE Butiker THEN
    FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
        WHERE ROWID(ArtBas) = irBuffer:

      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
        ocValue = IF bKunTilb AND NOT ArtPris.Tilbud THEN "skiprow" ELSE STRING(Artpris.Tilbud).
    END.
END.
