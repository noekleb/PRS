DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

DEF VAR iCl         AS INT  NO-UNDO.
DEF VAR fKalkPris   AS DEC  NO-UNDO.
DEF VAR fVgRabatt   AS DEC  NO-UNDO.
DEF VAR cValue      AS CHAR NO-UNDO.
DEF VAR fArtikkelNr AS DEC  NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK 
     WHERE Butiker.Butik = iCl NO-ERROR.
FIND KOrdreLinje NO-LOCK
     WHERE ROWID(KOrdreLinje) = irKOrdreLinje
     NO-ERROR.
FIND FIRST KOrdreHode OF KOrdreLinje NO-LOCK NO-ERROR.

IF AVAILABLE Butiker AND AVAIL KOrdreLinje AND AVAIL KOrdreHode THEN DO:
  fArtikkelNr = DEC(KOrdreLinje.Varenr) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  FOR FIRST ArtBas FIELDS(ArtikkelNr)
      WHERE ArtBas.ArtikkelNr = fArtikkelNr
      NO-LOCK:
    FIND FIRST ArtPris NO-LOCK 
         WHERE ArtPris.ArtikkelNr = fArtikkelNr 
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN DO:
      fKalkPris = ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1].
      RUN art_vgrabatt%.p(ROWID(ArtBas),STRING(KordreHode.KundeNr),icSessionId,OUTPUT cValue).
      fVgRabatt = DEC(cValue).

      ocValue = STRING(MIN(fKalkPris,ArtPris.pris[1] * (100 - fVgRabatt) / 100)).
    END.
  END.
END.

