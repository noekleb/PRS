DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFilLik AS CHAR NO-UNDO.
DEF VAR cUtFilULik AS CHAR NO-UNDO.

DEF VAR dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEF VAR lWebButikkArtikkel LIKE ArtBAs.WebButikkArtikkel NO-UNDO.
DEF VAR cKode LIKE Strekkode.Kode NO-UNDO.
DEF VAR cLevKod LIKE ArtBas.LevKod NO-UNDO.
DEF VAR cBeskr  LIKE ArtBas.Beskr NO-UNDO.
DEF VAR cLevFargKod LIKE ArtBas.LEvFargKod NO-UNDO.
DEF VAR iStrKode LIKE Strekkode.StrKode NO-UNDO.
DEF VAR cStorl LIKE ArtLag.Storl NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DEF STREAM Inn.
DEF STREAM UtUlik.
DEF STREAM UtLik.

ASSIGN
    cInnFil    = 'Q:\Appdir\SE\AntonKonv\Anton_Artikkler.csv'
    cUtFilLik  = 'Q:\Appdir\SE\AntonKonv\Anton_Artikkler_sport1_Lik.csv'
    cUtFilULik = 'Q:\Appdir\SE\AntonKonv\Anton_Artikkler_sport1_Ulik.csv'
    .


INPUT STREAM Inn FROM VALUE(cInnFil).
OUTPUT STREAM UtLik TO VALUE(cUtFilLik).
OUTPUT STREAM UtULik TO VALUE(cUtFilULik).

PUT STREAM UtLik UNFORMATTED
    'dArtikkelNr;'
    'lWebButikkArtikkel;'
    'cKode;'
    'cLevKod;'
    'cBeskr;'
    'cLevFargKod;'
    'Ulik;'
    'Strekkode.ArtikkelNr;'
    'ArtBas.LevKod;'
    'ArtBas.Beskr;'
    'ArtBas.LevFargKod'
    SKIP.
PUT STREAM UtULik UNFORMATTED
    'dArtikkelNr;'
    'lWebButikkArtikkel;'
    'cKode;'
    'cLevKod;'
    'cBeskr;'
    'cLevFargKod;'
    'Ulik;'
    'Strekkode.ArtikkelNr;'
    'ArtBas.LevKod;'
    'ArtBas.Beskr;'
    'ArtBas.LevFargKod'
    SKIP.

REPEAT:

  piLoop = piLoop + 1.
  IF piLoop MODULO 1000 = 0 THEN
  DO:
      PAUSE 0.
      DISPLAY piLoop WITH FRAME GG.
  END.
  IMPORT STREAM Inn DELIMITER ';' 
      dArtikkelNr 
      lWebButikkArtikkel
      cKode 
      cLevKod 
      cBeskr  
      cLevFargKod 
      iStrKode
      cStorl
      NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
      NEXT.

  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cKode NO-ERROR.
  IF AVAILABLE Strekkode THEN
      FIND ArtBas OF Strekkode NO-LOCK  NO-ERROR.
  IF AVAILABLE Strekkode THEN
  DO:
      /*
      DISPLAY 
          dArtikkelNr
          lWebButikkArtikkel
          cKode
          cLevKod
          cBeskr
          cLevFargKod
          'Ulik' WHEN dArtikkelNr <> Strekkode.ArtikkelNr
          Strekkode.ArtikkelNr
          ArtBas.LevKod
          ArtBas.Beskr
          ArtBas.LevFargKod
          WITH WIDTH 250.
      */
      IF dArtikkelNr = Strekkode.ArtikkelNr THEN
      PUT STREAM UtLik UNFORMATTED
          dArtikkelNr ';'
          lWebButikkArtikkel ';'
          cKode ';'
          cLevKod ';'
          cBeskr ';'
          cLevFargKod ';'
          (IF dArtikkelNr <> Strekkode.ArtikkelNr THEN 'Ulik' ELSE '') ';'
          Strekkode.ArtikkelNr ';'
          ArtBas.LevKod ';'
          ArtBas.Beskr ';'
          ArtBas.LevFargKod
          SKIP.

      IF dArtikkelNr <> Strekkode.ArtikkelNr THEN
      PUT STREAM UtULik UNFORMATTED
          dArtikkelNr ';'
          lWebButikkArtikkel ';'
          cKode ';'
          cLevKod ';'
          cBeskr ';'
          cLevFargKod ';'
          (IF dArtikkelNr <> Strekkode.ArtikkelNr THEN 'Ulik' ELSE '') ';'
          Strekkode.ArtikkelNr ';'
          ArtBas.LevKod ';'
          ArtBas.Beskr ';'
          ArtBas.LevFargKod
          SKIP.
  END.
END.

OUTPUT STREAM UtULik CLOSE.
OUTPUT STREAM UtLik CLOSE.
INPUT STREAM Inn CLOSE.

