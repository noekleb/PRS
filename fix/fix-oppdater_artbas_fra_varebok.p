/* Flytter artikkelinfo fra varebok til ArtBas. */

DEF VAR cNrListe AS CHAR NO-UNDO.
DEF VAR piLoop   AS INT  NO-UNDO.
DEF VAR bOK        AS LOG    NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR cYY        AS CHAR   NO-UNDO.
DEF VAR cWW        AS CHAR   NO-UNDO.
DEF VAR oiWeek     AS INT    NO-UNDO.
DEF VAR dFirst     AS DATE   NO-UNDO.

ASSIGN
    cNrListe = "90000073,90000075"
    .

CURRENT-WINDOW:WIDTH = 200.

VAREBOKLOOP:
DO piLoop = 1 TO NUM-ENTRIES(cNrListe):
  VAREBOK:
  FOR EACH VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = dec(ENTRY(piLoop,cNrListe)):
    DISPLAY
        VareBokHode.VareBokNr
        WITH WIDTH 200.
    FOR EACH VareBokLinje OF VareBokHode NO-LOCK:
      FIND ArtBas of VareBokLinje EXCLUSIVE-LOCK NO-ERROR.

      /*
      DISPLAY
          ArtBas.ArtikkelNr WHEN AVAILABLE ArtBas
          VareBokLinje.ArtikkelNr
          WITH WIDTH 200.
      */
      /* Flytter informasjonen. */
      IF AVAILABLE ArtBas THEN
      ASSIGN
          /*ArtBas.FrittTillegg*/
          ArtBas.KjedeVare         = VareBokLinje.Kjedevare
          ArtBas.Gjennomfaktureres = VareBokLinje.Gjennomfaktureres
          ArtBas.LevFargKod        = VareBokLinje.LevFargKod
          ArtBas.Beskr             = VareBokLinje.Beskr
          ArtBas.Katalogpris       = VareBokLinje.Innkjopspris
          ArtBas.Anbefaltpris      = VareBokLinje.AnbefaltPris
          ArtBas.KjedeInnkPris     = VareBokLinje.KjedeInnkPris
          ArtBas.KjedeRab%         = VareBokLinje.KjedeRab%
          ArtBas.supRab%           = VareBokLinje.supRab%
          ArtBas.forhRab%          = VareBokLinje.forhRab%
          ArtBas.LevDato1          = VareBokLinje.LevDato1
          ArtBas.LevDato2          = VareBokLinje.LevDato2
          ArtBas.LevDato3          = VareBokLinje.LevDato3 
          ArtBas.LEvDato4          = VareBokLinje.LevDato4
          .
    END.

  END. /* VAREBOK */
END. /* VAREBOKLOOP */


