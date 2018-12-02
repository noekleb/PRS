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
    cNrListe = "90000022,90000023"
    .

CURRENT-WINDOW:WIDTH = 200.

OUTPUT TO VALUE('Strekkode_err.txt').
PUT UNFORMATTED 
  "Kontroll av messeordre - strekkode" skip.
OUTPUT CLOSE.


MESSEORDRELOOP:
DO piLoop = 1 TO NUM-ENTRIES(cNrListe):
  MESSEORDRE:
  FOR EACH VareBehLinjeTrans WHERE
      VareBehLinjeTrans.VareBehNr = dec(ENTRY(piLoop,cNrListe)):

      FIND Strekkode WHERE
          Strekkode.Kode = VareBehLinjeTrans.Kode NO-ERROR.
      IF NOT AVAILABLE Strekkode THEN
      DO:
          FIND ArtBas WHERE
              ArtBas.ArtikkelNr = VareBehLinjetrans.ArtikkelNr NO-ERROR.
          OUTPUT TO VALUE('Strekkode_err.txt') append.
          PUT UNFORMATTED 
            "Ukjent strekkode;"
            VarebehLinjeTrans.VareBehNr ";"
            VareBehLinjeTrans.ButikkNr ";"
            (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE ' ') ";" 
            VareBehLinjeTrans.StrKode ";"
            VareBehLinjeTrans.Kode
            skip
            .
          OUTPUT CLOSE.
      END.
      ELSE IF Strekkode.ArtikkelNr <> VAreBehLinjetrans.ArtikkelNr /*OR
              Strekkode.StrKode    <> VareBehLinjeTrans.StrKode*/ THEN
      DO:
          FIND ArtBas WHERE
              ArtBas.ArtikkelNr = VareBehLinjetrans.ArtikkelNr NO-ERROR.
          OUTPUT TO VALUE('Strekkode_err.txt') append.
          PUT UNFORMATTED 
            "Diff i Info;"
            VarebehLinjeTrans.VareBehNr ";"
            VareBehLinjeTrans.ButikkNr ";"
            Strekkode.ArtikkelNr ";"
            (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE ' ') ";" 
            Strekkode.StrKode ";"
            VareBehLinjeTrans.StrKode ";"
            Strekkode.Kode ";"
            VareBehLinjeTrans.Kode
            skip
            .
          OUTPUT CLOSE.
      END.

  END. /* MESSEORDRE */
END. /* MESSEORDRELOOP */


