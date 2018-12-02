/************************************************************
    Program:  skriv-tick-row.p
    Created:  TN   21 Dec 99
Description:

Last change:  TN   28 Dec 99    2:31 am
************************************************************/

{plukkliste.i}
{runlib.i}

DEF VAR wGrpListe as CHAR NO-UNDO.
DEF VAR wLoop     as INT  NO-UNDO.
DEF VAR wUtEnhet  as CHAR NO-UNDO.
DEF VAR wKommando as CHAR NO-UNDO.
DEF VAR wFilNavn  as CHAR NO-UNDO.

form
  PlukkListe.LopNr FORMAT "zzz9"  COLUMN-LABEL "LpNr"
  PlukkListe.Vg      COLUMN-LABEL "Vg"
  PlukkListe.Storl   COLUMN-LABEL "Str"
  PlukkListe.Ant     COLUMN-LABEL "Ant"
  PlukkListe.RabKr   COLUMN-LABEL "Rabatt"
  PlukkListe.Pris    COLUMN-LABEL "Pris"
WITH FRAME PlukkListe DOWN stream-io.

DEF STREAM Skriv.

{syspara.i 200 1 101 wUtEnhet}
{syspara.i 200 1 103 wKommando}
{syspara.i 200 1 104 wGrpListe}

IF NOT CAN-FIND(FIRST PlukkListe) then
  RETURN.

IF VALID-HANDLE(wLibHandle) then
  RUN GetTempFileName IN wLibHandle ("Plukk","txt",output wFilNavn).

OUTPUT STREAM Skriv to VALUE(wFilNavn).

/* Liste heading. */
display STREAM Skriv
  "P L U K K L I S T E" skip
  TODAY  STRING(TIME,"HH:MM:SS") skip
WITH FRAME Tid NO-LABELS stream-io.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK:

HGLOOP:
do wLoop = 1 To NUM-ENTRIES(wGrpListe):

LESPOSTER:
FOR EACH PlukkListe no-lock where
  PlukkListe.Butik = Butiker.Butik and
  PlukkListe.Hg    = INT(ENTRY(wLoop,wGrpListe))
  BREAK by PlukkListe.Butik
        by PlukkListe.Hg
        /* by PlukkListe.Vg */
        by PlukkListe.LopNr
        by PlukkListe.Storl:

  /* Legger ut butikknummer og navn p† ny butikk. */
  IF FIRST-OF(PlukkListe.Butik) then
    DO:
      DISPLAY STREAM Skriv
        STRING(PlukkListe.Butik) FORMAT "xxxx" @ Plukkliste.Butik
        Butiker.ButNamn WHEN AVAILABLE Butiker
      WITH FRAME Butikk no-labels stream-io.
    END.

  /* Legger ut hovedgruppenummer og navn. */
  IF FIRST-OF(PlukkListe.Hg) then
    DO:
      FIND HuvGr NO-LOCK where
        HuvGr.Hg = PlukkListe.Hg NO-ERROR.
      display STREAM Skriv
        STRING(PlukkListe.Hg) @ Plukkliste.HG
        HuvGr.HgBeskr WHEN AVAILABLE HuvGr
      WITH FRAME HuvGr no-labels stream-io.
    END.

  /* Viser plukklistelinjene. */
  DISPLAY STREAM Skriv
    PlukkListe.LopNr FORMAT ">>zzz9"
    PlukkListe.Vg
    PlukkListe.Storl
    PlukkListe.Antall
    PlukkListe.RabKr
    PlukkListe.Pris
  WITH FRAME PlukkListe.
  DOWN STREAM Skriv 1 WITH FRAME PlukkListe.

END. /* LESPOSTER */
END. /* HGLOOP */
END. /* BUTIKKLOOP */

DOWN STREAM Skriv 7 WITH FRAME PlukkListe.
OUTPUT STREAM Skriv CLOSE.

/*MESSAGE wKommando wFilNavn wUtEnhet VIEW-AS ALERT-BOX.*/

/* Sender listen til utskriftsenheten. */
IF SEARCH(wFilNavn) <> ? then
  DO:
    os-command silent VALUE(wKommando) VALUE(wFilNavn) VALUE(wUtEnhet).
    os-delete VALUE (wFilNavn).
  END.


