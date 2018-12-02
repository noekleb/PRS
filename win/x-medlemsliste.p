/************************************************************
    Program:  x-kundeliste.p
    Created:  TN   8 Jan 00
Description:  Bygger jobblinje for kundeliste.

Last change:  TN   25 Apr 100   11:05 am
************************************************************/

DEF INPUT  PARAMETER wJobbNr as INT  NO-UNDO.
DEF OUTPUT parameter wStatus as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wButNavn     as CHAR  NO-UNDO.
DEF VAR wButTekst    as CHAR  NO-UNDO.
DEF VAR wSumVerdi    as INT   NO-UNDO.
DEF VAR wSumAntall   as INT   NO-UNDO.
DEF VAR wvVareKost   as DEC   NO-UNDO.
DEF VAR wAntLager    as INT   NO-UNDO.
DEF VAR wAntSolgt    as INT   NO-UNDO.
DEF VAR wAntKjop     as INT   NO-UNDO.
DEF VAR wUts%        as INT   NO-UNDO.
DEF VAR wSkipFirst   as LOG   NO-UNDO.
def var wAdrTekst    as CHAR  NO-UNDO.
DEF VAR wLevTekst    as CHAR  NO-UNDO.
DEF VAR wLeverandor  as CHAR  NO-UNDO.
DEF VAR wLevAdr      as CHAR  NO-UNDO.
DEF VAR wAntSort     as INT   NO-UNDO.
DEF VAR wSkriv       as log   NO-UNDO.
DEF VAR wAntPar      as INT   NO-UNDO.
DEF VAR wSortId      as CHAR  NO-UNDO.
DEF VAR wStr2Liste   as CHAR  NO-UNDO.
DEF VAR wFordeling2  as CHAR  NO-UNDO.
DEF VAR wKontakt     as CHAR  NO-UNDO.
DEF VAR wAdresse     as CHAR  NO-UNDO.
DEF VAR wAntJLinjer  as INT   NO-UNDO.
DEF VAR wFilNavn     as CHAR  NO-UNDO.
DEF VAR wCl          as INT   NO-UNDO.
DEF VAR wAntall      as INT   NO-UNDO.
DEF VAR wButSum      as INT   NO-UNDO.
DEF VAR wButSum2     as INT   NO-UNDO.
DEF VAR wStrGrp      as CHAR  NO-UNDO.
DEF VAR wFordeling   as CHAR  NO-UNDO.
DEF VAR w2Fordeling  as CHAR  NO-UNDO.
DEF VAR w2Loop       as INT   NO-UNDO.
DEF VAR wLoop        as INT   NO-UNDO.
DEF VAR wIdx1        as INT   NO-UNDO.
DEF VAR wIdx2        as INT   NO-UNDO.
DEF VAR wLayout      as INT   NO-UNDO.
DEF VAR wSortering   as INT   NO-UNDO.
DEF VAR wBryt1       as CHAR  NO-UNDO.
DEF VAR wBryt2       as CHAR  NO-UNDO.
DEF VAR wBryt3       as CHAR  NO-UNDO.
DEF VAR wAntLinjer   as INT   NO-UNDO.
DEF VAR wTButik      as CHAR  NO-UNDO.
DEF VAR wTTotalt     as CHAR  NO-UNDO.
DEF VAR wTBestilling as CHAR  NO-UNDO.
DEF VAR wTLevert     as CHAR  NO-UNDO.
DEF VAR wTRest       as CHAR  NO-UNDO.
DEF VAR wTAvskrevet  as CHAR  NO-UNDO.
DEF VAR wTKolTot     as CHAR  NO-UNDO.
DEF VAR wRestRecid   as RECID NO-UNDO.
DEF VAR wLopNr       as CHAR  NO-UNDO.
DEF VAR wBTeller     as INT   NO-UNDO.
DEF VAR wN2Liste     as CHAR  NO-UNDO.
DEF VAR wTotAntBest  as INT   NO-UNDO.
DEF VAR wStorrelser  as CHAR  NO-UNDO.
DEF VAR wListe       as CHAR  NO-UNDO.
DEF VAR wWork        as INT   NO-UNDO.
def var wStatListe   as char  no-undo.
DEF VAR wChar2       as CHAR  NO-UNDO.

DEF BUFFER clButiker FOR Butiker.
DEF BUFFER bJobbLinje FOR JobbLinje.

/* Setter sentrallager. */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK where
  clButiker.Butik = wCl NO-ERROR.
if NOT AVAILABLE clButiker then
  RETURN.

{syspara.i 6 200 4 wButTekst}

{runlib.i}

DEF BUFFER levPost FOR Post.

assign
  wStatus = "AVBRYT"
  wButNavn = STRING(clButiker.Butik,"zzzzz9") + " " + clButiker.ButNamn.

/* Henter jobbrecorden. */
FIND Jobb NO-LOCK where
  Jobb.JobbNr = wJobbNr NO-ERROR.
if NOT AVAILABLE Jobb then
  RETURN.

/* Henter lister. Recid ligger i Jobb.Kriterier. */
FIND Lister NO-LOCK where
  RECID(Lister) = INT(ENTRY(1,Jobb.Kriterier)) NO-ERROR.
if NOT AVAILABLE Lister then
  RETURN.

/* Avslutter hvis jobblinje allerede er bygget. */
FIND FIRST JobbLinje NO-LOCK where
  JobbLinje.JobbNr = wJobbNr NO-ERROR.
if AVAILABLE JobbLinje then
  RETURN.

/* Setter valg som gjelder for bygging av listen. */
assign
  wAntJLinjer = 0
  w2Loop      = 0
  wLayout     = INT(ENTRY(2,Jobb.Kriterier))
  wSortering  = INT(ENTRY(3,Jobb.Kriterier)).

BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:

  assign
    w2Loop = w2Loop + 1.
  if valid-handle(wParentHandle) AND w2Loop modulo 5 = 0 then
    RUN FremdriftProgressBar in wParentHandle (INPUT w2Loop).

  FIND Medlem NO-LOCK where
    Medlem.MedlemsNr = int(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE Medlem THEN
    NEXT BYGGJOBB.

  FIND MedlemsType OF Medlem NO-LOCK NO-ERROR.
  FIND MedlemsGruppe OF Medlem NO-LOCK NO-ERROR.
  FIND Post NO-LOCK where
    Post.PostNr = Medlem.PostNr NO-ERROR.

  /* Oppretter jobbrecord. */
  assign
    wSkriv      = true
    wAntJLinjer = wAntJLinjer + 1.
  RUN OpprettJobbLinje.

END. /* BYGGJOBB */

assign
  wStatus = "OK".

/* -------------- Internprocedurer ----------------------- */

PROCEDURE OpprettJobbLinje:

  CREATE JobbLinje.

  assign
    JobbLinje.JobbNr     = wJobbNr
    JobbLinje.Char1      = "LEDIG"
    JobbLinje.Char2      = STRING(Medlem.MedlemsNr,"9999999999999")
    JobbLinje.Char3      = Medlem.Adresse1
    JobbLinje.DivX[ 1]   = STRING(Medlem.MedlemsNr,">>>>>>>>>>>>>")
    JobbLinje.DivX[ 2]   = Medlem.ForNavn + " " + Medlem.EtterNavn
    JobbLinje.DivX[ 5]   = Medlem.Telefon
    JobbLinje.DivX[ 6]   = MEdlem.Telefaks
    JobbLinje.DivX[ 7]   = ""
    JobbLinje.DivX[ 8]   = Medlem.Adresse1
    JobbLinje.DivX[ 9]   = Medlem.ForNavn + " " + Medlem.EtterNavn
    JobbLinje.DivX[10]   = Medlem.Land
    JobbLinje.DivX[11]   = Medlem.ForNavn + " " + Medlem.EtterNavn
    JobbLinje.DivX[14]   = (IF AVAILABLE Post
                              THEN Post.Beskrivelse
                              ELSE "")
    JobbLinje.DivX[15]   = Medlem.PostNr
    JobbLinje.DivX[17]   = Medlem.Telefon
    JobbLinje.DivX[24]   = STRING(Medlem.MedType)
    JobbLinje.DivX[25]   = STRING(Medlem.MedGruppe)
    JobbLinje.DivX[26]   = if AVAILABLE Post THEN Post.Beskrivelse ELSE ""

    /* Sortering og brytgrupper */
    JobbLinje.DivX[40] = if wSortering = 1 THEN JobbLinje.Char1
                         ELSE if wSortering = 2 THEN JobbLinje.Char1
                         ELSE if wSortering = 3 THEN JobbLinje.Char1
                         ELSE ""
    JobbLinje.DivX[41] = if wSortering = 1 THEN JobbLinje.Char2
                         ELSE if wSortering = 2 THEN Medlem.ForNavn + " " + Medlem.EtterNavn
                         ELSE if wSortering = 3 THEN MEdlem.PostNr
                         ELSE ""
    JobbLinje.DivX[42] = if wSortering = 1 THEN ""
                         ELSE if wSortering = 2 THEN ""
                         ELSE if wSortering = 3 THEN Medlem.ForNavn + " " + Medlem.EtterNavn
                         ELSE ""
    JobbLinje.DivX[43] = if wSortering = 1 THEN ""
                         ELSE if wSortering = 2 THEN ""
                         ELSE if wSortering = 3 THEN ""
                         ELSE ""
    JobbLinje.DivX[44] = if wSortering = 1 THEN ""
                         ELSE if wSortering = 2 THEN ""
                         ELSE if wSortering = 3 THEN ""
                         ELSE "".
END PROCEDURE.

