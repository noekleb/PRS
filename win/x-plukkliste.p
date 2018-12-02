/************************************************************
    Program:  x-plukkliste.p
    Created:  TN   19 Feb 99
Description:  Bygger jobblinje for plukkliste.

Last change:  TN    2 Dec 99   10:49 am
************************************************************/

DEF INPUT  PARAMETER wJobbNr       as INT    NO-UNDO.
DEF OUTPUT parameter wStatus       as CHAR   NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wTekst    as CHAR NO-UNDO.
DEF VAR wFilNavn  as CHAR NO-UNDO.
DEF VAR wCl       as INT  NO-UNDO.
DEF VAR wAntall   as INT  NO-UNDO.
DEF VAR wIdx      as int  NO-UNDO.
DEF VAR wButSum1  as INT  NO-UNDO.
DEF VAR wButSum2  as INT  NO-UNDO.
def var wAntTrans as int  no-undo.
DEF VAR wButListe as CHAR NO-UNDO.
DEF VAR wDato1    as DATE NO-UNDO.
DEF VAR wDato2    as DATE NO-UNDO.
DEF VAR wVg1      as INT  NO-UNDO.
DEF VAR wVg2      as INT  NO-UNDO.
DEF VAR wLopNr1   as INT  NO-UNDO.
DEF VAR wLopNr2   as INT  NO-UNDO.
DEF VAR wButik1   as INT  NO-UNDO.
DEF VAR wButik2   as INT  NO-UNDO.
DEF VAR wT-PrDag  as LOG  NO-UNDO.
DEF VAR wT-Alle   as LOG  NO-UNDO.
DEF VAR wT-Lager  AS LOG  NO-UNDO.
DEF VAR wFI-Info  as CHAR NO-UNDO.
DEFINE VARIABLE cPlukkbutikk AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAltCL AS INTEGER     NO-UNDO.
DEF BUFFER bArtLag FOR ArtLag.

{syspara.i 5 1 1 wCl INT}
IF iAltCL > 0 THEN
    wCl = iAltCL.
FIND Butiker NO-LOCK where
  Butiker.Butik = wCl NO-ERROR.
if NOT AVAILABLE Butiker then
  RETURN.

assign
  wStatus = "AVBRYT".

/* Henter jobbrecorden. */
FIND Jobb NO-LOCK where
  Jobb.JobbNr = wJobbNr NO-ERROR.
if NOT AVAILABLE Jobb then
  RETURN.

assign
  wButik1  = INT(ENTRY(1,Jobb.Kriterier))
  wButik2  = INT(ENTRY(1,ENTRY(2,Jobb.Kriterier),"|")) /* lagt in ett entry med plukkbutikk */
  cPlukkbutikk = ENTRY(2,Jobb.Kriterier)                /* behandlas längre ner */
  wTekst   = ENTRY(3,Jobb.Kriterier)
  wDato1   = DATE(
                  INT(ENTRY(1,wTekst,"|")),
                  INT(ENTRY(2,wTekst,"|")),
                  INT(ENTRY(3,wTekst,"|"))
                 )
  wTekst   = ENTRY(4,Jobb.Kriterier)
  wDato2   = DATE(
                  INT(ENTRY(1,wTekst,"|")),
                  INT(ENTRY(2,wTekst,"|")),
                  INT(ENTRY(3,wTekst,"|"))
                 )
  wVg1     = INT(ENTRY(5,Jobb.Kriterier))
  wVg2     = INT(ENTRY(6,Jobb.Kriterier))
  wLopNr1  = INT(ENTRY(7,Jobb.Kriterier))
  wLopNr2  = INT(ENTRY(8,Jobb.Kriterier))
  wT-PrDag = if ENTRY(9,Jobb.Kriterier) = "TRUE"
               THEN true
               ELSE FALSE
  wT-Alle  = if ENTRY(10,Jobb.Kriterier) = "TRUE"
               THEN true
               ELSE FALSE
  wT-Lager = if ENTRY(11,Jobb.Kriterier) = "TRUE"
               THEN true
               ELSE FALSE
  .
  IF NUM-ENTRIES(cPlukkbutikk,"|") = 2 THEN DO:
      cPlukkbutikk = ENTRY(2,cPlukkbutikk,"|").
      IF NOT cPlukkbutikk = "0" THEN
          wCL = INT(cPlukkbutikk).
  END.
/*
/* Avslutter hvis jobblinje allerede er bygget. */
FIND FIRST JobbLinje NO-LOCK where
  JobbLinje.JobbNr = wJobbNr NO-ERROR.
if AVAILABLE JobbLinje then
  RETURN.
*/  

/* Bygger liste over butikker. */
assign
  wButListe = ""
  wAntall   = 0.
BUTIKKLISTE:
FOR EACH Butiker NO-LOCK where
  Butiker.Butik >= wButik1 and
  Butiker.Butik <= wButik2:

  /* Maks antall butikker som kan spesifiseres. */
  wAntall = wAntall + 1.
  if wAntall > 3 then
    LEAVE BUTIKKLISTE.

  wButListe = wButListe +
              (if wButListe = ""
                 THEN ""
                 ELSE ",") +
              STRING(Butiker.Butik).
END. /* BUTIKKLISTE */

wAntTrans = 0.

BUTIKK:
FOR EACH Butiker NO-LOCK where
  Butiker.Butik >= wButik1 and
  Butiker.Butik <= wButik2:

  ASSIGN
    wIdx = if LOOKUP(STRING(Butiker.Butik),wButListe) <> 0
             then LOOKUP(STRING(Butiker.Butik),wButListe)
             else 50.

  TRANSLOGG:
  FOR EACH TransLogg WHERE
    TransLogg.Plukket = false and
    TransLogg.Butik   = Butiker.Butik and
    TransLogg.Vg     >= wVg1 and
    TransLogg.Vg     <= wVg2 and
    TransLogg.LopNr  >= wLopNr1 and
    TransLogg.LopNr  <= wLopNr2 and
    TransLogg.Dato   >= wDato1 and
    TransLogg.Dato   <= wDato2
    TRANSACTION:
    
    wAntTrans = wAntTrans + 1.
    wFI-Info = "Behandler translogg " + string(wAntTrans).

    if wAntTrans modulo 25 = 0 AND VALID-HANDLE(wParentHandle) then
      run VisInfo in wParentHandle (wFI-Info) no-error.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    
    /* Lager på sentrallager skal logges på artikkelen. */
    FIND ArtLag NO-LOCK where
      ArtLag.Butik = wCl and
      ArtLag.artikkelnr = TransLogg.artikkelnr and
      ArtLag.Storl = TransLogg.Storl NO-ERROR.

    /* Lager på butikk/størelse skal logges på artikkelen. */
    IF wT-Lager THEN
    FIND bArtLag NO-LOCK where
      bArtLag.Butik = Translogg.Butik and
      bArtLag.artikkelnr = TransLogg.artikkelnr and
      bArtLag.Storl = TransLogg.Storl NO-ERROR.
    
    /* Ingenting igjen på lageret.                   */
    /* Linjen undertrykkes hvis bruker velger dette. */
    if wT-Alle = FALSE then
      DO:
        /* Ukjent lager. */
        if NOT AVAILABLE ArtLag then
          NEXT TRANSLOGG.
        /* Tomt på sentrallager. */
        if ArtLag.LagAnt <= 0 then
          NEXT TRANSLOGG.
      END.

    FIND FIRST JobbLinje exclusive-LOCK where
     JobbLinje.JobbNr = wJobbNr and
     JobbLinje.Char1  = (if wT-PrDag
                           then STRING(TransLogg.Dato)
                           ELSE "") and
    JobbLinje.Char2    = STRING(TransLogg.Vg,"9999") + "/" + STRING(TransLogg.LopNr,"9999") and
    JobbLinje.Char3    = TransLogg.Storl NO-ERROR.

    if NOT AVAILABLE JobbLinje then
      DO:
        CREATE JobbLinje.
        assign
          JobbLinje.JobbNr   = wJobbNr
          JobbLinje.Char1    = if wT-PrDag
                                 then STRING(TransLogg.Dato)
                                 ELSE ""
          JobbLinje.Char2    = STRING(TransLogg.Vg,"9999") + "/" + STRING(TransLogg.LopNr,"9999")
          JobbLinje.Char3    = TransLogg.Storl

          JobbLinje.DecX[49] = if available ArtLag
                                 then ArtLag.Lagant
                                 else 0
          Jobblinje.DecX[47] = TransLogg.Vg
          Jobblinje.DecX[48] = TransLogg.LopNr
          JobbLinje.DivX[1]  = STRING(TransLogg.Vg) + "/" + STRING(TransLogg.LopNr)
          JobbLinje.DivX[ 2] = IF AVAILABLE ArtBas
                                 THEN substring(ArtBas.Beskr,1,30)
                                 ELSE ""
          JobbLinje.DivX[ 3] = IF AVAILABLE ArtBas
                                 THEN substring(ArtBas.LevKod,1,15)
                                 ELSE ""
          JobbLinje.DivX[ 4] = IF AVAILABLE ArtBas
                                 THEN substring(ArtBas.LevFargKod,1,15)
                                 ELSE ""

          .
      END.

    assign
      JobbLinje.DecX[46]    = JobbLinje.DecX[46]    + TransLogg.Antall
      JobbLinje.DecX[wIdx]  = JobbLinje.DecX[wIdx]  + TransLogg.Antall
      JobbLinje.Dec2X[wIdx] = /*JobbLinje.Dec2X[wIdx] +*/ 
                              (IF AVAILABLE bArtLag
                                 THEN bArtLag.Lagant
                                 ELSE 0)
      .
  END. /* TRANSLOGG */
END. /* BUTIKK */

/* Preper visningsfelt */
IF wT-Lager THEN
FOR EACH JobbLinje EXCLUSIVE-LOCK WHERE
    JobbLinje.JobbNr = wJobbNr:
    DO wIdx = 1 TO 50:
      ASSIGN
        JobbLinje.Div2X[wIdx] = STRING(JobbLinje.DecX[wIdx]) + "/" +
                               STRING(JobbLinje.Dec2X[wIdx])
        JobbLinje.Div2X[wIdx] = (IF JobbLinje.Div2X[wIdx] = "0/0"
                                  THEN ""
                                  ELSE JobbLinje.Div2X[wIdx])
        .
    END.
END.

/* Blanker Vg/LpNr som ikke skal vises. */
for each JobbLinje exclusive-lock where
  JobbLinje.JobbNr = wJobbNr
  break by JobbLinje.Char1
        by JobbLinje.Char2:
  if first-of(JobbLinje.Char2) then. /* Gjør ingenting. */
  else
    assign JobbLinje.DivX[1] = "".        
end.

assign
  wStatus = "OK".

