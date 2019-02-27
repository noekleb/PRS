/************************************************************
    Program:  x-artikkler.p
    Created:  TN   19 Feb 99
Description:  Bygger jobblinje for artikkelrapporter.

Last change:  TN   25 Oct 100    7:45 pm
              ghg  16 Jul 2012
************************************************************/

DEF INPUT  PARAMETER wJobbNr as INT  NO-UNDO.
DEF OUTPUT parameter wStatus as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wSjekkLAger  as INT   NO-UNDO.     /* ken1-lager */
DEF VAR wJobbLinjeRecid as RECID NO-UNDO. /* ken1-lager */
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
DEF VAR wSideBryt    AS LOG   NO-UNDO.

/* nytt feb-09 ken1 */
DEFINE VARIABLE dSumSolgt AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSumSVK   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSolgtDb% AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTBkr AS DECIMAL     NO-UNDO.
/*  */


DEF BUFFER bufBestLinje FOR BestLinje.
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

assign
  wStatus = "AVBRYT"
  wButNavn = STRING(clButiker.Butik,"zzzzz9") + " " + clButiker.ButNamn.


/*Sjekker om det skal gis sidebryt */
ASSIGN wSideBryt = FALSE.
IF VALID-HANDLE(wParentHandle) THEN
    RUN GetSidebryt IN wParentHandle (OUTPUT wSideBryt) NO-ERROR.

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
  wSortering  = INT(ENTRY(3,Jobb.Kriterier))
  wSjekkLager = INT(ENTRY(4,Jobb.Kriterier)) - 1              /* ken1-lager */
  wSjekkLAger = if wSjekkLager < 0 then 0 else wSjekkLager.   /* ken1-lager */
BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:
  assign
    w2Loop = w2Loop + 1.
  if valid-handle(wParentHandle) AND w2Loop modulo 5 = 0 then
    RUN FremdriftProgressBar in wParentHandle (INPUT w2Loop).

  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE ArtBas THEN
    NEXT BYGGJOBB.

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType  OF ArtBas NO-LOCK NO-ERROR.

  FIND ArtPris NO-LOCK where
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
    ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  if NOT AVAILABLE ArtPris THEN
    NEXT BYGGJOBB.
  /* Henter butikken som er satt inn på listelinjen. */
  if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 then /* Ikke spes pr. butikk */
    DO:
      FIND Butiker NO-LOCK where
        Butiker.Butik = wCl NO-ERROR.
      wButNavn = wButTekst.
    END.
  else DO: /* Spesifisert pr. butikk. */
    FIND Butiker NO-LOCK where
      Butiker.Butik = INT(ENTRY(2,ListeLinje.DataObjekt,",")) NO-ERROR.
    if AVAILABLE Butiker then
      wButNavn = STRING(Butiker.Butik,"zzzzz9") + " " + Butiker.ButNamn.
  END.

  /* Henter bilde. Finnes ikke bilde, benyttes blankt bilde. */
  FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
  if VALID-HANDLE(wLibHandle) then
    RUN HentBildePeker in wLibHandle
                       (input Bilderegister.BildNr,
                        INPUT 1,
                        (if available Bilderegister
                          then BildeRegister.Filnavn
                          else ""),
                        OUTPUT wFilNavn).

  FIND first VgKat NO-LOCK where
    VgKat.Vg    = ArtBas.Vg and
    VgKat.VgKat = ArtBas.VgKat no-error.
  if AVAILABLE VgKat then
    FIND Kategori NO-LOCK where
      Kategori.KatNr = VgKat.KatNr NO-ERROR.

  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.

  /* Henter antall på lager og antall solgt - sum alle butikker. */
  if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 then /* Ikke spes pr. butikk */
    DO:
      assign
        wAntSolgt  = 0
        wAntLager  = 0
        wAntKjop   = 0
        wSumAntall = 0
        wSumVerdi  = 0
        dSumSolgt  = 0       /* ghg 20120716 */
        dSumSVK    = 0.      /* ghg 20120716 */
      FOR EACH Lager OF ArtBas NO-LOCK:
        assign
          wAntKjop   = wAntKjop   + Lager.KjopAnt
          wAntSolgt  = wAntSolgt  + Lager.AntSolgt
          wAntLager  = wAntLager  + Lager.LagAnt
          wSumAntall = wSumAntall + Lager.LagAnt
          wSumVerdi  = wSumVerdi  + (Lager.LagAnt * Lager.VVareKost)
          dSumSolgt  = dSumSolgt  + Lager.VerdiSolgt
          dSumSVK    = dSumSVK    + (Lager.VVarekost * Lager.AntSolgt).  /* ghg 20120716 */
/*        dSumSVK    = dSumSVK    + Lager.SVK. ghg */
      END.
      assign
        wvVareKost = wSumVerdi / wSumAntall
        wvVareKost = if wvVareKost = ? THEN 0 ELSE wvVareKost
        wUts%      = (wAntSolgt / wAntKjop) * 100
        wUts%      = if wUts% = ? THEN 0 ELSE wUts%
        dTBkr      = dSumSolgt - dSumSVK
        dSolgtDb%  = ((dSumSolgt - dSumSVK) * 100) / dSumSolgt
        dSolgtDb%  = IF dSolgtDb% = ? THEN 0 ELSE dSolgtDb%.
/*     dSolgtDb%  = (dSumSolgt - dSumSVK) / dSumSolgt * 100  ghg */
    END.

  /* Behandler hvert lager for seg. */
  ELSE DO:
    FIND Lager OF ArtBas NO-LOCK where
      Lager.Butik = INT(ENTRY(2,ListeLinje.DataObjekt,",")) no-error.
    if AVAILABLE Lager then
      assign
        wAntKjop   = Lager.KjopAnt
        wAntSolgt  = Lager.AntSolgt
        wAntLager  = Lager.LagAnt
        wUts%      = (Lager.AntSolgt / Lager.KjopAnt) * 100
        wUts%      = if wUts% = ? THEN 0 ELSE wUts%
        wvVareKost = Lager.VVareKost.
    else
      assign
        wAntKjop   = 0
        wAntSolgt  = 0
        wAntLager  = 0
        wUts%      = 0
        wUts%      = 0
        wvVareKost = 0.
  END.

/*  IF ArtBas.LopNr = 1000 THEN
  DO:
  MESSAGE "tb" dTBkr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  MESSAGE "dSumSVK" dSumSVK
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  MESSAGE "dSumSolgt" dSumSolgt
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  MESSAGE "tb% " dSolgtDb%
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END. */

  /* Oppretter jobbrecord. */
  assign
    wSkriv      = true
    wAntJLinjer = wAntJLinjer + 1.
  RUN OpprettJobbLinje.

/* ken1-lager */
  if wSjekkLager > 0 then
  SJEKKLAGER:
  DO:
      FIND CURRENT JobbLinje EXCLUSIVE-LOCK NO-ERROR.
/*     FIND JobbLinje EXCLUSIVE-LOCK where            */
/*       RECID(JobbLinje) = wJobbLinjeRecid NO-ERROR. */
    if AVAILABLE JobbLinje then
      DO:
        CASE wSjekkLager:
          /* Skriv ut alle som har lager forskjellig fra 0 */
          WHEN 1 then
            DO:
              if JobbLinje.DecX[5] = 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager st›rre enn 0 */
          WHEN 2 then
            DO:
              if JobbLinje.DecX[5] <= 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager mindre enn 0 */
          WHEN 3 then
            DO:
              if JobbLinje.DecX[5] >= 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager lik 0 */
          WHEN 4 then
            DO:
              if JobbLinje.DecX[5] <> 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.

        END CASE.
      END.
  END. /* SJEKKLAGER */
/* ken1-lager */


END. /* BYGGJOBB */

assign
  wStatus = "OK".

/* -------------- Internprocedurer ----------------------- */

PROCEDURE OpprettJobbLinje:

  assign
    wLopNr = if ArtBas.LopNr = ? or ArtBas.LopNr = 0 then "?"
             ELSE STRING(ArtBas.LopNr)
    wChar2 = STRING(ArtBas.Vg,"9999") + "," +
             (if ArtBas.LopNr = ? or ArtBas.LopNr = 0
               then "?"
               ELSE STRING(ArtBas.LopNr)) + "," +
             STRING(ArtBas.ArtikkelNr).

  CREATE JobbLinje.

  assign
    JobbLinje.JobbNr   = wJobbNr
    JobbLinje.Char1    = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                           THEN STRING(wCl,"999999")
                           ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
    JobbLinje.Char2    = wChar2
    JobbLinje.Char3    = STRING(ArtBas.LevNr,"999999999") + "," +
                         ArtBas.LevKod
    JobbLinje.DivX[ 1] = STRING(ArtBas.ArtikkelNr)
    JobbLinje.DivX[ 2] = STRING(ArtBas.LevNr)
    JobbLinje.DivX[ 3] = ArtBas.LevKod + '/' + TRIM(ArtBas.Beskr) /* TN 18/11-14 */
    JobbLinje.DivX[ 4] = TRIM(ArtBas.LevFargKod) + '/' + STRING(ArtBas.Farg) + ' ' + IF AVAILABLE Farg THEN TRIM(Farg.FarBeskr) ELSE ''
    JobbLinje.DivX[ 5] = wLopNr
    JobbLinje.DivX[ 6] = wFilNavn
    JobbLinje.DivX[ 7] = STRING(ArtBas.Vg)
    JobbLinje.DivX[ 8] = wButNavn
    JobbLinje.DivX[ 9] = STRING(ArtBas.BildNr)
    JobbLinje.DivX[15] = if (CAN-DO("1,2,3,6",STRING(wSortering)) AND wSideBryt = TRUE)
                           THEN "1"
                           ELSE "" 
    JobbLinje.DivX[16] = if CAN-DO("1,2,6",STRING(wSortering))
                           THEN STRING(ArtBas.Vg) + "  " + VarGr.VgBeskr
                         else if CAN-DO("3",STRING(wSortering))
                           THEN STRING(ArtBas.LevNr) + "  " + LevBas.LevNamn
                         ELSE ""
    JobbLinje.DivX[17] = LevBas.LevNamn
    /* Sortering og brytgrupper */
    JobbLinje.DivX[10] = if wSortering = 1 THEN JobbLinje.Char1
                         ELSE if wSortering = 2 THEN JobbLinje.Char1
                         ELSE if wSortering = 3 THEN JobbLinje.Char1
                         ELSE if wSortering = 4 THEN JobbLinje.Char1
                         ELSE if wSortering = 5 THEN JobbLinje.Char1
                         ELSE if wSortering = 6 THEN JobbLinje.Char1
                         ELSE ""
    JobbLinje.DivX[11] = if wSortering = 1 THEN STRING(ArtBas.Vg,"9999")
                         ELSE if wSortering = 2 THEN STRING(ArtBas.Vg,"9999")
                         ELSE if wSortering = 3 THEN STRING(ArtBas.LevNr,"9999999")
                         ELSE if wSortering = 4 THEN ""
                         ELSE if wSortering = 5 THEN ""
                         ELSE if wSortering = 6 THEN STRING(ArtBas.Vg,"9999")
                         ELSE ""
    JobbLinje.DivX[12] = if wSortering = 1 THEN (if ArtBas.LopNr = ? or ArtBas.LopNr = 0
                                                   then "?"
                                                   ELSE STRING(ArtBas.LopNr))
                         ELSE if wSortering = 2 THEN STRING(ArtBas.LevNr,"9999999")
                         ELSE if wSortering = 3 THEN ArtBas.LevKod
                         ELSE if wSortering = 4 THEN STRING(ListeLinje.CellNr,"9999999999")
                         ELSE if wSortering = 5 THEN STRING(ArtBas.BildNr,"9999999999")
                         ELSE if wSortering = 6 THEN STRING(ArtBas.VgKat)
                         ELSE ""
    JobbLinje.DivX[13] = if wSortering = 1 THEN STRING(ArtBas.ArtikkelNr)
                         ELSE if wSortering = 2 THEN ArtBas.LevKod
                         ELSE if wSortering = 3 THEN ""
                         ELSE if wSortering = 4 THEN ""
                         ELSE if wSortering = 5 THEN ""
                         ELSE if wSortering = 6 THEN STRING(ArtBas.LevNr,"9999999")
                         ELSE ""
    JobbLinje.DivX[14] = if wSortering = 1 THEN ""
                         ELSE if wSortering = 2 THEN ""
                         ELSE if wSortering = 3 THEN ""
                         ELSE if wSortering = 4 THEN ""
                         ELSE if wSortering = 5 THEN ""
                         ELSE if wSortering = 6 THEN ArtBas.LevKod
                         ELSE ""
    /* ------------------------ */

    JobbLinje.DivX[24] = (if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + "/" + wLopNr + "/" + STRING(ArtBas.VgKat) + " " + VarGr.VgBesk
                            ELSE STRING(ArtBas.Vg) + "/" + wLopNr + "/" + STRING(ArtBas.VgKat))
    JobbLinje.DivX[25] = (if AVAILABLE Farg
                            THEN STRING(ArtBas.Farg) + " " + Farg.FarBeskr
                            else STRING(ArtBas.Farg))
    JobbLinje.DivX[26] = (if AVAILABLE Material
                            THEN STRING(ArtBas.MatKod) + " " + Material.MatBeskr
                            else STRING(ArtBas.MatKod))
    JobbLinje.DivX[27] = (if AVAILABLE VareMerke
                            THEN STRING(ArtBas.VmId) + " " + VareMerke.Beskrivelse
                            else STRING(ArtBas.VmId))
    JobbLinje.DivX[31] = STRING(ListeLinje.CellNr,"9999999999")

    JobbLinje.DecX[ 1] = ArtPris.VareKost[if ArtPris.Tilbud THEN 2 ELSE 1]
    JobbLinje.DecX[ 2] = wVVareKost
    JobbLinje.DecX[ 3] = IF dSolgtDb% <> 0 THEN dSolgtDb% ELSE ArtPris.DB%[1]
/*     JobbLinje.DecX[ 3] = ArtPris.DB%[if ArtPris.Tilbud THEN 2 ELSE 1] */
    JobbLinje.DecX[ 4] = ArtPris.Pris[1]
/*     JobbLinje.DecX[ 4] = ArtPris.Pris[if ArtPris.Tilbud THEN 2 ELSE 1] */
    JobbLinje.DecX[ 5] = wAntLager /* På lager */
    JobbLinje.DecX[ 6] = wUts%
    JobbLinje.DecX[ 7] = wAntKjop
    JobbLinje.DecX[ 8] = ArtPris.InnkjopsPris[if ArtPris.Tilbud THEN 2 ELSE 1].
    JobbLinje.DecX[ 9] = wAntSolgt.

END PROCEDURE.

