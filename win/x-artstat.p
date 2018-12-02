/************************************************************
    Program:  x-artstat.p.p
    Created:  TN   19 Feb 99
Description:  Bygger jobblinje for artikkelstatistikk.

Last change:  TN   29 Nov 100    7:06 pm
************************************************************/

DEF INPUT  PARAMETER wJobbNr       as INT  NO-UNDO.
DEF OUTPUT parameter wStatus       as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wByggLinjeHandle as HANDLE NO-UNDO.

DEF VAR piTotAnt     AS INT   NO-UNDO.
DEF VAR wPeriTekst   as CHAR  NO-UNDO.
DEF VAR wVisPerLin   as LOG   NO-UNDO.
DEF VAR wPerLinTxt   as CHAR  NO-UNDO.
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
DEF VAR wChar1       AS CHAR  NO-UNDO.
DEF VAR wChar2       as CHAR  NO-UNDO.
DEF VAR wChar3       AS CHAR  NO-UNDO.
DEF VAR wFraAar      as INT   NO-UNDO.
DEF VAR wTilAar      as INT   NO-UNDO.
DEF VAR wFraLinje    as INT   NO-UNDO.
DEF VAR wTilLinje    as INT   NO-UNDO.
DEF VAR wStatKrit    as CHAR  NO-UNDO.
DEF VAR wStTypeId    as CHAR INITIAL "ARTIKKEL" NO-UNDO.
DEF VAR wPerId       as CHAR  NO-UNDO.

DEF BUFFER bufBestLinje FOR BestLinje.
DEF BUFFER clButiker FOR Butiker.
DEF BUFFER bufButiker FOR Butiker.
DEF BUFFER bJobbLinje FOR JobbLinje.

DEF new SHARED TEMP-TABLE tStLinje no-undo like StLinje.

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

/* Teller opp listelinjer. */
ASSIGN
    piTotAnt = 0.
FOR EACH ListeLinje OF Lister NO-LOCK:
    ASSIGN
        piTotAnt = piTotAnt + 1
        .
END.

/* Avslutter hvis jobblinje allerede er bygget. */
FIND FIRST JobbLinje NO-LOCK where
  JobbLinje.JobbNr = wJobbNr NO-ERROR.
if AVAILABLE JobbLinje then
  RETURN.

/* Sjekker datasett */
if NUM-ENTRIES(Jobb.Kriterier) < 10 then
  DO:
    MESSAGE "Gammelt datasett. Generer en ny liste." skip
            "Det ligger feil parametre i det gamle datasettet." SKIP(1)
            Jobb.Kriterier
            VIEW-AS ALERT-BOX.
    RETURN "AVBRYT".
  END.

/* Setter valg som gjelder for bygging av listen. */
assign
  SESSION:APPL-ALERT-BOXES = false
  wAntJLinjer = 0
  wLayout     = INT(ENTRY(2,Jobb.Kriterier))
  wSortering  = INT(ENTRY(3,Jobb.Kriterier))
  wFraAar     = INT(ENTRY(4,Jobb.Kriterier))
  wTilAar     = INT(ENTRY(5,Jobb.Kriterier))
  wFraLinje   = INT(ENTRY(6,Jobb.Kriterier))
  wTilLinje   = INT(ENTRY(7,Jobb.Kriterier))
  wStatKrit   = ENTRY(4,Jobb.Kriterier) + "," +
                ENTRY(5,Jobb.Kriterier) + "," +
                ENTRY(6,Jobb.Kriterier) + "," +
                ENTRY(7,Jobb.Kriterier)
  wPerId      = ENTRY(9,Jobb.Kriterier)
  wVisPerLin  = if ENTRY(10,Jobb.Kriterier) = "yes"
                  THEN true
                  ELSE FALSE
  wPeriTekst  = ENTRY(11,Jobb.Kriterier).

/*
message
  wLayout    skip
  wSortering skip
  wFraAar    skip
  wTilAar    skip
  wFraLinje  skip
  wTilLinje  skip
VIEW-AS alert-box.
*/

/* Bygger tmptabell som inneholder statistikkdata. */
RUN ByggTmpTbl.

/* Bygger datasett - Flytter data fra tmptabell til workdb. */
RUN ByggDatasett.

/* Sletter byggmakker */
if VALID-HANDLE(wByggLinjeHandle) THEN
  DELETE PROCEDURE wByggLinjeHandle.

/* Retur til kallende rutine. */
HIDE MESSAGE NO-PAUSE.
STATUS DEFAULT "".
assign
  wStatus = "OK".
RETURN wStatus.

/* -------------- Procedurer ------------------------------- */
PROCEDURE ByggTmpTbl:
  LISTELINJE:
  FOR EACH ListeLinje OF Lister NO-LOCK:
    STATUS DEFAULT "Artikkel " + ENTRY(1,ListeLinje.DataObjekt,",") + 
        " (" + STRING(wLoop) + " av " + STRING(piTotAnt) + ").".
    assign
      wLoop = wLoop + 1.
    if valid-handle(wParentHandle) AND wLoop modulo 5 = 0 then
      RUN FremdriftProgressBar in wParentHandle (INPUT wLoop).

    FIND ArtBas NO-LOCK where
      ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
    if NOT AVAILABLE ArtBas THEN
      NEXT LISTELINJE.

    /*
    hide message NO-PAUSE.
    MESSAGE "Leser statistikk: " ArtBas.Vg ArtBas.LopNr ArtBas.LevKod.
    */

    /* Bygger temp-table */
    if VALID-HANDLE(wByggLinjeHandle) then
      run Historikk IN wByggLinjeHandle
                       (STRING(dec(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999"),
                       wPerId,
                       wStTypeId,
                       NO,
                       wStatKrit + "," + ENTRY(2,ListeLinje.DataObjekt,",") + ",,1,1," + STRING(wLayout)).
    else
      run byggstlinje.p PERSISTENT SET wByggLinjeHandle
                       (STRING(DEC(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999"),
                       wPerId,
                       wStTypeId,
                       NO,
                       wStatKrit + "," + ENTRY(2,ListeLinje.DataObjekt,",") + ",,1,1," + STRING(wLayout)).
  END. /* LISTELINJE */

  /* TEST */
  /*
  OUTPUT to t.sdv append.
  FOR EACH tStLinje NO-LOCK:
    EXPORT DELIMITER ";"
      tStLinje.DataObjekt
      tStLinje.StTypeId
      tStLinje.PerId
      tStLinje.Aar
      tStLinje.PerLinNr
      tStLinje.Butik.
  END.
  OUTPUT CLOSE.
  */
END PROCEDURE. /* ByggTmpTbl */

PROCEDURE ByggDataSett:
DEF VAR wOk         as LOG NO-UNDO.

ASSIGN wLoop = 0.
if valid-handle(wParentHandle) then
  RUN VisProgressBar in wParentHandle.

BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:
    assign
      wLoop = wLoop + 1.

    if valid-handle(wParentHandle) AND wLoop modulo 5 = 0  then
      RUN FremdriftProgressBar in wParentHandle (INPUT wLoop).

  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE ArtBas THEN
    NEXT BYGGJOBB.

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType  OF ArtBas NO-LOCK NO-ERROR.
  FIND Sasong   OF ArtBas NO-LOCK NO-ERROR.

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
  /*
  FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
  if VALID-HANDLE(wLibHandle) then
    RUN HentBildePeker in wLibHandle
                       (input Bilderegister.BildNr,
                        INPUT 1,
                        (if available Bilderegister
                          then BildeRegister.Filnavn
                          else ""),
                        OUTPUT wFilNavn).
  */

  FIND first VgKat NO-LOCK where
    VgKat.Vg    = ArtBas.Vg and
    VgKat.VgKat = ArtBas.VgKat no-error.
  if AVAILABLE VgKat then
    FIND Kategori NO-LOCK where
      Kategori.KatNr = VgKat.KatNr NO-ERROR.

  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.

  /* Legger opp en post pr. tStLinje. */
  ASSIGN wOk = FALSE.
  STATLINJE:
  FOR EACH tStLinje where
    tStLinje.DataObjekt = STRING(dec(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999") and
    tStLinje.StTypeId   = wStTypeId and
    tStLinje.PerId      = wPerId and
    (if (wVisPerLin = false and INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0)
      THEN true
     else if (wVisPerLin = true and INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0)
      THEN tStLinje.Butik = 9999998
     ELSE tStLinje.Butik = INT(ENTRY(2,ListeLinje.DataObjekt,","))) and

    (if (wVisPerLin  = false and INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0)
      THEN tStLinje.PerLinNr   = 1999999 /* Sum alle butikker. */
     ELSE if wVisPerLin
      THEN tStLinje.PerLinNr  < 1000000  /* Spes hver periode. */
     ELSE tStLinje.PerLinNr   = 1000000) /* Sum pr. butikk.    */
    break
    by tStLinje.DataObjekt
    by tStLinje.StTypeId
    by tStLinje.Butik
    by tStLinje.Aar descending
    by tStLinje.PerId descending
    by tStLinje.PerLinNr descending:
    /* Oppretter jobbrecord. */
    assign
      wSkriv      = true
      wAntJLinjer = wAntJLinjer + 1
      wOk         = TRUE.

    /* Undertrykker tomme linjer.                        */
    /* Her benyttes ulike kriterier for de ulike layout. */
    if CAN-DO("250",STRING(wLayout)) AND tStLinje.AntSolgt = 0 AND tStLinje.VerdiSolgt = 0 then
      NEXT STATLINJE.
    else if CAN-DO("252",STRING(wLayout)) AND tStLinje.AntSolgt = 0 AND tStLinje.VerdiSolgt = 0
                                          AND tStLinje.KjopAnt  = 0 AND tStLinje.KjopVerdi  = 0
                                          AND tStLinje.LagerAnt = 0 AND tStLinje.LagerVerdi = 0 then
      NEXT STATLINJE.

    RUN OpprettJobbLinje.
  END. /* STATLINJE */
  /* Legger opp en tom linje hvis det ikke finnes noen statistikk. */
  /*
  if wOk = FALSE and not CAN-DO("250",STRING(wLAyout)) then
    DO:
      assign
        wSkriv      = true
        wAntJLinjer = wAntJLinjer + 1.

      CREATE tStLinje.
      assign
        tStLinje.DataObjekt = STRING(dec(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999")
        tStLinje.StTypeId   = wStTypeId
        tStLinje.PerId      = wPerId
        tStLinje.Butik      = INT(ENTRY(2,ListeLinje.DataObjekt,","))
        tStLinje.Aar        = wTilAar
        tStLinje.PerLinNr   = wTilLinje.

      RUN OpprettJobbLinje.
    END.
  */
END. /* BYGGJOBB */

end PROCEDURE. /* ByggDataSett */

PROCEDURE OpprettJobbLinje:

  /*
  hide message NO-PAUSE.
  MESSAGE "Jobblinje: " wAntJLinjer.
  */

  CASE wLayout:
    WHEN 253 THEN
      ASSIGN
      wChar1    = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                         THEN STRING(wCl,"999999")
                         ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
      wChar2    = STRING(ArtBas.Vg,"9999") + ",,,"
      wChar3    = STRING(ArtBas.LevNr,"999999999") + ",,"
      wLopNr    = ""
      .
    OTHERWISE 
      ASSIGN
      wChar1    = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                         THEN STRING(wCl,"999999")
                         ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
      wChar2    = STRING(ArtBas.Vg,"9999") + "," +
                         (if ArtBas.LopNr = ? or ArtBas.LopNr = 0
                           then "?"
                           ELSE STRING(ArtBas.LopNr)) + "," +
                         STRING(ArtBas.ArtikkelNr)
      wChar3    = STRING(ArtBas.LevNr,"999999999") + "," +
                           ArtBas.LevKod +  "," + STRING(wAntJLinjer,"999999999")
      wLopNr = if ArtBas.LopNr = ? or ArtBas.LopNr = 0 then "?"
               ELSE STRING(ArtBas.LopNr)
      .
  END CASE.

  FIND JobbLinje EXCLUSIVE-LOCK WHERE
    jobblinje.JobbNr = wJobbNr AND
    JobbLinje.Char1  = wChar1 AND
    JobbLinje.Char2  = wChar2 AND
    JobbLinje.Char3  = wChar3 NO-ERROR.
  IF NOT AVAILABLE JobbLinje THEN
    CREATE JobbLinje.

  assign
      JobbLinje.JobbNr   = wJobbNr
      JobbLinje.Char1    = wChar1
      JobbLinje.Char2    = wChar2 
      JobbLinje.Char3    = wChar3
      .

  assign
    JobbLinje.DivX[ 1] = STRING(ArtBas.ArtikkelNr)
    JobbLinje.DivX[ 2] = STRING(ArtBas.LevNr)
    JobbLinje.DivX[ 3] = SUBSTRING(ArtBas.LevKod,1,12)
    JobbLinje.DivX[ 4] = ArtBas.LevFargKod
    JobbLinje.DivX[ 5] = wLopNr
    JobbLinje.DivX[ 6] = wFilNavn
    JobbLinje.DivX[ 7] = STRING(ArtBas.Vg)
    JobbLinje.DivX[ 8] = wButNavn
    JobbLinje.DivX[ 9] = STRING(ArtBas.BildNr)
    JobbLinje.DivX[15] = STRING(ArtBas.SaSong)
    JobbLinje.DivX[33] = SUBSTRING(ArtBas.Beskr,1,25)
    JobbLinje.DivX[40] = if available Sasong then SaSong.SasBeskr else ""

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
    JobbLinje.DivX[31] = STRING(ListeLinje.CellNr,"9999999999").

    FIND bufButiker NO-LOCK where
      bufButiker.butik = INT(JobbLinje.Char1) NO-ERROR.

    /* Setter inn statistikkverdiene. */
    if AVAILABLE tStLinje then
      DO:
        if wVisPerLin = false then
          wPerLinTxt = "".
        ELSE
        case tStLinje.PerId:
          when "AAR"   then wPerLinTxt = string(tStLinje.Aar).
          when "MANED" then wPerLinTxt = string(tStLinje.Aar) + "/" + string(tStLinje.PerLinNr).
          when "UKE"   then wPerLinTxt = string(tStLinje.Aar) + "/" + string(tStLinje.PerLinNr).
          when "DAG"   then
            do:
              wPerLinTxt = string(date(1,1,tStLinje.Aar) + (tStLinje.PerLinNr - 1)).
            end.
          otherwise
            do:
              wPerLinTxt = string(PerLin.FraDato) + "-" + string(PerLin.TilDato).
            end.
        end case.

        ASSIGN
          JobbLinje.DivX[16] = wPerLinTxt
          JobbLinje.DecX[ 1] = JobbLinje.DecX[ 1] + tStLinje.LagerAnt
          JobbLinje.DecX[ 2] = JobbLinje.DecX[ 2] + tStLinje.PrimoAnt
          JobbLinje.DecX[ 3] = JobbLinje.DecX[ 3] + tStLinje.VVarekost
          JobbLinje.DecX[ 4] = JobbLinje.DecX[ 4] + tStLinje.AntSolgt
          JobbLinje.DecX[ 5] = JobbLinje.DecX[ 5] + tStLinje.BrekkAnt
          JobbLinje.DecX[ 6] = JobbLinje.DecX[ 6] + tStLinje.IntAnt
          JobbLinje.DecX[ 7] = JobbLinje.DecX[ 7] + tStLinje.ReklAnt
          JobbLinje.DecX[ 8] = JobbLinje.DecX[ 8] + tStLinje.ReklLAnt
          JobbLinje.DecX[ 9] = JobbLinje.DecX[ 9] + tStLinje.GjenkjopAnt
          JobbLinje.DecX[10] = JobbLinje.DecX[10] + tStLinje.KjopAnt
          JobbLinje.DecX[11] = JobbLinje.DecX[11] + tStLinje.OvAnt
          JobbLinje.DecX[12] = JobbLinje.DecX[12] + tStLinje.JustAnt
          JobbLinje.DecX[13] = JobbLinje.DecX[13] + tStLinje.JustVerdi
          JobbLinje.DecX[14] = JobbLinje.DecX[14] + tStLinje.SvinnAnt
          JobbLinje.DecX[15] = JobbLinje.DecX[15] + tStLinje.SvinnVerdi
          JobbLinje.DecX[16] = JobbLinje.DecX[16] + tStLinje.NedAnt
          JobbLinje.DecX[17] = JobbLinje.DecX[17] + tStLinje.NedVerdi
          JobbLinje.DecX[18] = JobbLinje.DecX[18] + tStLinje.VerdiSolgt
          JobbLinje.DecX[19] = JobbLinje.DecX[19] + tStLinje.KjopVerdi
          JobbLinje.DecX[20] = JobbLinje.DecX[20] + tStLinje.BrekkVerdi
          JobbLinje.DecX[21] = JobbLinje.DecX[21] + tStLinje.IntVerdi
          JobbLinje.DecX[22] = JobbLinje.DecX[22] + tStLinje.ReklVerdi
          JobbLinje.DecX[23] = JobbLinje.DecX[23] + tStLinje.ReklLVerdi
          JobbLinje.DecX[24] = JobbLinje.DecX[24] + tStLinje.GjenkjopVerdi
          JobbLinje.DecX[25] = JobbLinje.DecX[25] + tStLinje.OvVerdi
          JobbLinje.DecX[26] = JobbLinje.DecX[26] + tStLinje.MvaVerdi
          JobbLinje.DecX[27] = JobbLinje.DecX[27] + tStLinje.OmlHast
          JobbLinje.DecX[29] = JobbLinje.DecX[29] + tStLinje.OmlHast
          JobbLinje.DecX[30] = JobbLinje.DecX[30] + tStLinje.VerdiSolgt - tStLinje.VVarekost /* DBKr */
          JobbLinje.DecX[31] = (JobbLinje.DecX[30] * 100) / JobbLinje.DecX[18] /* DB% */
          JobbLinje.DecX[31] = if JobbLinje.DecX[31] = ? then 0 else JobbLinje.DecX[31]
          JobbLinje.DecX[32] = JobbLinje.DecX[32] + tStLinje.LagerAnt
          JobbLinje.DecX[33] = (JobbLinje.DecX[ 4] / (JobbLinje.DecX[10] + JobbLinje.DecX[11])) * 100 /* Utsolgt% */
          JobbLinje.DecX[33] = if JobbLinje.DecX[33] = ? then 0 else JobbLinje.DecX[33]
          JobbLinje.DecX[34] = JobbLinje.DecX[ 4] / ((JobbLinje.DecX[32] + JobbLinje.DecX[10] + JobbLinje.DecX[11]) / 2) /* Oml.hast */
          JobbLinje.DecX[35] = JobbLinje.DecX[35] + tStLinje.PrimoAnt
          JobbLinje.DecX[36] = JobbLinje.DecX[36] + ArtBas.BildNr
          JobbLinje.DecX[37] = JobbLinje.DecX[37] + INT(ENTRY(2,ListeLinje.DataObjekt,","))
          JobbLinje.DecX[38] = JobbLinje.DecX[38] + wSortering
          JobbLinje.DecX[39] = JobbLinje.DecX[39] + tStLinje.VVarekost
          JobbLinje.DecX[40] = JobbLinje.DecX[40] + tStLinje.AntRabatt
          JobbLinje.DecX[41] = JobbLinje.DecX[41] + tStLinje.VerdiRabatt
          JobbLinje.DecX[42] = JobbLinje.DecX[42] + tStLinje.SvinnAnt
                                                  + tStLinje.JustAnt
                                                  + tStLinje.IntAnt
                                                  + tStLinje.BrekkAnt
          JobbLinje.DecX[43] = JobbLinje.DecX[43] + tStLinje.SvinnVerdi
                                                  + tStLinje.JustVerdi
                                                  + tStLinje.IntVerdi
                                                  + tStLinje.BrekkVerdi
          .
      END.

    /* Sortering og brytgrupper */
    CASE wSortering:
      WHEN 1 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[12] = STRING(INT(wLopNr),"9999")
        JobbLinje.DivX[13] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Varegruppe " + (if AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 2 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[12] = STRING(ArtBas.LevNr,"9999999")
        JobbLinje.DivX[13] = ArtBas.LevKod
        JobbLinje.DivX[14] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Varegruppe " + (if AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[19] = "Leverandør "  + (if AVAILABLE LevBas
                                              THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                              ELSE STRING(ArtBas.LevNr))
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 3 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.LevNr,"9999999")
        JobbLinje.DivX[12] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[13] = ArtBas.LevKod
        JobbLinje.DivX[14] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Leverandør "  + (if AVAILABLE LevBas
                                              THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                              ELSE STRING(ArtBas.LevNr))
        JobbLinje.DivX[19] = "Varegruppe " + (if AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 4 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ListeLinje.CellNr,"9999999999")
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 5 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.BildNr,"9999999999")
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 6 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[12] = STRING(ArtBas.VgKat)
        JobbLinje.DivX[13] = STRING(ArtBas.LevNr,"9999999")
        JobbLinje.DivX[14] = ArtBas.LevKod
        JobbLinje.DivX[22] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Varegruppe " + (if AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[19] = "Kategori " + (if AVAILABLE Kategori
                                              THEN STRING(ArtBas.VgKat) + " " + Kategori.Beskrivelse
                                              ELSE STRING(ArtBas.VgKat))
        JobbLinje.DivX[20] = "Leverandør "  + (if AVAILABLE LevBas
                                              THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                              ELSE STRING(ArtBas.LevNr))
        JobbLinje.DivX[21] = "".
      WHEN 7 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[18]),"9999999999999.99")
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 8 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[18] * -1)),"9999999999999.99")
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 9 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[31]),"9999999999999.99")
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 10 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[31] * -1)),"9999999999999.99")
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 11 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = ArtBas.Beskr
        JobbLinje.DivX[12] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
    END CASE.
END PROCEDURE.

