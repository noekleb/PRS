/************************************************************
    Program:  x-Kundeestat.p.p
    Created:  TN   31 Des 00
Description:  Bygger jobblinje for Kundeestatistikkstatistikk.

************************************************************/

DEF INPUT  PARAMETER wJobbNr       as INT  NO-UNDO.
DEF OUTPUT parameter wStatus       as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wByggLinjeHandle as HANDLE NO-UNDO.

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
DEF VAR wChar2       as CHAR  NO-UNDO.
DEF VAR wFraAar      as INT   NO-UNDO.
DEF VAR wTilAar      as INT   NO-UNDO.
DEF VAR wFraLinje    as INT   NO-UNDO.
DEF VAR wTilLinje    as INT   NO-UNDO.
DEF VAR wStatKrit    as CHAR  NO-UNDO.
DEF VAR wStTypeId    as CHAR INITIAL "KUNDSTAT" NO-UNDO.
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
  wStatKrit  skip
  wPerId     skip
  wVisPerLin skip
  wPeriTekst skip
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
assign
  wStatus = "OK".
RETURN wStatus.

/* -------------- Procedurer ------------------------------- */
PROCEDURE ByggTmpTbl:
  LISTELINJE:
  FOR EACH ListeLinje OF Lister NO-LOCK:
    assign
      wLoop = wLoop + 1.
    if valid-handle(wParentHandle) AND wLoop modulo 5 = 0 then
      RUN FremdriftProgressBar in wParentHandle (INPUT wLoop).

    FIND Kunde NO-LOCK where
      Kunde.KundeNr = int(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
    if NOT AVAILABLE Kunde THEN
      NEXT LISTELINJE.

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
                       (STRING(dec(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999"),
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
DEF VAR wOk as LOG NO-UNDO.

ASSIGN wLoop = 0.
if valid-handle(wParentHandle) then
  RUN VisProgressBar in wParentHandle.

BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:
    assign
      wLoop = wLoop + 1.
    if valid-handle(wParentHandle) AND wLoop modulo 5 = 0  then
      RUN FremdriftProgressBar in wParentHandle (INPUT wLoop).

  FIND Kunde NO-LOCK where
    Kunde.KundeNr = int(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE Kunde THEN
    NEXT BYGGJOBB.

  FIND KundeGruppe OF Kunde NO-LOCK NO-ERROR.

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

  /* Legger opp en post pr. tStLinje. */
  ASSIGN wOk = FALSE.
  STATLINJE:
  FOR EACH tStLinje where
    tStLinje.DataObjekt = STRING(INT(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999") and
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
      wOk         = true.

    /* Undertrykker tomme linjer. */
    if CAN-DO("250,301",STRING(wLayout)) AND tStLinje.AntSolgt = 0 AND tStLinje.VerdiSolgt = 0 then
      NEXT STATLINJE.

    RUN OpprettJobbLinje.
  END. /* STATLINJE */
  /* Legger opp en tom linje hvis det ikke finnes noen statistikk. */
  if wOk = FALSE then
    DO:
      /*
      assign
        wSkriv      = true
        wAntJLinjer = wAntJLinjer + 1.

      RUN OpprettJobbLinje.
      */
    END.
END. /* BYGGJOBB */

end PROCEDURE. /* ByggDataSett */

PROCEDURE OpprettJobbLinje:
  /*
  hide message NO-PAUSE.
  MESSAGE "Jobblinje: " wAntJLinjer skip
          wVisPerLin.
  */

  CREATE JobbLinje.

  assign
    JobbLinje.JobbNr   = wJobbNr
    JobbLinje.Char1    = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                           THEN STRING(wCl,"999999")
                           ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
    JobbLinje.Char2    = STRING(Kunde.KundeNr,"9999999999999")
    JobbLinje.Char3    = STRING(wAntJLinjer)
    JobbLinje.DivX[ 1] = " " /*STRING(ArtBas.ArtikkelNr)*/
    JobbLinje.DivX[ 2] = " " /*STRING(ArtBas.LevNr)*/
    JobbLinje.DivX[ 3] = Kunde.Navn /*ArtBas.LevKod*/
    JobbLinje.DivX[ 4] = " " /*ArtBas.LevFargKod*/
    JobbLinje.DivX[ 6] = " " /*wFilNavn*/
    JobbLinje.DivX[ 7] = STRING(Kunde.KundeNr)
    JobbLinje.DivX[ 8] = wButNavn
    JobbLinje.DivX[ 9] = " " /*STRING(ArtBas.BildNr)*/
    JobbLinje.DivX[15] = " " /*STRING(ArtBas.SaSong)*/

    JobbLinje.DivX[24] = " " /*(if AVAILABLE Moms
                            THEN STRING(VarGr.Vg) + "/" + wLopNr + "/" + STRING(ArtBas.VgKat) + " " + VarGr.VgBesk
                            ELSE STRING(ArtBas.Vg) + "/" + wLopNr + "/" + STRING(ArtBas.VgKat))*/
    JobbLinje.DivX[25] = " " /*(if AVAILABLE Moms
                            THEN STRING(VarGr.MomsKod) + " " + Moms.Beskrivelse + " (" + STRING(Moms.MomsProc) + ")"
                            else STRING(VarGr.MomsKod)) */
    JobbLinje.DivX[26] = (if AVAILABLE KundeGruppe
                            THEN STRING(KundeGruppe.GruppeId) + " " + KundeGruppe.Beskrivelse
                            else STRING(Kunde.GruppeId))
    JobbLinje.DivX[27] = " " /*(if AVAILABLE VareMerke
                            THEN STRING(ArtBas.VmId) + " " + VareMerke.Beskrivelse
                            else STRING(ArtBas.VmId))*/
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
          JobbLinje.DecX[ 1] = tStLinje.LagerAnt
          JobbLinje.DecX[ 2] = tStLinje.PrimoAnt
          JobbLinje.DecX[ 3] = tStLinje.VVarekost
          JobbLinje.DecX[ 4] = tStLinje.AntSolgt
          JobbLinje.DecX[ 5] = tStLinje.BrekkAnt
          JobbLinje.DecX[ 6] = tStLinje.IntAnt
          JobbLinje.DecX[ 7] = tStLinje.ReklAnt
          JobbLinje.DecX[ 8] = tStLinje.ReklLAnt
          JobbLinje.DecX[ 9] = tStLinje.GjenkjopAnt
          JobbLinje.DecX[10] = tStLinje.KjopAnt
          JobbLinje.DecX[11] = tStLinje.OvAnt
          JobbLinje.DecX[12] = tStLinje.JustAnt
          JobbLinje.DecX[13] = tStLinje.JustVerdi
          JobbLinje.DecX[14] = tStLinje.SvinnAnt
          JobbLinje.DecX[15] = tStLinje.SvinnVerdi
          JobbLinje.DecX[16] = tStLinje.NedAnt
          JobbLinje.DecX[17] = tStLinje.NedVerdi
          JobbLinje.DecX[18] = tStLinje.VerdiSolgt
          JobbLinje.DecX[19] = tStLinje.KjopVerdi
          JobbLinje.DecX[20] = tStLinje.BrekkVerdi
          JobbLinje.DecX[21] = tStLinje.IntVerdi
          JobbLinje.DecX[22] = tStLinje.ReklVerdi
          JobbLinje.DecX[23] = tStLinje.ReklLVerdi
          JobbLinje.DecX[24] = tStLinje.GjenkjopVerdi
          JobbLinje.DecX[25] = tStLinje.OvVerdi
          JobbLinje.DecX[26] = tStLinje.MvaVerdi
          JobbLinje.DecX[27] = tStLinje.OmlHast
          JobbLinje.DecX[29] = tStLinje.OmlHast
          JobbLinje.DecX[30] = tStLinje.VerdiSolgt - tStLinje.VVarekost /* DBKr */
          JobbLinje.DecX[31] = (JobbLinje.DecX[30] * 100) / tStLinje.VerdiSolgt /* DB% */
          JobbLinje.DecX[31] = if JobbLinje.DecX[31] = ? then 0 else JobbLinje.DecX[31]
          JobbLinje.DecX[32] = tStLinje.LagerAnt
          JobbLinje.DecX[33] = (tStLinje.AntSolgt / (tStLinje.KjopAnt + tStLinje.OvAnt)) * 100 /* Utsolgt% */
          JobbLinje.DecX[33] = if JobbLinje.DecX[34] = ? then 0 else JobbLinje.DecX[34]
          JobbLinje.DecX[34] = tStLinje.OmlHast
          JobbLinje.DecX[35] = tStLinje.PrimoAnt
          JobbLinje.DecX[36] = Kunde.GruppeId /*ArtBas.BildNr*/
          JobbLinje.DecX[37] = INT(ENTRY(2,ListeLinje.DataObjekt,","))
          JobbLinje.DecX[38] = wSortering.

      END.

    /* Sortering og brytgrupper */
    CASE wSortering:
      WHEN 1 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(Kunde.KundeNr,"9999999999999")
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
      WHEN 2 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = Kunde.Navn
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
      WHEN 3 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(Kunde.KundeNr,"9999999999999")
        JobbLinje.DivX[12] = STRING(Kunde.KundeNr,"9999999999999")
        JobbLinje.DivX[13] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = STRING(Kunde.KundeNr) + " " +
                              (if AVAILABLE KundeGruppe
                                THEN Kundegruppe.Beskrivelse
                                ELSE "")
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 4 THEN assign
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(Kunde.GruppeId,"9999")
        JobbLinje.DivX[12] = Kunde.Navn
        JobbLinje.DivX[13] = STRING(tStLinje.Aar,"9999") + STRING(tStLinje.PerLinNr,"999999999")
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = STRING(Kunde.GruppeId) + " " +
                              (if AVAILABLE KundeGruppe
                                THEN Kundegruppe.Beskrivelse
                                ELSE "")
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
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
    END CASE.
END PROCEDURE.

