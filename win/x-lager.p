/************************************************************
    Program:  x-lager.p
    Created:  TN   19 Feb 99
Description:  Bygger jobblinje for lagerlistene.

Last change:  TN    7 Nov 100    1:23 pm
************************************************************/

DEF INPUT  PARAMETER wJobbNr       as INT  NO-UNDO.
DEF OUTPUT parameter wStatus       as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wByggLinjeHandle as HANDLE NO-UNDO.

DEF VAR wSjekkLAger  as INT   NO-UNDO.
DEF VAR wJobbLinjeRecid as RECID NO-UNDO.
DEF VAR wBrutto      as INT   NO-UNDO. /* 0-Vektet lagerverdi, 1-Brutto verdi. */
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
DEF VAR wChar1       as CHAR  NO-UNDO.
DEF VAR wChar2       as CHAR  NO-UNDO.
DEF VAR wChar3       as CHAR  NO-UNDO.
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

/* Setter sentrallager. */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK where
  clButiker.Butik = wCl NO-ERROR.
if NOT AVAILABLE clButiker then
  RETURN.

{syspara.i 6 200 4 wButTekst}
{syspara.i 3 2 1 wBrutto int}

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
if NUM-ENTRIES(Jobb.Kriterier) < 3 then
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
  wSjekkLager = INT(ENTRY(4,Jobb.Kriterier)) - 1
  wSjekkLAger = if wSjekkLager < 0 then 0 else wSjekkLager.

/* Bygger tmptabell som inneholder størrelser pr. artikkel. */
/* RUN ByggStrMatrise. */

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
PROCEDURE ByggStrMatrise:
  /*
  STRLINJE:
  FOR EACH ListeLinje OF Lister NO-LOCK:
    assign
      wLoop = wLoop + 1.
    if valid-handle(wParentHandle) AND wLoop modulo 5 = 0 then
      RUN FremdriftProgressBar in wParentHandle (INPUT wLoop).

    FIND ArtBas NO-LOCK where
      ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
    if NOT AVAILABLE ArtBas THEN
      NEXT STRLINJE.

    /*
    /* Bygger temp-table */
    if VALID-HANDLE(wByggLinjeHandle) then
      run Historikk IN wByggLinjeHandle
                       (STRING(INT(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999"),
                       wPerId,
                       wStTypeId,
                       NO,
                       wStatKrit + "," + ENTRY(2,ListeLinje.DataObjekt,",")).
    else
      run byggstlinje.p PERSISTENT SET wByggLinjeHandle
                       (STRING(INT(ENTRY(1,ListeLinje.DataObjekt)),"9999999999999"),
                       wPerId,
                       wStTypeId,
                       NO,
                       wStatKrit + "," + ENTRY(2,ListeLinje.DataObjekt,",")).
    */
  END. /* STRLINJE */
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

  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE ArtBas THEN
    NEXT BYGGJOBB.

  /* PLU'er skal ikke være med */
  IF ArtBas.OPris THEN
      NEXT BYGGJOBB.

  /* Ikke lagerstyrte varer, skal ikke med. */
  IF ArtBas.LAger = FALSE THEN
    NEXT BYGGJOBB.

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.

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
                       (input ArtBas.BildNr,
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

  LAGERLINJE:
  FOR EACH Lager of ArtBas no-lock where
    (if INT(ENTRY(2,ListeLinje.DataObjekt)) = 0
       THEN true
       ELSE Lager.Butik = INT(ENTRY(2,ListeLinje.DataObjekt))):
    /* Oppretter jobbrecord. */
    assign
      wSkriv          = true
      wAntJLinjer     = wAntJLinjer + 1
      wJobbLinjeRecid = if INT(ENTRY(2,ListeLinje.DataObjekt)) <> 0
                          then ? /* Ny jobblinjepost skal opprettes */
                          else wJobbLinjeRecid. /* Akkumuler på forrige post. */

    RUN OpprettJobbLinje.
  END. /* STATLINJE */
  /* Sjekker om det er noe i lager.                                  */
  /* HVis bruker har valgt † kun skrive ut poster med:               */
  /* 0-Alle poster. 1-LagerAnt <> 0, 2-LAgerAnt > 0, 3-LAgerAnt < 0, 4-LagerAnt = 0. */
  if wSjekkLager > 0 then
  SJEKKLAGER:
  DO:
    FIND JobbLinje EXCLUSIVE-LOCK where
      RECID(JobbLinje) = wJobbLinjeRecid NO-ERROR.
    if AVAILABLE JobbLinje then
      DO:
        CASE wSjekkLager:
          /* Skriv ut alle som har lager forskjellig fra 0 */
          WHEN 1 then
            DO:
              if JobbLinje.DecX[26] = 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager st›rre enn 0 */
          WHEN 2 then
            DO:
              if JobbLinje.DecX[26] <= 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager mindre enn 0 */
          WHEN 3 then
            DO:
              if JobbLinje.DecX[26] >= 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager lik 0 */
          WHEN 4 then
            DO:
              if JobbLinje.DecX[26] <> 0 then
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.

        END CASE.
      END.
  END. /* SJEKKLAGER */

  wJobbLinjeRecid = ?.
END. /* BYGGJOBB */

end PROCEDURE. /* ByggDataSett */

PROCEDURE OpprettJobbLinje:
  /* Lager pr. varegruppe. */
  if CAN-DO("151",STRING(wLayout)) then
    assign
      wLopNr = "?"
      wChar1 = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                 THEN STRING(wCl,"999999")
                 ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
      wChar2 = STRING(ArtBas.Vg,"999999")
      wChar3 = "".
  ELSE if CAN-DO("152",STRING(wLayout)) then
    assign
      wLopNr = "?"
      wChar1 = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                 THEN STRING(wCl,"999999")
                 ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
      wChar2 = STRING(ArtBas.LevNr,"999999")
      wChar3 = "".
  /* Lager pr. artikke. */
  else
    assign
      wLopNr = if (ArtBas.LopNr = ? or ArtBas.LopNr = 0)
                 then "?"
                 ELSE STRING(ArtBas.LopNr)
      wChar1 = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                 THEN STRING(wCl,"999999")
                 ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
      wChar2 = STRING(ArtBas.Vg,"999999") + "," +
               (if ArtBas.LopNr = ? or ArtBas.LopNr = 0
                 then "?"
                 ELSE STRING(ArtBas.LopNr)) + "," +
               STRING(ArtBas.ArtikkelNr)
      wChar3 = STRING(ArtBas.LevNr,"999999999") + "," +
               ArtBas.LevKod +  "," + STRING(wAntJLinjer,"999999999").

  /* Henter posten for akkumulering */
  /* VAREGRUPPE */
  if CAN-DO("151",STRING(wLayout)) then
    DO:
      FIND JobbLinje NO-LOCK where
        JobbLinje.JobbNr = wJobbNr and
        JobbLinje.Char1  = wChar1  and
        JobbLinje.Char2  = wChar2  and
        JobbLinje.Char3  = wChar3 NO-ERROR.
      if AVAILABLE JobbLinje then
        wJobbLinjeRecid = RECID(JobbLinje).
      else
        wJobbLinjeRecid = ?.
    END.
  /* LEVERANDØR */
  ELSE if CAN-DO("152",STRING(wLayout)) then
    DO:
      FIND JobbLinje NO-LOCK where
        JobbLinje.JobbNr = wJobbNr and
        JobbLinje.Char1  = wChar1  and
        JobbLinje.Char2  = wChar2  and
        JobbLinje.Char3  = wChar3 NO-ERROR.
      if AVAILABLE JobbLinje then
        wJobbLinjeRecid = RECID(JobbLinje).
      else
        wJobbLinjeRecid = ?.
    END.

  /* Oppretter ny eller henter gammel for akkumulering. */
  if wJobbLinjeRecid = ? then
    NY-JOBBLINJE:
    DO:
      CREATE JobbLinje.
      assign
        wJobbLinjeRecid    = RECID(JobbLinje)
        JobbLinje.JobbNr   = wJobbNr
        JobbLinje.Char1    = wChar1
        JobbLinje.Char2    = wChar2
        JobbLinje.Char3    = wChar3
        JobbLinje.DivX[ 1] = STRING(ArtBas.ArtikkelNr)
        JobbLinje.DivX[ 2] = STRING(ArtBas.LevNr)
        JobbLinje.DivX[ 3] = ArtBas.LevKod
        JobbLinje.DivX[ 4] = ArtBas.LevFargKod
        JobbLinje.DivX[ 5] = wLopNr
        JobbLinje.DivX[ 6] = wFilNavn
        JobbLinje.DivX[ 7] = STRING(ArtBas.Vg)
        JobbLinje.DivX[ 8] = wButNavn
        JobbLinje.DivX[ 9] = STRING(ArtBas.BildNr)
        JobbLinje.DivX[15] = STRING(ArtBas.SaSong)
        Jobblinje.DivX[30] = substring(ArtBas.Beskr,1,25)

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

    END. /* NY-JOBBLINJE */
  ELSE
    FIND JobbLinje EXCLUSIVE-LOCK WHERE RECID(JobbLinje) = wJobbLinjeRecid.

  /* Setter inn lagerverdier. */
  ASSIGN
    JobbLinje.DecX[ 3] =  ((JobbLinje.DecX[ 3] * JobbLinje.DecX[26]) + (Lager.VVarekost * Lager.LagAnt)) /
                          (JobbLinje.DecX[26] + Lager.LagAnt)
    JobbLinje.DecX[ 3] = (if JobbLinje.DecX[ 3] = ? THEN 0 ELSE JobbLinje.DecX[ 3])
    JobbLinje.DecX[ 4] = JobbLinje.DecX[ 4] + Lager.AntSolgt
    JobbLinje.DecX[ 5] = JobbLinje.DecX[ 5] + Lager.BrekkAnt
    JobbLinje.DecX[ 6] = JobbLinje.DecX[ 6] + Lager.IntAnt
    JobbLinje.DecX[ 7] = JobbLinje.DecX[ 7] + Lager.ReklAnt
    JobbLinje.DecX[ 8] = JobbLinje.DecX[ 8] + Lager.ReklLAnt
    JobbLinje.DecX[ 9] = JobbLinje.DecX[ 9] + Lager.GjenkjopAnt
    JobbLinje.DecX[10] = JobbLinje.DecX[10] + Lager.KjopAnt
    JobbLinje.DecX[11] = JobbLinje.DecX[11] + Lager.OvAnt
    JobbLinje.DecX[12] = JobbLinje.DecX[12] + Lager.JustAnt
    JobbLinje.DecX[13] = JobbLinje.DecX[13] + Lager.JustVerdi
    JobbLinje.DecX[14] = JobbLinje.DecX[14] + Lager.SvinnAnt
    JobbLinje.DecX[15] = JobbLinje.DecX[15] + Lager.SvinnVerdi
    JobbLinje.DecX[16] = JobbLinje.DecX[16] + Lager.NedAnt
    JobbLinje.DecX[17] = JobbLinje.DecX[17] + Lager.NedVerdi
    JobbLinje.DecX[18] = JobbLinje.DecX[18] + Lager.VerdiSolgt
    JobbLinje.DecX[19] = JobbLinje.DecX[19] + Lager.BrekkVerdi
    JobbLinje.DecX[20] = JobbLinje.DecX[20] + Lager.KjopVerdi
    JobbLinje.DecX[21] = JobbLinje.DecX[21] + Lager.IntVerdi
    JobbLinje.DecX[22] = JobbLinje.DecX[22] + Lager.ReklVerdi
    JobbLinje.DecX[23] = JobbLinje.DecX[23] + Lager.ReklLVerdi
    JobbLinje.DecX[24] = JobbLinje.DecX[24] + Lager.GjenkjopVerdi
    JobbLinje.DecX[26] = JobbLinje.DecX[26] + Lager.LagAnt
    JobbLinje.DecX[25] = JobbLinje.DecX[ 4] / ((JobbLinje.DecX[10] + JobbLinje.DecX[26]) / 2) /* Oml.hast */
    JobbLinje.DecX[25] = IF JobbLinje.DecX[25] = ? THEN 0 ELSE JobbLinje.DecX[25]
    JobbLinje.DecX[30] = JobbLinje.DecX[30] + (Lager.VerdiSolgt - (Lager.VVarekost * Lager.AntSolgt)) /* DBKr */
    JobbLinje.DecX[31] = (JobbLinje.DecX[30] * 100) / JobbLinje.DecX[18] /* DB% */
    JobbLinje.DecX[31] = if JobbLinje.DecX[31] = ? then 0 else JobbLinje.DecX[31]
    JobbLinje.DecX[33] = (JobbLinje.DecX[ 4] / (JobbLinje.DecX[10] + JobbLinje.DecX[11])) * 100 /* Utsolgt% */
    JobbLinje.DecX[33] = if JobbLinje.DecX[33] = ? then 0 else JobbLinje.DecX[33]
    JobbLinje.DecX[36] = ArtBas.BildNr
    JobbLinje.DecX[37] = INT(ENTRY(2,ListeLinje.DataObjekt,","))
    JobbLinje.DecX[38] = wSortering
    JobbLinje.DecX[39] = wBrutto
    JobbLinje.DecX[40] = JobbLinje.DecX[40] + (Lager.LagAnt * ArtPris.Pris[if ArtPris.Tilbud then 2 else 1])
    JobbLinje.DecX[41] = ArtPris.Pris[if ArtPris.Tilbud then 2 else 1].

  /* Setter sortering ob brytgrupper. */
  if CAN-DO("151",STRING(wLayout)) then
    DO:
      assign
        JobbLinje.DivX[ 3] = if AVAILABLE VarGr
                               THEN VarGr.VgBeskr
                               ELSE "".
      RUN VgAkkum.
    END.
  /* Setter sortering ob brytgrupper. */
  ELSE if CAN-DO("152",STRING(wLayout)) then
    DO:
      assign
        JobbLinje.DivX[ 3] = if AVAILABLE LevBas
                               THEN LevBas.LevNamn
                               ELSE "".
      RUN LevAkkum.
    END.
  else
    RUN ArtBasAkkum.

END PROCEDURE.

PROCEDURE ArtBasAkkum:
  FIND HuvGr NO-LOCK WHERE
    HuvGr.Hg = ArtBas.Hg NO-ERROR.
  /* Sortering og brytgrupper */
  CASE wSortering:
    WHEN 1 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.Vg,"999999")
      JobbLinje.DivX[12] = STRING(INT(wLopNr),"999999")
      JobbLinje.DivX[13] = ""
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
      JobbLinje.DivX[11] = STRING(ArtBas.Vg,"999999")
      JobbLinje.DivX[12] = STRING(ArtBas.LevNr,"9999999")
      JobbLinje.DivX[13] = ArtBas.LevKod
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
      JobbLinje.DivX[19] = "Leverandør "  + (if AVAILABLE LevBas
                                            THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                            ELSE STRING(ArtBas.LevNr))
      JobbLinje.DivX[20] = ""
      JobbLinje.DivX[21] = "".
    WHEN 3 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.LevNr,"9999999")
      JobbLinje.DivX[12] = STRING(ArtBas.Vg,"999999")
      JobbLinje.DivX[13] = ArtBas.LevKod
      JobbLinje.DivX[14] = ""
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
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING(ArtBas.Vg,"999999")
      JobbLinje.DivX[12] = STRING(ArtBas.VgKat)
      JobbLinje.DivX[13] = STRING(ArtBas.LevNr,"9999999")
      JobbLinje.DivX[14] = ArtBas.LevKod
      JobbLinje.DivX[22] = ""
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
      JobbLinje.DivX[11] = STRING(9999999999.99 - JobbLinje.DecX[10],"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[10] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[26]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[26] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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

PROCEDURE VgAkkum:
  /* Sikrer at det er riktig hovedgruppe */
  FIND HuvGr NO-LOCK WHERE
    HuvGr.Hg = ArtBAs.Hg NO-ERROR.

  /* Sortering og brytgrupper */
  CASE wSortering:
    WHEN 1 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.Hg,"9999")
      JobbLinje.DivX[12] = STRING(ArtBas.Vg,"999999")
      JobbLinje.DivX[13] = ""
      JobbLinje.DivX[14] = ""
      /* Brytgruppetekster */
      JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                             THEN "Alle butikker"
                             else "Butikk " + (if available bufButiker
                                         then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                         ELSE "")
      JobbLinje.DivX[18] = "Hovedgruppe " + (if AVAILABLE HuvGr
                                            THEN STRING(ArtBas.Hg) + " " + HuvGr.HgBeskr
                                            ELSE STRING(ArtBas.Hg))
      JobbLinje.DivX[19] = ""
      JobbLinje.DivX[20] = ""
      JobbLinje.DivX[21] = "".
    WHEN 2 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.Hg,"9999")
      JobbLinje.DivX[12] = (if available VarGr
                              then VarGr.VgBeskr
                              ELSE "")
      JobbLinje.DivX[13] = ""
      JobbLinje.DivX[14] = ""
      /* Brytgruppetekster */
      JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                             THEN "Alle butikker"
                             else "Butikk " + (if available bufButiker
                                         then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                         ELSE "")
      JobbLinje.DivX[18] = "Hovedgruppe " + (if AVAILABLE HuvGr
                                            THEN STRING(ArtBas.Hg) + " " + HuvGr.HgBeskr
                                            ELSE STRING(ArtBas.Hg))
      JobbLinje.DivX[19] = ""
      JobbLinje.DivX[20] = ""
      JobbLinje.DivX[21] = "".
    WHEN 3 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.Vg,"999999")
      JobbLinje.DivX[12] = ""
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
    WHEN 4 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = (if AVAILABLE VarGr
                             THEN VarGr.VgBeskr
                             ELSE "")

      JobbLinje.DivX[12] = ""
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
    WHEN 7 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(9999999999.99 - JobbLinje.DecX[10],"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[10] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[26]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[26] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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

PROCEDURE LevAkkum:
  FIND HuvGr NO-LOCK WHERE
    HuvGr.Hg = ArtBas.Hg NO-ERROR.
  /* Sortering og brytgrupper */
  CASE wSortering:
    WHEN 1 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.HG,"9999")
      JobbLinje.DivX[12] = STRING(ArtBas.LevNr,"999999")
      JobbLinje.DivX[13] = ""
      JobbLinje.DivX[14] = ""
      /* Brytgruppetekster */
      JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                             THEN "Alle butikker"
                             else "Butikk " + (if available bufButiker
                                         then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                         ELSE "")
      JobbLinje.DivX[18] = "Hovedgruppe " + (if AVAILABLE HuvGr
                                            THEN STRING(ArtBas.Hg) + " " + HuvGr.HgBeskr
                                            ELSE STRING(ArtBas.Hg))
      JobbLinje.DivX[19] = ""
      JobbLinje.DivX[20] = ""
      JobbLinje.DivX[21] = "".
    WHEN 2 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.Hg,"9999")
      JobbLinje.DivX[12] = (if available LevBas
                              then LevBas.LevNamn
                              ELSE "")
      JobbLinje.DivX[13] = ""
      JobbLinje.DivX[14] = ""
      /* Brytgruppetekster */
      JobbLinje.DivX[17] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                             THEN "Alle butikker"
                             else "Butikk " + (if available bufButiker
                                         then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                         ELSE "")
      JobbLinje.DivX[18] = "Hovedgruppe " + (if AVAILABLE HuvGr
                                            THEN STRING(ArtBas.Hg) + " " + HuvGr.HgBeskr
                                            ELSE STRING(ArtBas.Hg))
      JobbLinje.DivX[19] = ""
      JobbLinje.DivX[20] = ""
      JobbLinje.DivX[21] = "".
    WHEN 3 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(ArtBas.LevNr,"999999")
      JobbLinje.DivX[12] = ""
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
    WHEN 4 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = (if AVAILABLE LevBas
                             THEN LevBas.LevNamn
                             ELSE "")

      JobbLinje.DivX[12] = ""
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
    WHEN 7 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING(9999999999.99 - JobbLinje.DecX[10],"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[10] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[26]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[26] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[18]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 12 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[18] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 13 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[30]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 14 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[30] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 15 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[31]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 16 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[31] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 17 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - JobbLinje.DecX[33]),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
    WHEN 18 THEN assign
      JobbLinje.DivX[10] = JobbLinje.Char1
      JobbLinje.DivX[11] = STRING((9999999999.99 - (JobbLinje.DecX[33] * -1)),"9999999999999.99")
      JobbLinje.DivX[12] = ""
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
