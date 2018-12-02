/************************************************************
    Program:  x-kundekonto.p
    Created:  TN   10 Des 01
Description:  Bygger jobblinje for Kundesaldo rapporten.

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
DEF VAR wTTIdListe   AS CHAR  NO-UNDO.
DEF VAR pcTekst      AS CHAR                     NO-UNDO.
DEF VAR pdFraDato    AS DATE FORMAT "99/99/99"   NO-UNDO.
DEF VAR pdTilDato    AS DATE FORMAT "99/99/99"   NO-UNDO.
DEF VAR pbKredit     AS LOG  NO-UNDO.

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
  pcTekst     = ENTRY(8,Jobb.Kriterier)
  pdFraDato   = DATE(ENTRY(1,pcTekst,"|"))
  pdTilDato   = DATE(ENTRY(2,pcTekst,"|"))
  .


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
PROCEDURE ByggDataSett:
DEF VAR wOk as LOG NO-UNDO.

ASSIGN wLoop = 0.
if valid-handle(wParentHandle) then
  RUN VisProgressBar in wParentHandle.

BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:
    assign
      wLoop         = wLoop + 1
        wAntJLinjer = 1
      .
    if valid-handle(wParentHandle) AND wLoop modulo 5 = 0  then
      RUN FremdriftProgressBar in wParentHandle (INPUT wLoop).

  FIND Kunde NO-LOCK where
    Kunde.KundeNr = int(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE Kunde THEN
    NEXT BYGGJOBB.

  FIND KundeGruppe OF Kunde NO-LOCK NO-ERROR.
  FIND KundeType   OF Kunde NO-LOCK NO-ERROR.

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
  /* Legger opp en post kundetrans. */
  KUNDETRANS:
  FOR EACH KundeTrans OF Kunde NO-LOCK WHERE
      KundeTrans.Dato >= pdFraDato AND
      KundeTrans.DAto <= pdTilDato AND 
      KundeTrans.MotPostert = FALSE:
      assign
        wSkriv      = true
        wAntJLinjer = wAntJLinjer + 1
        wOk         = true
        pbKredit    = FALSE
        .
      RUN OpprettJobbLinje(KundeTrans.TTId,
                           KundeTrans.Dato,
                           KundeTrans.Tid,
                           KundeTrans.Butik,
                           KundeTrans.KassaNr,
                           KundeTrans.BongId,
                           KundeTrans.BongLinjeNr,
                           KundeTrans.Vg,
                           KundeTrans.LopNr,
                           KundeTrans.Storl,
                           KundeTrans.Antall,
                           (KundeTrans.Pris - KundeTrans.RabKr - KundeTrans.SubTotalRab) /* * KundeTrans.Antall */,
                           (IF pbKredit
                              THEN "*"
                              ELSE ""),
                          KundeTrans.Bongtekst).
  END. /* KUNDETRANS */
  /* Legger opp en post for salg av gavekort. */
  KUNDETRANS:
  FOR EACH KundeBetTrans OF Kunde NO-LOCK WHERE
      KundeBetTrans.Dato >= pdFraDato AND
      KundeBetTrans.Dato <= pdTilDato AND 
      KundeBetTrans.TTID  = 134 AND
      KundeBetTrans.MotPostert = TRUE:
      assign
        wSkriv      = true
        wAntJLinjer = wAntJLinjer + 1
        wOk         = true
        pbKredit    = FALSE
        .
      RUN OpprettJobbLinje(KundeBetTrans.TTId,
                           KundeBetTrans.Dato,
                           KundeBetTrans.Tid,
                           KundeBetTrans.Butik,
                           KundeBetTrans.KassaNr,
                           KundeBetTrans.BongId,
                           KundeBetTrans.BongLinjeNr,
                           0,
                           0,
                           ".",
                           1,
                           KundeBetTrans.Belop,
                           (IF pbKredit
                              THEN "*"
                              ELSE "")
                           ,
                           "").
  END. /* KUNDETRANS */

  /* Legger opp en post kundeBetTrans. */
  KUNDEBETTRANS:
  FOR EACH KundeBetTrans OF Kunde NO-LOCK WHERE
      KundeBetTrans.Dato >= pdFraDato AND
      KundeBetTrans.DAto <= pdTilDato AND
      KundeBetTrans.MotPostert = FALSE:
      assign
        wSkriv      = true
        wAntJLinjer = wAntJLinjer + 1
        wOk         = true
        .
      /* Utbetaling, innbetaling, kreditkjøp og rekvisisjonskjøp teller ikke med i saldo */
      IF CAN-DO("055,062,065",STRING(KundeBetTrans.TTId,"999")) THEN
          pbKredit = TRUE.
      ELSE
          pbKredit = FALSE.

      RUN OpprettJobbLinje(KundeBetTrans.TTId,
                           KundeBetTrans.Dato,
                           KundeBetTrans.Tid,
                           KundeBetTrans.Butik,
                           KundeBetTrans.KassaNr,
                           KundeBetTrans.BongId,
                           KundeBetTrans.BongLinjeNr,
                           0,
                           0,
                           "",
                           0,
                           KundeBetTrans.Belop * IF KundeBetTrans.TTId = 70 THEN -1 ELSE 1,
                           (IF pbKredit
                              THEN "*"
                              ELSE ""),
                           ""
                          ).
  END. /* KUNDEBETTRANS */
  /* Legger opp en post kundeBetTrans. */
  KUNDEBETTRANS:
  FOR EACH KundeReskontr NO-LOCK WHERE 
      Kundereskontr.KundeNr = Kunde.KundeNr AND
      Kundereskontr.Bilagstype >= 3 AND
      Kundereskontr.Bilagstype <= 4 AND
      KundeReskontr.FakturertDato >= pdFraDato AND
      KundeReskontr.FakturertDAto <= pdTilDato AND
      Kundereskontr.B_Id = 0 AND
      Kundereskontr.BongLinjeNr = 0:
      assign
        wSkriv      = true
        wAntJLinjer = wAntJLinjer + 1
        wOk         = true
        .
      RUN OpprettJobbLinje(89,
                           KundeReskontr.FakturertDato,
                           0,
                           Kunde.Butik,
                           0,
                           0,
                           0,
                           0,
                           0,
                           "",
                           0,
                           KundeReskontr.Belop * -1,
                           "",
                           ""
                          ).
  END. /* KUNDEBETTRANS */
END. /* BYGGJOBB */
end PROCEDURE. /* ByggDataSett */

PROCEDURE OpprettJobbLinje:
  DEF INPUT PARAMETER piTTId        AS INT  NO-UNDO.
  DEF INPUT PARAMETER pdDato        AS DATE NO-UNDO.
  DEF INPUT PARAMETER piTid         AS INT  NO-UNDO.
  DEF INPUT PARAMETER piButikkNr    AS INT  NO-UNDO.
  DEF INPUT PARAMETER piKassaNr     AS INT  NO-UNDO.
  DEF INPUT PARAMETER piBongId      AS INT  NO-UNDO.
  DEF INPUT PARAMETER piBongLinjeNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER piVg          AS INT  NO-UNDO.                    
  DEF INPUT PARAMETER piLopNr       AS INT  NO-UNDO.                 
  DEF INPUT PARAMETER pcStorl       AS CHAR NO-UNDO.                 
  DEF INPUT PARAMETER piAntall      AS INT  NO-UNDO.                
  DEF INPUT PARAMETER plPris        AS DEC  NO-UNDO.
  DEF INPUT PARAMETER pcKredit      AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcBongTekst   AS CHAR NO-UNDO.

  FIND TransType NO-LOCK WHERE
      TransType.TTId = piTTId NO-ERROR.

  IF AVAILABLE ArtBas THEN
      RELEASE ArtBas.
  FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg = piVg AND
      ArtBas.LopNr = piLopNr NO-ERROR.

  CREATE JobbLinje.

  assign
    JobbLinje.JobbNr   = wJobbNr
    JobbLinje.Char1    = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                           THEN STRING(wCl,"999999")
                           ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
    JobbLinje.Char2    = STRING(Kunde.KundeNr,"9999999999999")
    JobbLinje.Char3    = STRING(wAntJLinjer)
    JobbLinje.DivX[ 1] = STRING(Kunde.KundeNr)
    JobbLinje.DivX[ 2] = Kunde.Navn
    JobbLinje.DivX[ 3] = Kunde.Adresse1
    JobbLinje.DivX[ 4] = Kunde.Adresse2
    JobbLinje.DivX[ 6] = Kunde.PostNr + " " + 
                               (IF AVAILABLE Post
                                  THEN Post.Beskrivelse
                                  ELSE "")
    JobbLinje.DivX[ 7] = IF AVAILABLE ArtBas
                           THEN ArtBas.Beskr
                         ELSE IF AVAILABLE TransType
                           THEN TransType.Beskrivelse
                         ELSE
                             "*Ukjent"
    JobbLinje.DivX[ 7] = IF JobbLinje.DivX[ 7] = ""
                                 THEN pcBongTekst
                         ELSE JobbLinje.DivX[ 7]
    JobbLinje.DivX[14] = pcKredit
    JobbLinje.DivX[15] = STRING(piTTId)
    JobbLinje.DivX[16] = IF AVAILABLE TransType
                               THEN TransType.Beskrivelse
                               ELSE "* Ukjent *"
    JobbLinje.DivX[17] = string(pdDato,"99/99/99")
    JobbLinje.DivX[18] = string(piTid,"HH:MM")
    JobbLinje.DivX[20] = string(piButikkNr)
    JobbLinje.DivX[21] = STRING(piKassaNr)
    JobbLinje.DivX[22] = STRING(piBongId)
    JobbLinje.DivX[23] = STRING(piBongLinjeNr,"999999")
    .
  IF piTTId < 50 OR piTTID = 134 /* Gavekort ut */ THEN
  ASSIGN
    JobbLinje.DivX[24] = STRING(piVg)
    JobbLinje.DivX[25] = STRING(piLopNr)
    JobbLinje.DivX[26] = STRING(pcStorl)
    JobbLinje.DivX[27] = STRING(piAntall)
    JobbLinje.DivX[28] = STRING(plPris)
    .
  ELSE 
  ASSIGN
    JobbLinje.DivX[24] = " "
    JobbLinje.DivX[25] = " " 
    JobbLinje.DivX[26] = " "
    JobbLinje.DivX[27] = " "
    JobbLinje.DivX[28] = STRING(plPris)
    .

    FIND bufButiker NO-LOCK where
      bufButiker.butik = INT(JobbLinje.Char1) NO-ERROR.


    /* Sortering og brytgrupper */
    CASE wSortering:
      WHEN 1 THEN assign
        JobbLinje.DivX[40] = JobbLinje.Char1
        JobbLinje.DivX[41] = STRING(Kunde.KundeNr,"9999999999999")
        JobbLinje.DivX[42] = STRING(wAntJLinjer)
        JobbLinje.DivX[43] = ""
        JobbLinje.DivX[44] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[47] = if INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               else "Butikk " + (if available bufButiker
                                           then STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[48] = ""
        JobbLinje.DivX[49] = ""
        JobbLinje.DivX[50] = ""
        JobbLinje.DivX[39] = "".
    END CASE.
END PROCEDURE.

