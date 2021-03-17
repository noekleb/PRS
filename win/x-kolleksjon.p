/************************************************************
    Program:  x-kolleksjon.p
    Created:  TN   19 Feb 99
Description:  Bygger jobblinje for kolleksjonsrapport.

Last change:  TN   26 Oct 100    9:22 am
************************************************************/

DEF INPUT  PARAMETER wJobbNr as INT  NO-UNDO.
DEF OUTPUT parameter wStatus as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wX1          as INT   NO-UNDO.
DEF VAR wX2          as INT   NO-UNDO.
DEF VAR wFolgeSeddler as LOG  NO-UNDO.
DEF VAR wInnlevRapp  as LOG   NO-UNDO.
DEF VAR wButikLoop   as INT   NO-UNDO.
DEF VAR wButikkListe as CHAR  NO-UNDO.
DEF VAR wBestHLevRec as RECID NO-UNDO.
DEF VAR wSisteEntry  as INT   NO-UNDO.
DEF VAR wForsteEntry as INT   NO-UNDO.
DEF VAR wTekst       as CHAR  NO-UNDO.
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
DEF VAR wChar1       as CHAR  NO-UNDO.
DEF VAR wChar2       as CHAR  NO-UNDO.
DEF VAR wChar3       as CHAR  NO-UNDO.
DEF VAR wAntISort    as INT   NO-UNDO.
DEF VAR wLeveringsNr AS INT   NO-UNDO.
DEF VAR wButListe    AS CHAR  NO-UNDO.

DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.


FUNCTION SisteEntry RETURNS INTEGER
  ( input ipListe     as CHAR,
    input ipDelimiter as char)  FORWARD.

FUNCTION ForsteEntry RETURNS INTEGER
  ( input ipListe     as CHAR,
    input ipDelimiter as char)  FORWARD.

{syspara.i 6 101 4   wTButik}
{syspara.i 6 101 20 wTTotalt}
{syspara.i 6 101 21 wTBestilling}
{syspara.i 6 101 22 wTLevert}
{syspara.i 6 101 23 wTRest}
{syspara.i 6 101 24 wTKolTot}
{syspara.i 6 101 25 wTAvskrevet}
{syspara.i 5 2   99 wStatListe}

DEF BUFFER bufBestLinje FOR BestLinje.
DEF BUFFER clButiker    FOR Butiker.
DEF BUFFER bJobbLinje   FOR JobbLinje.
DEF BUFFER bufBestHLev  FOR BestHLev.
DEF BUFFER bufButiker   FOR Butiker.

/* Temp-table for lagring av størrelsesmatrise. */
def temp-table tBestillt
  field Butik       as dec
  field Bestilltlst as char.

/* Temp-table for beregning av rest storrelsesmatrise. */
def temp-table trBestillt
  field Butik       as dec
  field Bestilltlst as char.

/* Setter sentrallager. */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK where
  clButiker.Butik = wCl NO-ERROR.
if NOT AVAILABLE clButiker then
  RETURN.
FIND PrisProfil NO-LOCK where
  PrisProfil.ProfilNr = clButiker.ProfilNr NO-ERROR.
if NOT AVAILABLE PrisProfil then
  RETURN.

{runlib.i}

assign
  wStatus = "AVBRYT".

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
  w2Loop       = 0
  wAntJLinjer  = 0
  wLayout      = INT(ENTRY(2,Jobb.Kriterier))
  wSortering   = INT(ENTRY(3,Jobb.Kriterier))
  wLeveringsNr = (IF CAN-DO("107,109",STRING(wLayout))
                    THEN INT(Lister.Kriterier[16])
                    ELSE 0)
  .

/* Setter styreparameter for innleveransrapporten når denne skal vise alle innleveranser, */
/* ikke bare den aktuelle.                                                                */
IF wLayout = 109 then
  DO:
    assign
      wLayout     = 107
      wInnlevRapp = TRUE.
  END.
ELSE
  wInnlevRapp = FALSE.

if CAN-DO("107",STRING(wLayout)) then
  DO:
    assign
      wBestHLevRec  = INT(entry(1,ENTRY(4,Jobb.Kriterier),"|")).
    if NUM-ENTRIES(ENTRY(4,Jobb.Kriterier),"|") > 1 then
      assign
        wFolgeSeddler = if INT(entry(2,ENTRY(4,Jobb.Kriterier),"|")) = 1
                          THEN true
                          ELSE FALSE.
    ELSE
      assign
        wFolgeseddler = FALSE.

    if wInnLevRapp
      then RUN ByggButikkListe2.
    else RUN ByggButikkListe.

    assign
      wButikLoop = 0.
    RUN ByggJobb.
    /* Skriver ut følgeseddler. */
    if wFolgeseddler then
    DO wButikLoop = 1 to NUM-ENTRIES(wButikkListe):
      FIND bufButiker NO-LOCK where
        bufButiker.Butik = INT(ENTRY(wButikLoop,wButikkListe)) NO-ERROR.
      RUN ByggJobb.
    END.
  END.
ELSE
  RUN ByggJobb.

assign
  wStatus = "OK".

/* -------------- Internprocedurer ----------------------- */
PROCEDURE ByggJobb:

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

  FIND BestHode NO-LOCK where
    BestHode.BestNr = INT(ENTRY(2,ListeLinje.DataObjekt,",")) NO-ERROR.
  if NOT AVAILABLE BestHode THEN
    NEXT BYGGJOBB.

  if BestHode.OrdreNr <> 0 then
    FIND Ordre  NO-LOCK where
      Ordre.OrdreNr = BestHode.OrdreNr NO-ERROR.
  FIND LevBas OF BestHode NO-LOCK NO-ERROR.
  FIND Valuta OF LevBas   NO-LOCK NO-ERROR.

  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType  OF ArtBas NO-LOCK NO-ERROR.

  FIND BestPris NO-LOCK where
    BestPris.BestNr   = BestHode.BestNr and
    BestPris.BestStat = BestHode.BestStat and
    BestPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
  if NOT AVAILABLE BestPris THEN
    NEXT BYGGJOBB.

  /* Henter butikken som er satt inn på listelinjen. */
  if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 then /* Ikke spes pr. butikk */
    FIND Butiker NO-LOCK where
      Butiker.Butik = wCl NO-ERROR.
  else /* Spesifisert pr. butikk. */
    FIND Butiker NO-LOCK where
      Butiker.Butik = INT(ENTRY(3,ListeLinje.DataObjekt,",")) NO-ERROR.

  /* Henter storrelseslisten fra det forste fri sortimentet. */
  /* Alle bestillinger har et slikt sortiment definert.      */
  FIND FIRST BestSort OF BestHode WHERE
    BestSort.Fri = YES NO-LOCK NO-ERROR.
  assign
    wStorrelser = if available BestSort
                    then BestSort.Storrelser
                    ELSE ""
    wListe      = ""
    wFordeling2 = if available BestSort
                    then BestSort.Fordeling
                    ELSE ""
    wSortId     = "FRI"
    wAntPar     = 0
    wAntSort    = 0
    wSkipFirst  = false.
  /* TN 27/4-00 Fiks av fordelingsfelt hvis dette er blankt. */
  if AVAILABLE BestSort then
    DO:
      if NUM-ENTRIES(BestSort.Storrelser," ") <> NUM-ENTRIES(BestSort.Fordeling," ") then
        DO:
          /* Setter opp en 0 maske for alle st›rrelser. */
          wFordeling2 = "".
          DO wX1 = 1 to NUM-ENTRIES(BestSort.Storrelser," "):
            wFordeling2 = wFordeling2 +
                          (if wFordeling2 = ""
                             THEN ""
                             ELSE " ") +
                          "0".
          END.
          /* Sumerer opp for hver enkelt st›rrelse. */
          FOR EACH BestStr NO-LOCK where
            BestStr.BestNr   = BestSort.BestNr and
            BestStr.BestStat = BestHode.BestStat:
            assign
              wIdx1 = LOOKUP(TRIM(BestStr.Storl),BestSort.Storrelser," ")
              wX2   = INT(ENTRY(wIdx1,wFordeling2," ")) + BestStr.Bestilt
              .
            ENTRY(wIdx1,wFordeling2," ") = STRING(wX2).
          END.
        END.
    END.

  /* Teller opp antall par p† raden. */
  do wLoop = 1 to num-entries(wStorrelser," "):
    if wLoop < num-entries(wStorrelser," ") then
      wListe = wListe + ";".
    wAntPar = wAntPar + INT(ENTRY(wLoop,wFordeling2," ")).
  end.

  /* Teller opp antall butikker som har bestilling. */
  ASSIGN
    wAntLinjer = 1 /* Teller for antall linjer i som skapes i Jobblinje for hver linje i ListeLinje. */
    wAntall    = 0
    wButSum2   = 0
    wButListe = "".
  FOR EACH BestStr NO-LOCK where
    BestSTr.BestNr = BestHode.BestNr and
    (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
       then BestStr.Butik > 0
       else BestStr.Butik = INT(ENTRY(3,ListeLinje.DataObjekt,","))) and
    BestStr.BestStat = BestHode.BestStat
    BREAK BY BestStr.BestNr
          By BestStr.Butik:

    /* Bygger liste med sum antall bestillt for butikken. */
    /* Benyttes kun for rapport 102 og 111.               */
    if lookup(trim(BestStr.Storl),wStorrelser," ") > 0 then
    assign
      wWork = INT(ENTRY(lookup(trim(BestStr.Storl),wStorrelser," "),wListe,";")) + BestStr.Bestilt
      ENTRY(lookup(trim(BestStr.Storl),wStorrelser," "), wListe, ";") = string(wWork).

    if FIRST-OF(BestStr.Butik) then
      assign wButSum = 0.

    /* Totalen. */
    assign wButSum = wButSum + BestStr.Bestilt.


    if last-OF(BestStr.Butik) then
    DO:
        IF NOT CAN-DO(wButListe,string(BestStr.Butik)) AND wButSum > 0 THEN
           wButListe = wButListe + 
                       (IF wButListe = ""
                          THEN ""
                          ELSE ",") + 
                       STRING(BestStr.Butik). /* lagt tillbaka */
      assign
        wAntall  = wAntall +
                   (if wButSum > 0
                     THEN 1
                     ELSE 0)
        wButSum2 = wButSum /* Kun for en butikk hvis det spes. pr butikk. */
        wButSum  = 0.
    END.
  END.

  /* Henter bilde. Finnes ikke bilde, benyttes blankt bilde. */
  FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
  if VALID-HANDLE(wLibHandle) then
    RUN HentBildePeker in wLibHandle
                       (input Bilderegister.BildNr,
                 IF CAN-DO("101,107,108",STRING(wLayout)) THEN 3 ELSE 1,
/*                         INPUT 1, */
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

  /* Fikser storrelsesgruppe og fordeling             */
  /* Tar bort ledende nuller og etterfolgende nuller. */
  if AVAILABLE BestSort then
    FRI:
    DO:
      /* Er det kun benyttet faste sortering, er dette feltet blankt.  */
      if BestSort.Fordeling = "" THEN
        LEAVE FRI.

      assign
        wIdx1 = 0
        wIdx2 = 0.
      LOOP1:
      DO wLoop = 1 TO NUM-ENTRIES(BestSort.Storrelser," "):
        if INT(ENTRY(wLoop,BestSort.Fordeling," ")) <> 0 then
          DO:
            wIdx1 = wLoop.
            LEAVE LOOP1.
          END.
      END. /* LOOP1 */
      LOOP2:
      DO wLoop = NUM-ENTRIES(BestSort.Storrelser," ") TO 1 by -1:
        if INT(ENTRY(wLoop,BestSort.Fordeling," ")) <> 0 then
          DO:
            wIdx2 = wLoop.
            LEAVE LOOP2.
          END.
      END. /* LOOP2 */

      if wIdx1 = 0 THEN wIdx1 = 1.
      if wIdx2 = 0 THEN wIdx2 = NUM-ENTRIES(BestSort.Fordeling," ").
      wStrGrp    = ENTRY(wIdx1,BestSort.Storrelser," ") + "-" + ENTRY(wIdx2,BestSort.Storrelser," ").
      /* Renser størrelseslisten */
      wStr2Liste = "".
      do wLoop = wIdx1 TO wIdx2:
        wStr2Liste = wStr2Liste +
                     (if wStr2Liste = ""
                       THEN ""
                       ELSE " ") +
                     ENTRY(wLoop,BestSort.Storrelser," ").
      END.
      /* Renser fordelingslisten */
      wFordeling = "".
      DO wLoop = wIdx1 TO wIdx2:
        wFordeling = wFordeling +
                     (if wFordeling = ""
                        THEN ""
                        ELSE " ") +
                     (if ENTRY(wLoop,wListe,";") = ""
                       then "0"
                       else ENTRY(wLoop,wListe,";")).
      END.
    END. /* FRI */

  /* Setter teksten p† brytgruppene. */
  if wLayout = 102 then
    DO:
      CASE wSortering:
        WHEN 1 then
               assign
                 wBryt1 = (if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + " " + SUBSTRING(VarGr.VgBesk,1,22)
                            ELSE STRING(ArtBas.Vg))
                 wBryt2 = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + LevBas.LevNamn
                            else STRING(BestHode.LevNr))
                 wBryt3 = STRING(BestHode.BestNr).
        WHEN 2 then
               assign
                 wBryt1 = (if AVAILABLE LevBas
/*                             THEN SUBSTRING(LevBas.LevNamn,1,22) + " " + STRING(LevBas.LevNr) */
                            THEN STRING(LevBas.LevNr) + " " + SUBSTRING(LevBas.LevNamn,1,22)
                            else STRING(BestHode.LevNr))
                 wBryt2 = STRING(BestHode.BestNr) + " " + BestHode.Merknad
                 wBryt3 = "".
        WHEN 3 then
               assign
                 wBryt1 = (if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + " " + SUBSTRING(VarGr.VgBesk,1,22)
                            ELSE STRING(ArtBas.Vg))
                 wBryt2 = (if AVAILABLE Kategori
                            THEN STRING(ArtBas.VgKat) + " " + Kategori.Beskrivelse
                            else STRING(ArtBas.VgKat))
                 wBryt3 = STRING(BestHode.BestNr) + " " + BestHode.Merknad.
        WHEN 4 then
               assign
                 wBryt1 = (if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + " " + SUBSTRING(VarGr.VgBesk,1,22)
                            ELSE STRING(ArtBas.Vg))
                 wBryt2 = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + LevBas.LevNamn
                            else STRING(BestHode.LevNr))
                 wBryt3 = STRING(BestHode.BestNr).
        WHEN 5 then
               assign
                 wBryt1 = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + SUBSTRING(LevBas.LevNamn,1,22)
                            else STRING(BestHode.LevNr))
                 wBryt2 = STRING(BestHode.BestNr) + " " + BestHode.Merknad
                 wBryt3 = "".
        WHEN 6 then
               assign
                 wBryt1 = (if AVAILABLE VarGr
                            THEN SUBSTRING(VarGr.VgBesk,1,22) + " " + STRING(VarGr.Vg)
                            ELSE STRING(ArtBas.Vg))
                 wBryt2 = (if AVAILABLE Kategori
                            THEN STRING(ArtBas.VgKat) + " " + Kategori.Beskrivelse
                            else STRING(ArtBas.VgKat))
                 wBryt3 = STRING(BestHode.BestNr) + " " + BestHode.Merknad.
      END CASE.
    END.
  else if wLayout = 111 then
    DO:
      CASE wSortering:
        WHEN 1 then
               assign
                 wBryt1 = "" /*(if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + " " + SUBSTRING(VarGr.VgBesk,1,22)
                            ELSE STRING(ArtBas.Vg))*/
                 wBryt2 = ""
                 wBryt3 = "".
      END CASE.
    END.

  /* Oppretter jobbrecord. */
  assign
    wSkriv      = true
    wAntJLinjer = wAntJLinjer + 1.
  RUN OpprettJobbLinje.

  /* Her legges det opp linjer for spesifikasjon av bestillt, levert og restantall. */
  if CAN-DO("101,107,108",STRING(wLayout)) then
    DO:
      if CAN-DO("101,108",STRING(wLayout)) then
        RUN Bestillt.
      if CAN-DO("107",STRING(wLayout)) then
        RUN Levert.
      /*
      RUN Avskrevet.
      RUN Rest.
      */
    END.

END. /* BYGGJOBB */
END PROCEDURE.

PROCEDURE OpprettJobbLinje:
  /* Slipper buffer (Hvis det skulle ligge igjen) */
  if AVAILABLE JobbLinje then
    RELEASE JobbLinje.

  CASE wLayout:
    /* Her opprettes pr varegruppe. */
    WHEN 111 then
      DO:
        assign
          wChar1 = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                               THEN STRING(wCl,"999999")
                               ELSE STRING(INT(ENTRY(3,ListeLinje.DataObjekt,",")),"999999")
          wChar2 = STRING(ArtBas.Vg,"9999") + "," +
                   " " + "," +
                   " " + "," +
                   " " + ","
          wChar3 = " " + "," + " ".
      END.
    /* Her opprettes pr. artikkel. */
    OTHERWISE
      DO:
        assign
          wChar1 = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                               THEN STRING(wCl,"999999")
                               ELSE STRING(INT(ENTRY(3,ListeLinje.DataObjekt,",")),"999999")
          wChar2 = STRING(ArtBas.Vg,"9999") + "," +
                   STRING(BestHode.LevNr,"999999999") + "," +
                        BestHode.LevKod + "," +
                        ENTRY(2,ListeLinje.DataObjekt,",") + /* For at det skal bli unikt! */
                          (if CAN-DO("101,102,107,108,111",STRING(wLayout))
                            then "," + STRING(wAntJLinjer + wBTeller)
                            ELSE " ")
          wChar3 = if (wLayout = 100 AND wSortering = 4) then
                     STRING(ArtBas.Vg,"9999") + "," + STRING(ArtBas.VgKat,"99") + "," + STRING(BestHode.LevNr,"9999999") + "," + ArtBas.LevKod
                   ELSE if (can-do("100,110",string(wLayout)) AND wSortering = 5) then
                                   STRING(ArtBas.Vg,"9999") + "," + STRING(ArtBas.VgKat,"99") + "," + STRING(ArtBas.Farg,"999")
                   else if (wLayout = 110 AND wSortering = 4) then
                     STRING(ArtBas.Vg,"9999") + "," + STRING(ArtBas.VgKat,"99") + "," + STRING(BestHode.LevNr,"9999999") + "," + ArtBas.LevKod
                   else
                     STRING(BestHode.LevNr,"999999999") + "," + BestHode.LevKod.
     END.
   END CASE.

  /* Henter posten hvis den skal hentes. */
  if CAN-DO("111",STRING(wLayout)) then
    DO:
      FIND JobbLinje EXCLUSIVE-LOCK where
        JobbLinje.JobbNr   = wJobbNr and
        JobbLinje.Char1    = wChar1  and
        JobbLinje.Char2    = wChar2  and
        JobbLinje.Char3    = wChar3 no-error.
    END.

  if NOT AVAILABLE JobbLinje then
    OPPSANDELSEN:
    DO:
      CREATE JobbLinje.

      assign
        JobbLinje.JobbNr   = wJobbNr
        JobbLinje.Char1    = wChar1
        JobbLinje.Char2    = wChar2
        JobbLinje.Char3    = wChar3.
    END. /* OPPSANDELSEN */
  
  assign
    JobbLinje.DivX[ 1] = STRING(ArtBas.ArtikkelNr)
    JobbLinje.DivX[ 2] = TRIM(STRING(BestHode.LevNr))
    JobbLinje.DivX[ 3] = TRIM(STRING(BestHode.LevKod)) + '/' + TRIM(ArtBas.Beskr)
    JobbLinje.DivX[ 4] = TRIM(BestHode.LevFargKod) + '/' + STRING(ArtBas.Farg) + ' ' + IF AVAILABLE Farg THEN TRIM(Farg.FarBeskr) ELSE ''
    JobbLinje.DivX[ 5] = STRING(ArtBas.VgKat)
    JobbLinje.DivX[ 6] = wFilNavn
    JobbLinje.DivX[ 7] = STRING(ArtBas.Vg)
    JobbLinje.DivX[ 8] = STRING(BestHode.BestNr) + " " + entry(BestHode.BestStat,wStatListe) +
                         (IF BestHode.SendtDato <> ?
                            THEN "  (Sendt: " + STRING(BestHode.SendtDato) + ")"
                            ELSE "")
    JobbLinje.DivX[ 9] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Butikk */
                          THEN STRING(wCl)
                          ELSE ENTRY(3,ListeLinje.DataObjekt,",")
    JobbLinje.DivX[10] = (if BestHode.LevDato <> ?
                           then STRING(BestHode.LevDato)
                           ELSE "")
    JobbLinje.DivX[11] = STRING(BestHode.Bestillingsdato)
    JobbLinje.DivX[12] = (if available Material
                            then STRING(Material.MatKod) + " " + Material.MatBeskr
                            ELSE "")
    JobbLinje.DivX[13] = wStrGrp
    JobbLinje.DivX[14] = wFordeling
    JobbLinje.DivX[15] = (if available StrType
                            THEN string(StrType.StrTypeId)+ "/" + StrType.KortNavn
                          ELSE "")
    JobbLinje.DivX[16] = wBryt2
    JobbLinje.DivX[17] = wBryt1
    JobbLinje.DivX[18] = (if CAN-DO("107",STRING(wLayout))
                            THEN STRING(wButikLoop,"99999999")
                          ELSE if (CAN-DO("111",string(wLayout)) AND INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0)
                            THEN STRING(ArtBas.Vg,"9999")
                          ELSE if (CAN-DO("111",string(wLayout)) AND INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0)
                            THEN STRING(wButikLoop,"99999999")
                          ELSE "") +
                         STRING(ArtBas.Vg,"9999")
    JobbLinje.DivX[19] = if (CAN-DO("111",string(wLayout)) AND INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0)
                            THEN STRING(wButikLoop,"99999999")
                         else if (CAN-DO("111",string(wLayout)) AND INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0)
                            THEN STRING(ArtBas.Vg,"9999")
                         else STRING(BestHode.LevNr,"999999")
    JobbLinje.DivX[20] = STRING(BestHode.BestNr,"99999999")
    JobbLinje.DivX[21] = wBryt3
    JobbLinje.DivX[22] = STRING(ArtBas.VgKat,"9999")
    JobbLinje.DivX[23] = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + LevBas.LevNamn
                            else STRING(BestHode.LevNr))
    JobbLinje.DivX[49] = wButListe
    JobbLinje.DivX[50] = if AVAILABLE bufButiker
                           THEN STRING(bufButiker.Butik) + "  "  + bufButiker.ButNamn
                           ELSE "".
  assign
    wLopNr = if ArtBas.LopNr = ? or ArtBas.LopNr = 0 then "?"
             ELSE STRING(ArtBas.LopNr)
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
    JobbLinje.DivX[28] = "" /* Flagger at artikkelinfo skal skrives. */
    JobbLinje.DivX[31] = STRING(ListeLinje.CellNr,"9999999999")
    JobbLinje.DivX[32] = if AVAILABLE BestHode
                           THEN STRING(BestHode.OrdreNr,"99999999")
                           ELSE "00000000"
    JobbLinje.DivX[33] = ENTRY(1,wBryt3)
    JobbLinje.DivX[34] = wStr2Liste
    JobbLinje.DivX[35] = wFordeling2
    JobbLinje.DivX[45] = TRIM(ArtBas.Beskr) 
    JobbLinje.DivX[46] = ArtBas.BongTekst
    .
  if CAN-DO("100,110",STRING(wLayout)) then
    DO:
      if CAN-DO("1,4",STRING(wSortering)) then
        JobbLinje.Div2X[1] = STRING(ArtBas.Vg) + "  " + STRING(VarGr.VgBeskr).
      ELSE if CAN-DO("2",string(wSortering)) then
        JobbLinje.Div2X[1] = STRING(ArtBAs.LEvNr) + "  " + STRING(LEvBas.LEvNamn).
      ELSE
        JobbLinje.Div2X[1] = "".
      assign
        JobbLinje.Div2X[2] = LevBas.LevNamn
        JobbLinje.Div2X[3] = if CAN-DO("1,2,4",STRING(wSortering))
                               THEN "1"
                               ELSE ""
        .
    END.

  if CAN-DO("101,107,108",STRING(wLayout)) then DO: /* Bestillingskort */
      FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
      
      cTxt = FILL(CHR(10),4).
      ii = 1.
      cTmp1 = TRIM(besthode.merknad).
      IF cTmp1 <> "" THEN DO i2 = 1 TO NUM-ENTRIES(cTmp1," "):
          IF LENGTH(cTmp2 + " " + ENTRY(i2,cTmp1," ")) < 33 AND i2 <> NUM-ENTRIES(cTmp1," ") THEN
              cTmp2 = cTmp2 + (IF cTmp2 <> "" THEN " " ELSE "") + ENTRY(i2,cTmp1," ").
          ELSE DO:
              IF LENGTH(cTmp2 + " " + ENTRY(i2,cTmp1," ")) < 33 AND i2 = NUM-ENTRIES(cTmp1," ") THEN DO:
                  cTmp2 = cTmp2 + (IF cTmp2 <> "" THEN " " ELSE "") + ENTRY(i2,cTmp1," ").
                  ENTRY(ii,cTxt,CHR(10)) = cTmp2.
                  ii = ii + 1.  
              END.
              ELSE DO:
                  ENTRY(ii,cTxt,CHR(10)) = cTmp2.
                  cTmp2 = ENTRY(i2,cTmp1," ").
                  ii = ii + 1.
                  IF i2 = NUM-ENTRIES(cTmp1," ") THEN DO:
                      IF ii < 6 THEN
                          ENTRY(ii,cTxt,CHR(10)) = cTmp2.
                  END.
              END.
          END.
          IF ii > 5 THEN
              LEAVE.
      END.
/*       ii = ii + 1.                                                                                           */
/*       IF ii < 6 AND TRIM(artbas.notat) <> "" THEN DO:                                                        */
/*           cTmp2 = "".                                                                                        */
/*           cTmp1 = "".                                                                                        */
/*           DO i2 = 1 TO NUM-ENTRIES(artbas.notat," ").                                                        */
/*               IF ENTRY(ii,artbas.notat," ") <> CHR(10) THEN                                                  */
/*                   cTmp1 = cTmp1 + (IF cTmp1 <> "" THEN " " ELSE "") + ENTRY(ii,artbas.notat," ").            */
/*           END.                                                                                               */
/*           cTmp1 = REPLACE(TRIM(artbas.notat),CHR(10)," ") .                                                  */
/*           IF cTmp1 <> "" THEN DO i2 = 1 TO NUM-ENTRIES(cTmp1," "):                                           */
/*               IF LENGTH(cTmp2 + " " + ENTRY(i2,cTmp1," ")) < 33 AND i2 <> NUM-ENTRIES(cTmp1," ") THEN        */
/*                   cTmp2 = cTmp2 + (IF cTmp2 <> "" THEN " " ELSE "") + ENTRY(i2,cTmp1," ").                   */
/*               ELSE DO:                                                                                       */
/*                   IF LENGTH(cTmp2 + " " + ENTRY(i2,cTmp1," ")) < 33 AND i2 = NUM-ENTRIES(cTmp1," ") THEN DO: */
/*                       cTmp2 = cTmp2 + (IF cTmp2 <> "" THEN " " ELSE "") + ENTRY(i2,cTmp1," ").               */
/*                       ENTRY(ii,cTxt,CHR(10)) = cTmp2.                                                        */
/*                       ii = ii + 1.                                                                           */
/*                   END.                                                                                       */
/*                   ELSE DO:                                                                                   */
/*                       ENTRY(ii,cTxt,CHR(10)) = cTmp2.                                                        */
/*                       cTmp2 = ENTRY(i2,cTmp1," ").                                                           */
/*                       ii = ii + 1.                                                                           */
/*                       IF i2 = NUM-ENTRIES(cTmp1," ") THEN DO:                                                */
/*                           IF ii < 6 THEN                                                                     */
/*                               ENTRY(ii,cTxt,CHR(10)) = cTmp2.                                                */
/*                       END.                                                                                   */
/*                   END.                                                                                       */
/*               END.                                                                                           */
/*               IF ii > 5 THEN                                                                                 */
/*                   LEAVE.                                                                                     */
/*           END.                                                                                               */
/*       END.                                                                                                   */

    assign
      JobbLinje.DivX[36] = STRING(ArtBas.Sasong) + " " + if AVAILABLE Sasong THEN Sasong.SasBeskr ELSE ""
      JobbLinje.DivX[37] = ENTRY(1,cTxt,CHR(10))
      JobbLinje.DivX[38] = ENTRY(2,cTxt,CHR(10))
      JobbLinje.DivX[39] = ENTRY(3,cTxt,CHR(10))
      JobbLinje.DivX[40] = ENTRY(4,cTxt,CHR(10))
      JobbLinje.DivX[41] = ENTRY(5,cTxt,CHR(10))
/*       JobbLinje.DivX[37] = ENTRY(1,ArtBas.Notat,CHR(10))                                                       */
/*       JobbLinje.DivX[38] = if NUM-ENTRIES(ArtBas.Notat,CHR(10)) > 1 then ENTRY(2,ArtBas.Notat,CHR(10)) ELSE "" */
/*       JobbLinje.DivX[39] = if NUM-ENTRIES(ArtBas.Notat,CHR(10)) > 2 then ENTRY(3,ArtBas.Notat,CHR(10)) ELSE "" */
/*       JobbLinje.DivX[40] = if NUM-ENTRIES(ArtBas.Notat,CHR(10)) > 3 then ENTRY(4,ArtBas.Notat,CHR(10)) ELSE "" */
/*       JobbLinje.DivX[41] = if NUM-ENTRIES(ArtBas.Notat,CHR(10)) > 4 then ENTRY(5,ArtBas.Notat,CHR(10)) ELSE "" */
      JobbLinje.DivX[42] = STRING(PrisProfil.ProfilNr) + " " +
                           PrisProfil.KortNavn
      JobbLinje.DivX[43] = if BestPris.EuroManuel
                             THEN "x"
                             ELSE " "
      JobbLinje.DivX[44] = if AVAILABLE Valuta
                             THEN Valuta.ValKod
                             ELSE ""
      .
    END.

  assign
    JobbLinje.DecX[ 1] = BestPris.InnkjopsPris
    JobbLinje.DecX[ 2] = BestPris.VareKost
    JobbLinje.DecX[ 3] = BestPris.DB%
    JobbLinje.DecX[ 4] = BestPris.Pris
    JobbLinje.DecX[ 5] = JobbLinje.DecX[ 5] +
                         (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                            then BestHode.TotAntPar
                         ELSE wButSum2)
    JobbLinje.DecX[50] = JobbLinje.DecX[50] +
                         (
                          BestPris.VareKost *
                          (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                             then BestHode.TotAntPar
                           ELSE wButSum2)
                         )
    JobbLinje.DecX[ 6] = BestHode.TotInnKjVerdi
    JobbLinje.DecX[ 7] = BestHode.TotSalgsVerdi
    JobbLinje.DecX[ 8] = BestPris.ValPris
    JobbLinje.DecX[ 9] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                          then wAntall
                          ELSE 1
    JobbLinje.DecX[10] = BestHode.BestNr
    JobbLinje.DecX[11] = BestHode.LevNr
    JobbLinje.DecX[12] = ArtBas.Vg
    JobbLinje.DecX[13] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Butikk */
                            THEN wCl
                            ELSE INT(STRING(INT(ENTRY(3,ListeLinje.DataObjekt,",")),"999999"))
    JobbLinje.DecX[22] = BestPris.Rab1Kr
    JobbLinje.DecX[23] = BestPris.Rab2Kr
    JobbLinje.DecX[24] = BestPris.Frakt
    JobbLinje.DecX[25] = BestPris.DivKostKr
    JobbLinje.DecX[26] = BestPris.Rab3Kr
    JobbLinje.DecX[27] = BestPris.DbKr
    JobbLinje.DecX[28] = BestPris.MvaKr
    JobbLinje.DecX[29] = BestPris.EuroPris

    JobbLinje.DecX[30] = BestPris.Rab1%
    JobbLinje.DecX[31] = BestPris.Rab2%
    JobbLinje.DecX[32] = BestPris.Frakt%
    JobbLinje.DecX[33] = BestPris.DivKost%
    JobbLinje.DecX[34] = BestPris.Rab3%
    JobbLinje.DecX[35] = BestPris.Mva%
    JobbLinje.DecX[36] = JobbLinje.DecX[36] +
                         (JobbLinje.DecX[ 2] * (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                                                   then BestHode.TotAntPar
                                                 ELSE wButSum2))

    /* Styrer sumlinje alle butikker.                                                */
    /* Layout 100 - Feltet brukes ikke.                                              */
    /* Layout 102 Sortering 1. Display sl†s av og p† avhengig av om butikk er valgt. */
    /* Layout 102 Sortering 2. Display er alltid avsl†tt.                            */
    JobbLinje.DecX[14] =  (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 AND (wSortering = 1)
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0 AND (wSortering = 1)
                             THEN 1
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 AND wSortering = 3
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0 AND wSortering = 3
                             THEN 1
                           ELSE 0)
    JobbLinje.DecX[14] = (if CAN-DO("111",STRING(wLayout))
                           THEN 0
                           ELSE JobbLinje.DecX[14])

    /* Styrer sumlinje pr ordrenummer.                                               */
    /* Layout 100 - Feltet brukes ikke.                                              */
    /* Layout 102 Sortering 1. Display sl†s alltid p†.                               */
    /* Layout 102 Sortering 2. Display sl†s av n†r det ikke er visning pr. butikk.   */
    JobbLinje.DecX[15] =  (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 AND (wSortering = 2)
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0 AND (wSortering = 2)
                             THEN 1
                           ELSE 1)
    JobbLinje.DecX[15] = (if CAN-DO("111",STRING(wLayout))
                           THEN 0
                           ELSE JobbLinje.DecX[15])

    JobbLinje.DecX[16] = if BestPRis.Rab1% = 0
                           then ?
                           else BestPris.Rab1%
    JobbLinje.DecX[17] = BestPris.Rab1Kr

    /* Styrer sumlinje pr leveringssted.                                             */
    JobbLinje.DecX[18] =  1 /*(if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0
                             THEN 1
                           ELSE 1)*/
    JobbLinje.DecX[19] = if wAntSort = 0
                           then ?
                           else wAntSort
    JobbLinje.DecX[20] = if wAntISort = 0
                           THEN ?
                           ELSE wAntISort
    JobbLinje.DecX[21] = if wSkriv = true
                           then 1
                           else 0

    JobbLinje.DecX[37] = (if CAN-DO("111",STRING(wLayout)) and INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                            THEN 0
                          ELSE if CAN-DO("111",STRING(wLayout)) and INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0
                            THEN 1
                          ELSE JobbLinje.DecX[37])
    .

  /* Fiffer opp posten for rapport 111 */
  if CAN-DO("111",STRING(wLayout)) then
    assign
      JobbLinje.DivX[ 3] = VarGr.VgBeskr
      JobbLinje.DecX[10] = VarGr.Vg.

  if CAN-DO("101,107,108",STRING(wLayout)) and wAntLinjer > 1 then
    DO:

      assign
        JobbLinje.DivX[28] = STRING(wAntLinjer,"999999")
        JobbLinje.DivX[29] = (if tBestillt.Butik = -5
                                then wTBestilling
                              else if tBestillt.Butik = -2 or tBestillt.Butik = -4 or tBestillt.Butik = -7
                                then " " /* Blank linje forran. */
                              else if tBestillt.Butik = -3
                                then wTRest
                              else if tBestillt.Butik = -1
                                then wTLevert
                              else if tBestillt.Butik = -6
                                then wTAvskrevet
                              else if tBestillt.Butik = 9999996
                                then wTRest
                              else if tBestillt.Butik = 0
                                THEN wTButik
                              else if tBestillt.Butik = 9999998
                                then " " /* Blank linje etter. */
                              ELSE if tBestillt.Butik = 9999999
                                THEN wTTotalt
                              else
                                STRING(tBestillt.Butik))
        JobbLinje.DivX[30] = tBestillt.BestilltLst.
    END.

END PROCEDURE.

/* Legger opp matrise for visning av bestilling */
PROCEDURE Bestillt:
def var wBestNr      as int  no-undo.
def var wAntStr      as int  no-undo.
def var wBestilltLst as char no-undo.
def var wStorrelser  as char no-undo.
def var wLoop        as int  no-undo.
def var wListe       as char no-undo.
def var wOListe      as char no-undo.
def var wNListe      as char no-undo.
DEF VAR wTotListe    as CHAR NO-UNDO.
DEF VAR wWork        as INT  NO-UNDO.
DEF VAR wtRecid      as RECID NO-UNDO.
DEF VAR wVerdiInnlev as DEC   NO-UNDO.

wBTeller = -5000.

/* Tømmer buffere */
for each tBestillt:  delete tBestillt.  end.
for each trBestillt: delete trBestillt. end.

/* Oppretter record for overskrift til bestilling. */
CREATE tBestillt.
  assign
    tBestillt.Butik       = -5
    tBestillt.BestilltLst = "         " + STRING(BestHode.BestillingsDato) + " " +
                            BestHode.BrukerId
    wtRecid               = RECID(tBestillt).

FIND FIRST BestSort OF BestHode WHERE
  BestSort.Fri = YES NO-LOCK NO-ERROR.

assign
  wStorrelser = BestSort.Storrelser.
do wLoop = 1 to num-entries(wStorrelser," "):
  if wLoop < num-entries(wStorrelser," ") then
    wListe = wListe + "    ;".
  else
    wListe = wListe + "    ".
end.
assign
  wOListe   = wListe
  wTotListe = wOListe.

/* Bygger record med størrelser. */
STORRELSER:
do:
  do wLoop = 1 to num-entries(BestSort.Storrelser," "):
    assign wAntStr = wAntStr + 1
    ENTRY(wLoop, wListe, ";") = fill(" ",4 - length(entry(wLoop,wStorrelser," "))) + 
                                entry(wLoop,wStorrelser," ").               
  end.
  wNListe = "".
  do wLoop = 1 to num-entries(wListe,";"):
    wNListe = wNListe + " " + entry(wLoop,wListe,";").
  end.
  create tBestillt.
  assign
    tBestillt.Butik       = 0.
    tBestillt.BestilltLst = wNListe + fill(" ",6 - LENGTH(wTKolTot)) + wTKolTot.
end. /* STORRELSER */

/* Leser alle butikkene */
for each BestLinje of BestHode no-lock:
  assign
    wAntStr   = 0
    wListe    = wOListe.

  /* Bygger liste med bestillt antall for butikken. */
  for each BestStr of BestLinje no-lock where
    BestSTr.BestStat = BestHode.BestStat:

    if lookup(trim(BestStr.Storl),wStorrelser," ") <> 0 then
    DO:
    assign wAntStr = wAntStr + 1
      ENTRY(lookup(trim(BestStr.Storl),wStorrelser," "), wListe, ";") = fill(" ",4 - length(string(BestStr.Bestilt))) +
                                string(BestStr.Bestilt).
    if Beststr.Bestilt > 0 then
      assign
        wWork   = INT(ENTRY(lookup(trim(BestStr.Storl),wStorrelser," "),wTotListe,";")) +
                  Beststr.Bestilt
        ENTRY(lookup(trim(BestStr.Storl),wStorrelser," "), wTotListe, ";") = fill(" ",4 - length(string(wWork))) +
                                  string(wWork).
    END.
    /* Her er det feil fra konvertering fra FlexiCon.
       Konvertering av ordre med faste sortiment, m† kontrolleres. TN 26/4-99
    else
      MESSAGE "BestStr.BestNr:" BestStr.BestNr skip
              "Storrelser:" wStorrelser skip
              "BestStr.Storl:" BestStr.Storl skip
              VIEW-AS ALERT-BOX.
    */

  end.

  /* Legger opp en linje på de butikker som har noe bestillt */
  if wAntStr > 0 or BestLinje.Butik = wCl then
    do:
      assign
        wWork    = 0
        wNListe  = ""
        wN2Liste = "".
      do wLoop = 1 to num-entries(wListe,";"):
        assign
          wNListe  = wNListe  + " " + entry(wLoop,wListe,";")
          wN2Liste = wN2Liste + ";" + TRIM(entry(wLoop,wListe,";"))
          wWork    = wWork    + INT(entry(wLoop,wListe,";")).
      end.
      
      create tBestillt.
      assign
        tBestillt.Butik       = BestLinje.Butik.
        tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).
        
      /* For beregning av restlaveranse. */
      create trBestillt.
      assign
        trBestillt.Butik       = BestLinje.Butik.
        trBestillt.BestilltLst = wN2Liste.
    end.
end.
  /* Linje som inneholder totalen */
  assign
    wWork   = 0
    wNListe = "".
  do wLoop = 1 to num-entries(wTotListe,";"):
    assign
      wNListe = wNListe + " " + entry(wLoop,wTotListe,";")
      wWork = wWork + INT(entry(wLoop,wTotListe,";")).
  end.

  /* Legger på sum på overskriftslinjen. */
  FIND tBestillt WHERE
    RECID(tBestillt) = wtRecid NO-ERROR.
  if AVAILABLE tBestillt then
    DO:
      {syspara.i 6 wLayout 27 wTekst} /*"Verdi Bestilling: "*/
      assign
        wVerdiInnlev = wWork * (if available BestPris
                                  then BestPris.VareKost
                                  else 0)
        tBestillt.BestilltLst = tBestillt.BestilltLst + "   " +
                                wTekst +
                                STRING(wVerdiInnlev,">>,>>>,>>9.99").
    END.

  create tBestillt.
  assign
    tBestillt.Butik       = 9999999
    tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork)
    wTotAntBest           = wWork.

/* Finner Første og siste entry som det står noe i. */
wSisteEntry  = SisteEntry(wTotListe,";").
wForsteEntry = ForsteEntry(wTotListe,";").

/* Oppretter JobbRecord som skal inneholde storrelses/fordelingslistene. */
for each tBestillt
  by tBestillt.Butik:

  assign
    wAntJLinjer = wAntJLinjer + 1
    wAntLinjer  = wAntLinjer  + 1.

  /* Stripper tomme entries i slutten av linjen. */
  if tBestillt.Butik >= 0 and wSisteEntry <> ? then
    assign
      tBestillt.BestilltLst = SUBSTRING(tBestillt.BestilltLst,1,wSisteEntry * 5) +
                                SUBSTRING(tBestillt.BestilltLst,LENGTH(tBestillt.BestilltLst) - 5).
  /* Stripper tomme entries i begynnelsen av linjen. */
  if tBestillt.Butik >= 0 and wForsteEntry <> ? then
    assign
      tBestillt.BestilltLst = SUBSTRING(tBestillt.BestilltLst,wForsteEntry * 5).

  RUN OpprettJobbLinje.
end.

END PROCEDURE.

/* Legger opp en matrise pr innleveranse. */
PROCEDURE Levert:
def var wBestNr      as int   no-undo.
def var wAntStr      as int   no-undo.
def var wBestilltLst as char  no-undo.
def var wStorrelser  as char  no-undo.
def var wLoop        as int   no-undo.
def var wListe       as char  no-undo.
def var wOListe      as char  no-undo.
def var wNListe      as char  no-undo.
DEF VAR wTotListe    as CHAR  NO-UNDO.
DEF VAR wWork        as INT   NO-UNDO.
DEF VAR wtRecid      as RECID NO-UNDO.
DEF VAR wVerdiInnlev as DEC   NO-UNDO.

wBTeller  = -4000.

/* Setter opp storrelser. */
FIND FIRST BestSort OF BestHode WHERE
  BestSort.Fri = YES NO-LOCK NO-ERROR.

assign
  wRestRecid  = ?
  wStorrelser = BestSort.Storrelser.
do wLoop = 1 to num-entries(wStorrelser," "):
  if wLoop < num-entries(wStorrelser," ") then
    wListe = wListe + "    ;".
  else
    wListe = wListe + "    ".
end.
assign
   wOListe = wListe.

/* Leser alle innleveranser.                             */
/* Det skrives ut en storrelsesmatrise pr. innleveranse. */
INNLEVERANSE:
FOR EACH BestHLev of BestHode NO-LOCK WHERE
  (IF wLeveringsNr <> 0 
     THEN BestHLev.LeveringsNr = wLeveringsNr
     ELSE TRUE)
  break by BestHLev.LevertDato
        by BestHLev.LeveringsNr:

  /* For disse rapportene skal kun AKTUEL leveranse skrives ut.  */
  /* For layout 109 settes wInnlevRapp = true.                   */
  if CAN-DO("107",STRING(wLayout)) and wInnlevRapp = false then
    DO:
      if RECID(BestHLev) <> wBestHLevRec then
        NEXT INNLEVERANSE.
    END.

  /* Tømmer buffer */
  for each tBestillt:
    delete tBestillt.
  end.
  assign
    wListe    = wOListe
    wTotListe = wOListe
    wBTeller  = wBTeller + 100.

  /* Oppretter blank record. */
  CREATE tBestillt.
    assign
      tBestillt.Butik       = -2
      tBestillt.BestilltLst = " ".

  /* Oppretter record for overskrift til leveranse. */
  CREATE tBestillt.
    assign
      tBestillt.Butik       = -1
      tBestillt.BestilltLst = "         " + STRING(BestHLev.LevertDato) + " " +
                              STRING(BestHLev.LevTidspunkt,"HH:MM:SS") + " " +
                              BestHLev.LevertAv
      wtRecid               = RECID(tBestillt).

  /* Bygger record med størrelser. */
  STORRELSER:
  do:
    do wLoop = 1 to num-entries(BestSort.Storrelser," "):
      assign wAntStr = wAntStr + 1
      ENTRY(wLoop, wListe, ";") = fill(" ",4 - length(entry(wLoop,wStorrelser," "))) +
                                entry(wLoop,wStorrelser," ").
    end.
    wNListe = "".
    do wLoop = 1 to num-entries(wListe,";"):
      wNListe = wNListe + " " + entry(wLoop,wListe,";").
    end.
    create tBestillt.
    assign
      tBestillt.Butik       = 0.
      tBestillt.BestilltLst = wNListe + fill(" ",6 - LENGTH(wTKolTot)) + wTKolTot.
  end. /* STORRELSER */

  /* Leser alle innleveranselinjene */
  BESTLEVERT:
  for each BestLevert no-lock where
    BestLevert.BestNr      = BestHLev.BestNr and
    BestLevert.LeveringsNr = BestHLev.LeveringsNr
    break by BestLevert.Butik
          by BestLevert.Storl:

    if FIRST-OF(BestLevert.Butik) then
      assign
        wAntStr = 0
        wListe  = wOListe.

    /* Ved spesifisering pr. butikk, skal kun linjer for den aktuelle butikken legges ut.        */
    /* bufferet bufButikk er ikke tilgjengelig på første siden - når alle butikker spesifiseres. */
    if CAN-DO("107",STRING(wLayout)) then
      DO:
        if AVAILABLE bufButiker then
          DO:
            if BestLevert.Butik <> bufButiker.Butik then
              NEXT BESTLEVERT.
          END.
      END.

    assign
      wAntStr = wAntStr + 1
      ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "), wListe, ";") = fill(" ",4 - length(string(BestLevert.Levert))) +
                                string(BestLevert.Levert).
    if BestLevert.Levert > 0 then
      assign
      wWork   = INT(ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "),wTotListe,";")) +
                INT(ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "),wListe,";"))

      ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "), wTotListe, ";") = fill(" ",4 - length(string(wWork))) +
                                string(wWork).

    /* Oppdaterer restleveansen */
    FIND trBestillt WHERE
      trBestillt.Butik = BestLevert.Butik NO-ERROR.
    if AVAILABLE trBestillt then
      assign
        wWork   = INT(ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "),trBestillt.BestilltLst,";")) - BestLevert.Levert
        ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "), trBestillt.BestilltLst, ";") = string(wWork).

    /* Oppretter recorden. */
    if LAST-OF(BestLevert.Butik) then
      DO:
        /* Legger opp en linje på de butikker som har noe bestillt */
        if wAntStr > 0 or BestLevert.Butik = wCl then
          do:
            assign
              wWork  = 0
              wNListe = "".
            do wLoop = 1 to num-entries(wListe,";"):
              assign
                wNListe = wNListe + " " + entry(wLoop,wListe,";")
                wWork   = wWork   + INT(entry(wLoop,wListe,";")).
            end.

            create tBestillt.
            assign
              tBestillt.Butik       = BestLevert.Butik.
              tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).
          end.
      END.
  end. /* BESTLEVERT */

  if LAST-OF(BestHLev.LeveringsNr) then
    DO:
      assign
        wRestRecid = RECID(BestHLev).

      /* Linje som inneholder totalen */
      assign
        wWork   = 0
        wNListe = "".

      do wLoop = 1 to num-entries(wTotListe,";"):
        assign
          wNListe = wNListe + " " + entry(wLoop,wTotListe,";")
          wWork   = wWork   + INT(entry(wLoop,wTotListe,";")).
      end.

      /* Legger på sum på overskriftslinjen. */
      FIND tBestillt WHERE
        RECID(tBestillt) = wtRecid NO-ERROR.
      if AVAILABLE tBestillt then
        DO:
          {syspara.i 6 wLayout 27 wTekst} /*"Verdi innleveranse: "*/
          assign
            wVerdiInnlev = wWork * (if available BestPris
                                      then BestPris.VareKost
                                      else 0)
            tBestillt.BestilltLst = tBestillt.BestilltLst + "   " +
                                    wTekst +
                                    STRING(wVerdiInnlev,">>,>>>,>>9.99").
        END.

      create tBestillt.
      assign
        tBestillt.Butik       = 9999999.
        tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).

      /* Oppretter JobbRecord som skal inneholde storrelses/fordelingslistene. */
      /* Innleveransegriden skrives kun ut n†r det er noe innlevert.           */
      if wWork <> 0 then
        DO:
          /* Finner Første og siste entry som det står noe i. */
          wSisteEntry  = SisteEntry(wTotListe,";").
          wForsteEntry = ForsteEntry(wTotListe,";").

          for each tBestillt
            by tBestillt.Butik:

            assign
              wAntJLinjer = wAntJLinjer + 1
              wAntLinjer  = wAntLinjer  + 1.

            /* Stripper tomme entries i slutten av linjen. */
            if tBestillt.Butik >= 0 and wSisteEntry <> ? then
              assign
                tBestillt.BestilltLst = SUBSTRING(tBestillt.BestilltLst,1,wSisteEntry * 5) +
                                          SUBSTRING(tBestillt.BestilltLst,LENGTH(tBestillt.BestilltLst) - 5).
            /* Stripper tomme entries i begynnelsen av linjen. */
            if tBestillt.Butik >= 0 and wForsteEntry <> ? then
              assign
                tBestillt.BestilltLst = SUBSTRING(tBestillt.BestilltLst,wForsteEntry * 5).

            RUN OpprettJobbLinje.
          end.
        END.
    END.
END. /* INNLEVERANSE */

END PROCEDURE.

/* Legger opp en matrise pr innleveranse med AVSKREVET. */
PROCEDURE Avskrevet:
def var wBestNr      as int  no-undo.
def var wAntStr      as int  no-undo.
def var wBestilltLst as char no-undo.
def var wStorrelser  as char no-undo.
def var wLoop        as int  no-undo.
def var wListe       as char no-undo.
def var wOListe      as char no-undo.
def var wNListe      as char no-undo.
DEF VAR wTotListe    as CHAR NO-UNDO.
DEF VAR wWork        as INT  NO-UNDO.

wBTeller  = -3000.

/* Setter opp storrelser. */
FIND FIRST BestSort OF BestHode WHERE
  BestSort.Fri = YES NO-LOCK NO-ERROR.

assign
  wRestRecid  = ?
  wStorrelser = BestSort.Storrelser.
do wLoop = 1 to num-entries(wStorrelser," "):
  if wLoop < num-entries(wStorrelser," ") then
    wListe = wListe + "    ;".
  else
    wListe = wListe + "    ".
end.
assign
   wOListe = wListe.

/* Leser alle innleveranser.                             */
/* Det skrives ut en storrelsesmatrise pr. innleveranse. */
INNLEVERANSE:
FOR EACH BestHLev of BestHode NO-LOCK WHERE
  (IF wLeveringsNr <> 0
     THEN BestHLev.LeveringsNr = wLeveringsNr
     ELSE TRUE)
  break by BestHLev.LevertDato
        by BestHLev.LeveringsNr:

  /* Tømmer buffer */
  for each tBestillt:
    delete tBestillt.
  end.
  assign
    wListe    = wOListe
    wTotListe = wOListe
    wBTeller  = wBTeller + 100.

  /* Oppretter blank record. */
  CREATE tBestillt.
    assign
      tBestillt.Butik       = -7
      tBestillt.BestilltLst = " ".

  /* Oppretter record for overskrift til leveranse. */
  CREATE tBestillt.
    assign
      tBestillt.Butik       = -6
      tBestillt.BestilltLst = "         " + STRING(BestHLev.LevertDato) + " " +
                              STRING(BestHLev.LevTidspunkt,"HH:MM:SS") + " " +
                              BestHLev.LevertAv.

  /* Bygger record med størrelser. */
  STORRELSER:
  do:
    do wLoop = 1 to num-entries(BestSort.Storrelser," "):
      assign wAntStr = wAntStr + 1
      ENTRY(wLoop, wListe, ";") = fill(" ",4 - length(entry(wLoop,wStorrelser," "))) +
                                entry(wLoop,wStorrelser," ").
    end.
    wNListe = "".
    do wLoop = 1 to num-entries(wListe,";"):
      wNListe = wNListe + " " + entry(wLoop,wListe,";").
    end.
    create tBestillt.
    assign
      tBestillt.Butik       = 0.
      tBestillt.BestilltLst = wNListe + fill(" ",6 - LENGTH(wTKolTot)) + wTKolTot.
  end. /* STORRELSER */

  /* Leser alle avskrevne innleveranselinjene */
  for each BestLevert no-lock where
    BestLevert.BestNr      = BestHLev.BestNr and
    BestLevert.LeveringsNr = BestHLev.LeveringsNr and
    BestLevert.Avskrevet   = true
    break by BestLevert.Butik
          by BestLevert.Storl:

    if FIRST-OF(BestLevert.Butik) then
      assign
        wAntStr = 0
        wListe  = wOListe.

    assign
      wAntStr = wAntStr + 1
      ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "), wListe, ";") = fill(" ",4 - length(string(BestLevert.Rest))) +
                                string(BestLevert.Rest).
    if BestLevert.Rest > 0 then
      assign
      wWork   = INT(ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "),wTotListe,";")) +
                INT(ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "),wListe,";"))

      ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "), wTotListe, ";") = fill(" ",4 - length(string(wWork))) +
                                string(wWork).

    /* Oppdaterer restleveansen */
    FIND trBestillt WHERE
      trBestillt.Butik = BestLevert.Butik NO-ERROR.
    if AVAILABLE trBestillt then
      assign
        wWork   = INT(ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "),trBestillt.BestilltLst,";")) - BestLevert.Rest.
        ENTRY(lookup(trim(BestLevert.Storl),wStorrelser," "), trBestillt.BestilltLst, ";") = string(wWork).

    /* Oppretter recorden. */
    if LAST-OF(BestLevert.Butik) then
      DO:
        /* Legger opp en linje på de butikker som har noe bestillt */
        if wAntStr > 0 or BestLevert.Butik = wCl then
          do:
            assign
              wWork  = 0
              wNListe = "".
            do wLoop = 1 to num-entries(wListe,";"):
              assign
                wNListe = wNListe + " " + entry(wLoop,wListe,";")
                wWork   = wWork   + INT(entry(wLoop,wListe,";")).
            end.

            create tBestillt.
            assign
              tBestillt.Butik       = BestLevert.Butik.
              tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).
          end.
      END.
  end.

  if LAST-OF(BestHLev.LeveringsNr) then
    DO:
      assign
        wRestRecid = RECID(BestHLev).

      /* Linje som inneholder totalen */
      assign
        wWork   = 0
        wNListe = "".

      do wLoop = 1 to num-entries(wTotListe,";"):
        assign
          wNListe = wNListe + " " + entry(wLoop,wTotListe,";")
          wWork   = wWork   + INT(entry(wLoop,wTotListe,";")).
      end.

      create tBestillt.
      assign
        tBestillt.Butik       = 9999999.
        tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).

      /* Oppretter JobbRecord som skal inneholde storrelses/fordelingslistene. */
      /* Griden skrives kun ut n†r det er noe som er avskrevet.                */
      if wWork <> 0 then
        for each tBestillt
          by tBestillt.Butik:

         assign
           wAntJLinjer = wAntJLinjer + 1
           wAntLinjer  = wAntLinjer  + 1.

          RUN OpprettJobbLinje.
        end.
    END.
END. /* INNLEVERANSE */

END PROCEDURE. /* Avskrevet */

/* Legger opp en matrise som viser restleveransen. */
PROCEDURE Rest:
def var wBestNr      as int  no-undo.
def var wAntStr      as int  no-undo.
def var wBestilltLst as char no-undo.
def var wStorrelser  as char no-undo.
def var wLoop        as int  no-undo.
def var wListe       as char no-undo.
def var wOListe      as char no-undo.
def var wNListe      as char no-undo.
DEF VAR wTotListe    as CHAR NO-UNDO.
DEF VAR wWork        as INT  NO-UNDO.
DEF VAR w2Work       as INT  NO-UNDO.

wBTeller = -1000.

/* Tømmer buffer */
for each tBestillt: delete tBestillt. end.

/* Oppretter blank record. */
CREATE tBestillt.
  assign
    tBestillt.Butik       = -4
    tBestillt.BestilltLst = " ".

/* Oppretter record for overskrift til restleveranse. */
CREATE tBestillt.
  assign
    tBestillt.Butik       = -3
    tBestillt.BestilltLst = "         " + STRING(today) + " " +
                            STRING(time,"HH:MM:SS") + " " +
                            USERID("dictdb").

/* Setter opp storrelser. */
FIND FIRST BestSort OF BestHode WHERE
  BestSort.Fri = YES NO-LOCK NO-ERROR.

assign
  wStorrelser = BestSort.Storrelser.
do wLoop = 1 to num-entries(wStorrelser," "):
  if wLoop < num-entries(wStorrelser," ") then
    wListe = wListe + "    ;".
  else
    wListe = wListe + "    ".
end.
assign
  wOListe  = wListe.
  wN2Liste = wListe.

  /* Bygger record med størrelser. */
  STORRELSER:
  do:
    do wLoop = 1 to num-entries(BestSort.Storrelser," "):
      assign wAntStr = wAntStr + 1
      ENTRY(wLoop, wListe, ";") = fill(" ",4 - length(entry(wLoop,wStorrelser," "))) +
                                entry(wLoop,wStorrelser," ").
    end.
    wNListe = "".
    do wLoop = 1 to num-entries(wListe,";"):
      wNListe = wNListe + " " + entry(wLoop,wListe,";").
    end.
    create tBestillt.
    assign
      tBestillt.Butik       = 0.
      tBestillt.BestilltLst = wNListe + fill(" ",6 - LENGTH(wTKolTot)) + wTKolTot.
  end. /* STORRELSER */

/* Oppretter poster. */
FOR EACH trBestillt:
  assign
    wWork    = 0
    wNListe  = "".
  do wLoop = 1 to num-entries(trBestillt.BestilltLst,";"):
    assign
      wNListe = wNListe  + fill(" ",5 - length(entry(wLoop,trBestillt.BestilltLst,";"))) + entry(wLoop,trBestillt.BestilltLst,";")
      wWork   = wWork    + INT(entry(wLoop,trBestillt.BestilltLst,";")).
    if INT(ENTRY(wLoop,trBestillt.BestilltLst,";")) <> 0 then
      assign
        w2Work  = INT(ENTRY(wLoop,wN2Liste,";")) + INT(ENTRY(wLoop,trBestillt.BestilltLst,";"))
        ENTRY(wLoop,wN2Liste,";") = string(w2Work).
  end.

  /* Legger opp linjer. Tomme linjer undertrykkes */
  if wWork <> 0 then
    DO:
      CREATE tBestillt.
      assign
        tBestillt.Butik       = trBestillt.Butik
        tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).
    END.
END.

/* Linje som inneholder totalen */
assign
wWork   = 0
wNListe = "".

do wLoop = 1 to num-entries(wN2Liste,";"):
  assign
    wNListe = wNListe + fill(" ",5 - length(TRIM(entry(wLoop,wN2Liste,";")))) + TRIM(entry(wLoop,wN2Liste,";"))
    wWork   = wWork   + INT(entry(wLoop,wN2Liste,";")).
end.

create tBestillt.
assign
  tBestillt.Butik       = 9999999.
  tBestillt.BestilltLst = wNListe + fill(" ",6 - length(string(wWork))) + STRING(wWork).

/* Oppretter JobbRecord som skal inneholde storrelses/fordelingslistene.    */
/* Skrives ikke ut hvis rest er null eller rest lik opprindelig bestilling. */
if (wTotAntBest <> wWork) and (wWork <> 0) then
  for each tBestillt
    by tBestillt.Butik:

    assign
      wAntJLinjer = wAntJLinjer + 1
      wAntLinjer  = wAntLinjer  + 1.

    RUN OpprettJobbLinje.
  end.

END PROCEDURE.

PROCEDURE RensListe:
  DEF INPUT-OUTPUT PARAMETER wStrListe  as CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wFordeling as CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wStrGrp    as CHAR NO-UNDO.

  DEF var w2StrListe  as CHAR NO-UNDO.
  DEF var w2Fordeling as CHAR NO-UNDO.

  DEF VAR wIdx1 as INT NO-UNDO.
  DEF VAR wIdx2 as INT NO-UNDO.
  DEF VAR wLoop as INT NO-UNDO.

  assign
    wIdx1 = 0
    wIdx2 = 0.
  LOOP1:
  DO wLoop = 1 TO NUM-ENTRIES(wStorrelser," "):
    if INT(ENTRY(wLoop,wFordeling," ")) <> 0 then
      DO:
        wIdx1 = wLoop.
        LEAVE LOOP1.
      END.
  END. /* LOOP1 */
  LOOP2:
  DO wLoop = NUM-ENTRIES(wStorrelser," ") TO 1 by -1:
    if INT(ENTRY(wLoop,wFordeling," ")) <> 0 then
      DO:
        wIdx2 = wLoop.
        LEAVE LOOP2.
      END.
  END. /* LOOP2 */
  if wIdx1 = 0 THEN wIdx1 = 1.
  if wIdx2 = 0 THEN wIdx2 = NUM-ENTRIES(wFordeling," ").
  wStrGrp    = ENTRY(wIdx1,wStrListe," ") + "-" + ENTRY(wIdx2,wStrListe," ").
  /* Renser størrelseslisten */
  w2StrListe = "".
  do wLoop = wIdx1 TO wIdx2:
    w2StrListe = w2StrListe +
                (if w2StrListe = ""
                  THEN ""
                  ELSE " ") +
                ENTRY(wLoop,wStrListe," ").
  END.
  /* Renser fordelingslisten */
  w2Fordeling = "".
  DO wLoop = wIdx1 TO wIdx2:
    w2Fordeling = w2Fordeling +
                 (if w2Fordeling = ""
                    THEN ""
                    ELSE " ") +
                 (if ENTRY(wLoop,wFordeling," ") = ""
                   then "0"
                   else ENTRY(wLoop,wFordeling," ")).
  END.
  assign
    wFordeling = w2Fordeling
    wStrListe  = w2StrListe.
END procedure.

/* -------------- Funksjoner ----------------- */
FUNCTION SisteEntry RETURNS INTEGER
  ( input ipListe     as CHAR,
    input ipDelimiter as char) :

  DEF VAR ipSisteEntry as INT NO-UNDO.
  DEF VAR ipLoop       as INT NO-UNDO.

  LOOPEN:
  DO ipLoop = NUM-ENTRIES(ipListe,ipDelimiter) to 1 by -1:
    if INT(ENTRY(ipLoop,ipListe,ipDelimiter)) <> 0 then
      DO:
        ipSisteEntry = ipLoop.
        LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

  /* Sjekker om det er et gyldig resultat. */
  if ipSisteEntry = 1 or
     ipSisteEntry = NUM-ENTRIES(ipListe,ipDelimiter) then
     ipSisteEntry = ?.

  RETURN ipSisteEntry.   /* Function return value. */

END FUNCTION.

FUNCTION ForsteEntry RETURNS INTEGER
  ( input ipListe     as CHAR,
    input ipDelimiter as char) :

  DEF VAR ipForsteEntry as INT NO-UNDO.
  DEF VAR ipLoop        as INT NO-UNDO.

  LOOPEN:
  DO ipLoop = 1 to NUM-ENTRIES(ipListe,ipDelimiter):
    if INT(ENTRY(ipLoop,ipListe,ipDelimiter)) <> 0 then
      DO:
        ipForsteEntry = ipLoop - 1.
        LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

  /* Sjekker om det er et gyldig resultat. */
  if ipForsteEntry <= 1 then
     ipForsteEntry = ?.

  RETURN ipForsteEntry.   /* Function return value. */

END FUNCTION.

PROCEDURE ByggButikkListe:
  FIND BestHLev NO-LOCK where
    RECID(BestHLev) = wBestHLevRec NO-ERROR.

  if AVAILABLE BestHLev then
  BESTLEV:
  FOR EACH BestLevert no-lock where
    BestLevert.BestNr = BestHLev.BestNr and
    BestLevert.LeveringsNr = BestHLev.LeveringsNr:

    /* Sentrallager skal ikke med. */
    if BestLevert.Butik = wCl then
      NEXT BESTLEV.

    if not CAN-DO(wButikkListe,STRING(BestLevert.Butik)) then
      assign
        wButikkListe = wButikkListe +
                       (if wButikkListe = ""
                          THEN ""
                          ELSE ",") +
                       STRING(BestLevert.Butik).
  END. /* BESTLEV */
END PROCEDURE.

PROCEDURE ByggButikkListe2:
  FIND FIRST ListeLinje OF Lister NO-LOCK.
  FIND BestHode NO-LOCK where
    BestHode.BestNr = INT(ENTRY(2,ListeLinje.DataObjekt,",")) NO-ERROR.

  BESTLEV:
  FOR EACH BestLevert no-lock where
    BestLevert.BestNr = BestHode.BestNr:

    /* Sentrallager skal ikke med. */
    if BestLevert.Butik = wCl then
      NEXT BESTLEV.

    if not CAN-DO(wButikkListe,STRING(BestLevert.Butik)) then
      assign
        wButikkListe = wButikkListe +
                       (if wButikkListe = ""
                          THEN ""
                          ELSE ",") +
                       STRING(BestLevert.Butik).
  END. /* BESTLEV */
END PROCEDURE.
