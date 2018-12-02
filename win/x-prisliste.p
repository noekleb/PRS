/************************************************************
    Program:  x-prisliste.p
    Created:  TN   19 Feb 99
Description:  Bygger jobblinje for prislistene.

Last change:  TN   29 Jun 100    2:51 pm
************************************************************/

DEF INPUT  PARAMETER wJobbNr       AS INT  NO-UNDO.
DEF OUTPUT PARAMETER wStatus       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle AS HANDLE NO-UNDO.

DEF VAR wByggLinjeHandle AS HANDLE NO-UNDO.

DEF VAR wJobbLinjeRecid AS RECID NO-UNDO.

DEF VAR wSjekkLAger  AS INT   NO-UNDO.
DEF VAR wStrListe    AS CHAR  NO-UNDO.
DEF VAR wTotListe    AS CHAR  NO-UNDO.
DEF VAR w2TotListe   AS CHAR  NO-UNDO.
DEF VAR wTekst       AS CHAR  NO-UNDO.
DEF VAR wLinje       AS CHAR  NO-UNDO.
DEF VAR wButListe    AS CHAR  NO-UNDO.
DEF VAR wProfilNr    AS INT   NO-UNDO.
DEF VAR wButNavn     AS CHAR  NO-UNDO.
DEF VAR wButTekst    AS CHAR  NO-UNDO.
DEF VAR wSumVerdi    AS INT   NO-UNDO.
DEF VAR wSumAntall   AS INT   NO-UNDO.
DEF VAR wvVareKost   AS DEC   NO-UNDO.
DEF VAR wAntLager    AS INT   NO-UNDO.
DEF VAR wAntSolgt    AS INT   NO-UNDO.
DEF VAR wAntKjop     AS INT   NO-UNDO.
DEF VAR wUts%        AS INT   NO-UNDO.
DEF VAR wSkipFirst   AS LOG   NO-UNDO.
DEF VAR wAdrTekst    AS CHAR  NO-UNDO.
DEF VAR wLevTekst    AS CHAR  NO-UNDO.
DEF VAR wLeverandor  AS CHAR  NO-UNDO.
DEF VAR wLevAdr      AS CHAR  NO-UNDO.
DEF VAR wAntSort     AS INT   NO-UNDO.
DEF VAR wSkriv       AS LOG   NO-UNDO.
DEF VAR wAntPar      AS INT   NO-UNDO.
DEF VAR wSortId      AS CHAR  NO-UNDO.
DEF VAR wStr2Liste   AS CHAR  NO-UNDO.
DEF VAR wFordeling2  AS CHAR  NO-UNDO.
DEF VAR wKontakt     AS CHAR  NO-UNDO.
DEF VAR wAdresse     AS CHAR  NO-UNDO.
DEF VAR wAntJLinjer  AS INT   NO-UNDO.
DEF VAR wFilNavn     AS CHAR  NO-UNDO.
DEF VAR wCl          AS INT   NO-UNDO.
DEF VAR wAntall      AS INT   NO-UNDO.
DEF VAR wButSum      AS INT   NO-UNDO.
DEF VAR wButSum2     AS INT   NO-UNDO.
DEF VAR wStrGrp      AS CHAR  NO-UNDO.
DEF VAR wFordeling   AS CHAR  NO-UNDO.
DEF VAR w2Fordeling  AS CHAR  NO-UNDO.
DEF VAR wLoop        AS INT   NO-UNDO.
DEF VAR wLoop2       AS INT   NO-UNDO.
DEF VAR wIdx1        AS INT   NO-UNDO.
DEF VAR wIdx2        AS INT   NO-UNDO.
DEF VAR wLayout      AS INT   NO-UNDO.
DEF VAR wSortering   AS INT   NO-UNDO.
DEF VAR wBryt1       AS CHAR  NO-UNDO.
DEF VAR wBryt2       AS CHAR  NO-UNDO.
DEF VAR wBryt3       AS CHAR  NO-UNDO.
DEF VAR wAntLinjer   AS INT   NO-UNDO.
DEF VAR wTButik      AS CHAR  NO-UNDO.
DEF VAR wTTotalt     AS CHAR  NO-UNDO.
DEF VAR wTBestilling AS CHAR  NO-UNDO.
DEF VAR wTLevert     AS CHAR  NO-UNDO.
DEF VAR wTRest       AS CHAR  NO-UNDO.
DEF VAR wTAvskrevet  AS CHAR  NO-UNDO.
DEF VAR wTKolTot     AS CHAR  NO-UNDO.
DEF VAR wRestRecid   AS RECID NO-UNDO.
DEF VAR wLopNr       AS CHAR  NO-UNDO.
DEF VAR wBTeller     AS INT   NO-UNDO.
DEF VAR wN2Liste     AS CHAR  NO-UNDO.
DEF VAR wTotAntBest  AS INT   NO-UNDO.
DEF VAR wStorrelser  AS CHAR  NO-UNDO.
DEF VAR wListe       AS CHAR  NO-UNDO.
DEF VAR wWork        AS INT   NO-UNDO.
DEF VAR wStatListe   AS CHAR  NO-UNDO.
DEF VAR wChar2       AS CHAR  NO-UNDO.
DEF VAR wFraAar      AS INT   NO-UNDO.
DEF VAR wTilAar      AS INT   NO-UNDO.
DEF VAR wFraLinje    AS INT   NO-UNDO.
DEF VAR wTilLinje    AS INT   NO-UNDO.
DEF VAR wStatKrit    AS CHAR  NO-UNDO.
DEF VAR wStTypeId    AS CHAR INITIAL "ARTIKKEL" NO-UNDO.
DEF VAR wPerId       AS CHAR  NO-UNDO.
DEF VAR wLagAnt      LIKE LAger.LagAnt NO-UNDO.
DEF VAR wLagVerd     AS DEC   NO-UNDO.
DEF VAR wSalgVerd    AS DEC   NO-UNDO.

DEF BUFFER bufBestLinje FOR BestLinje.
DEF BUFFER clButiker    FOR Butiker.
DEF BUFFER bufButiker   FOR Butiker.
DEF BUFFER bJobbLinje   FOR JobbLinje.
DEF BUFFER bArtPris     FOR ArtPris.

/* Setter sentrallager. */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN
  RETURN.

{syspara.i 6 200 4 wButTekst}

{runlib.i}

ASSIGN
  wStatus = "AVBRYT"
  wButNavn = STRING(clButiker.Butik,"zzzzz9") + " " + clButiker.ButNamn.

/* Henter jobbrecorden. */
FIND Jobb NO-LOCK WHERE
  Jobb.JobbNr = wJobbNr NO-ERROR.
IF NOT AVAILABLE Jobb THEN
  RETURN.

/* Henter lister. Recid ligger i Jobb.Kriterier. */
FIND Lister NO-LOCK WHERE
  RECID(Lister) = INT(ENTRY(1,Jobb.Kriterier)) NO-ERROR.
IF NOT AVAILABLE Lister THEN
  RETURN.

/* Avslutter hvis jobblinje allerede er bygget. */
FIND FIRST JobbLinje NO-LOCK WHERE
  JobbLinje.JobbNr = wJobbNr NO-ERROR.
IF AVAILABLE JobbLinje THEN
  RETURN.
/*
/* Sjekker datasett */
if NUM-ENTRIES(Jobb.Kriterier) < 10 then
  DO:
    MESSAGE "Gammelt datasett. Generer en ny liste." skip
            "Det ligger feil parametre i det gamle datasettet." SKIP(1)
            Jobb.Kriterier
            VIEW-AS ALERT-BOX.
    RETURN "AVBRYT".
  END.
*/

/* Setter valg som gjelder for bygging av listen. */
ASSIGN
  SESSION:APPL-ALERT-BOXES = FALSE
  wAntJLinjer = 0
  wLayout     = INT(ENTRY(2,Jobb.Kriterier))
  wSortering  = INT(ENTRY(3,Jobb.Kriterier))
  wSjekkLager = INT(ENTRY(4,Jobb.Kriterier)) - 1
  wSjekkLAger = IF wSjekkLager < 0 THEN 0 ELSE wSjekkLager
  .

/* Bygger datasett - Flytter data fra tmptabell til workdb. */
RUN ByggDatasett.

/* Retur til kallende rutine. */
HIDE MESSAGE NO-PAUSE.
ASSIGN
  wStatus = "OK".
RETURN wStatus.

/* -------------- Procedurer ------------------------------- */
PROCEDURE ByggDataSett:
DEF VAR wOk AS LOG NO-UNDO.

ASSIGN wLoop = 0.
IF VALID-HANDLE(wParentHandle) THEN
  RUN VisProgressBar IN wParentHandle.

BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:
    ASSIGN
      wLagAnt   = 0
      wLagVerd  = 0
      wSalgVerd = 0
      wTekst    = ""
      wButListe = ""
      wTotListe = ""
      wLoop     = wLoop + 1.
    IF VALID-HANDLE(wParentHandle) AND wLoop MODULO 5 = 0  THEN
      RUN FremdriftProgressBar IN wParentHandle (INPUT wLoop).

  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
    NEXT BYGGJOBB.

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND StrType  OF ArtBas NO-LOCK NO-ERROR.

  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
    NEXT BYGGJOBB.
  /* Henter butikken som er satt inn på listelinjen. */
  IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 THEN /* Ikke spes pr. butikk */
    DO:
      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = wCl NO-ERROR.
      wButNavn = wButTekst.
      IF CAN-DO("212,213",STRING(wLayout)) THEN
      DO:
        FOR EACH Lager OF ArtBas NO-LOCK:
          ASSIGN
            wLagAnt   = wLagAnt   + Lager.LAgAnt
            wLagVerd  = wLagVerd  + (Lager.LAgAnt * Lager.VVareKost)
            wSalgVerd = wSalgVerd + (Lager.LagAnt * ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]).
        END.
      END.
      IF CAN-DO("213",STRING(wLayout)) THEN
      DO:
        FOR EACH Butiker NO-LOCK WHERE
          Butiker.Butik > 0:
          RUN ByggStrListe(INPUT Butiker.Butik, INPUT-OUTPUT wTekst, INPUT-OUTPUT wButListe).
        END.
      END.
    END.
  ELSE DO: /* Spesifisert pr. butikk. */
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = INT(ENTRY(2,ListeLinje.DataObjekt,",")) NO-ERROR.
    IF AVAILABLE Butiker THEN
    DO:
      wButNavn = STRING(Butiker.Butik,"zzzzz9") + " " + Butiker.ButNamn.
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      IF CAN-DO("212,213",STRING(wLayout)) THEN
      DO:
        FIND Lager OF ArtBas NO-LOCK WHERE
          Lager.Butik = Butiker.butik NO-ERROR.
        IF AVAILABLE Lager THEN
          ASSIGN
            wLagAnt   = wLagAnt   + Lager.LAgAnt
            wLagVerd  = wLagVerd  + (Lager.LAgAnt * Lager.VVareKost)
            wSalgVerd = wSalgVerd + (Lager.LagAnt * ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]).
      END.
      IF CAN-DO("213",STRING(wLayout)) THEN
        RUN ByggStrListe(INPUT Butiker.Butik, INPUT-OUTPUT wTekst, INPUT-OUTPUT wButListe).
    END.
  END.

  /* Henter bilde. Finnes ikke bilde, benyttes blankt bilde. */
  FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
  IF VALID-HANDLE(wLibHandle) THEN
    RUN HentBildePeker IN wLibHandle
                       (INPUT Bilderegister.BildNr,
                        INPUT 1,
                        (IF AVAILABLE Bilderegister
                          THEN BildeRegister.Filnavn
                          ELSE ""),
                        OUTPUT wFilNavn).

  FIND FIRST VgKat NO-LOCK WHERE
    VgKat.Vg    = ArtBas.Vg AND
    VgKat.VgKat = ArtBas.VgKat NO-ERROR.
  IF AVAILABLE VgKat THEN
    FIND Kategori NO-LOCK WHERE
      Kategori.KatNr = VgKat.KatNr NO-ERROR.

  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  FIND Material OF ArtBas NO-LOCK NO-ERROR.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.

  RUN OpprettJobbLinje.

  /* Sjekker om det er noe i lager.                                  */
  /* HVis bruker har valgt † kun skrive ut poster med:               */
  /* 0-Alle poster. 1-LagerAnt <> 0, 2-LAgerAnt > 0, 3-LAgerAnt < 0, 4-LagerAnt = 0. */
  IF wSjekkLager > 0 THEN
  SJEKKLAGER:
  DO:
    FIND JobbLinje EXCLUSIVE-LOCK WHERE
      RECID(JobbLinje) = wJobbLinjeRecid NO-ERROR.
    IF AVAILABLE JobbLinje THEN
      DO:
        CASE wSjekkLager:
          /* Skriv ut alle som har lager forskjellig fra 0 */
          WHEN 1 THEN
            DO:
              IF JobbLinje.DecX[5] = 0 THEN
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager st›rre enn 0 */
          WHEN 2 THEN
            DO:
              IF JobbLinje.DecX[5] <= 0 THEN
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager mindre enn 0 */
          WHEN 3 THEN
            DO:
              IF JobbLinje.DecX[5] >= 0 THEN
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.
          /* Skriv ut alle som har lager lik 0 */
          WHEN 4 THEN
            DO:
              IF JobbLinje.DecX[5] <> 0 THEN
                DELETE JobbLinje.
              wJobbLinjeRecid = ?.
            END.

        END CASE.
      END.
  END. /* SJEKKLAGER */

END. /* BYGGJOBB */
END PROCEDURE. /* ByggDataSett */

PROCEDURE OpprettJobbLinje:

  /*
  hide message NO-PAUSE.
  MESSAGE "Jobblinje: " wAntJLinjer.
  */

  ASSIGN
    wLopNr = IF ArtBas.LopNr = ? OR ArtBas.LopNr = 0 THEN "?"
             ELSE STRING(ArtBas.LopNr)
    wChar2 = STRING(ArtBas.Vg,"9999") + "," +
             (IF ArtBas.LopNr = ? OR ArtBas.LopNr = 0
               THEN "?"
               ELSE STRING(ArtBas.LopNr)) + "," +
             STRING(ArtBas.ArtikkelNr).

  CREATE JobbLinje.

  ASSIGN
    JobbLinje.JobbNr   = wJobbNr
    JobbLinje.Char1    = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                           THEN STRING(wCl,"999999")
                           ELSE STRING(INT(ENTRY(2,ListeLinje.DataObjekt,",")),"999999")
    JobbLinje.Char2    = wChar2
    JobbLinje.Char3    = STRING(ArtBas.LevNr,"999999999") + "," +
                         ArtBas.LevKod +  "," + STRING(wAntJLinjer,"999999999")
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

    JobbLinje.DivX[24] = (IF AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + "/" + wLopNr + "/" + STRING(ArtBas.VgKat) + " " + VarGr.VgBesk
                            ELSE STRING(ArtBas.Vg) + "/" + wLopNr + "/" + STRING(ArtBas.VgKat))
    JobbLinje.DivX[25] = (IF AVAILABLE Farg
                            THEN STRING(ArtBas.Farg) + " " + Farg.FarBeskr
                            ELSE STRING(ArtBas.Farg))
    JobbLinje.DivX[26] = (IF AVAILABLE Material
                            THEN STRING(ArtBas.MatKod) + " " + Material.MatBeskr
                            ELSE STRING(ArtBas.MatKod))
    JobbLinje.DivX[27] = (IF AVAILABLE VareMerke
                            THEN STRING(ArtBas.VmId) + " " + VareMerke.Beskrivelse
                            ELSE STRING(ArtBas.VmId))
    JobbLinje.DivX[31] = STRING(ListeLinje.CellNr,"9999999999")
    JobbLinje.DivX[36] = ArtBas.Beskr + " / " + 
                           IF AVAILABLE Farg 
                             THEN Farg.FarBeskr
                             ELSE ""
    JobbLinje.DivX[36] = IF wLayout = 212
                           THEN SUBSTRING(JobbLinje.DivX[36],1,22)
                           ELSE SUBSTRING(JobbLinje.DivX[36],1,34)

    JobbLinje.DecX[ 2] = ArtPris.Pris[1]
    JobbLinje.DivX[34] = IF ArtPris.Tilbud THEN "*" ELSE ""
    JobbLinje.DecX[ 1] = ArtPris.VareKost[1]

    Jobblinje.DecX[ 5] = wLagAnt
    JobbLinje.DecX[ 6] = wLagVerd
    JobbLinje.DecX[ 7] = wSalgVerd
    wJobbLinjeRecid    = RECID(JobbLinje)
    .

    /* Sjekker priskøen. */
    FIND FIRST PrisKo NO-LOCK WHERE
      PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
      PrisKo.ProfilNr      = Butiker.ProfilNr AND
      PrisKo.Tilbud        = TRUE AND
      PrisKo.Type          = 2 NO-ERROR.
    IF AVAILABLE PrisKo THEN
    DO:
      ASSIGN
        JobbLinje.DivX[32] = STRING(Prisko.AktiveresDato)
        JobbLinje.DivX[33] = STRING(PrisKo.GyldigTilDato)

        JobbLinje.DecX[ 3] = PrisKo.VareKost
        JobbLinje.DecX[ 4] = PrisKo.Pris
        JobbLinje.DecX[ 7] = wLagAnt * PrisKo.Pris
        .
    END.
    ELSE DO:
      ASSIGN
        JobbLinje.DivX[32] = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudFraDato) ELSE ?
        JobbLinje.DivX[33] = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudTilDato) ELSE ?

        JobbLinje.DecX[ 3] = IF ArtPris.Tilbud THEN ArtPris.VareKost[2] ELSE ?
        JobbLinje.DecX[ 4] = IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE ?
        .
    END.

    FIND bufButiker NO-LOCK WHERE
      bufButiker.butik = INT(JobbLinje.Char1) NO-ERROR.

    /* Legger opp størrelser pr. butikk */
    IF wTekst <> "" AND CAN-DO("213",STRING(wLayout)) AND wLagant <> 0 THEN
    DO:
      DO wLoop2 = 1 TO NUM-ENTRIES(wTekst,";"):
        ASSIGN
          wLinje = IF wLoop2      = 2 THEN ENTRY(wLoop2,wTekst,";") ELSE wLinje
          JobbLinje.Div2X[wLoop2] = ENTRY(wLoop2,wTekst,";")
          JobbLinje.Dec2X[wLoop2] = INT(ENTRY(wLoop2,wButListe,";")).
      END.
      /* Legger inn sumlinje når det er flere butikker */
      IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0 THEN
        ASSIGN
          JobbLinje.Div2X[wLoop2 + 1] = wLinje
          JobbLinje.Dec2X[wLoop2 + 1] = ?  /* Undertrykker butikknummer */
          JobbLinje.Div2X[wLoop2 + 2] = w2TotListe
          JobbLinje.Dec2X[wLoop2 + 2] = ?. /* Undertrykker butikknummer */
    END.

    /* Sortering og brytgrupper */
    CASE wSortering:
      WHEN 1 THEN ASSIGN
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[12] = STRING(INT(wLopNr),"9999")
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               ELSE "Butikk " + (IF AVAILABLE bufButiker
                                           THEN STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Varegruppe " + (IF AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 2 THEN ASSIGN
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[12] = STRING(ArtBas.LevNr,"9999999")
        JobbLinje.DivX[13] = ArtBas.LevKod
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               ELSE "Butikk " + (IF AVAILABLE bufButiker
                                           THEN STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Varegruppe " + (IF AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[19] = "Leverandør "  + (IF AVAILABLE LevBas
                                              THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                              ELSE STRING(ArtBas.LevNr))
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 3 THEN ASSIGN
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.LevNr,"9999999")
        JobbLinje.DivX[12] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[13] = ArtBas.LevKod
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               ELSE "Butikk " + (IF AVAILABLE bufButiker
                                           THEN STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Leverandør "  + (IF AVAILABLE LevBas
                                              THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                              ELSE STRING(ArtBas.LevNr))
        JobbLinje.DivX[19] = "Varegruppe " + (IF AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 4 THEN ASSIGN
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ListeLinje.CellNr,"9999999999")
        JobbLinje.DivX[12] = ""
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               ELSE "Butikk " + (IF AVAILABLE bufButiker
                                           THEN STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 5 THEN ASSIGN
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.BildNr,"9999999999")
        JobbLinje.DivX[12] = ""
        JobbLinje.DivX[13] = ""
        JobbLinje.DivX[14] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               ELSE "Butikk " + (IF AVAILABLE bufButiker
                                           THEN STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = ""
        JobbLinje.DivX[19] = ""
        JobbLinje.DivX[20] = ""
        JobbLinje.DivX[21] = "".
      WHEN 6 THEN ASSIGN
        JobbLinje.DivX[10] = JobbLinje.Char1
        JobbLinje.DivX[11] = STRING(ArtBas.Vg,"9999")
        JobbLinje.DivX[12] = STRING(ArtBas.VgKat)
        JobbLinje.DivX[13] = STRING(ArtBas.LevNr,"9999999")
        JobbLinje.DivX[14] = ArtBas.LevKod
        JobbLinje.DivX[22] = ""
        /* Brytgruppetekster */
        JobbLinje.DivX[17] = IF INT(ENTRY(2,ListeLinje.DataObjekt,",")) = 0
                               THEN "Alle butikker"
                               ELSE "Butikk " + (IF AVAILABLE bufButiker
                                           THEN STRING(bufButiker.Butik,"zzzzz9") + " " + bufButiker.ButNamn
                                           ELSE "")
        JobbLinje.DivX[18] = "Varegruppe " + (IF AVAILABLE VarGr
                                              THEN STRING(VarGr.Vg) + " " + VarGr.VgBeskr
                                              ELSE STRING(ArtBas.Vg))
        JobbLinje.DivX[19] = "Kategori " + (IF AVAILABLE Kategori
                                              THEN STRING(ArtBas.VgKat) + " " + Kategori.Beskrivelse
                                              ELSE STRING(ArtBas.VgKat))
        JobbLinje.DivX[20] = "Leverandør "  + (IF AVAILABLE LevBas
                                              THEN STRING(ArtBas.LevNr) + " " + LevBas.LevNamn
                                              ELSE STRING(ArtBas.LevNr))
        JobbLinje.DivX[21] = "".
    END CASE.
END PROCEDURE.

PROCEDURE ByggStrListe:
  DEF INPUT        PARAMETER ipButik    AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER ipTekst    AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER ipButListe AS CHAR NO-UNDO.

  DEF VAR ipAntListe AS CHAR NO-UNDO.
  DEF VAR ipWrk      AS CHAR NO-UNDO.
  DEF VAR ipLinje    AS CHAR NO-UNDO.

  /* Bygger størrelsesmatrise. */
  IF ipTekst = "" THEN
    DO:
      ASSIGN
        wStrListe = "".
      FOR EACH StrTStr NO-LOCK WHERE
        StrTStr.StrTypeId = ArtBas.StrTypeId:
        ASSIGN
          ipTekst    = ipTekst +
                       (IF ipTekst = ""
                          THEN ""
                          ELSE " ") +
                       (FILL(" ",4 - LENGTH(TRIM(StrTStr.SoStorl))) + TRIM(StrTStr.SoStorl))
          wStrListe = wStrListe +
                       (IF wStrListe = ""
                         THEN ""
                         ELSE "|") +
                       TRIM(StrTStr.SoStorl).
      END.
      /* Fyller opp med eventuelle manglende størrelser */
      FOR EACH ArtLAg NO-LOCK WHERE
        ArtLAg.artikkelnr = ArtBas.artikkelnr
        BY ArtLag.artikkelnr
        BY ArtLAg.Storl
        BY ArtLAg.Butik:

        IF LOOKUP(TRIM(ArtLag.Storl),wStrListe,"|") = 0 THEN
          ASSIGN
            ipTekst    = ipTekst +
                         (IF ipTekst = ""
                            THEN ""
                            ELSE " ") +
                         (FILL(" ",4 - LENGTH(TRIM(ArtLag.Storl))) + TRIM(ArtLag.Storl))
            wStrListe = wStrListe +
                         (IF wStrListe = ""
                           THEN ""
                           ELSE "|") +
                         TRIM(ArtLag.Storl).
      END.
    END.
  /* Bygger butiklisten */
  ASSIGN
    ipButListe = IF ipButListe = "" THEN ";;" ELSE ipButListe
    ipButListe = ipButListe + STRING(ipButik).

  /* Bygger kommaseparert liste */
  ipAntListe = "".
  DO wLoop2 = 2 TO NUM-ENTRIES(wStrListe,"|"):
    ASSIGN
      ipAntListe = ipAntListe + "|".
  END.
  /* Initierer totallisten */
  IF wTotListe = "" THEN
    wTotListe = ipAntListe.

  /* Leser inn størrelser og bygger antallstreng. */
  FOR EACH ArtLag NO-LOCK WHERE
    ArtLag.Butik = ipButik AND
    ArtLAg.artikkelnr = ArtBas.artikkelnr:

    IF LOOKUP(TRIM(ArtLag.Storl),wStrListe,"|") <> 0 THEN
    DO:
      /* Linjesum */
      ENTRY(
            LOOKUP(TRIM(ArtLag.Storl),wStrListe,"|"),
            ipAntListe,
            "|"
            ) = STRING(
                       INT(
                           ENTRY(
                                 LOOKUP(TRIM(ArtLag.Storl),wStrListe,"|"),
                                 ipAntListe,"|"
                                )
                          ) + ArtLag.LagAnt
                      ).
      /* Totalsum */
      ENTRY(
            LOOKUP(TRIM(ArtLag.Storl),wStrListe,"|"),
            wTotListe,
            "|"
            ) = STRING(
                       INT(
                           ENTRY(
                                 LOOKUP(TRIM(ArtLag.Storl),wStrListe,"|"),
                                 wTotListe,"|"
                                )
                          ) + ArtLag.LagAnt
                      ).
    END.
  END.

  /* Konverterer pipe separert streng til space separert og formatert streng. */
  ASSIGN
    ipWrk      = ""
    ipLinje    = ""
    w2TotListe = "".
  DO wLoop2 = 1 TO NUM-ENTRIES(ipAntListe,"|"):
    /* Setter 0 til blank */
    ENTRY(wLoop2,ipAntListe,"|") = (IF ENTRY(wLoop2,ipAntListe, "|") = "0"
                                      THEN ""
                                      ELSE ENTRY(wLoop2,ipAntListe, "|")).
    ENTRY(wLoop2,wTotListe,"|") = (IF ENTRY(wLoop2,wTotListe, "|") = "0"
                                      THEN ""
                                      ELSE ENTRY(wLoop2,wTotListe, "|")).
    ASSIGN
      ipWrk = ipWrk +
              (IF wLoop2 = 1
                 THEN ""
                 ELSE " ") +
              FILL(" ",4 - LENGTH(ENTRY(wLoop2,ipAntListe,"|"))) +  ENTRY(wLoop2,ipAntListe,"|")
      ipLinje = ipLinje +
              (IF ipLinje = ""
                 THEN ""
                 ELSE " ") +
              "----"
      w2TotListe = w2TotListe +
                   (IF wLoop2 = 1
                     THEN ""
                     ELSE " ") +
                   FILL(" ",4 - LENGTH(ENTRY(wLoop2,wTotListe,"|"))) +  ENTRY(wLoop2,wTotListe,"|")
      .
  END.

  /* Legger resultatstrengen på. */
  ASSIGN
    ipTekst    = ipTekst +
                 (IF ipTekst = ""
                   THEN ""
                   ELSE ";") +
                 (IF NUM-ENTRIES(ipTekst,";") = 1
                   THEN ipLinje + ";"
                   ELSE "") +
                 ipWrk
    ipButListe = ipButListe +
                 (IF ipButListe = ""
                   THEN ""
                   ELSE ";").

/*
MESSAGE ArtBas.Vg ArtBas.LopNr skip
"Butikk..:"   ipButik skip
"InnListe:" ipTekst skip
"StrListe:" wStrListe skip
"AntListe:" ipAntListe skip
"ipWrk...:"    ipWrk skip
"ipButListe:" ipButListe skip
"ipLinje:" ipLinje skip
VIEW-AS ALERT-BOX.
*/

END PROCEDURE.
