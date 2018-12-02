/************************************************************
    Program:  x-rutskartikkel.w
    Created:  TN   27 Apr 99
Description:

Last change:  TN   27 Apr 99    3:26 pm
************************************************************/

DEF INPUT  PARAMETER wJobbNr as INT  NO-UNDO.
DEF OUTPUT parameter wStatus as CHAR NO-UNDO.

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
DEF VAR wTilbud      as INT   NO-UNDO.

{syspara.i 6 141 4  wTButik}
{syspara.i 6 141 20 wTTotalt}
{syspara.i 6 141 21 wTBestilling}
{syspara.i 6 141 22 wTLevert}
{syspara.i 6 141 23 wTRest}
{syspara.i 6 141 24 wTKolTot}
{syspara.i 6 141 25 wTAvskrevet}

DEF BUFFER bufBestLinje FOR BestLinje.

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
FIND Butiker NO-LOCK where
  Butiker.Butik = wCl NO-ERROR.
if NOT AVAILABLE Butiker then
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
  wLayout    = INT(ENTRY(2,Jobb.Kriterier))
  wSortering = INT(ENTRY(3,Jobb.Kriterier)).

BYGGJOBB:
FOR EACH ListeLinje OF Lister NO-LOCK TRANSACTION:

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
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  if NOT AVAILABLE BestPris THEN
    NEXT BYGGJOBB.

  if ArtPris.Tilbud = TRUE then
    wTilbud = 2.
  else
    wTilbud = 1.

  /* Henter bilde. Finnes ikke bilde, benyttes blankt bilde. */
  FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
  if VALID-HANDLE(wLibHandle) then
    RUN HentBildePeker in wLibHandle
                       (INPUT BildeRegister.BildNr,
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

  /* Setter teksten p† brytgruppene. */
  if wLayout = 140 then
    DO:
      CASE wSortering:
        WHEN 1 then
               assign
                 wBryt1 = (if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + " " + SUBSTRING(VarGr.VgBesk,1,22)
                            ELSE STRING(ArtBas.Vg))
                 wBryt2 = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + LevBas.LevNamn
                            else STRING(ArtBas.LevNr))
                 wBryt3 = STRING(0 /*BestHode.BestNr*/).
        WHEN 2 then
               assign
                 wBryt1 = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + SUBSTRING(LevBas.LevNamn,1,22)
                            else STRING(ArtBas.LevNr))
                 wBryt2 = STRING(0 /*BestHode.BestNr*/) + " " + ArtBas.Beskr
                 wBryt3 = "".
        WHEN 3 then
               assign
                 wBryt1 = (if AVAILABLE VarGr
                            THEN STRING(VarGr.Vg) + " " + SUBSTRING(VarGr.VgBesk,1,22)
                            ELSE STRING(ArtBas.Vg))
                 wBryt2 = (if AVAILABLE Kategori
                            THEN STRING(ArtBas.VgKat) + " " + Kategori.Beskrivelse
                            else STRING(ArtBas.VgKat))
                 wBryt3 = STRING(0 /*BestHode.BestNr*/) + " " + ArtBas.Beskr.
      END CASE.
    END.

  /* Oppretter jobbrecord. */
  RUN OpprettJobbLinje.

  /* Her legges det opp spesifikasjon av lager og storrelser. */
  /*
  if wLayout = 101 then
    DO:

    END.
  */


END. /* BYGGJOBB */

assign
  wStatus = "OK".

/* -------------- Internprocedurer ----------------------- */

PROCEDURE OpprettJobbLinje:
  CREATE JobbLinje.
  assign

    /* Felter som det sorteres/brytes p†.                                                   */
    /* Disse feltene m† alle v‘re av samme type for at cas statment i rapporten skal virke. */
    JobbLinje.Char1    = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Ikke spes pr. butikk */
                           THEN STRING(wCl,"999999")
                           ELSE STRING(INT(ENTRY(3,ListeLinje.DataObjekt,",")),"999999")
    JobbLinje.DivX[ 3] = BestHode.LevKod
    JobbLinje.DivX[18] = STRING(ArtBas.Vg,"9999")
    JobbLinje.DivX[19] = STRING(ArtBas.LevNr,"999999")
    JobbLinje.DivX[20] = STRING(0 /* BestNr*/,"99999999")
    JobbLinje.DivX[22] = STRING(ArtBas.VgKat,"9999")

    /* informasjonsfelter. */
    JobbLinje.JobbNr   = wJobbNr
    JobbLinje.Char2    = STRING(ArtBas.Vg,"9999") + "," +
                         STRING(ArtBas.LevNr,"999999999") + "," +
                         ArtBas.LevKod + "," +
                         ENTRY(2,ListeLinje.DataObjekt,",") + /* For at det skal bli unikt! */
                         (if wLayout = 101
                            then "," + STRING(wAntLinjer + wBTeller)
                             ELSE " ")

    JobbLinje.Char3    = STRING(ArtBas.LevNr,"999999999") + "," +
                         ArtBas.LevKod
    JobbLinje.DivX[ 1] = STRING(ArtBas.ArtikkelNr)
    JobbLinje.DivX[ 2] = STRING(ArtBas.LevNr)
    JobbLinje.DivX[ 4] = ArtBas.LevFargKod
    JobbLinje.DivX[ 5] = STRING(ArtBas.VgKat)
    JobbLinje.DivX[ 6] = wFilNavn
    JobbLinje.DivX[ 7] = STRING(ArtBas.Vg)
    JobbLinje.DivX[ 8] = STRING(0 /* BestNr*/) + " " + " " /*entry(BestHode.BestStat,wStatListe) */
    JobbLinje.DivX[ 9] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Butikk */
                          THEN STRING(wCl)
                          ELSE ENTRY(3,ListeLinje.DataObjekt,",")
    JobbLinje.DivX[10] = ""
    JobbLinje.DivX[11] = STRING(ArtBas.RegistrertDato) /* BestHode.Bestillingsdato */
    JobbLinje.DivX[12] = (if available Material
                            then STRING(Material.MatKod) + " " + Material.MatBeskr
                            ELSE "")
    JobbLinje.DivX[13] = wStrGrp
    JobbLinje.DivX[14] = wFordeling
    JobbLinje.DivX[15] = (if AVAILABLE StrType
                            THEN string(StrType.StrTypeId)+ "/" + StrType.KortNavn
                            ELSE "")
    JobbLinje.DivX[16] = wBryt2
    JobbLinje.DivX[17] = wBryt1
    JobbLinje.DivX[21] = wBryt3
    JobbLinje.DivX[23] = (if AVAILABLE LevBas
                            THEN STRING(LevBas.LevNr) + " " + LevBas.LevNamn
                            else STRING(ArtBAs.LevNr)).

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

    JobbLinje.DecX[ 1] = ArtPris.InnkjopsPris[wTilbud]
    JobbLinje.DecX[ 2] = ArtPris.VareKost[wTilbud]
    JobbLinje.DecX[ 3] = ArtPris.DB%[wTilbud]
    JobbLinje.DecX[ 4] = ArtPris.Pris[wTilbud]
    JobbLinje.DecX[ 5] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                          then 0 /*BestHode.TotAntPar*/
                          ELSE wButSum2
    JobbLinje.DecX[ 6] = 0 /*BestHode.TotInnKjVerdi*/
    JobbLinje.DecX[ 7] = 0 /* BestHode.TotSalgsVerdi */
    JobbLinje.DecX[ 8] = ArtPris.ValPris[wTilbud]
    JobbLinje.DecX[ 9] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0
                          then wAntall
                          ELSE 1
    JobbLinje.DecX[10] = 0 /*BestHode.BestNr*/
    JobbLinje.DecX[11] = ArtBas.LevNr
    JobbLinje.DecX[12] = ArtBas.Vg
    JobbLinje.DecX[13] = if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 /* Butikk */
                            THEN wCl
                            ELSE INT(STRING(INT(ENTRY(3,ListeLinje.DataObjekt,",")),"999999"))

    /* Styrer sumlinje alle butikker.                                                */
    /* Layout 100 - Feltet brukes ikke.                                              */
    /* Layout 102 Sortering 1. Display sl†s av og p† avhengig av om butikk er valgt. */
    /* Layout 102 Sortering 2. Display er alltid avsl†tt.                            */
    JobbLinje.DecX[14] =  (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 AND wSortering = 1
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0 AND wSortering = 1
                             THEN 1
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 AND wSortering = 3
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0 AND wSortering = 3
                             THEN 1
                           ELSE 0)

    /* Styrer sumlinje pr ordrenummer.                                               */
    /* Layout 100 - Feltet brukes ikke.                                              */
    /* Layout 102 Sortering 1. Display sl†s alltid p†.                               */
    /* Layout 102 Sortering 2. Display sl†s av n†r det ikke er visning pr. butikk.   */
    JobbLinje.DecX[15] =  (if INT(ENTRY(3,ListeLinje.DataObjekt,",")) = 0 AND wSortering = 2
                             THEN 0
                           ELSE if INT(ENTRY(3,ListeLinje.DataObjekt,",")) <> 0 AND wSortering = 2
                             THEN 1
                           ELSE 1).

    if wLayout = 101 and wAntLinjer > 1 then
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


