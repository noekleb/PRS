/************************************************************
    Program:  x-ordre.p
    Created:  TN   4 Aug 00
Description:  Bygger jobblinje for ordreutskrift.

  Ordreutskriften skal ogs† kunne benyttes p† et HK ved
  sammenstilling av bestillinger for flere butikker.
  Leverand›ren vil da se en info linje pr. artikkel og
  en linje pr sortiment p† artikklene.
  NB: Finnes det bestillinger p† samme artikkel som har
      ulik st›rrelsestype, s† vil det p† ordren bli
      spesifisert pr. st›rrelestype.

Last change:  TN   20 Oct 100   10:24 am
************************************************************/

DEF INPUT  PARAMETER wJobbNr as INT  NO-UNDO.
DEF OUTPUT PARAMETER wStatus as CHAR NO-UNDO.
DEF INPUT  PARAMETER wParentHandle as HANDLE NO-UNDO.

DEF VAR wCl            as INT  NO-UNDO.
DEF VAR w2Loop         as INT  NO-UNDO.
DEF VAR wFilNavn       as CHAR NO-UNDO.
DEF VAR wLayout        as INT  NO-UNDO.
DEF VAR wSortering     as INT  NO-UNDO.
DEF VAR wStorrelser    as CHAR NO-UNDO.
DEF VAR wAntLinjer     as INT  NO-UNDO.
DEF VAR wChar1         as CHAR NO-UNDO.
DEF VAR wChar2         as CHAR NO-UNDO.
DEF VAR wChar3         as CHAR NO-UNDO.
DEF VAR wStatListe     as CHAR NO-UNDO.
DEF VAR w1Loop         as INT  NO-UNDO.
DEF VAR giLoop         as INT  NO-UNDO.
DEF VAR wAntPoster     as INT  NO-UNDO.
DEF VAR wLeveringsSted as CHAR NO-UNDO.
DEF VAR wSortId        as CHAR NO-UNDO.
DEF VAR wLng           as CHAR NO-UNDO.
DEF VAR gcLblListe     as CHAR NO-UNDO.
DEF VAR gcVi           as CHAR NO-UNDO.

DEF BUFFER clButiker    FOR Butiker.
DEF BUFFER bufJobbLinje FOR JobbLinje.

{syspara.i 1 1 101 gcVi}

/* Spesifikasjon av ordren. */
DEF TEMP-TABLE tmpOrdre
  FIELD OrdreNr       as DEC FORMAT ">>>>>>>>>>>>9"
  FIELD LevNr         as int
  .

/* Spesifikasjon av artikkel. */
DEF TEMP-TABLE tmpOrdLin
  FIELD OrdreNr       as DEC FORMAT ">>>>>>>>>>>>9"
  FIELD LinjeNr       as INT   /* Unik 1. n›kkel for ordrelinje OrdreNr + LinjeNr. */
  FIELD BestNr        as INT   /* TN 12/9-00 */
  FIELD LevKod        as char
  FIELD StrTypeId     as int
  FIELD LevSted       as int
  FIELD LevDato       as date
  FIELD Sesong        as char
  FIELD ArtikkelNr    as DEC
  FIELD FilNavn       as char
  FIELD LevFargKod    as char
  FIELD BestillingsDato as date
  /* Navn og adresse p† firma som sender ordren. */
  FIELD AvsFirma      as char
  FIELD AvsButSjef    as char
  FIELD AvsAdresse    as char
  FIELD AvsPostBoks   as char
  FIELD AvsPostNr     as char
  FIELD AvsPostSted   as char
  FIELD AvsTelefon    as char
  /* Navn og adresse p† leveringssted (Mottagende butikk) */
  FIELD MotFirma      as char
  FIELD MotKontakt    as char
  FIELD MotAdresse    as char
  FIELD MotPostBoks   as char
  FIELD MotPostNr     as char
  FIELD MotPostSted   as char
  FIELD MotTelefon    as char
  /* Navn og adresse p† mottager av ordre (Leverand›r) */
  FIELD LevFirma      as char
  FIELD LevKontakt    as char
  FIELD LevAdresse    as char
  FIELD LevPostBoks   as char
  FIELD LevPostNr     as char
  FIELD LevPostSted   as char
  FIELD LevTelefon    as char
  /* PRisfelter */
  field ValPris       like BestPris.ValPris
  field Rab1%         LIKE BestPris.Rab1%
  field NettoPris     LIKE BestPris.ValPris
  .

/* Spesifikasjon av de bestillinger som skal inn p† linjen. */
DEF TEMP-TABLE tmpBestilling
  FIELD OrdreNr       as int
  FIELD LinjeNr       as int
  FIELD BestNr        as int
  .

/* Spesifikasjon av sortimentene for en ordrelinje. */
DEF TEMP-TABLE tmpSort
  FIELD OrdreNr       as int
  FIELD LinjeNr       as int
  FIELD SortId        as CHAR
  FIELD Fri           as log
  FIELD AntSort       as INT
  FIELD Antall        as int
  FIELD StrIntervall  as char
  FIELD Fordeling     as char
  FIELD Storrelser    as char
  FIELD FriAntall     as INT EXTENT 50
  .

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
{syspara.i 5 2   99 wStatListe}

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
  wLayout     = INT(ENTRY(2,Jobb.Kriterier))
  wSortering  = INT(ENTRY(3,Jobb.Kriterier)).

/* Her akkumuleres bestillingene opp p† LevNr, ORdreNr, LevSted og artikkelnr. */
RUN ByggTmpTabeller.

/* Her legges de akkumulerte bestillingene over i WorkBasen */
RUN ByggJobb.

/* TEST  */
/*
OUTPUT to VALUE("TEST.DAT").
FOR EACH JobbLinje where
  JobbLinje.JobbNr   = wJobbNr:
  display
    JobbLinje.Div2X[1] FORMAT "x(6)"
    JobbLinje.Div2X[2] FORMAT "x(13)"
    JobbLinje.Div2X[3] FORMAT "x(6)"
    JobbLinje.Div2X[4] FORMAT "x(10)"
    JobbLinje.Div2X[5] FORMAT "x(20)"
    JobbLinje.Div2X[6] FORMAT "x(20)"
  WITH frame g WIDTH 180 down.
  DOWN 1 with frame g.
END. /* TEST */
OUTPUT CLOSE.
*/

assign
  wStatus = "OK".
RETURN wStatus.

/* -------------- Internprocedurer ----------------------- */
PROCEDURE ByggJobb:

BYGGJOBB:
FOR EACH tmpOrdre
  by tmpOrdre.LevNr
  by tmpOrdre.OrdreNr TRANSACTION:

  FIND LevBas NO-LOCK where
    LevBas.LevNr = tmpOrdre.LevNr NO-ERROR.

  /* Henter labler i leverand›res spr†k. */
  RUN SettLblListe (INPUT LevBas.Lng, OUTPUT gcLblListe).

  tmpORDLIN:
  FOR EACH tmpOrdLin where
    tmpOrdLin.OrdreNr = tmpOrdre.OrdreNr:

    assign
      w2Loop = w2Loop + 1.
    if valid-handle(wParentHandle) AND w2Loop modulo 5 = 0 then
      RUN FremdriftProgressBar in wParentHandle (INPUT w2Loop).

    FIND ArtBas NO-LOCK where
      ArtBas.ArtikkelNr = tmpOrdLin.ArtikkelNr NO-ERROR.
    if NOT AVAILABLE ArtBas THEN
      NEXT BYGGJOBB.

    FIND VarGr     OF ArtBas NO-LOCK NO-ERROR.
    FIND Material  OF ArtBas NO-LOCK NO-ERROR.
    FIND StrType   OF ArtBas NO-LOCK NO-ERROR.
    FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
    FIND Sasong    OF ArtBas NO-LOCK NO-ERROR.
    FIND Farg      OF ArtBas NO-LOCK NO-ERROR.
    FIND Material  OF ArtBas NO-LOCK NO-ERROR.
    FIND Innersula OF ArtBas NO-LOCK NO-ERROR.
    FIND Ovandel   OF ArtBas NO-LOCK NO-ERROR.
    FIND SlitSula  OF ArtBas NO-LOCK NO-ERROR.
    FIND Last-Sko  OF ArtBas NO-LOCK NO-ERROR.
    FIND Anv-Kod   OF ArtBas NO-LOCK NO-ERROR.

    /* Oppretter jobbrecord. */
    assign
      wAntLinjer = wAntLinjer + 1.

    /* Oppretter poster i WorkBasen */
    RUN OpprettJobbLinje.

  END. /* tmpORDLIN */
END. /* BYGGJOBB */

END PROCEDURE.

PROCEDURE OpprettJobbLinje:
  DEF VAR iSortId as CHAR NO-UNDO.
  DEF VAR iLoop   as INT  NO-UNDO.

  /* Slipper buffer (Hvis det skulle ligge igjen) */
  if AVAILABLE JobbLinje then
    RELEASE JobbLinje.

  FIND first VgKat NO-LOCK where
    VgKat.Vg    = ArtBas.Vg and
    VgKat.VgKat = ArtBas.VgKat no-error.
  if AVAILABLE VgKat then
    FIND Kategori NO-LOCK where
      Kategori.KatNr = VgKat.KatNr NO-ERROR.
  FIND Ordre NO-LOCK where
    DEC(Ordre.OrdreNr) = tmpOrdre.OrdreNr NO-ERROR.

  /* Setter linjens indexfelter. */
  /* TN 12/9-00
  assign
    iSortId = ""
    wChar1  = STRING(tmpOrdre.LevNr,"999999") +
              STRING(tmpOrdre.OrdreNr,"9999999999999") +
              STRING(tmpOrdLin.LevSted,"999999") +
              (if tmpOrdLin.LevDato = ?
                then "********"
                else STRING(tmpOrdLin.LevDato))
    wChar2  = tmpOrdLin.LevKod
    wChar3  = iSortId /* Denne er alltid "0" p† artikkelinfo raden. */
    .
  */
  assign
    iSortId = ""
    wChar1  = STRING(tmpOrdre.LevNr,"999999") +
              STRING(tmpOrdre.OrdreNr,"9999999999999") +
              STRING(tmpOrdLin.LevSted,"999999") +
              string(tmpOrdLin.BestNr,"99999999") +
              (if tmpOrdLin.LevDato = ?
                then "********"
                else STRING(tmpOrdLin.LevDato))
    wChar2  = tmpOrdLin.LevKod
    wChar3  = iSortId /* Denne er alltid "0" p† artikkelinfo raden. */
    .

  /* Akkumulering av linjer. */
  FIND JobbLinje EXCLUSIVE-LOCK where
    JobbLinje.JobbNr   = wJobbNr and
    JobbLinje.Char1    = wChar1  and
    JobbLinje.Char2    = wChar2  and
    JobbLinje.Char3    = wChar3  NO-ERROR.

  if NOT AVAILABLE JobbLinje then
    OPPSTANDELSEN:
    DO:
      CREATE JobbLinje.

      assign
        wAntPoster         = wAntPoster + 1
        JobbLinje.JobbNr   = wJobbNr
        JobbLinje.Char1    = wChar1
        JobbLinje.Char2    = wChar2
        JobbLinje.Char3    = wChar3
        JobbLinje.DivX[ 1] = "".

    END. /* OPPSTANDELSEN */

  /* Setter sorteringsfelter for ReportBuilder. */
  assign
    JobbLinje.Div2X[ 1] = STRING(tmpOrdre.LevNr,"999999")
    JobbLinje.Div2X[ 2] = STRING(tmpOrdre.OrdreNr,"9999999999999")
    JobbLinje.Div2X[ 3] = STRING(tmpOrdLin.LevSted,"999999")
    JobbLinje.Div2X[ 4] = STRING(tmpOrdLin.BestNr,"99999999") +
                          STRING(tmpOrdLin.LevKod)
    JobbLinje.Div2X[ 5] = (if tmpOrdLin.LevDato = ?
                             then "********"
                             else STRING(tmpOrdLin.LevDato))
    JobbLinje.Div2X[ 6] = wChar3
    .

  assign
    /* Diverse felter. */
    JobbLinje.Div2X[ 7] = tmpOrdLin.Sesong
    JobbLinje.Div2X[ 8] = gcVi
    JobbLinje.Div2X[ 9] = USERID("SkoTex") + " " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS")
    .
  if AVAILABLE Ordre then
    JobbLinje.Div2X[10] = if Ordre.SendtDato <> ?
                            THEN STRING(Ordre.SendtDato)
                            ELSE "".

    /* DivX[ 1] Opptatt - Koblingsfelt mot detaljradene. */
  assign
    JobbLinje.DivX[ 2] = tmpOrdLin.FilNavn
    JobbLinje.DivX[ 3] = "" /* entry(BestHode.BestStat,wStatListe) */
    JobbLinje.DivX[ 4] = tmpOrdLin.LevFargKod
    JobbLinje.DivX[ 5] = "" /* Ledig */
    JobbLinje.DivX[ 6] = tmpOrdLin.LevKod
    /* DecX[7-8 + 36] GrunnInfo */
    JobbLinje.DivX[ 9] = STRING(tmpOrdLin.Bestillingsdato)
    JobbLinje.DivX[10] = (if tmpOrdLin.LevDato = ?
                             then "********"
                             else STRING(tmpOrdLin.LevDato))
  /* Adresse - Ordrens avsender */
    JobbLinje.DivX[11] = tmpOrdLin.AvsFirma
    JobbLinje.DivX[12] = tmpOrdLin.AvsButSjef
    JobbLinje.DivX[13] = tmpOrdLin.AvsAdresse
    JobbLinje.DivX[14] = tmpOrdLin.AvsPostBoks
    JobbLinje.DivX[15] = tmpOrdLin.AvsPostNr
    JobbLinje.DivX[16] = tmpOrdLin.AvsPostSted
    JobbLinje.DivX[17] = tmpOrdLin.AvsTelefon

  /* Adresse - Leverand›rs adresse */
    JobbLinje.DivX[18] = tmpOrdLin.LevFirma
    JobbLinje.DivX[19] = tmpOrdLin.LevKontakt
    JobbLinje.DivX[20] = tmpOrdLin.LevAdresse
    JobbLinje.DivX[21] = tmpOrdLin.LevPostBoks
    JobbLinje.DivX[22] = tmpOrdLin.LevPostNr
    JobbLinje.DivX[23] = tmpOrdLin.LevPostSted
    JobbLinje.DivX[24] = tmpOrdLin.LevTelefon

  /* Adresse - Leveringsadresse m/kontaktperson og telefonnummer. */
    JobbLinje.DivX[25] = tmpOrdLin.MotFirma
    JobbLinje.DivX[26] = tmpOrdLin.MotKontakt
    JobbLinje.DivX[27] = tmpOrdLin.MotAdresse
    JobbLinje.DivX[28] = tmpOrdLin.MotPostBoks
    JobbLinje.DivX[29] = tmpOrdLin.MotPostNr
    JobbLinje.DivX[30] = tmpOrdLin.MotPostSted
    JobbLinje.DivX[31] = tmpOrdLin.MotTelefon
  /* DivX[32 -35] fylles ut p† sortimentsraden. */
  /* DivX[36] GrunInfo. */
    .

  /* Notatfelt fra artikkel. */
  if ArtBas.VisDivInfo[ 9] then
    DO giLoop = 1 to NUM-ENTRIES(ArtBas.Notat,CHR(10)):
       if giLoop <= 5 then
         ASSIGN
           JobbLinje.DivX[44 + giLoop] = ENTRY(giLoop,ArtBas.Notat,CHR(10)).
    END.


  assign
    JobbLinje.DecX[ 1] = tmpOrdLin.OrdreNr
    JobbLinje.DecX[ 2] = tmpOrdre.LevNr
    JobbLinje.DecX[ 3] = tmpOrdLin.LevSted
    /* DecX[10 - 20] fylles ut av sortimentinfo. */
    .

  /* Artikkelens grunninformasjon. DivX[7-8].        */
  /* Informasjon som er valgt lagt ut p† artikkelen. */
  /* Det er bruker som setter opp masken.            */
  RUN SettGrunInfo.

  /* Legger inn LAbler for utskriften. */
  /* Disse legges i Div2X[20 til 50].  */
  LABLER:
  DO iLoop = 1 to NUM-ENTRIES(gcLblListe):
    if iLoop > 30 then
      LEAVE LABLER.

    assign
      JobbLinje.Div2X[iLoop + 19] = ENTRY(iLoop,gcLblListe).
  END. /* LABLER */

  /* Legger opp ekstra poster.           */
  /* En ekstra post for hvert sortiment. */
  tmpSORT:
  FOR EACH tmpSort where
    tmpSort.OrdreNr = tmpOrdLin.OrdreNr and
    tmpSort.LinjeNr = tmpOrdLin.LinjeNr
    by tmpSort.Fri descending
    by tmpSort.SortId:

    /* Null i antall p† faste sorteringer skal ikke med. */
    if tmpSort.Fri = FALSE AND tmpSort.AntSort = 0 then
      NEXT tmpSORT.

    /* Null i antall p† fri sorteringer, skal ikke med. */
    ELSE if tmpSort.Antall = 0 then
      NEXT tmpSORT.

    CREATE bufJobbLinje.
    BUFFER-COPY JobbLinje to bufJobbLinje
      assign
        bufJobbLinje.Char3    = tmpSort.SortId
        bufJobbLinje.DivX[ 1] = if tmpSort.Fri /* Kobling til detaljradene. */
                                  THEN "HODE"
                                  ELSE ""
        bufJobbLinje.DecX[10] = tmpSort.AntSort
        bufJobbLinje.DecX[11] = tmpSort.Antall
        bufJobbLinje.DecX[12] = tmpOrdLin.ValPris
        bufJobbLinje.DecX[13] = tmpOrdLin.Rab1%
        bufJobbLinje.DecX[14] = tmpOrdLin.NettoPris
        bufJobbLinje.DecX[15] = tmpSort.AntSort * tmpSort.Antall * tmpOrdLin.NettoPris
        bufJobbLinje.DecX[16] = tmpSort.AntSort * tmpSort.Antall

        bufJobbLinje.DivX[32] = tmpSort.StrIntervall
        bufJobbLinje.DivX[33] = if tmpSort.Fri
                                  then ""
                                  else tmpSort.Fordeling
        bufJobbLinje.DivX[34] = tmpSort.Storrelser
        bufJobbLinje.DivX[35] = "" /* Ledig */
     /* Setter sorteringsfelter for ReportBuilder. */
        bufJobbLinje.Div2X[ 1] = STRING(tmpOrdre.LevNr,"999999")
        bufJobbLinje.Div2X[ 2] = STRING(tmpOrdre.OrdreNr,"9999999999999")
        bufJobbLinje.Div2X[ 3] = STRING(tmpOrdLin.LevSted,"999999")
        bufJobbLinje.Div2X[ 4] = STRING(tmpOrdLin.BestNr,"99999999") +
                                 tmpOrdLin.LevKod
        bufJobbLinje.Div2X[ 5] = (if tmpOrdLin.LevDato = ?
                                 then "********"
                                 else STRING(tmpOrdLin.LevDato))
        bufJobbLinje.Div2X[ 6] = if tmpSort.Fri
                                   then "ZZZ"
                                   else tmpSort.SortId
    .

   /* Oppretter detaljposter for fri sorteringer. */
   if tmpSort.Fri then
     RUN OpprettDetaljer.

  END. /* tmpSORT */

END PROCEDURE.

/* Legger ut artikkelens grunninformasjon. */
PROCEDURE SettGrunInfo:
  DEF VAR cTekst    as CHAR NO-UNDO.
  DEF VAR iLoop     as INT  NO-UNDO.
  DEF VAR cRefListe as CHAR NO-UNDO.

  /* VareGruppe */
  if ArtBas.VisDivInfo[11] then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(19,gcLblListe) + " " + STRING(ArtBas.Vg).
  /* RefListe */
  /* -- TN 12/9-00                                            */
  /* -- Dette skal n† alltid ut. En bestilling pr inforlinje. */
  /* if ArtBas.VisDivInfo[12] then */
    DO:
      assign
        cRefListe = "".
      for each tmpBestilling where
        tmpBestilling.OrdreNr  = tmpOrdLin.OrdreNr  and
        tmpBestilling.LinjeNr  = tmpOrdLin.LinjeNr:
        assign
          cRefListe = cRefListe +
                      (if cRefListe = ""
                         THEN ""
                         ELSE ", ") +
                      STRING(tmpBestilling.BestNr).
      END.
      assign
        cTekst = cTekst +
                 (if cTekst <> ""
                   THEN ",|"
                   ELSE "") +
                 ENTRY(29,gcLblListe) + " " + cRefListe.
    END.

  /*  1 Sesong */
  if ArtBas.VisDivInfo[ 1] and available Sasong then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(20,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 1] <> ""
                  then ArtBas.DivInfo[ 1]
                  else Sasong.SasBeskr).
  /* 10 Fargekode */
  /*    H†ndteres spesielt.... */
  /*  2 Material */
  if ArtBas.VisDivInfo[ 2] and Available Material then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(22,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 2] <> ""
                  then ArtBas.DivInfo[ 2]
                  ELSE Material.MatBeskr).
  /*  3 H‘l */
  if ArtBas.VisDivInfo[ 3] then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(23,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 3] <> ""
                  then ArtBas.DivInfo[ 3]
                  ELSE STRING(ArtBas.Klack)).
  /*  4 For */
  if ArtBas.VisDivInfo[ 4] and Available InnerSula then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(24,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 4] <> ""
                  then ArtBas.DivInfo[ 4]
                  ELSE InnerSula.InnerBeskr).
  /*  5 Overdel */
  if ArtBas.VisDivInfo[ 5] and Available Ovandel then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(25,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 5] <> ""
                  then ArtBas.DivInfo[ 5]
                  ELSE Ovandel.OvBeskr).
  /*  6 Slites†le */
  if ArtBas.VisDivInfo[ 6] and Available SlitSula then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(26,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 6] <> ""
                  then ArtBas.DivInfo[ 6]
                  ELSE SlitSula.SlitBeskr).
  /*  7 L‘st */
  if ArtBas.VisDivInfo[ 7] and Available Last-Sko then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(27,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 7] <> ""
                  then ArtBas.DivInfo[ 7]
                  ELSE Last-Sko.LastBeskr).
  /*  8 Bruksomr†de */
  if ArtBas.VisDivInfo[ 8] and Available Anv-Kod then
    assign
      cTekst = cTekst +
               (if cTekst <> ""
                 THEN ",|"
                 ELSE "") +
               ENTRY(28,gcLblListe) + " " +
               (if ArtBas.DivInfo[ 8] <> ""
                  then ArtBas.DivInfo[ 8]
                  ELSE Anv-Kod.AnvBeskr).

  /* Fyller opp tekstfeltene. */
  LOOPEN:
  do iLoop = 1 to  NUM-ENTRIES(cTekst,"|"):
    if LENGTH(JobbLinje.DivX[ 7] + " " + ENTRY(iLoop,cTekst,"|")) < 60 then
      DO:
        JobbLinje.DivX[ 7] = JobbLinje.DivX[ 7] + " " + ENTRY(iLoop,cTekst,"|").
        NEXT LOOPEN.
      END.

    if LENGTH(JobbLinje.DivX[ 8] + " " + ENTRY(iLoop,cTekst,"|")) < 60 then
      DO:
        JobbLinje.DivX[ 8] = JobbLinje.DivX[ 8] + " " + ENTRY(iLoop,cTekst,"|").
        NEXT LOOPEN.
      END.

    JobbLinje.DivX[36] = JobbLinje.DivX[36] + " " + ENTRY(iLoop,cTekst,"|").
  END. /* LOOPEN */
END.

/* Bygger temp-tabeller for † kunne sl† sammen bestillingene. */
PROCEDURE ByggTmpTabeller:
  DEF VAR iLinjeNr    as INT  NO-UNDO.
  DEF VAR cStorrelser as CHAR NO-UNDO.
  DEF VAR iLoop       as INT  NO-UNDO.
  DEF VAR cTekst      as CHAR NO-UNDO.
  DEF VAR iInt        as INT  NO-UNDO.

  BYGG:
  FOR EACH ListeLinje OF Lister NO-LOCK:

    FIND ArtBas NO-LOCK where
      ArtBas.ArtikkelNr = DEC(ENTRY(1,ListeLinje.DataObjekt,",")) NO-ERROR.
    if NOT AVAILABLE ArtBas THEN
      NEXT BYGG.

    FIND BestHode NO-LOCK where
      BestHode.BestNr = INT(ENTRY(2,ListeLinje.DataObjekt,",")) NO-ERROR.
    if NOT AVAILABLE BestHode THEN
      NEXT BYGG.

    if BestHode.OrdreNr <> 0 then
      FIND Ordre  NO-LOCK where
        Ordre.OrdreNr = BestHode.OrdreNr NO-ERROR.
    FIND LevBas OF BestHode NO-LOCK NO-ERROR.

    /* Oppretter en tmp-ordre. */
    FIND tmpOrdre where
      tmpOrdre.OrdreNr = BestHode.OrdreNr NO-ERROR.
    if NOT AVAILABLE tmpOrdre then
      DO:
        CREATE tmpOrdre.
        assign
          tmpOrdre.OrdreNr = (if BestHode.OrdreNr = ?
                                then 0
                                else BestHode.OrdreNr)
          tmpOrdre.LevNr   = BestHode.LevNr
          .
      END.

    /* Oppretter tmp-ordrelinje. */
    if BestHode.DirekteLev = FALSE then
      SENTRAL-LEVERING:
      DO:
        /* TN 12/9-00
        FIND tmpOrdLin where
          tmpOrdLin.OrdreNr       = BestHode.OrdreNr   and
          tmpOrdLin.LevKod        = ArtBas.LevKod      and
          tmpOrdLin.StrTypeId     = BestHode.StrTypeId and
          tmpOrdLin.LevSted       = wCl and
          tmpOrdLin.LevDato       = BestHode.LevDato NO-ERROR.
        */
        FIND tmpOrdLin where
          tmpOrdLin.OrdreNr       = BestHode.OrdreNr   and
          tmpOrdLin.LevKod        = ArtBas.LevKod      and
          tmpOrdLin.BestNr        = BestHode.BestNr    and
          tmpOrdLin.StrTypeId     = BestHode.StrTypeId and
          tmpOrdLin.LevSted       = wCl and
          tmpOrdLin.LevDato       = BestHode.LevDato NO-ERROR.
        if NOT AVAILABLE tmpOrdLin then
          DO:
            CREATE tmpOrdLin.
            /* TN 12/9-00
            assign
              iLinjeNr = iLinjeNr + 1
              tmpOrdLin.OrdreNr   = BestHode.OrdreNr
              tmpOrdLin.LinjeNr   = iLinjeNr
              tmpOrdLin.LevKod    = ArtBas.LevKod
              tmpOrdLin.StrTypeId = BestHode.StrTypeId
              tmpOrdLin.LevSted   = wCl
              tmpOrdLin.LevDato   = BestHode.LevDato
              tmpOrdLin.ArtikkelNr = ArtBas.ArtikkelNr.
            */
            assign
              iLinjeNr = iLinjeNr + 1
              tmpOrdLin.OrdreNr   = BestHode.OrdreNr
              tmpOrdLin.LinjeNr   = iLinjeNr
              tmpOrdLin.LevKod    = ArtBas.LevKod
              tmpOrdLin.BestNr    = BestHode.BestNr
              tmpOrdLin.StrTypeId = BestHode.StrTypeId
              tmpOrdLin.LevSted   = wCl
              tmpOrdLin.LevDato   = BestHode.LevDato
              tmpOrdLin.ArtikkelNr = ArtBas.ArtikkelNr.
            RUN SettAdresserOgPriser (wCl).
          END.

        /* Logger de bestillingene som skal inn p† linjen */
        FIND tmpBestilling where
          tmpBestilling.OrdreNr  = BestHode.OrdreNr  and
          tmpBestilling.LinjeNr  = tmpOrdLin.LinjeNr and
          tmpBestilling.BestNr   = BestHode.BestNr NO-ERROR.
        if NOT AVAILABLE tmpBestilling then
          DO:
            CREATE tmpBestilling.
            assign
              tmpBestilling.OrdreNr  = BestHode.OrdreNr
              tmpBestilling.LinjeNr  = tmpOrdLin.LinjeNr
              tmpBestilling.BestNr   = BestHode.BestNr.
          END.

        /* Henter st›rrelsesliste */
        FIND FIRST BestSort of BestHode NO-LOCK where
          BestSort.Fri = TRUE NO-ERROR.
        assign
          cStorrelser = BestSort.Storrelser.

        /* Oppretter og akkumulerer sortimentene */
        FOR EACH BestSort OF BestHode NO-LOCK:
          FIND tmpSort where
            tmpSort.OrdreNr = BestHode.OrdreNr and
            tmpSort.LinjeNr = tmpOrdLin.LinjeNr and
            tmpSort.SortId  = BestSort.SortId NO-ERROR.
          if NOT AVAILABLE tmpSort then
            DO:
              CREATE tmpSort.
              assign
                tmpSort.OrdreNr     = BestHode.OrdreNr
                tmpSort.LinjeNr     = tmpOrdLin.LinjeNr
                tmpSort.SortId      = BestSort.SortId
                tmpSort.Fri         = BestSort.Fri
                tmpSort.Antall      = BestSort.Antall
                tmpSort.StrInterval = BestSort.StrInterval
                tmpSort.Fordeling   = BestSort.Fordeling
                tmpSort.Storrelser  = cStorrelser.
            END.
          /* Akkumulerer faste inndelinger */
          if BestSort.Fri = FALSE then
            assign
              tmpSort.AntSort = tmpSort.AntSort + BestSort.AntSort.
          /* Akkumulerer fri inndelinger */
          ELSE DO:
            FOR EACH FriButik OF BestHode NO-LOCK: /* BestStat skal ikke tas hensyn til */
              DO iLoop = 1 to NUM-ENTRIES(cStorrelser," "):
                assign
                  tmpSort.FriAntall[iLoop] = tmpSort.FriAntall[iLoop] + FriButik.FriAntal[iLoop].
              END.
            END.
            assign
              cTekst = ""
              iInt   = 0.
            DO iLoop = 1 to NUM-ENTRIES(cStorrelser," "):
              assign
                cTekst = cTekst +
                         (if cTekst = ""
                           THEN ""
                           ELSE " ") +
                         STRING(tmpSort.FriAntall[iLoop])
                iInt   = iInt + tmpSort.FriAntall[iLoop].
            END.
            assign
              tmpSort.AntSort     = 1
              tmpSort.Antall      = iInt
              tmpSort.Fordeling   = cTekst
              tmpSort.StrInterval = ENTRY(1,cStorrelser," ") + " - " + ENTRY(NUM-ENTRIES(cStorrelser," "),cStorrelser," ")
              .
          END.
        END.

      END. /* SENTRAL-LEVERING */
    ELSE
    DIREKTE-LEVERING:
    DO:
      /* I BestLinje ligger en post pr. leveringssted. */
      FOR EACH BestLinje of BestHode NO-LOCK:
        /* TN 12/9-00
        FIND tmpOrdLin where
          tmpOrdLin.OrdreNr       = BestHode.OrdreNr   and
          tmpOrdLin.LevKod        = ArtBas.LevKod      and
          tmpOrdLin.StrTypeId     = BestHode.StrTypeId and
          tmpOrdLin.LevSted       = BestLinje.Butik and
          tmpOrdLin.LevDato       = BestHode.LevDato NO-ERROR.
        */
        FIND tmpOrdLin where
          tmpOrdLin.OrdreNr       = BestHode.OrdreNr   and
          tmpOrdLin.LevKod        = ArtBas.LevKod      and
          tmpOrdLin.BestNr        = BestHode.BestNr    and
          tmpOrdLin.StrTypeId     = BestHode.StrTypeId and
          tmpOrdLin.LevSted       = BestLinje.Butik and
          tmpOrdLin.LevDato       = BestHode.LevDato NO-ERROR.
        if NOT AVAILABLE tmpOrdLin then
          DO:
            CREATE tmpOrdLin.
            /* TN 12/9-00
            assign
              iLinjeNr = iLinjeNr + 1
              tmpOrdLin.OrdreNr    = BestHode.OrdreNr
              tmpOrdLin.LinjeNr    = iLinjeNr
              tmpOrdLin.LevKod     = ArtBas.LevKod
              tmpOrdLin.StrTypeId  = BestHode.StrTypeId
              tmpOrdLin.LevSted    = BestLinje.Butik
              tmpOrdLin.LevDato    = BestHode.LevDato
              tmpOrdLin.ArtikkelNr = ArtBas.ArtikkelNr.
            */
            assign
              iLinjeNr = iLinjeNr + 1
              tmpOrdLin.OrdreNr    = BestHode.OrdreNr
              tmpOrdLin.LinjeNr    = iLinjeNr
              tmpOrdLin.LevKod     = ArtBas.LevKod
              tmpOrdLin.BestNr     = BestHode.BestNr
              tmpOrdLin.StrTypeId  = BestHode.StrTypeId
              tmpOrdLin.LevSted    = BestLinje.Butik
              tmpOrdLin.LevDato    = BestHode.LevDato
              tmpOrdLin.ArtikkelNr = ArtBas.ArtikkelNr.
            RUN SettAdresserOgPriser (BestLinje.Butik).
          END.

        /* Logger de bestillingene som skal inn p† linjen */
        FIND tmpBestilling where
          tmpBestilling.OrdreNr  = BestHode.OrdreNr  and
          tmpBestilling.LinjeNr  = tmpOrdLin.LinjeNr and
          tmpBestilling.BestNr   = BestHode.BestNr NO-ERROR.
        if NOT AVAILABLE tmpBestilling then
          DO:
            CREATE tmpBestilling.
            assign
              tmpBestilling.OrdreNr  = BestHode.OrdreNr
              tmpBestilling.LinjeNr  = tmpOrdLin.LinjeNr
              tmpBestilling.BestNr   = BestHode.BestNr.
          END.

        /* Henter st›rrelsesliste */
        FIND FIRST BestSort of BestHode NO-LOCK where
          BestSort.Fri = TRUE NO-ERROR.
        assign
          cStorrelser = BestSort.Storrelser.

        /* Oppretter og akkumulerer sortimentene */
        FOR EACH BestSort OF BestHode NO-LOCK:
          FIND tmpSort where
            tmpSort.OrdreNr = BestHode.OrdreNr and
            tmpSort.LinjeNr = tmpOrdLin.LinjeNr and
            tmpSort.SortId  = BestSort.SortId NO-ERROR.
          if NOT AVAILABLE tmpSort then
            DO:
              CREATE tmpSort.
              assign
                tmpSort.OrdreNr     = BestHode.OrdreNr
                tmpSort.LinjeNr     = tmpOrdLin.LinjeNr
                tmpSort.SortId      = BestSort.SortId
                tmpSort.Fri         = BestSort.Fri
                tmpSort.Antall      = BestSort.Antall
                tmpSort.StrInterval = BestSort.StrInterval
                tmpSort.Fordeling   = BestSort.Fordeling
                tmpSort.Storrelser  = cStorrelser.
            END.
          /* Akkumulerer faste inndelinger                          */
          /* Her er det ikke n›dvendig † akkumulere pr. st›rrelse.  */
          /* Den informasjonen ligger allerede i Fordelings feltet. */
          if BestSort.Fri = FALSE then
            DO:
              FIND BestKasse OF BestHode NO-LOCK where
                BestKasse.SortId = BestSort.SortId and
                BestKasse.Butik  = BestLinje.Butik NO-ERROR.
              if AVAILABLE BestKasse then
                assign
                  tmpSort.AntSort = tmpSort.AntSort + BestKasse.Antal.
            END.
          /* Akkumulerer fri inndelinger                            */
          /* Tar n† kun med de som er aktuelle for leveringsstedet. */
          ELSE DO:
            assign
              tmpSort.AntSort = 1.
            FOR EACH FriButik OF BestHode NO-LOCK where
              FriButik.Butik    = BestLinje.Butik:  /* BestStat skal ikke tas hensyn til */
              DO iLoop = 1 to NUM-ENTRIES(cStorrelser," "):
                assign
                  tmpSort.FriAntall[iLoop] = tmpSort.FriAntall[iLoop] + FriButik.FriAntal[iLoop].
              END.
            END.
            assign
              cTekst = ""
              iInt   = 0.
            DO iLoop = 1 to NUM-ENTRIES(cStorrelser," "):
              assign
                cTekst = cTekst +
                         (if cTekst = ""
                           THEN ""
                           ELSE " ") +
                         STRING(tmpSort.FriAntall[iLoop])
                iInt   = iInt + tmpSort.FriAntall[iLoop].
            END.
            assign
              tmpSort.AntSort     = 1
              tmpSort.Antall      = iInt
              tmpSort.Fordeling   = cTekst
              tmpSort.StrInterval = ENTRY(1,cStorrelser," ") + " - " + ENTRY(NUM-ENTRIES(cStorrelser," "),cStorrelser," ")
              .
          END.
        END.
      END. /* BestLinje */

    END. /* DIREKTE-LEVERING */
  END. /* BYGG */

  /*
  /* TEST TEST TEST */
  FOR EACH tmpOrdre:
    DISPLAY tmpOrdre.
    FOR EACH tmpBestilling NO-LOCK where
      tmpBestilling.OrdreNr = tmpOrdre.OrdreNr:
      DISPLAY tmpBestilling.
      PAUSE.
    END.

    FOR EACH tmpOrdLin WHERE
      tmpOrdLin.OrdreNr = tmpOrdre.ORdreNr:
      DISPLAY tmpOrdLin.
      PAUSE.
      FOR EACH tmpSort where
        tmpSort.ORdreNr = tmpOrdre.ORdreNr and
        tmpSort.LinjeNr = tmpOrdLin.LinjeNr:
        DISPLAY tmpSort.
        PAUSE.
      END.
    END.
  END. /* TEST TEST TEST */
  */
END PROCEDURE.

/* Setter opp adressefelt i tempor‘r tabell */
PROCEDURE SettAdresserOgPriser:
  DEF INPUT PARAMETER iButik as INT NO-UNDO.

  DEF VAR cFirma as CHAR NO-UNDO.

  DEF BUFFER bufButikk FOR Butiker.
  DEF BUFFER bufProfil FOR PrisProfil.

  {syspara.i 1 1 100 cFirma}

  FIND Sasong OF ArtBas NO-LOCK NO-ERROR.

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
  assign
    tmpOrdLin.Sesong          = if available Sasong
                                then SaSong.SasBeskr
                                else ""
    tmpOrdLin.FilNavn         = wFilNavn
    tmpOrdLin.BestillingsDato = BestHode.BestillingsDato.
    .

  if ArtBas.VisDivInfo[10] then
    DO:
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.
      assign
        tmpOrdLin.LevFargKod = if ArtBas.LevFargKod <> ""
                                 then ArtBas.LevFargKod
                                 ELSE "/"  + Farg.FarBeskr.
    END.

  /* Navn og adresse p† firma som sender ordren. */
  find bufButikk NO-LOCK where
    bufButikk.Butik = wCl NO-ERROR.
  if NOT AVAILABLE bufButikk then
    RETURN.
  find Post no-lock where
    Post.PostNr = bufButikk.BuPoNr no-error.

  assign
    tmpOrdLin.AvsFirma      = cFirma
    tmpOrdLin.AvsButSjef    = bufButikk.BuKon
    tmpOrdLin.AvsAdresse    = bufButikk.BuAdr
    tmpOrdLin.AvsPostBoks   = bufButikk.BuPadr
    tmpOrdLin.AvsPostNr     = bufButikk.BuPoNr
    tmpOrdLin.AvsPostSted   = if available Post
                                then Post.Beskrivelse
                                else "".
    tmpOrdLin.AvsTelefon    = bufButikk.BuTel.
  /* Navn og adresse p† leveringssted (Mottagende butikk) */
  find bufButikk NO-LOCK where
    bufButikk.Butik = iButik NO-ERROR.
  if NOT AVAILABLE bufButikk then
    RETURN.
  find Post no-lock where
    Post.PostNr = bufButikk.LevPostNr no-error.

  assign
    tmpOrdLin.MotFirma      = bufButikk.ButNamn
    tmpOrdLin.MotKontakt    = bufButikk.LevKontakt
    tmpOrdLin.MotAdresse    = bufButikk.LevAdresse1
    tmpOrdLin.MotPostBoks   = bufButikk.LevPostBoks
    tmpOrdLin.MotPostNr     = bufButikk.LevPostNr
    tmpOrdLin.MotPostSted   = if available Post
                                then Post.Beskrivelse
                                ELSE ""
    tmpOrdLin.MotTelefon    = bufButikk.LevTelefon.

  /* Henter og setter priser ut fra bestillingens kalkyle. */
  FIND bufProfil NO-LOCK where
    bufProfil.ProfilNr = bufButikk.ProfilNr NO-ERROR.
  if NOT AVAILABLE bufProfil then
    FIND bufProfil NO-LOCK where
      bufProfil.ProfilNr = clButiker.ProfilNr NO-ERROR.
  if AVAILABLE bufProfil then
    DO:
      find BestPris NO-LOCK where
        BestPris.BestNr   = BestHode.BestNr and
        BestPris.BestStat = BestHode.BestStat and
        BestPris.ProfilNr = bufPRofil.ProfilNr NO-ERROR.
      if NOT available BestPris then
        find BestPris NO-LOCK where
          BestPris.BestNr   = BestHode.BestNr and
          BestPris.BestStat = BestHode.BestStat and
          BestPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
      if AVAILABLE BestPris then
      assign
        tmpOrdLin.ValPris   = BestPris.ValPris
        tmpOrdLin.Rab1%     = BestPris.Rab1%
        tmpOrdLin.NettoPris = BestPris.ValPris - ((BestPris.ValPris * BestPris.Rab1%) / 100).
    END.

  /* Navn og adresse p† mottager av ordre (Leverand›r) */
  find Post no-lock where
    Post.PostNr = LevBas.LevPoNr no-error.
  assign
    tmpOrdLin.LevFirma      = LevBas.LevNamn
    tmpOrdLin.LevKontakt    = LevBas.LevKon
    tmpOrdLin.LevAdresse    = LevBas.LevAdr
    tmpOrdLin.LevPostBoks   = ""
    tmpOrdLin.LevPostNr     = LevBas.LevPoNr
    tmpOrdLin.LevPostSted   = if available Post
                                then Post.Beskrivelse
                                ELSE ""
    tmpOrdLin.LevTelefon    = LevBas.LevTel.
END PROCEDURE.

PROCEDURE OpprettDetaljer:
  DEF VAR iLoop  as INT NO-UNDO.
  DEF VAR i2Loop as INT NO-UNDO.

  DEF BUFFER bufDetaljer FOR Detaljer.

  CREATE bufDetaljer.
  assign
    bufDetaljer.JobbNr      = bufJobbLinje.JobbNr
    bufDetaljer.Char1       = bufJobbLinje.Char1
    bufDetaljer.Char2       = bufJobbLinje.Char2
    bufDetaljer.Char3       = bufJobbLinje.Char3
    bufDetaljer.SeqNr       = 1
    bufDetaljer.DivChar[ 1] = "DETALJER"
    .

  i2Loop = 0.
  LOOPEN:
  DO iLoop = 1 to NUM-ENTRIES(tmpSort.Storrelser," "):
    if TRIM(ENTRY(iLoop,tmpSort.Fordeling," ")) = "0" then
      NEXT LOOPEN.
    assign
      i2Loop = i2Loop + 1
      bufDetaljer.DefChar[i2Loop] = TRIM(ENTRY(iLoop,tmpSort.Storrelser," "))
      bufDetaljer.DefDec[i2Loop]  = DEC(TRIM(ENTRY(iLoop,tmpSort.Fordeling," ")))
      .
  END. /* LOOPEN */

  /* Undertrykker tomme st›rrelser p† slutten. */
  2LOOPEN:
  DO iLoop = 50 to 1 by -1:
    if bufDetaljer.DefDec[iLoop] <> 0 then
      LEAVE 2LOOPEN.
    assign
      bufDetaljer.DefChar[iLoop] = ?
      bufDetaljer.DefDec[iLoop]  = ?
      .
  END. /* 2LOOPEN */

END PROCEDURE.

/* Bygger liste over labler basert p† leverand›rens spr†k. */
PROCEDURE SettLblListe:
  DEF INPUT PARAMETER  cLng      as CHAR NO-UNDO.
  DEF OUTPUT PARAMETER cLblListe as CHAR NO-UNDO.

  DEF VAR iSysHId as INT NO-UNDO.
  DEF VAR iSysGr  as INT NO-UNDO.

  assign
    iSysHId = 5
    iSysGr  = ?.

  if (cLng = "DES" or cLng = "") then
    DO:
      assign
        cLng    = "DES"
        iSysHId = 6
        iSysGr  = 105.
    END.
  ELSE DO:
    FIND FIRST SysGruppe NO-LOCK where
      SysGruppe.SysHId = iSysHId and
      SysGruppe.SysGr  >= 201 and
      SysGruppe.SysGr  <= 249 and
      SysGruppe.Beskrivelse = cLng NO-ERROR.
    if NOT AVAILABLE SysGruppe then
      assign
        cLng    = "DES"
        iSysHId = 6
        iSysGr  = 105.
    else
      assign
        iSysHId = 5
        iSysGr  = SysGruppe.SysGr.
  END.

  assign
    cLblListe  = "".
  LBL-LISTE:
  FOR EACH SysPara NO-LOCK where
    SysPAra.SysHId = iSysHId and
    SysPAra.SysGr  = iSysGr:

    assign
      cLblListe = cLblListe +
                  (if cLblListe = ""
                     THEN ""
                     ELSE ",") +
                  SysPara.Parameter1.
  END. /* LBL-LISTE */

  /* TEST */
  /*
  MESSAGE "Lng:" cLng skip
          "Koder:" iSysHId iSysGr skip
          "LAbler:" cLblListe
          VIEW-AS ALERT-BOX.
  */
END PROCEDURE.

