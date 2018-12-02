/************************************************************
    Program:  byggstlinje.p
    Created:  TN   24 Aug 99
Description:

Last change:  TN   29 Nov 100    7:27 pm
************************************************************/

def input parameter wDataObjekt        as char   no-undo.
def input parameter wPerId             as char   no-undo.
def input parameter wStTypeId          as char   no-undo.
DEF INPUT parameter wNullstill         as LOG    NO-UNDO.
DEF INPUT PARAMETER wKriterier         as CHAR   NO-UNDO.

DEF var wAAr1       as int  NO-UNDO.
DEF var wAAr2       as int  NO-UNDO.
DEF var wPerLin1    as int  NO-UNDO.
DEF var wPerLin2    as int  NO-UNDO.
DEF VAR wButik      as INT  NO-UNDO.
DEF VAR wPeriodeTot as log  NO-UNDO.
DEF VAR wTidsavgr   as LOG  NO-UNDO.
DEF VAR wSettLager  as LOG  NO-UNDO.
DEF VAR wHg         as INT  NO-UNDO.
DEF VAR wStartTid   as int  NO-UNDO.
DEF VAR wLoop1      as INT  NO-UNDO.
DEF VAR wLoop2      as INT  NO-UNDO.
DEF VAR wFra        as INT  NO-UNDO.
DEF VAR wTil        as INT  NO-UNDO.
DEF VAR wWeekNum    as INT  NO-UNDO.
DEF VAR wLayout     as CHAR NO-UNDO.
DEF VAR wCL         AS INT  NO-UNDO.

DEF VAR wBevegelse  as DEC  NO-UNDO.
DEF VAR wLagerVerdi as DEC  NO-UNDO.

/*
MESSAGE 
    "wDataObjekt" wDataObjekt SKIP
    "wPerId"      wPerId      SKIP
    "wStTypeId"   wStTypeId   SKIP
    "wNullstill"  wNullstill  SKIP
    "wKriterier"  wKriterier  SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

DEF SHARED TEMP-TABLE tStLinje no-undo like StLinje.

DEF BUFFER ttStLinje FOR tStLinje.

RUN ByggTabell.

{syspara.i 5 1 1 wCL INT}

RUN EksporterTabell.

/* ------------- Procedurer ------------------- */
/* Ny inngang som gjør det samme som vanlig inngang.                       */
/* Gjør at det ikke er nødvendig  slette programmet før det kalles pånytt. */
/* Dvs at rutinen kan kjøres persistent ved oppdatering av mange poster.   */
PROCEDURE Historikk:
  def input parameter ipDataObjekt        as char   no-undo.
  def input parameter ipPerId             as char   no-undo.
  def input parameter ipStTypeId          as char   no-undo.
  DEF INPUT PARAMETER ipNullstill         as LOG    NO-UNDO.
  DEF INPUT PARAMETER ipKriterier         as CHAR   NO-UNDO.

  assign
    wDataObjekt = ipDataObjekt
    wPerId      = ipPerId
    wStTypeId   = ipStTypeId
    wNullstill  = ipNullstill
    wKriterier  = ipKriterier.

  RUN ByggTabell.

  /* RUN EksporterTabell.*/

END.

PROCEDURE ByggTabell:

/* Flagger setting av inn/utgående lager.                           */
/* Layout som ikke viser lager, skal ikke kjøre beregning av lager. */
IF NUM-ENTRIES(wKriterier) >= 9 then
DO:
  IF CAN-DO("Nei,250,301,401,501,601,701,801,901,1001",ENTRY(9,wKriterier)) then
    assign
      wSettLager = FALSE.
  else
    assign
      wSettLager = TRUE.
END.
ELSE
  assign
    wSettLager = TRUE.
/* Disse skal ikke ha visning av lager. */
IF can-do("KUNDSTAT,MEDLEM,MEDLEMTOT,SELGER,SELGERSTAT,SELGER-ART,SELGER-VG,KASS-ART,KASS-VG",wStTypeId) THEN
    wSettLager = FALSE.

/* Flagger bygging av periodetotaler. */
/* Default verdi er Ja.               */
/* 0-Nei, 1-Ja                        */
IF num-entries(wKriterier) >= 7 then
    wPeriodeTot   = if INT(ENTRY(7,wKriterier)) = 1
                      THEN true
                      ELSE FALSE.
ELSE wPeriodeTot = TRUE.
/* Flagger at avgrensing skal benyttes */
/* Default verdi er Nei                */
/* 0-Nei, 1-Ja                         */
IF num-entries(wKriterier) >= 8 then
    wTidsavgr   = if INT(ENTRY(8,wKriterier)) = 1
                      THEN true
                      ELSE FALSE.
ELSE wTidsavgr = false.

/* Ekstra info. Sender med varegruppens HG n†r HG stat skal plukkes ut. */
IF num-entries(wKriterier) >= 6 then
  wHg = INT(ENTRY(6,wKriterier)).
ELSE wHg = 0.

/* Nullstiller. */
IF wNullstill then
  DO:
    EMPTY TEMP-TABLE tStLinje  NO-ERROR.
    EMPTY TEMP-TABLE ttStLinje NO-ERROR.
  END.

/* Leser avgrensinger hvis avgrensing skal benyttes. */
/* NB: Dette gjøres om igjen etter byggstlinje.i.    */
IF wTidsavgr then
  assign
    wAAr1    = INT(ENTRY(1,wKriterier))
    wAAr2    = INT(ENTRY(2,wKriterier))
    wPerLin1 = INT(ENTRY(3,wKriterier))
    wPerLin2 = INT(ENTRY(4,wKriterier))
    wButik   = INT(ENTRY(5,wKriterier)).

/* Bygger temp-table.                                */
/* Kopierer alle postene fra StLinje inn i tStLinje. */
/* Kopieres her ved hjelp av Buffer-Copy.            */
IF wButik = 0 then {byggstlinje.i &Butikk = " "}
else {byggstlinje.i &Butikk = "and stLinje.Butik = wButik"}

/* Frisker opp igjen fordi variablene er blitt endret i byggstlinje.i */
IF wTidsavgr then
  assign
    wAAr1    = INT(ENTRY(1,wKriterier))
    wAAr2    = INT(ENTRY(2,wKriterier))
    wPerLin1 = INT(ENTRY(3,wKriterier))
    wPerLin2 = INT(ENTRY(4,wKriterier))
    wButik   = INT(ENTRY(5,wKriterier)).

/* For å få med alle artikkler, må det opprettes en tom post innenfor */
/* den perioden det spørres om.                                       */
/* FIX for å få med post på utskrift.                                 */
IF wTidsAvgr THEN
DO:
  IF wButik = 0 THEN
  DO:
    IF NOT CAN-FIND(FIRST tStLinje where
                    tStLinje.DataObjekt = wDataObjekt and
                    tStLinje.StTypeId   = wStTypeId and
                    tStLinje.PerId      = wPerId and
                    tStLinje.AAr        = wAAr2 and
                    tStLinje.PerLinNr   = wPerLin2 AND
                    tStLinje.Diverse    = "") THEN
    DO:
      CREATE tStLinje.
      assign
        tStLinje.StTypeId   = wStTypeId 
        tStLinje.PerId      = wPerId 
        tStLinje.DataObjekt = wDataObjekt 
        tStLinje.Diverse    = ""
        tStLinje.Butik      = wCL
        tStLinje.Aar        = wAAr2
        tStLinje.PerLinNr   = wPerLin2.      
    END.
  END.
  else
  DO:
    IF NOT CAN-FIND(FIRST tStLinje where
                    tStLinje.DataObjekt = wDataObjekt and
                    tStLinje.StTypeId   = wStTypeId and
                    tStLinje.PerId      = wPerId and
                    tStLinje.AAr        = wAAr2 and
                    tStLinje.PerLinNr   = wPerLin2 AND
                    tStLinje.Butik      = wButik AND
                    tStLinje.Diverse    = "") THEN
    DO:
      CREATE tStLinje.
      assign
        tStLinje.StTypeId   = wStTypeId 
        tStLinje.PerId      = wPerId 
        tStLinje.DataObjekt = wDataObjekt 
        tStLinje.Diverse    = ""
        tStLinje.Butik      = wButik
        tStLinje.Aar        = wAAr2
        tStLinje.PerLinNr   = wPerLin2.      
    END.
  END.

END. /* FIX for å få med post på utskrift. */

/* Legger inn utgående lager for alle perioder.             */
/* Lagerverdien "rulles" opp egjennom alle periodenlinjene. */
IF wSettLager then /* TEST */
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   < 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Butik
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending:

  IF first-of(tStLinje.Butik) then
    RUN FinnLager.
  ELSE assign
    tStLinje.LagerAnt   = wBevegelse
    tStLinje.LagerVerdi = wLagerVerdi. /* Verdi fra forrige post. */

  assign
    wBevegelse = tStLinje.LagerAnt

                 + tStLinje.AntSolgt
                 + tStLinje.BrekkAnt
                 + tStLinje.IntAnt
                 - tStLinje.OvAnt
                   /* - tStLinje.ReklAnt  */
                 + tStLinje.ReklLAnt
                   /* - tStLinje.GjenkjopAnt */
                 - tStLinje.KjopAnt
                 - tStLinje.JustAnt
                 - tStLinje.SvinnAnt
    wLagerVerdi = tStLinje.LagerVerdi

                 + tStLinje.VVareKost
                 + tStLinje.BrekkVerdi
                 + tStLinje.IntVerdi
                 - tStLinje.OvVerdi
                   /* - tStLinje.ReklVerdi  */
                 + tStLinje.ReklLAnt
                   /* - tStLinje.GjenkjopVerdi */
                 - tStLinje.KjopVerdi
                 - tStLinje.JustVerdi
                 - tStLinje.SvinnVerdi
    tStLinje.PrimoAnt     = wBevegelse
    tStLinje.PrimoVerdi   = wLagerVerdi
    tStLinje.OmlHast      = tStLinje.AntSolgt / ((tStLinje.LagerAnt + tStLinje.KjopAnt + tStLinje.OvAnt) / 2)
    tStLinje.OmlHast      = IF tStLinje.OmlHast = ? THEN 0 ELSE tStLinje.OmlHast
    tStLinje.DiverseAnt   =   tStLinje.SvinnAnt
                            + tStLinje.JustAnt
                            + tStLinje.IntAnt
                            + tStLinje.BrekkAnt
    tStLinje.DiverseVerdi =   tStLinje.SvinnVerdi
                            + tStLinje.JustVerdi
                            + tStLinje.IntVerdi
                            + tStLinje.BrekkVerdi
    .
/*
MESSAGE
  tStLinje.DataObjekt skip
  tStLinje.StTypeId   skip
  tStLinje.Butik      skip
  tStLinje.Aar        skip
  tStLinje.PerId      skip
  tStLinje.PerLinNr   SKIP(1)

  "tStLinje.LagerAnt" tStLinje.LagerAnt SKIP(1)
  "tStLinje.AntSolgt" tStLinje.AntSolgt skip
  "tStLinje.BrekkAnt" tStLinje.BrekkAnt skip
  "tStLinje.IntAnt  " tStLinje.IntAnt   skip
  "tStLinje.ReklLAnt" tStLinje.ReklLAnt skip
  "tStLinje.KjopAnt " tStLinje.KjopAnt  skip
  "tStLinje.OvAnt   " tStLinje.OvAnt    skip
  "tStLinje.JustAnt " tStLinje.JustAnt  skip
  "tStLinje.SvinnAnt" tStLinje.SvinnAnt SKIP(1)
  "wBevegelse"        wBevegelse
 VIEW-AS ALERT-BOX.
*/
END.
/*
wStartTid = TIME.
MESSAGE "Test-1" VIEW-AS ALERT-BOX.
MESSAGE "Test-2" STRING(TIME - wStartTid,"HH:MM:SS") VIEW-AS ALERT-BOX.
*/

/* Tar bort periodelinjer som ligger utenfor forespurt tidsområde. */
IF wTidsavgr then
  KRITERIER:
  DO:
    IF NUM-ENTRIES(wKriterier) < 5 then
      LEAVE KRITERIER.

    FOR EACH tStLinje where
      tStLinje.DataObjekt = wDataObjekt and
      tStLinje.StTypeId   = wStTypeId and
      tStLinje.PerId      = wPerId and
      tStLinje.PerLinNr   < 1000000 and
      (if wButik = 0
        THEN true
        ELSE tStLinje.Butik = wButik)
      break
      by tStLinje.DataObjekt
      by tStLinje.StTypeId
      by tStLinje.Butik
      by tStLinje.Aar descending
      by tStLinje.PerId descending
      by tStLinje.PerLinNr descending:

      /* Ligger i †ret f›r. */
      if tStLinje.Aar < wAAr1 then
        DELETE tStLinje.

      /* Ligger i †ret etter. */
      if AVAILABLE tStLinje then
        DO:
         if tStLinje.Aar > wAAr2 then
           DELETE tStLinje.
        END.

      /* Fase 2 */
      if AVAILABLE tStLinje then
        DO:
          /* F›ste †r */
          if tStLinje.Aar = wAAr1 then
            DO:
              if tStLinje.PerLinNr < wPerLin1 then
                DELETE tStLinje.
            END.

          /* rene imellom er greie */

          /* Siste †r. */
          if AVAILABLE tStLinje then
            DO:
              if tStLinje.Aar = wAAr2 then
                DO:
                  if tStLinje.PerLinNr > wPerLin2 then
                    DELETE tStLinje.
                END.
            END.
        END.

      /* Tar bort butikkene som ikke skal v‘re med. */
      if wButik <> 0 and available tStLinje then
        DO:
          if tStLinje.Butik <> wButik then
            DELETE tStLinje.
        end.

    END.
  END. /* KRITERIER */

/* Genererer butikktotaler. */
if wPeriodeTot then
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   < 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Butik
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending:

  IF FIRST-OF(tStLinje.Butik) then
    DO:
      FIND FIRST ttStLinje where
                 ttStLinje.DataObjekt = wDataObjekt and
                 ttStLinje.StTypeId   = wStTypeId   and
                 ttStLinje.PerId      = wPerId      and
                 ttStLinje.butik      = tStLinje.Butik  and
                 ttStLinje.PerLinNr   = 1000000     and
                 ttStLinje.Aar        = tStLinje.Aar NO-ERROR.
      IF NOT AVAILABLE ttStLinje THEN
      DO:
        CREATE ttStLinje.
        assign
          ttStLinje.DataObjekt = wDataObjekt
          ttStLinje.StTypeId   = wStTypeId
          ttStLinje.PerId      = wPerId
          ttStLinje.butik      = tStLinje.Butik
          ttStLinje.PerLinNr   = 1000000
          ttStLinje.Aar        = tStLinje.Aar
          ttStLinje.Hg         = tStLinje.Hg
          ttStLinje.TotalPost  = 1
          .
      END.
    END.

  if AVAILABLE ttStLinje then
  ASSIGN
    ttStLinje.LagerAnt       = if FIRST-OF(tStLinje.butik)
                                 then tStLinje.LagerAnt
                                 ELSE ttStLinje.LagerAnt
    ttStLinje.LagerVerdi     = if FIRST-OF(tStLinje.butik)
                                 then tStLinje.LagerVerdi
                                 ELSE ttStLinje.LagerVerdi
    ttStLinje.PrimoAnt       = if LAST-OF(tStLinje.Butik)
                                 then tStLinje.PrimoAnt
                                 ELSE ttStLinje.PrimoAnt
    ttStLinje.PrimoVerdi     = if LAST-OF(tStLinje.Butik)
                                 then tStLinje.PrimoVerdi
                                 ELSE ttStLinje.PrimoVerdi
    ttStLinje.VVarekost      = ttStLinje.VVarekost      + tStLinje.VVareKost
    ttStLinje.AntSolgt       = ttStLinje.AntSolgt       + tStLinje.AntSolgt
    ttStLinje.AntRab         = ttStLinje.AntRab         + tStLinje.AntRab
    ttStLinje.BrekkAnt       = ttStLinje.BrekkAnt       + tStLinje.BrekkAnt
    ttStLinje.IntAnt         = ttStLinje.IntAnt         + tStLinje.IntAnt
    ttStLinje.ReklAnt        = ttStLinje.ReklAnt        + tStLinje.ReklAnt
    ttStLinje.ReklLAnt       = ttStLinje.ReklLAnt       + tStLinje.ReklLAnt
    ttStLinje.GjenkjopAnt    = ttStLinje.GjenkjopAnt    + tStLinje.GjenkjopAnt
    ttStLinje.KjopAnt        = ttStLinje.KjopAnt        + tStLinje.KjopAnt
    ttStLinje.OvAnt          = ttStLinje.OvAnt          + tStLinje.OvAnt
    ttStLinje.JustAnt        = ttStLinje.JustAnt        + tStLinje.JustAnt
    ttStLinje.JustVerdi      = ttStLinje.JustVerdi      + tStLinje.JustVerdi
    ttStLinje.SvinnAnt       = ttStLinje.SvinnAnt       + tStLinje.SvinnAnt
    ttStLinje.SvinnVerdi     = ttStLinje.SvinnVerdi     + tStLinje.SvinnVerdi
    ttStLinje.NedAnt         = ttStLinje.NedAnt         + tStLinje.NedAnt
    ttStLinje.NedVerdi       = ttStLinje.NedVerdi       + tStLinje.NedVerdi
    ttStLinje.VerdiSolgt     = ttStLinje.VerdiSolgt     + tStLinje.VerdiSolgt
    ttStLinje.VerdiRabatt    = ttStLinje.VerdiRabatt    + tStLinje.VerdiRabatt
    ttStLinje.KjopVerdi      = ttStLinje.KjopVerdi      + tStLinje.KjopVerdi
    ttStLinje.BrekkVerdi     = ttStLinje.BrekkVerdi     + tStLinje.BrekkVerdi
    ttStLinje.IntVerdi       = ttStLinje.IntVerdi       + tStLinje.IntVerdi
    ttStLinje.ReklVerdi      = ttStLinje.ReklVerdi      + tStLinje.ReklVerdi
    ttStLinje.ReklLVerdi     = ttStLinje.ReklLVerdi     + tStLinje.ReklLVerdi
    ttStLinje.GjenkjopVerdi  = ttStLinje.GjenkjopVerdi  + tStLinje.GjenkjopVerdi
    ttStLinje.OvVerdi        = ttStLinje.OvVerdi        + tStLinje.OvVerdi
    ttStLinje.MvaVerdi       = ttStLinje.MvaVerdi       + tStLinje.MvaVerdi
    ttStLinje.OmlHast        = ttStLinje.AntSolgt / ((ttStLinje.LagerAnt + ttStLinje.KjopAnt + ttStLinje.OvAnt) / 2)
    ttStLinje.OmlHast        = IF ttStLinje.OmlHast = ? THEN 0 ELSE ttStLinje.OmlHast
    ttStLinje.DiverseAnt     =   ttStLinje.SvinnAnt
                               + ttStLinje.JustAnt
                               + ttStLinje.IntAnt
                               + ttStLinje.BrekkAnt
    ttStLinje.DiverseVerdi   =   ttStLinje.SvinnVerdi
                               + ttStLinje.JustVerdi
                               + ttStLinje.IntVerdi
                               + ttStLinje.BrekkVerdi
    .
END.

/* Genererer G-Total pr. periodeID - Utføres kun for ALLE butikker.*/
IF (wButik = 0 and wPeriodeTot) then
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   < 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending
  by tStLinje.Butik DESCENDING:

  IF FIRST-of(tStLinje.PerLinNr) then
    DO:
      FIND FIRST ttStLinje where
                 ttStLinje.DataObjekt = wDataObjekt and
                 ttStLinje.StTypeId   = wStTypeId   and
                 ttStLinje.PerId      = wPerId      and
                 ttStLinje.butik      = 9999998      and
                 ttStLinje.PerLinNr   = tStLinje.PerLinNr and
                 ttStLinje.Aar        = tStLinje.Aar no-error.
      IF AVAILABLE ttStLinje then
        DELETE ttStLinje.
      CREATE ttStLinje.
      assign
        ttStLinje.DataObjekt = wDataObjekt
        ttStLinje.StTypeId   = wStTypeId
        ttStLinje.PerId      = wPerId
        ttStLinje.butik      = 9999998
        ttStLinje.PerLinNr   = tStLinje.PerLinNr
        ttStLinje.Aar        = tStLinje.Aar
        ttStLinje.Hg         = tStLinje.Hg
        ttStLinje.TotalPost  = 1
        .

    END.

  IF AVAILABLE ttStLinje then
  ASSIGN
    ttStLinje.LagerAnt       = ttStLinje.LagerAnt       + tStLinje.LagerAnt
    ttStLinje.LagerVerdi     = ttStLinje.LagerVerdi     + tStLinje.LagerVerdi
    ttStLinje.PrimoAnt       = ttStLinje.PrimoAnt       + tStLinje.PrimoAnt
    ttStLinje.PrimoVerdi     = ttStLinje.PrimoVerdi     + tStLinje.PrimoVerdi
    ttStLinje.VVarekost      = ttStLinje.VVarekost      + tStLinje.VVareKost
    ttStLinje.AntSolgt       = ttStLinje.AntSolgt       + tStLinje.AntSolgt
    ttStLinje.AntRab         = ttStLinje.AntRab         + tStLinje.AntRab
    ttStLinje.BrekkAnt       = ttStLinje.BrekkAnt       + tStLinje.BrekkAnt
    ttStLinje.IntAnt         = ttStLinje.IntAnt         + tStLinje.IntAnt
    ttStLinje.ReklAnt        = ttStLinje.ReklAnt        + tStLinje.ReklAnt
    ttStLinje.ReklLAnt       = ttStLinje.ReklLAnt       + tStLinje.ReklLAnt
    ttStLinje.GjenkjopAnt    = ttStLinje.GjenkjopAnt    + tStLinje.GjenkjopAnt
    ttStLinje.KjopAnt        = ttStLinje.KjopAnt        + tStLinje.KjopAnt
    ttStLinje.OvAnt          = ttStLinje.OvAnt          + tStLinje.OvAnt
    ttStLinje.JustAnt        = ttStLinje.JustAnt        + tStLinje.JustAnt
    ttStLinje.JustVerdi      = ttStLinje.JustVerdi      + tStLinje.JustVerdi
    ttStLinje.SvinnAnt       = ttStLinje.SvinnAnt       + tStLinje.SvinnAnt
    ttStLinje.SvinnVerdi     = ttStLinje.SvinnVerdi     + tStLinje.SvinnVerdi
    ttStLinje.NedAnt         = ttStLinje.NedAnt         + tStLinje.NedAnt
    ttStLinje.NedVerdi       = ttStLinje.NedVerdi       + tStLinje.NedVerdi
    ttStLinje.VerdiSolgt     = ttStLinje.VerdiSolgt     + tStLinje.VerdiSolgt
    ttStLinje.VerdiRabatt    = ttStLinje.VerdiRabatt    + tStLinje.VerdiRabatt
    ttStLinje.KjopVerdi      = ttStLinje.KjopVerdi      + tStLinje.KjopVerdi
    ttStLinje.BrekkVerdi     = ttStLinje.BrekkVerdi     + tStLinje.BrekkVerdi
    ttStLinje.IntVerdi       = ttStLinje.IntVerdi       + tStLinje.IntVerdi
    ttStLinje.ReklVerdi      = ttStLinje.ReklVerdi      + tStLinje.ReklVerdi
    ttStLinje.ReklLVerdi     = ttStLinje.ReklLVerdi     + tStLinje.ReklLVerdi
    ttStLinje.GjenkjopVerdi  = ttStLinje.GjenkjopVerdi  + tStLinje.GjenkjopVerdi
    ttStLinje.OvVerdi        = ttStLinje.OvVerdi        + tStLinje.OvVerdi
    ttStLinje.MvaVerdi       = ttStLinje.MvaVerdi       + tStLinje.MvaVerdi
    ttStLinje.OmlHast        = ttStLinje.AntSolgt / ((ttStLinje.LagerAnt + ttStLinje.KjopAnt + ttStLinje.OvAnt) / 2)
    ttStLinje.OmlHast        = IF ttStLinje.OmlHast = ? THEN 0 ELSE ttStLinje.OmlHast
    ttStLinje.DiverseAnt     =   ttStLinje.SvinnAnt
                               + ttStLinje.JustAnt
                               + ttStLinje.IntAnt
                               + ttStLinje.BrekkAnt
    ttStLinje.DiverseVerdi   =   ttStLinje.SvinnVerdi
                               + ttStLinje.JustVerdi
                               + ttStLinje.IntVerdi
                               + ttStLinje.BrekkVerdi
    .
END.

/* Genererer G-Total.                                   */
/* G-Total finnes ved † summere alle butikktotallinjer. */
IF wPeriodeTot THEN /* TEST */
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   = 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Butik descending
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending:

  IF FIRST(tStLinje.Butik) then
    DO:
      FIND FIRST ttStLinje where
                 ttStLinje.DataObjekt = wDataObjekt and
                 ttStLinje.StTypeId   = wStTypeId   and
                 ttStLinje.PerId      = wPerId      and
                 ttStLinje.butik      = 9999999     and
                 ttStLinje.PerLinNr   = 1999999     and
                 ttStLinje.Aar        = tStLinje.Aar no-error.
      IF AVAILABLE ttStLinje then
        DELETE ttStLinje.
      CREATE ttStLinje.
      assign
        ttStLinje.DataObjekt = wDataObjekt
        ttStLinje.StTypeId   = wStTypeId
        ttStLinje.PerId      = wPerId
        ttStLinje.butik      = 9999999
        ttStLinje.PerLinNr   = 1999999
        ttStLinje.Aar        = tStLinje.Aar
        ttStLinje.Hg         = tStLinje.Hg
        ttStLinje.TotalPost  = 1
        .
    END.

  IF AVAILABLE ttStLinje then
  ASSIGN
    ttStLinje.LagerAnt       = ttStLinje.LagerAnt       + tStLinje.LagerAnt
    ttStLinje.LagerVerdi     = ttStLinje.LagerVerdi     + tStLinje.LagerVerdi
    ttStLinje.PrimoAnt       = ttStLinje.PrimoAnt       + tStLinje.PrimoAnt
    ttStLinje.PrimoVerdi     = ttStLinje.PrimoVerdi     + tStLinje.PrimoVerdi
    ttStLinje.VVarekost      = ttStLinje.VVarekost      + tStLinje.VVareKost
    ttStLinje.AntSolgt       = ttStLinje.AntSolgt       + tStLinje.AntSolgt
    ttStLinje.AntRab         = ttStLinje.AntRab         + tStLinje.AntRab
    ttStLinje.BrekkAnt       = ttStLinje.BrekkAnt       + tStLinje.BrekkAnt
    ttStLinje.IntAnt         = ttStLinje.IntAnt         + tStLinje.IntAnt
    ttStLinje.ReklAnt        = ttStLinje.ReklAnt        + tStLinje.ReklAnt
    ttStLinje.ReklLAnt       = ttStLinje.ReklLAnt       + tStLinje.ReklLAnt
    ttStLinje.GjenkjopAnt    = ttStLinje.GjenkjopAnt    + tStLinje.GjenkjopAnt
    ttStLinje.KjopAnt        = ttStLinje.KjopAnt        + tStLinje.KjopAnt
    ttStLinje.OvAnt          = ttStLinje.OvAnt          + tStLinje.OvAnt
    ttStLinje.JustAnt        = ttStLinje.JustAnt        + tStLinje.JustAnt
    ttStLinje.JustVerdi      = ttStLinje.JustVerdi      + tStLinje.JustVerdi
    ttStLinje.SvinnAnt       = ttStLinje.SvinnAnt       + tStLinje.SvinnAnt
    ttStLinje.SvinnVerdi     = ttStLinje.SvinnVerdi     + tStLinje.SvinnVerdi
    ttStLinje.NedAnt         = ttStLinje.NedAnt         + tStLinje.NedAnt
    ttStLinje.NedVerdi       = ttStLinje.NedVerdi       + tStLinje.NedVerdi
    ttStLinje.VerdiSolgt     = ttStLinje.VerdiSolgt     + tStLinje.VerdiSolgt
    ttStLinje.VerdiRabatt    = ttStLinje.VerdiRabatt    + tStLinje.VerdiRabatt
    ttStLinje.KjopVerdi      = ttStLinje.KjopVerdi      + tStLinje.KjopVerdi
    ttStLinje.BrekkVerdi     = ttStLinje.BrekkVerdi     + tStLinje.BrekkVerdi
    ttStLinje.IntVerdi       = ttStLinje.IntVerdi       + tStLinje.IntVerdi
    ttStLinje.ReklVerdi      = ttStLinje.ReklVerdi      + tStLinje.ReklVerdi
    ttStLinje.ReklLVerdi     = ttStLinje.ReklLVerdi     + tStLinje.ReklLVerdi
    ttStLinje.GjenkjopVerdi  = ttStLinje.GjenkjopVerdi  + tStLinje.GjenkjopVerdi
    ttStLinje.OvVerdi        = ttStLinje.OvVerdi        + tStLinje.OvVerdi
    ttStLinje.MvaVerdi       = ttStLinje.MvaVerdi       + tStLinje.MvaVerdi
    ttStLinje.OmlHast        = ttStLinje.AntSolgt / ((ttStLinje.LagerAnt + ttStLinje.KjopAnt + ttStLinje.OvAnt) / 2)
    ttStLinje.OmlHast        = IF ttStLinje.OmlHast = ? THEN 0 ELSE ttStLinje.OmlHast
                                                    /* Det blir her feil å regne oms.hast fra primo, da */
                                                    /* denne alltid er 0 eller mindre enn null.         */
    ttStLinje.DiverseAnt     =   ttStLinje.SvinnAnt
                               + ttStLinje.JustAnt
                               + ttStLinje.IntAnt
                               + ttStLinje.BrekkAnt
    ttStLinje.DiverseVerdi   =   ttStLinje.SvinnVerdi
                               + ttStLinje.JustVerdi
                               + ttStLinje.IntVerdi
                               + ttStLinje.BrekkVerdi
    .
END.

/* Regner ut DB% ++ */
RUN KalkFelt.

/*
OUTPUT TO t.sdv.
EXPORT "Gurre var her".
FOR EACH tStLinje:
export DELIMITER ";"
  tStLinje.StTypeId
  tStLinje.PerId
  tStLinje.DataObjekt
  tstlinje.Diverse
  tStLinje.Butik
  tStLinje.Aar
  tStLinje.PerLinNr
  tStLinje.AntSolgt
  tStLinje.LagerAnt
  tStLinje.PrimoAnt
  tStLinje.AntSolgt / ((tStLinje.LagerAnt + tStLinje.PrimoAnt) / 2)
  .

END.
OUTPUT CLOSE.
*/

  /* Renser bort butikk 0. */
  FOR EACH tStLinje WHERE
      tStLinje.Butik = 0:
      DELETE tStLinje.
  END.

END PROCEDURE.

PROCEDURE FinnLager:
  case wStTypeId:
    when "ARTIKKEL" then
      /* ARTIKKEL */
      do:
        FIND Lager NO-LOCK where
          Lager.ArtikkelNr = INT(tStLinje.DataObjekt) and
          Lager.Butik      = tStLinje.Butik NO-ERROR.
        IF AVAILABLE Lager then
          assign
            tStLinje.LagerAnt   = Lager.LagAnt
            wBevegelse          = Lager.LagAnt
            tStLinje.LagerVerdi = Lager.LagAnt * Lager.VVareKost
            wLagerVerdi         = Lager.LagAnt * Lager.VVareKost.
        else
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
/*
MESSAGE "Butikk:" tStLinje.Butik skip
        "StLinje.LAgerAnt:" tStLinje.LagerAnt skip
        "wBevegelse:" wBevegelse skip
        "StLinje.LAgerVerdi:" tStLinje.LagerVerdi skip
        "sLAgerVerdi:" wLagerVerdi VIEW-AS ALERT-BOX.
*/

      end. /* ARTIKKEL */
    when "KUNDSTAT" then
      do:
        assign
          tStLinje.LagerAnt   = 0
          wBevegelse          = 0
          tStLinje.LagerVerdi = 0
          wLagerVerdi         = 0.
      end. /* KUNDSTAT */
    when "SELGERSTAT" then
      do:
        assign
          tStLinje.LagerAnt   = 0
          wBevegelse          = 0
          tStLinje.LagerVerdi = 0
          wLagerVerdi         = 0.
      end. /* SELGERSTAT */
      when "SELGER-ART" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
      when "SELGER-VG" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
      when "SELGER" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
      when "MEDLEM" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
      when "MEDLEMTOT" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
      when "KASS-ART" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
      when "KASS-VG" then
        do:
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
        end. /* SELGERSTAT */
    OTHERWISE DO: /* "VAREGR","LEVERAN","HOVEDGR","BUTSTAT"  */
        FIND StLager NO-LOCK where
          StLager.StTypeId   = wStTypeId and
          StLager.DataObjekt = tStLinje.DataObjekt and
          StLager.Butik      = tStLinje.Butik NO-ERROR.
        IF AVAILABLE StLager then
          assign
            tStLinje.LagerAnt   = StLager.LagAnt
            wBevegelse          = StLager.LagAnt
            tStLinje.LagerVerdi = /* StLager.LagAnt * */ StLager.VVareKost
            wLagerVerdi         = /* StLager.LagAnt * */ StLager.VVareKost.
        else
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
    END.
  end case. /* STATISTIKKTYPE */    

END PROCEDURE.

PROCEDURE KalkFelt:
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId:

  assign
    tStLinje.VerdiSolgt = tStLinje.VerdiSolgt /*- (tStLinje.ReklVerdi + tStLinje.GjenkjopVerdi)*/
    tStLinje.DbKr       = tStLinje.VerdiSolgt - tStLinje.VVareKost
    tStLinje.Db%        = (tStLinje.DbKr * 100) / tStLinje.VerdiSolgt
    tStLinje.Db%        = if tStLinje.Db% = ? then 0 else tStLinje.Db%
    tStLinje.LagerAnt   = tStLinje.LagerAnt
    tStLinje.Utsolgt%   = (tStLinje.AntSolgt / (tStLinje.KjopAnt + tStLinje.OvAnt)) * 100
    tStLinje.Utsolgt%   = if tStLinje.Utsolgt% = ? then 0 else tStLinje.Utsolgt%
    tStLinje.OmlHast    = tStLinje.OmlHast
    tStLinje.PrimoAnt   = tStLinje.PrimoAnt
    tStLinje.VisBut     = if tStLinje.Butik <= 999999
                    then string(tStLinje.Butik)
                    else ""
    tStLinje.VisBut     = fill(" ",7 - length(tStLinje.VisBut)) + tStLinje.VisBut.
    
  /* Hånterer sumlinjer */
  if tStLinje.PerLinNr = 1999999 then
    tStLinje.PerLinTxt = "G-Tot".
  else if tStLinje.PerLinNr = 1000000 then
    tStLinje.PerLinTxt = "ButikkTot".
  else  
  case tStLinje.PerId:
    when "AAR"   then tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
    when "MANED" then tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
    when "UKE"   then tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
    when "DAG"   then 
      do:
        find PerLin no-lock where
          PerLin.PerId    = tStLinje.PerId and
          PerLin.PerLinNr = tStLinje.PerLinNr no-error.
        if available PerLin then
          tStLinje.PerLinTxt = string(date(1,1,tStLinje.Aar) + (tStLinje.PerLinNr - 1)).
        else tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
      end.
    otherwise    
      do:
        find PerLin no-lock where
          PerLin.PerId    = tStLinje.PerId and
          PerLin.PerLinNr = tStLinje.PerLinNr no-error.
        if available PerLin then
          tStLinje.PerLinTxt = string(PerLin.FraDato) + "-" + string(PerLin.TilDato).
        else tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
      end.
  end case.
END.
END PROCEDURE.

PROCEDURE EksporterTabell:
  DEF VAR wLabel as CHAR.

  OUTPUT to t.sdv.

  EXPORT DELIMITER ";"
    "DataObjekt"
    "StTypeId"
    "Beskrivelse"
    "PerId"
    "Aar"
    "PerLinNr"
    "Butik"

    "KjopAnt"
    "KjopVerdi"
    "AntSolgt"
    "VerdiSolgt"
    "VVarekost"
    "LagerAnt"
    "LagerVerdi"
    "BrekkAnt"
    "IntAnt"
    "ReklAnt"
    "ReklLAnt"
    "GjenkjopAnt"
    "OvAnt"
    "JustAnt"
    "JustVerdi"
    "SvinnAnt"
    "SvinnVerdi"
    "NedAnt"
    "NedVerdi"
    "BrekkVerdi"
    "IntVerdi"
    "ReklVerdi"
    "ReklLVerdi"
    "GjenkjopVerdi"
    "OvVerdi"
    "MvaVerdi"
    "Diverse"
    "AntTilbSolgt"
    "VerdiTilbSolgt"
    "TilbVVarekost"
    "TilbMvaVerdi"
    "AntRabatt"
    "VerdiRabatt"
    "PrimoAnt"
    "OmlHast"
    "Hg"
    "But"
    "DbKr"
    "Utsolgt%"
    "DiverseAnt"
    "DiverseVerdi"
    "EDato"
    "ETid"
    "BrukerID"
    "RegistrertDato"
    "RegistrertTid"
    "RegistrertAv"
    .

  FOR EACH tStLinje NO-LOCK:
    EXPORT DELIMITER ";"
    tStLinje.DataObjekt
    tStLinje.StTypeId
    tStLinje.Beskrivelse
    tStLinje.PerId
    tStLinje.Aar
    tStLinje.PerLinNr
    tStLinje.Butik

    tStLinje.KjopAnt
    tStLinje.KjopVerdi
    tStLinje.AntSolgt
    tStLinje.VerdiSolgt
    tStLinje.VVarekost
    tStLinje.LagerAnt
    tStLinje.LagerVerdi
    tStLinje.BrekkAnt
    tStLinje.IntAnt
    tStLinje.ReklAnt
    tStLinje.ReklLAnt
    tStLinje.GjenkjopAnt
    tStLinje.OvAnt
    tStLinje.JustAnt
    tStLinje.JustVerdi
    tStLinje.SvinnAnt
    tStLinje.SvinnVerdi
    tStLinje.NedAnt
    tStLinje.NedVerdi
    tStLinje.BrekkVerdi
    tStLinje.IntVerdi
    tStLinje.ReklVerdi
    tStLinje.ReklLVerdi
    tStLinje.GjenkjopVerdi
    tStLinje.OvVerdi
    tStLinje.MvaVerdi
    tStLinje.Diverse
    tStLinje.AntTilbSolgt
    tStLinje.VerdiTilbSolgt
    tStLinje.TilbVVarekost
    tStLinje.TilbMvaVerdi
    tStLinje.AntRabatt
    tStLinje.VerdiRabatt
    tStLinje.PrimoAnt
    tStLinje.OmlHast
    tStLinje.Hg
    tStLinje.But
    tStLinje.DbKr
    tStLinje.Utsolgt%
    tStLinje.DiverseAnt
    tStLinje.DiverseVerdi
    tStLinje.EDato
    tStLinje.ETid
    tStLinje.BrukerID
    tStLinje.RegistrertDato
    tStLinje.RegistrertTid
    tStLinje.RegistrertAv
    .
  END.
  OUTPUT CLOSE.
END PROCEDURE.
