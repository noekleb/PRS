/************************************************************
    Program:  ov-ArtBas.p
    Created:  TN   13 Sep 99
Description:  Overf›ring av ArtBas til sentral database.

              run ov-ArtBas.p (input input RS-2).

Last change:  TN   28 Jul 100    9:18 am
************************************************************/

DEF INPUT parameter wRS-3         as INT NO-UNDO.
DEF INPUT parameter wLogFil       as char no-undo.
DEF INPUT parameter wwLogFil      as char no-undo.
DEF INPUT parameter wLogKatalog   as char no-undo.
DEF INPUT parameter wBruktTid     as int  no-undo.
DEF INPUT parameter wStartTid     as int  no-undo.
DEF INPUT parameter wOkStatus     as char no-undo.

DEF var wBildNr       as INT  NO-UNDO.
DEF var wFilNavn      as CHAR NO-UNDO.
DEF VAR wPrefix       as CHAR NO-UNDO initial "L".
DEF var wWorkDir      as CHAR initial ".\bilder" NO-UNDO.
def var trgArtikkelNr as int  no-undo.
def var trgBestNr     as int  no-undo.
DEF VAR wStop         as LOG  NO-UNDO.
def var wEDB-System   as char no-undo.
def var wTabell       as char no-undo.
DEF VAR wArtikkelNr   LIKE SkoTex.ArtBas.ArtikkelNr NO-UNDO.
DEF VAR wOrdreNr      as INT  NO-UNDO.
DEF VAR wTekst        as CHAR NO-UNDO.
DEF VAR wFilExt  as CHAR      INITIAL "jpg"    NO-UNDO.

DEF shared VAR wAvbryt as LOG INITIAL FALSE NO-UNDO.

DEF temp-Table tmpOrdre
  FIELD OrdreNr    as int
  FIELD NyOrdreNr  as int
  FIELD OrdreRecid as RECID.

/* Definerer stream */
def stream LoggData.

def buffer trgArtBas   for SentralDB.ArtBas.
def buffer trgBestHode for SentralDB.BestHode.

/* Kode for konvertering av artikkelnummer ved overføring til sentral database. */
FIND SkoTex.SysPara NO-LOCK where
  SkoTex.SysPara.SysHId = 1 and
  SkoTex.SysPara.SysGr  = 2 and
  SkoTex.SysPara.ParaNr = 5 NO-ERROR.
  if AVAILABLE SkoTex.SysPara then
    ASSIGN wEDB-System = SkoTex.SysPara.Parameter1.
if wEDB-System = "" then
  wEDB-System = "LAPTOP".

FIND SkoTex.SysPara NO-LOCK where
  SkoTex.SysPara.SysHId = 1 and
  SkoTex.SysPara.SysGr  = 2 and
  SkoTex.SysPara.ParaNr = 5 NO-ERROR.
  if AVAILABLE SkoTex.SysPara then
    ASSIGN wTabell = SkoTex.SysPara.Parameter2.
if wTabell = "" then
  wTabell = "ArtBas".

/* Ekstent på bilder fra ClipBoard*/
FIND SentralDB.SysPara NO-LOCK where
  SentralDB.SysPara.SysHId = 10 and
  SentralDB.SysPara.SysGr  = 2 and
  SentralDB.SysPara.ParaNr = 1 NO-ERROR.
if AVAILABLE SentralDB.SysPara then
  ASSIGN wFilExt = (SentralDB.SysPara.Parameter1).

/* Henter Katalognavn for billedarkivet fra ini filen */
RUN HentBildeKatalog (output wWorkDir).
IF CAN-DO("\,/",SUBSTRING(wWorkDir,LENGTH(wWorkDir),1)) then
  wWorkDir = SUBSTRING(wWorkDir,1,LENGTH(wWorkDir) - 1).

/* --------- Overstyring av schematriggere. --------------- */
on WRITE OF SentralDB.ArtBas override
  DO:

  END.

on CREATE OF SentralDB.ArtBas override
  DO:
    LOOPEN:
    do while true:
      /*trgArtikkelNr = NEXT-VALUE(ArtikkelNr,SentralDB).*/
      RUN ov/ov-genArtikkelnr (OUTPUT trgArtikkelNr).
      if not can-find(first SentralDB.trgArtBas where SentralDB.trgArtBas.ArtikkelNr =
                      trgArtikkelNr) then
        leave LOOPEN.
    end. /* LOOPEN */

    assign
      SentralDB.ArtBas.ArtikkelNr = trgArtikkelNr.
  END.

DISABLE TRIGGERS FOR LOAD OF SentralDB.ArtPris.
DISABLE TRIGGERS FOR LOAD OF SentralDB.Ordre.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestLinje.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BildeRegister.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BildeData.
DISABLE TRIGGERS FOR LOAD OF SentralDB.Lager.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestSort.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestStr.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestPris.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestLevert.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestHLev.
DISABLE TRIGGERS FOR LOAD OF SentralDB.BestKasse.
DISABLE TRIGGERS FOR LOAD OF SentralDB.FriButik.

/*DISABLE TRIGGERS FOR LOAD OF SentralDB.BestHode.*/
on WRITE OF SentralDB.BestHode override
  DO:

  END.

on CREATE OF SentralDB.BestHode override
  DO:
    LOOPEN:
    do while true:
      /*trgBestNr = NEXT-VALUE(BestSeq,SentralDB).*/
      RUN ov/ov-genbestnr (OUTPUT trgBestNr).
      if not can-find(first SentralDB.trgBestHode where
                        SentralDB.trgBestHode.BestNr =
                      trgBestNr) then
        leave LOOPEN.
    end. /* LOOPEN */

    assign
      SentralDB.BestHode.BestNr = trgBestNr.
  END.


/* -------------------------------------------------------- */

ASSIGN wStop = FALSE.

/* Overf›rer ny artikkler */
ARTIKKLER:
FOR EACH SkoTex.ArtBas EXCLUSIVE-LOCK where
  SkoTex.ArtBas.LapTop = TRUE:

  {process_events.i &BlokkLabel = "ARTIKKLER"}

  /* Bilder som finnes fra før får nytt bildenummer.  */
  /* Bildenummeret oppdateres på artbas i sentral db. */
  /* Bildenummer = 0, overf›res ikke.                 */
  if SkoTex.ArtBas.BildNr > 0 then
    DO:
      FIND SkoTex.BildeRegister NO-LOCK OF SkoTex.ArtBas NO-ERROR.
      /* Er bildenummeret i bruk p† HK, tildeles nytt bildenummer. */
      IF AVAILABLE SkoTex.BildeRegister then
        BILDEREGISTER:
        DO:
          FIND SentralDB.BildeRegister OF SkoTex.BildeRegister NO-LOCK NO-ERROR.
          IF AVAILABLE SentralDB.BildeRegister then
            DO:
              RUN BildeNummer (wPrefix, OUTPUT wBildNr, OUTPUT wFilNavn).
            END.
          ELSE DO:
            assign
              wBildNr  = SkoTex.BildeRegister.BildNr
              wFilNavn = SkoTex.BildeRegister.FilNavn
            OVERLAY(wFilNavn, 1, 1, "CHARACTER") = wPrefix.
          END.

          CREATE SentralDB.BildeRegister.
          BUFFER-COPY SkoTex.BildeRegister
                      Except SkoTex.BildeRegister.BildNr
                      to SentralDB.BildeRegister
            assign
              SentralDB.BildeRegister.BildNr     = wBildNr
              SentralDB.BildeRegister.FilNavn    = wFilNavn
              SentralDB.BildeRegister.DokumentNr = SkoTex.BildeRegister.BildNr.
          FOR EACH SkoTex.BildeData of SkoTex.bildeRegister NO-LOCK:
            CREATE SentralDB.BildeData.
            BUFFER-COPY SkoTex.BildeData
                        EXCEPT SkoTex.BildeData.BildNr
                        to SentralDB.BildeData
              assign
                SentralDB.BildeData.BildNr = wBildNr.
          END.

        END. /* BILDEREGISTER */
      /* Det er lagt inn bildenummer men ikke noe bilde.. */
      ELSE DO:
        wBildNr = SkoTex.ArtBas.BildNr.
      END.
    END.
  /* Bilde er ikke lagt inn p† laptop. */
  ELSE DO:
    wBildNr = SkoTex.ArtBas.BildNr.
    FIND SkoTex.BildeRegister of SkoTex.ArtBas NO-LOCK NO-ERROR.
  END.

  /* Ved overf›ring av artikkler forutsettes det at artikkelen ikke finnes i hoved */
  /* databasen. Applikasjonen er laget slik at dette er tilfelle.                  */
  /* Dette gjelder artikkler som har f†tt sitt LapTop flagg satt.                  */
  CREATE SentralDB.ArtBas.
  BUFFER-COPY SkoTex.ArtBas EXCEPT SkoTex.ArtBas.ArtikkelNr to SentralDB.ArtBas
    assign
      SentralDB.ArtBas.LapTop = FALSE
      SentralDB.ArtBas.BildNr = wBildNr.
  ASSIGN
    SkoTex.ArtBas.LapTop = FALSE /* Flagger overf›ring utf›rt. */
    wArtikkelNr = SentralDB.ArtBas.ArtikkelNr.

  /* Logger peker til artikkelen i den sentrale databasen. */
  find first SkoTex.KonvReg no-lock where
    SkoTex.KonvReg.EDB-System = wEDB-System and
    SkoTex.KonvReg.Tabell     = wTabell   and
    SkoTex.KonvReg.InterntId  = STRING(SkoTex.ArtBas.ArtikkelNr) no-error.
  if not available SkoTex.KonvReg then
    do:
      create SkoTex.KonvReg.
      assign
        SkoTex.KonvReg.EDB-System = wEDB-System
        SkoTex.KonvReg.Tabell     = wTabell
        SkoTex.KonvReg.EkstId     = STRING(SentralDB.ArtBas.ArtikkelNr)
        SkoTex.KonvReg.InterntID  = STRING(SkoTex.ArtBas.ArtikkelNr).
    END.

  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  put stream LoggData unformatted
     " ArtNr/LevNr/LevArtNr/EkstId/Ny-Gml bildnr:  "
     STRING(SentralDB.ArtBas.ArtikkelNr) "/"
     STRING(SentralDB.ArtBas.Vg) "/"
     SentralDB.ArtBas.LevKod "/"
     SkoTex.KonvReg.InterntId "/"
     wBildNr "-"
     (if available SkoTex.BildeRegister
        then SkoTex.BildeRegister.BildNr
        else 0) skip.
  OUTPUT STREAM LoggData close.

  ARTPRIS:
  FOR EACH SkoTex.ArtPris OF SkoTex.ArtBas NO-LOCK:
    CREATE SentralDB.ArtPris.
    BUFFER-COPY SkoTex.ArtPris EXCEPT SkoTex.ArtPris.ArtikkelNr to SentralDB.ArtPris
      assign
        SentralDB.ArtPris.ArtikkelNr = wArtikkelNr.
  END. /* ARTPRIS */

  LAGER:
  FOR EACH SkoTex.Lager OF SkoTex.ArtBas NO-LOCK:
    CREATE SentralDB.Lager.
    BUFFER-COPY SkoTex.Lager EXCEPT SkoTex.Lager.ArtikkelNr to SentralDB.Lager
      assign
        SentralDB.Lager.ArtikkelNr = wArtikkelNr.
  END. /* LAGER */
END. /* ARTIKKLER */

ORDRER:
FOR EACH SkoTex.Ordre exclusive-lock where
  SkoTex.Ordre.LapTop = true:
  {process_events.i &BlokkLabel = "ORDRER"}
  assign
    wOrdreNr = ?.

  /* Er ordrenummer er tatt ibruk i SentralDB, hentes et nytt ordrenummer. */
  if CAN-FIND(SentralDB.Ordre where
              SentralDB.Ordre.OrdreNr = SkoTex.Ordre.OrdreNr) then
    BYTT_ORDRE_NR:
    DO:
      FIND LAST sentralDB.Ordre NO-LOCK NO-ERROR.
      if AVAILABLE SentralDB.Ordre then
        wOrdreNr = SentralDB.Ordre.OrdreNr + 1.
      ELSE wOrdreNr = 1.
    END. /* BYTT_ORDRE_NR */
  /* Ordrenummer er ledig og kan benyttes. */
  ELSE
    assign
      wOrdreNr = SkoTex.Ordre.OrdreNr.

  /* Logger ordrenummerreferanse i tmp tabell for bruk ved overf›ring */
  /* av bestillinger.                                                 */
  FIND first tmpOrdre where
    tmpOrdre.OrdreNr = SkoTex.Ordre.OrdreNr NO-ERROR.
  if NOT AVAILABLE tmpOrdre then
    OPPSTANDELSEN:
    DO:
      /* Ingen logging hvis bestilling ikke er koblet. */
      if (SkoTex.Ordre.OrdreNr = 0 or
          SkoTex.Ordre.OrdreNr = ?) then
        LEAVE OPPSTANDELSEN.

      CREATE tmpOrdre.
      assign
        tmpOrdre.OrdreNr    = SkoTex.Ordre.OrdreNr
        tmpOrdre.NyOrdreNr  = wOrdreNr
        tmpOrdre.OrdreRecid = RECID(SkoTex.Ordre).
    END. /* OPPSTANDELSEN */

  /* Ordren overføres. */
  CREATE SentralDB.Ordre.
  BUFFER-COPY SkoTex.Ordre to SentralDB.Ordre
    assign
      SentralDB.Ordre.OrdreNr = wOrdreNr
      SentralDB.Ordre.EkstId  = "Ref: " + STRING(SkoTex.Ordre.OrdreNr)
      SentralDB.Ordre.LapTop  = false.
  assign
    SkoTex.Ordre.LapTop = FALSE.

  /* Logger overføringen. */
  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  put stream LoggData unformatted
     " OrdreNr/LevNr/GmlOrdreNr: "
     STRING(SentralDB.Ordre.OrdreNr) "/"
     STRING(SentralDB.Ordre.LevNr) "/"
     STRING(SkoTex.Ordre.OrdreNr) SKIP.
  OUTPUT STREAM LoggData close.
END. /* ORDRER */


/* Leser alle nye bestillinger. */
BESTHODE:
FOR EACH SkoTex.BestHode EXCLUSIVE-LOCK where
  SkoTex.BestHode.LapTop = TRUE /* and
  SkoTex.BestHode.BestNr >= 5053 and
  SkoTex.BestHode.BestNr <= 5072 */:

  {process_events.i &BlokkLabel = "BESTHODE"}

  FIND SkoTex.ArtBas EXCLUSIVE-LOCK where
    SkoTex.ArtBas.ArtikkelNr = SkoTex.BestHode.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE SkoTex.ArtBas then
    NEXT BESTHODE.

  /* Er artikkelen overf›rt fra f›r, hentes eksternt ID. */
  find first SkoTex.KonvReg no-lock where
    SkoTex.KonvReg.EDB-System = wEDB-System and
    SkoTex.KonvReg.Tabell     = wTabell   and
    SkoTex.KonvReg.InterntId  = STRING(SkoTex.ArtBas.ArtikkelNr) no-error.
  if available SkoTex.KonvReg then
    DO:
      wArtikkelNr = DEC(SkoTex.KonvReg.EkstId).
      FIND SentralDB.ArtBas NO-LOCK where
        SentralDB.ArtBas.ArtikkelNr = DEC(KonvReg.EkstID) NO-ERROR.
    END.
  /* Hvis den ikke finnes i konverteringsregisteret, er det en ny bestilling           */
  /* på en gammel artikkel. Da skal artikkelnummer være likt i den sentrale databasen. */
  ELSE DO:
    wArtikkelNr = SkoTex.ArtBas.ArtikkelNr.
      FIND SentralDB.ArtBas NO-LOCK where
        SentralDB.ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
  END.
  IF NOT AVAILABLE SentralDB.ArtBas then
    NEXT BESTHODE. /* Her er noe galt! */

  /* Bytte av eventuelt ordrenummer */
  FIND first tmpOrdre no-lock where
    tmpOrdre.OrdreNr = SkoTex.BestHode.OrdreNr NO-ERROR.

  /* Overf›rer bestillingen */
  CREATE SentralDB.BestHode.
  BUFFER-COPY SkoTex.BestHode
              except SkoTex.BestHode.BestNr SkoTex.BestHode.ArtikkelNr
              to SentralDB.BestHode
    assign
      SentralDB.BestHode.ArtikkelNr = wArtikkelNr
      SentralDB.BestHode.OrdreNr    = IF AVAILABLE tmpOrdre
                                        THEN tmpOrdre.NyOrdreNr
                                        ELSE SkoTex.BestHode.OrdreNr
      SentralDB.BestHode.LapTop     = false.
  assign
    SkoTex.BestHode.LapTop = FALSE.

  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  put stream LoggData unformatted
     " BestillingsNr/LevNr.:  "
     STRING(SentralDB.BestHode.BestNr) "/"
     STRING(SentralDB.BestHode.LevNr) skip.
  OUTPUT STREAM LoggData close.

  /* Bestillingslinjene */
  FOR EACH SkoTex.BestLinje OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestLinje.
    BUFFER-COPY SkoTex.BestLinje to SentralDB.BestLinje
      assign
        SentralDB.BestLinje.BestNr = SentralDB.BestHode.BestNr.
  END.
  /* Sortimentslinjene */
  FOR EACH SkoTex.BestSort OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestSort.
    BUFFER-COPY SkoTex.BestSort to SentralDB.BestSort
      assign
        SentralDB.BestSort.BestNr = SentralDB.BestHode.BestNr.
  END.
  /* Kalkylen */
  FOR EACH SkoTex.BestPris OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestPris.
    BUFFER-COPY SkoTex.BestPris to SentralDB.BestPris
      assign
        SentralDB.BestPris.BestNr = SentralDB.BestHode.BestNr.
  END.
  /* St›rrelsesdefinisjoner */
  FOR EACH SkoTex.BestStr OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestStr.
    BUFFER-COPY SkoTex.BestStr to SentralDB.BestStr
      assign
        SentralDB.BestStr.BestNr = SentralDB.BestHode.BestNr.
  END.

  /* Innleveranser - st›rrelser */
  FOR EACH SkoTex.BestLevert OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestLevert.
    BUFFER-COPY SkoTex.BestLevert to SentralDB.BestLevert
      assign
        SentralDB.BestLevert.BestNr = SentralDB.BestHode.BestNr.
  END.

  /* InnleveranseHode */
  FOR EACH SkoTex.BestHLev OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestHLev.
    BUFFER-COPY SkoTex.BestHLev to SentralDB.BestHLev
      assign
        SentralDB.BestHLev.BestNr = SentralDB.BestHode.BestNr.
  END.

  /* Kassainndelinger */
  FOR EACH SkoTex.BestKasse OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.BestKasse.
    BUFFER-COPY SkoTex.BestKasse to SentralDB.BestKasse
      assign
        SentralDB.BestKasse.BestNr = SentralDB.BestHode.BestNr.
  END.

  /* FriInndelinger */
  FOR EACH SkoTex.FriButik OF SkoTex.BestHode NO-LOCK:
    CREATE SentralDB.FriButik.
    BUFFER-COPY SkoTex.FriButik to SentralDB.FriButik
      assign
        SentralDB.FriButik.BestNr = SentralDB.BestHode.BestNr.
  END.

END. /* BESTHODE */

IF wStop then
  RETURN "AVBRYT".
ELSE
  RETURN "OK".

PROCEDURE BildeNummer:
  DEF INPUT PARAMETER  wPrefix  as CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wBildNr  as INT NO-UNDO.
  DEF OUTPUT PARAMETER wFilNavn as CHAR NO-UNDO.
  DEF VAR wLoop    as INT                        NO-UNDO.
  DEF VAR wReturn-Value as CHAR INITIAL "AVBRYT" NO-UNDO.

  DEF BUFFER bufBildeRegister FOR SentralDB.BildeRegister.

  /* Henter neste ledige bildenummer. */
  FIND LAST SentralDB.BildeRegister no-lock NO-ERROR.
  IF AVAILABLE SentralDB.Bilderegister then
    wLoop = SentralDB.Bilderegister.BildNr.
  IF wLoop >= 999999 then
    wLoop = 0.

  /* Finner første ledige bildenummer */
  EVIGHET:
  do WHILE true:
    wLoop = wLoop + 1.

    /* Utenfor bildenummeromr†de. */
    IF wLoop > 999999 then
      RETURN "AVBRYT".

    if not can-find(SentralDB.BildeRegister where
                    SentralDB.BildeRegister.BildNr = wLoop) then
      LEAVE EVIGHET.
  END. /* EVIGHET */
  assign
    wBildNr = wLoop.
    wFilNavn = wPrefix +  /* Alle bilder som er pastet fra ClipBoard får B. */
               string(wBildNr,"999999") + "." +
               wFilExt. /* Lagres normalt som JPG fil. */
END PROCEDURE.

procedure HentBildeKatalog:
  DEF OUTPUT PARAMETER wKatalog as CHAR NO-UNDO.

  /* Henter katalog for bilderegisteret fra systemparameter. */
  FIND SentralDB.SysPara NO-LOCK where
    SentralDB.SysPara.SysHId = 10 and
    SentralDB.SysPara.SysGr  = 1  and
    SentralDB.SysPara.ParaNr = 2  NO-ERROR.
  if AVAILABLE SentralDB.SysPara then
    ASSIGN wKatalog = SentralDB.SysPara.Parameter1.
  if can-do("\,/",substring(wKatalog,length(wKatalog),1)) THEN.
  else
    wKatalog = wKatalog + (if opsys = "unix"
                             then "/"
                             else "\").
END PROCEDURE.

