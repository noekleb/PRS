/************************************************************
    Program:  byggartikkler.P
    Created:  TN    8 Aug 99
Description:

Last change:  TN    8 Aug 100    4:58 pm
************************************************************/

DEF INPUT PARAMETER wListerRecid     as RECID NO-UNDO.
DEF INPUT PARAMETER FI-FraDato       as DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilDato       as DATE NO-UNDO.
DEF INPUT PARAMETER FI-FraLevDato    as Date NO-UNDO.
DEF INPUT PARAMETER FI-TilLevDato    as Date NO-UNDO.
DEF INPUT PARAMETER FI-FraLopNr      as INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilLopNr      as INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraArtikkelNr as dec  NO-UNDO.
DEF INPUT PARAMETER FI-TilArtikkelNr as dec  NO-UNDO.
DEF INPUT PARAMETER FI-LevKod        as CHAR NO-UNDO.
DEF INPUT PARAMETER RS-Aktiv         as INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraKateg      as INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilKateg      as INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraBildNr     as INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilBildNr     as INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraFraTilbud  as DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilFraTilbud  as DATE NO-UNDO.
DEF INPUT PARAMETER FI-FraTilTilbud  as DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilTilTilbud  as DATE NO-UNDO.
DEF INPUT PARAMETER T-Butikk         as LOG  NO-UNDO.
DEF INPUT PARAMETER wvVareGrupper    as char no-undo.
DEF INPUT PARAMETER wvLeverandorer   as char no-undo.
DEF INPUT PARAMETER wvSesonger       as char no-undo.
DEF INPUT PARAMETER wvVmId           as char no-undo.
DEF INPUT PARAMETER wvFarger         as char no-undo.
DEF INPUT PARAMETER wvMaterial       as char no-undo.
DEF INPUT PARAMETER wvButiker        as char no-undo.
DEF INPUT PARAMETER T-Annonse        as LOG  NO-UNDO.
DEF INPUT PARAMETER T-AktivTilbud    as LOG  NO-UNDO.
DEF INPUT PARAMETER T-Best           AS LOG  NO-UNDO.
DEF INPUT PARAMETER T-LAger          AS LOG  NO-UNDO.
DEF INPUT PARAMETER T-PLU            AS LOG  NO-UNDO.
DEF INPUT PARAMETER T-Salg           AS LOG  NO-UNDO.
DEF INPUT PARAMETER wParent          as handle NO-UNDO.

def var wAntall      as int  no-undo.
def var wLoop1       as int  no-undo.
def var wLoop2       as int  no-undo.
def var wStatListe   as char no-undo.
DEF VAR FI-Info      as CHAR NO-UNDO.
DEF VAR wStatLst     as CHAR NO-UNDO.
DEF VAR wAlle        as CHAR NO-UNDO.
DEF VAR wCellNr      as INT  NO-UNDO.
DEF VAR wSjekkTilbud as LOG  NO-UNDO.
DEF VAR wCl          as int  NO-UNDO.

find Lister no-lock where
  recid(Lister) = wListerRecid no-error.
if not available Lister then
  return "AVBRYT".

{syspara.i 1 100 1 wAlle}
{syspara.i 5   1 1 wCl int}

/* Skal tilbud sjekkes? */
if (FI-FraFraTilbud <> ? or
   FI-TilFraTilbud <> ? or
   FI-FraTilTilbud <> ? or
   FI-TilTilTilbud <> ? or
   T-AktivTilbud = TRUE) then
  wSjekkTilbud = TRUE.
else
  wSjekkTilbud = FALSE.

/*
MESSAGE
   "Fra:" FI-FraFraTilbud FI-TilFraTilbud skip
   "Til:" FI-FraTilTilbud FI-TilTilTilbud skip
   "AktivTilbud:" T-AktivTilbud skip
   "SjekkTilbud:" wSjekkTilbud skip
  VIEW-AS ALERT-BOX.
*/

/*** Dette er for DUMT, men det mangler en indeks. */
assign
  wCellNr = 0.
for each ListeLinje no-lock where
  ListeLinje.ListeType  = Lister.ListeType and
  ListeLinje.ListeNr    = Lister.ListeNr and
  ListeLinje.CellNr     <> ?
  by ListeLinje.ListeType
  by ListeLinje.ListeNr
  by ListeLinje.CellNr:

  wCellNr = ListeLinje.CellNr + 1.
END.

  /*
message "TEST byggartikkler.p" skip
  "wListerRecid"        wListerRecid skip
  "FI-FraDato"          FI-FraDato skip
  "FI-TilDato"          FI-TilDato skip
  "FI-FraLevUke"        FI-FraLevUke skip
  "FI-TilLevUke"        FI-TilLevUke skip
  "FI-FraLopNr"         FI-FraLopNr skip
  "FI-TilLopNr"         FI-TilLopNr skip
  "FI-FraKateg"         FI-FraKateg skip
  "FI-TilKateg"         FI-TilKateg skip
  "RS-Aktiv"            RS-Aktiv
  "T-Butikk"            T-Butikk skip
  "wvVareGrupper"       wvVareGrupper skip
  "wvLeverandorer"      wvLeverandorer skip
  "wStatLst"            wStatLst skip
  "wvSesonger"          wvSesonger skip
  "wvFarger"            wvFarger skip
  "wvMaterial"          wvMaterial skip
  "wvButiker"           wvButiker skip
view-as alert-box.
  */

if wvVareGrupper = wAlle then
  RUN InitVaregrupper.

MAIN_LOOP:
do wLoop1 = 1 to num-entries(wvVareGrupper):
  ARTBAS:
  for each ArtBas no-lock where
    ArtBas.Vg = INT(ENTRY(wLoop1,wvVareGrupper)) AND
    ArtBas.ArtikkelNr >= FI-FraArtikkelNr and
    (IF FI-TilArtikkelNr = 9999999999999
       THEN true
       ELSE ArtBas.ArtikkelNr <= FI-TilArtikkelNr) and
    ArtBas.LevKod matches FI-LevKod + "*":

    if FI-FraDato <> ? then
      DO:
        if ArtBas.RegistrertDato < FI-FraDato then
          NEXT ARTBAS.
      END.
    if FI-TilDato <> ? then
      DO:
        if ArtBas.RegistrertDato > FI-TilDato then
          NEXT ARTBAS.
      END.

    if FI-FraLevDato <> ? then
      DO:
        if ArtBas.Inn_Dato < FI-FraLevDato then
          NEXT ARTBAS.
      END.
    if FI-TilLevDato <> ? then
      DO:
        if ArtBas.Inn_Dato > FI-TilLevDato then
          NEXT ARTBAS.
      END.

    if FI-FraLopNr <> ? then
      DO:
        if ArtBas.LopNr >= FI-FraLopNr and
           ArtBas.LopNr <= FI-TilLopNr THEN. /* Gjør ingenting. */
        ELSE
          NEXT ARTBAS.
      END.

    if wvLeverandorer <> wAlle THEN
      DO:
        if can-do(wvLeverandorer,string(ArtBas.LevNr)) then.
           else next ARTBAS.
      END.

    if T-Annonse then
      DO:
        if ArtBas.AnonseArtikkel = FALSE then
          next ARTBAS.
      END.

    if ArtBas.VgKat >= FI-FraKateg and
       (IF FI-TilKateg = 99
          THEN TRUE
          ELSE ArtBas.VgKat <= FI-TilKateg) then.
    else next ARTBAS.

    if ArtBas.BildNr >= FI-FraBildNr and
       (IF FI-TilBildNr = 999999
          THEN true
          ELSE ArtBas.BildNr <= FI-TilBildNr) then.
    else next ARTBAS.

    if wvSesonger <> wAlle then
      DO:
        if can-do(wvSesonger,string(ArtBas.SaSong)) then.
          else next ARTBAS.
      END.

    if wvVmId <> wAlle then
      DO:
        if can-do(wvVmId,string(ArtBas.VmId)) then.
          else next ARTBAS.
      END.

    if wvFarger <> wAlle then
      DO:
        if can-do(wvFarger,string(ArtBas.Farg)) then.
          else next ARTBAS.
      END.

    if wvMaterial <> wAlle then
      DO:
        if can-do(wvMaterial,string(ArtBas.MatKod)) then.
          else next ARTBAS.
      END.

    if RS-Aktiv = 1 then /* Undertrykker utgåtte */
      DO:
        if ArtBas.Utgatt = utgatt then
          NEXT ARTBAS.
      END.
    ELSE if RS-Aktiv = 2 then /* Undertrykker aktive */
      DO:
        if ArtBas.Utgatt = false then
          NEXT ARTBAS.
      END.

    /* Sjekk på bestilling */
    /* TAR IKKE med artikkler med bestilling. */
    IF T-Best THEN
    DO:
      IF CAN-FIND(FIRST BestHode OF ArtBas WHERE 
                  BestHode.TotAntPar > 0) THEN
          NEXT ARTBAS.
    END.

    /* TAR IKKE med artikkler med lager. */
    IF T-Lager THEN
    DO:
      IF CAN-FIND(FIRST Lager WHERE
                  Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                  Lager.LagAnt > 0) THEN
          NEXT ARTBAS.
    END.

    /* Tar ikke med PLU artikkler */
    IF T-PLU THEN
    DO:
        IF ArtBas.OPris = TRUE THEN
            NEXT ARTBAS.
    END.

    /* Tar ikke med artikler med salg. */
    IF T-Salg THEN
    DO:
      IF CAN-FIND(FIRST Translogg NO-LOCK WHERE
                      TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND
                      TransLogg.TTId = 1) THEN
          NEXT ARTBAS.
    END.

    /* Er butikklisten blank, tas alle butikker. */
    if wvButiker = wAlle then
      RUN ByggButikkListe.

    if T-Butikk then
      BUTIKKER:
      do wLoop2 = 1 to num-entries(wvButiker):
        if wSjekkTilbud then
        SJEKKTILBUD:
        DO:
          FIND Butiker NO-LOCK where
            Butiker.Butik = INT(ENTRY(wLoop2,wvButiker)) NO-ERROR.
          if AVAILABLE Butiker then
            FIND ArtPris NO-LOCK WHERE
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
              ArtPRis.ProfilNr   = Butiker.ProfilNr NO-ERROR.
          /* Sjekker priskøen. */
          FIND first PrisKo NO-LOCK where
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr and
            PrisKo.ProfilNr      = Butiker.ProfilNr and
            PrisKo.Tilbud        = true and
            PrisKo.Type          = 2 NO-ERROR.
          if AVAILABLE PrisKo then
          DO:  /*----------*/
            if FI-FraFraTilbud <> ? then
            DO:
              if PrisKo.AktiveresDato < FI-FraFraTilbud then
                NEXT ARTBAS.
            END.
            if FI-TilFraTilbud <> ? then
            DO:
              if PrisKo.AktiveresDato > FI-TilFraTilbud then
                NEXT ARTBAS.
            END.
            if FI-FraTilTilbud <> ? then
            DO:
              if PrisKo.GyldigTilDato < FI-FraTilTilbud then
                NEXT ARTBAS.
            END.
            if FI-TilTilTilbud <> ? then
            DO:
              if PrisKo.GyldigTilDato > FI-TilTilTilbud then
                NEXT ARTBAS.
            END.
          END. /*----------*/
          ELSE DO:
            if AVAILABLE ArtPRis then
            DO:
              if T-AktivTilbud then
              DO:
                if ArtPris.Tilbud THEN
                  LEAVE SJEKKTILBUD.
                else next ARTBAS.
              END.
              if FI-FraFraTilbud <> ? then
              DO:
                if ArtPris.TilbudFraDato = ? then
                  NEXT ARTBAS.
                if ArtPris.TilbudFraDato < FI-FraFraTilbud then
                  NEXT ARTBAS.
              END.
              if FI-TilFraTilbud <> ? then
              DO:
                if ArtPris.TilbudFraDato = ? then
                  NEXT ARTBAS.
                if ArtPris.TilbudFraDato > FI-TilFraTilbud then
                  NEXT ARTBAS.
              END.
              if FI-FraTilTilbud <> ? then
              DO:
                if ArtPris.TilbudTilDato = ? then
                  NEXT ARTBAS.
                if ArtPris.TilbudTilDato < FI-FraTilTilbud then
                  NEXT ARTBAS.
              END.
              if FI-TilTilTilbud <> ? then
              DO:
                if ArtPris.TilbudTilDato = ? then
                  NEXT ARTBAS.
                if ArtPris.TilbudTilDato > FI-TilTilTilbud then
                  NEXT ARTBAS.
              END.
            END.
          END.
        END. /* SJEKKTILBUD */
        if NOT CAN-FIND(FIRST ListeLinje where
                        ListeLinje.ListeType  = Lister.ListeType and
                        ListeLinje.ListeNr    = Lister.ListeNr and
                        ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                                entry(wLoop2,wvButiker) and
                        ListeLinje.Div1       = entry(wLoop2,wvButiker)) then
          DO:
            create ListeLinje.
            assign
              ListeLinje.ListeType  = Lister.ListeType
              ListeLinje.ListeNr    = Lister.ListeNr
              ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                      entry(wLoop2,wvButiker)
              ListeLinje.Div1       = entry(wLoop2,wvButiker)
              ListeLinje.CellNr     = wCellNr
              wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"999999") +
                                      (if ArtBas.LopNr = ?
                                        then ""
                                        else STRING(ArtBas.LopNr,"9999"))
              ListeLinje.DivX[2]    = STRING(ArtBas.Vg,"999999") + STRING(ArtBas.LevNr,"9999999") + STRING(ArtBas.LevKod)
              ListeLinje.DivX[3]    = STRING(ArtBas.LevNr,"9999999") + STRING(ArtBas.LevKod)
              ListeLinje.DivX[4]    = STRING(wCellNr,"999999999")
              ListeLinje.DivX[5]    = STRING(ArtBas.BildNr,"999999999")
              ListeLinje.DivX[6]    = STRING(ArtBas.Vg,"999999") +
                                      STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[ 7]   = STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.Farg,"999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[ 8]   = STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.Vg,"999999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[ 9]   = STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[10]   = STRING(ArtBas.Farg,"999") +
                                      STRING(ArtBas.VgKat,"9999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[11]   = STRING(ArtBas.Farg,"999") +
                                      STRING(ArtBas.Vg,"999999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[12]   = STRING(ArtBas.Farg,"999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
                .
            assign
              wAntall = wAntall + 1
              FI-Info = "Butikk " + entry(wLoop2,wvButiker) + " - Antall poster opprettet " + string(wAntall).
            IF VALID-HANDLE(wParent) then
              RUN Disp-Info in wParent (FI-Info).
          END.
      end. /* BUTIKKER */
    else do:
      if wSjekkTilbud then
      DO:
        FIND Butiker NO-LOCK where
          Butiker.Butik = wCl NO-ERROR.
        if AVAILABLE Butiker then
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
            ArtPRis.ProfilNr   = Butiker.ProfilNr NO-ERROR.

        /* Sjekker ArtPris. */
        if AVAILABLE ArtPRis then
        SJEKKTILBUD2:
        DO:
          if T-AktivTilbud then
          DO:
            if ArtPris.Tilbud THEN
              LEAVE SJEKKTILBUD2.
            else next ARTBAS.
          END.
          /* Sjekker priskøen. */
          FIND first PrisKo NO-LOCK where
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr and
            PrisKo.ProfilNr      = Butiker.ProfilNr and
            PrisKo.Tilbud        = true and
            PrisKo.Type          = 2 NO-ERROR.
          if AVAILABLE PrisKo then
          DO:  /*----------*/
            if FI-FraFraTilbud <> ? then
            DO:
              if PrisKo.AktiveresDato < FI-FraFraTilbud then
                NEXT ARTBAS.
            END.
            if FI-TilFraTilbud <> ? then
            DO:
              if PrisKo.AktiveresDato > FI-TilFraTilbud then
                NEXT ARTBAS.
            END.
            if FI-FraTilTilbud <> ? then
            DO:
              if PrisKo.GyldigTilDato < FI-FraTilTilbud then
                NEXT ARTBAS.
            END.
            if FI-TilTilTilbud <> ? then
            DO:
              if PrisKo.GyldigTilDato > FI-TilTilTilbud then
                NEXT ARTBAS.
            END.
          END. /*----------*/
          /* Sjekker artpris */
          ELSE DO:
            if FI-FraFraTilbud <> ? then
            DO:
              if ArtPris.TilbudFraDato = ? then
                NEXT ARTBAS.
              if ArtPris.TilbudFraDato < FI-FraFraTilbud then
                NEXT ARTBAS.
            END.
            if FI-TilFraTilbud <> ? then
            DO:
              if ArtPris.TilbudFraDato = ? then
                NEXT ARTBAS.
              if ArtPris.TilbudFraDato > FI-TilFraTilbud then
                NEXT ARTBAS.
            END.
            if FI-FraTilTilbud <> ? then
            DO:
              if ArtPris.TilbudTilDato = ? then
                NEXT ARTBAS.
              if ArtPris.TilbudTilDato < FI-FraTilTilbud then
                NEXT ARTBAS.
            END.
            if FI-TilTilTilbud <> ? then
            DO:
              if ArtPris.TilbudTilDato = ? then
                NEXT ARTBAS.
              if ArtPris.TilbudTilDato > FI-TilTilTilbud then
                NEXT ARTBAS.
            END.
          END.
        END.
      END. /* SJEKKTILBUD2 */
      IF NOT CAN-FIND(FIRST ListeLinje where
                      ListeLinje.ListeType  = Lister.ListeType and
                      ListeLinje.ListeNr    = Lister.ListeNr and
                      ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                              "0") /* Allt legges på butikk 0 */ then
        DO:
          create ListeLinje.
          assign
            ListeLinje.ListeType  = Lister.ListeType
            ListeLinje.ListeNr    = Lister.ListeNr
            ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                    "0" /* Allt legges på butikk 0 */
            ListeLinje.CellNr     = wCellNr
            wCellNr               = wCellNr + 1
            ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"999999") +
                                      (if ArtBas.LopNr = ?
                                        then ""
                                        else STRING(ArtBas.LopNr,"9999"))
            ListeLinje.DivX[2]    = STRING(ArtBas.Vg,"999999") + STRING(ArtBas.LevNr,"9999999") + STRING(ArtBas.LevKod)
            ListeLinje.DivX[3]    = STRING(ArtBas.LevNr,"9999999") + STRING(ArtBas.LevKod)
            ListeLinje.DivX[4]    = STRING(wCellNr,"999999999")
            ListeLinje.DivX[5]    = STRING(ArtBas.BildNr,"999999999")
            ListeLinje.DivX[6]    = STRING(ArtBas.Vg,"999999") +
                                    STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[ 7]   = STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.Farg,"999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[ 8]   = STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.Vg,"999999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[ 9]   = STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[10]   = STRING(ArtBas.Farg,"999") +
                                    STRING(ArtBas.VgKat,"999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[11]   = STRING(ArtBas.Farg,"999") +
                                    STRING(ArtBas.Vg,"999999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[12]   = STRING(ArtBas.Farg,"999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            .
          assign
            wAntall = wAntall + 1
            FI-Info = "Antall poster opprettet " + string(wAntall).
          IF VALID-HANDLE(wParent) then
            RUN Disp-Info in wParent (FI-Info).
        END.
    end.


  end. /* ARTBAS */
end. /* MAIN_LOOP */

if available ListeLinje then
  release ListeLinje.

PROCEDURE ByggButikkListe:
  wvButiker = "".
  FOR EACH Butiker NO-LOCK WHERE
    Butiker.Butik > 0:
    wvButiker = wvButiker +
                (if wvButiker = ""
                   THEN ""
                   ELSE ",") +
                STRING(Butiker.Butik).
  END.
END PROCEDURE.

PROCEDURE InitVaregrupper:

  wvVareGrupper = "".

  /* Varegrupper */
  for each VarGr no-lock where VarGr.Vg > 0:
    assign
      wvVareGrupper = wvVareGrupper +
                     (if wvVareGrupper = ""
                        then ""
                        else ",") +
                     string(VarGr.Vg).
  end.
END PROCEDURE.





