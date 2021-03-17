/************************************************************
    Program:  byggartikkler.P
    Created:  TN    8 Aug 99
Description:

Last change:  TN    8 Aug 100    4:58 pm
************************************************************/

DEF INPUT PARAMETER wListerRecid     AS RECID NO-UNDO.
DEF INPUT PARAMETER FI-FraDato       AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilDato       AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-FraLevDato    AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilLevDato    AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-FraLopNr      AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilLopNr      AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraArtikkelNr AS DEC  NO-UNDO.
DEF INPUT PARAMETER FI-TilArtikkelNr AS DEC  NO-UNDO.
DEF INPUT PARAMETER FI-LevKod        AS CHAR NO-UNDO.
DEF INPUT PARAMETER RS-Aktiv         AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraKateg      AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilKateg      AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraBildNr     AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilBildNr     AS INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraFraTilbud  AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilFraTilbud  AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-FraTilTilbud  AS DATE NO-UNDO. 
DEF INPUT PARAMETER FI-TilTilTilbud  AS DATE NO-UNDO.
DEF INPUT PARAMETER T-Butikk         AS LOG  NO-UNDO.
DEF INPUT PARAMETER wvVareGrupper    AS CHAR NO-UNDO.
DEF INPUT PARAMETER wvLeverandorer   AS CHAR NO-UNDO.
DEF INPUT PARAMETER wvSesonger       AS CHAR NO-UNDO.
DEF INPUT PARAMETER wvVmId           AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wvProdusent   AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER wvFarger         AS CHAR NO-UNDO.
DEF INPUT PARAMETER wvMaterial       AS CHAR NO-UNDO.
DEF INPUT PARAMETER wvButiker        AS CHAR NO-UNDO.
DEF INPUT PARAMETER T-Annonse        AS LOG  NO-UNDO.
DEF INPUT PARAMETER T-AktivTilbud    AS LOG  NO-UNDO.
DEFINE INPUT PARAMETER cInnerSulaIdList   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cOvIdList          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cSlitSulaIdList    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cLast-SkoRowIdList AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cAnv-KodIdList     AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER wParent          AS HANDLE NO-UNDO.

DEF VAR wAntall      AS INT  NO-UNDO.
DEF VAR wLoop1       AS INT  NO-UNDO.
DEF VAR wLoop2       AS INT  NO-UNDO.
DEF VAR wStatListe   AS CHAR NO-UNDO.
DEF VAR FI-Info      AS CHAR NO-UNDO.
DEF VAR wStatLst     AS CHAR NO-UNDO.
DEF VAR wAlle        AS CHAR NO-UNDO.
DEF VAR wCellNr      AS INT  NO-UNDO.
DEF VAR wSjekkTilbud AS LOG  NO-UNDO.
DEF VAR wCl          AS INT  NO-UNDO.

FIND Lister NO-LOCK WHERE
  recid(Lister) = wListerRecid NO-ERROR.
IF NOT AVAILABLE Lister THEN
  RETURN "AVBRYT".

{syspara.i 1 100 1 wAlle}
{syspara.i 5   1 1 wCl int}

/* Skal tilbud sjekkes? */
IF (FI-FraFraTilbud <> ? OR
   FI-TilFraTilbud <> ? OR
   FI-FraTilTilbud <> ? OR
   FI-TilTilTilbud <> ? OR
   T-AktivTilbud = TRUE) THEN
  wSjekkTilbud = TRUE.
ELSE
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
ASSIGN
  wCellNr = 0.
FOR EACH ListeLinje NO-LOCK WHERE
  ListeLinje.ListeType  = Lister.ListeType AND
  ListeLinje.ListeNr    = Lister.ListeNr AND
  ListeLinje.CellNr     <> ?
  BY ListeLinje.ListeType
  BY ListeLinje.ListeNr
  BY ListeLinje.CellNr:

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

IF wvVareGrupper = wAlle THEN
  RUN InitVaregrupper.

MAIN_LOOP:
DO wLoop1 = 1 TO num-entries(wvVareGrupper):
  ARTBAS:
  FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.Vg = INT(ENTRY(wLoop1,wvVareGrupper)) AND

    ArtBas.ArtikkelNr >= FI-FraArtikkelNr AND
    ArtBas.ArtikkelNr <= FI-TilArtikkelNr AND
    ArtBas.LevKod MATCHES FI-LevKod + "*":

    IF FI-FraDato <> ? THEN
      DO:
        IF ArtBas.RegistrertDato < FI-FraDato THEN
          NEXT ARTBAS.
      END.
    IF FI-TilDato <> ? THEN
      DO:
        IF ArtBas.RegistrertDato > FI-TilDato THEN
          NEXT ARTBAS.
      END.

    IF FI-FraLevDato <> ? THEN
      DO:
        IF ArtBas.Inn_Dato < FI-FraLevDato THEN
          NEXT ARTBAS.
      END.
    IF FI-TilLevDato <> ? THEN
      DO:
        IF ArtBas.Inn_Dato > FI-TilLevDato THEN
          NEXT ARTBAS.
      END.

    IF FI-FraLopNr <> ? THEN
      DO:
        IF ArtBas.LopNr >= FI-FraLopNr AND
           ArtBas.LopNr <= FI-TilLopNr THEN. /* Gjør ingenting. */
        ELSE
          NEXT ARTBAS.
      END.

    IF wvLeverandorer <> wAlle THEN
      DO:
        IF can-do(wvLeverandorer,string(ArtBas.LevNr)) THEN.
           ELSE NEXT ARTBAS.
      END.

    IF T-Annonse THEN
      DO:
        IF ArtBas.AnonseArtikkel = FALSE THEN
          NEXT ARTBAS.
      END.

    IF ArtBas.VgKat >= FI-FraKateg AND
       ArtBas.VgKat <= FI-TilKateg THEN.
    ELSE NEXT ARTBAS.

    IF ArtBas.BildNr >= FI-FraBildNr AND
       ArtBas.BildNr <= FI-TilBildNr THEN.
    ELSE NEXT ARTBAS.

    IF wvSesonger <> wAlle THEN
      DO:
        IF can-do(wvSesonger,string(ArtBas.SaSong)) THEN.
          ELSE NEXT ARTBAS.
      END.

    IF wvVmId <> wAlle THEN
      DO:
        IF can-do(wvVmId,string(ArtBas.VmId)) THEN.
          ELSE NEXT ARTBAS.
      END.

    IF wvProdusent <> '' THEN
      DO:
        wvProdusent = REPLACE(wvProdusent,'|',',').
        IF can-do(wvProdusent,string(ArtBas.ProdNr)) THEN.
          ELSE NEXT ARTBAS.
      END.

    IF wvFarger <> wAlle THEN
      DO:
        IF can-do(wvFarger,string(ArtBas.Farg)) THEN.
          ELSE NEXT ARTBAS.
      END.

     IF cInnerSulaIdList <> wAlle AND cInnerSulaIdList <> '' THEN 
     DO:
        IF CAN-DO(cInnerSulaIdList,STRING(ArtBas.Inner-Id)) THEN.
          ELSE NEXT ARTBAS.
     END.
     IF cOvIdList <> wAlle AND cOvIdList <> '' THEN 
     DO:
        IF CAN-DO(cOvIdList,STRING(ArtBas.Ov-Id)) THEN.
          ELSE NEXT ARTBAS.
     END.
     IF cSlitSulaIdList <> wAlle AND cSlitSulaIdList <> '' THEN 
     DO:
        IF CAN-DO(cSlitSulaIdList,STRING(ArtBas.Slit-Id)) THEN.
          ELSE NEXT ARTBAS.
     END.
     IF cLast-SkoRowIdList <> wAlle AND cLast-SkoRowIdList <> '' THEN 
     DO:
        IF CAN-DO(cLast-SkoRowIdList,STRING(ArtBas.Last-Id)) THEN.
          ELSE NEXT ARTBAS.
     END.
     IF cAnv-KodIdList <> wAlle AND cAnv-KodIdList <> '' THEN 
     DO:
        IF CAN-DO(cAnv-KodIdList,STRING(ArtBas.Anv-Id)) THEN.
          ELSE NEXT ARTBAS.
     END.

    IF wvMaterial <> wAlle THEN
      DO:
        IF can-do(wvMaterial,string(ArtBas.MatKod)) THEN.
          ELSE NEXT ARTBAS.
      END.

    IF RS-Aktiv = 1 THEN /* Undertrykker utgåtte */
      DO:
        IF ArtBas.Utgatt = TRUE THEN
          NEXT ARTBAS.
      END.
    ELSE IF RS-Aktiv = 2 THEN /* Undertrykker aktive */
      DO:
        IF ArtBas.Utgatt = FALSE THEN
          NEXT ARTBAS.
      END.

    /* Er butikklisten blank, tas alle butikker. */
    IF wvButiker = wAlle THEN
      RUN ByggButikkListe.

    IF T-Butikk THEN
      BUTIKKER:
      DO wLoop2 = 1 TO num-entries(wvButiker):
        IF wSjekkTilbud THEN
        SJEKKTILBUD:
        DO:
          FIND Butiker NO-LOCK WHERE
            Butiker.Butik = INT(ENTRY(wLoop2,wvButiker)) NO-ERROR.
          IF AVAILABLE Butiker THEN
            FIND ArtPris NO-LOCK WHERE
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
              ArtPRis.ProfilNr   = Butiker.ProfilNr NO-ERROR.
          /* Sjekker priskøen. */
          FIND FIRST PrisKo NO-LOCK WHERE
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
            PrisKo.ProfilNr      = Butiker.ProfilNr AND
            PrisKo.Tilbud        = TRUE AND
            PrisKo.Type          = 2 NO-ERROR.
          IF AVAILABLE PrisKo THEN
          DO:  /*----------*/
            IF FI-FraFraTilbud <> ? THEN
            DO:
              IF PrisKo.AktiveresDato < FI-FraFraTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-TilFraTilbud <> ? THEN
            DO:
              IF PrisKo.AktiveresDato > FI-TilFraTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-FraTilTilbud <> ? THEN
            DO:
              IF PrisKo.GyldigTilDato < FI-FraTilTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-TilTilTilbud <> ? THEN
            DO:
              IF PrisKo.GyldigTilDato > FI-TilTilTilbud THEN
                NEXT ARTBAS.
            END.
          END. /*----------*/
          ELSE DO:
            IF AVAILABLE ArtPRis THEN
            DO:
              IF T-AktivTilbud THEN
              DO:
                IF ArtPris.Tilbud THEN
                  LEAVE SJEKKTILBUD.
                ELSE NEXT ARTBAS.
              END.
              IF FI-FraFraTilbud <> ? THEN
              DO:
                IF ArtPris.TilbudFraDato = ? THEN
                  NEXT ARTBAS.
                IF ArtPris.TilbudFraDato < FI-FraFraTilbud THEN
                  NEXT ARTBAS.
              END.
              IF FI-TilFraTilbud <> ? THEN
              DO:
                IF ArtPris.TilbudFraDato = ? THEN
                  NEXT ARTBAS.
                IF ArtPris.TilbudFraDato > FI-TilFraTilbud THEN
                  NEXT ARTBAS.
              END.
              IF FI-FraTilTilbud <> ? THEN
              DO:
                IF ArtPris.TilbudTilDato = ? THEN
                  NEXT ARTBAS.
                IF ArtPris.TilbudTilDato < FI-FraTilTilbud THEN
                  NEXT ARTBAS.
              END.
              IF FI-TilTilTilbud <> ? THEN
              DO:
                IF ArtPris.TilbudTilDato = ? THEN
                  NEXT ARTBAS.
                IF ArtPris.TilbudTilDato > FI-TilTilTilbud THEN
                  NEXT ARTBAS.
              END.
            END.
          END.
        END. /* SJEKKTILBUD */
        IF NOT CAN-FIND(FIRST ListeLinje WHERE
                        ListeLinje.ListeType  = Lister.ListeType AND
                        ListeLinje.ListeNr    = Lister.ListeNr AND
                        ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                                entry(wLoop2,wvButiker) AND
                        ListeLinje.Div1       = entry(wLoop2,wvButiker)) THEN
          DO:
            CREATE ListeLinje.
            ASSIGN
              ListeLinje.ListeType  = Lister.ListeType
              ListeLinje.ListeNr    = Lister.ListeNr
              ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                      entry(wLoop2,wvButiker)
              ListeLinje.Div1       = entry(wLoop2,wvButiker)
              ListeLinje.CellNr     = wCellNr
              wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"999999") +
                                      (IF ArtBas.LopNr = ?
                                        THEN ""
                                        ELSE STRING(ArtBas.LopNr,"999999"))
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
            ASSIGN
              wAntall = wAntall + 1
              FI-Info = "Butikk " + entry(wLoop2,wvButiker) + " - Antall poster opprettet " + string(wAntall).
            IF VALID-HANDLE(wParent) THEN
              RUN Disp-Info IN wParent (FI-Info).
          END.
      END. /* BUTIKKER */
    ELSE DO:
      IF wSjekkTilbud THEN
      DO:
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = wCl NO-ERROR.
        IF AVAILABLE Butiker THEN
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPRis.ProfilNr   = Butiker.ProfilNr NO-ERROR.

        /* Sjekker ArtPris. */
        IF AVAILABLE ArtPRis THEN
        SJEKKTILBUD2:
        DO:
          IF T-AktivTilbud THEN
          DO:
            IF ArtPris.Tilbud THEN
              LEAVE SJEKKTILBUD2.
            ELSE NEXT ARTBAS.
          END.
          /* Sjekker priskøen. */
          FIND FIRST PrisKo NO-LOCK WHERE
            PrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
            PrisKo.ProfilNr      = Butiker.ProfilNr AND
            PrisKo.Tilbud        = TRUE AND
            PrisKo.Type          = 2 NO-ERROR.
          IF AVAILABLE PrisKo THEN
          DO:  /*----------*/
            IF FI-FraFraTilbud <> ? THEN
            DO:
              IF PrisKo.AktiveresDato < FI-FraFraTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-TilFraTilbud <> ? THEN
            DO:
              IF PrisKo.AktiveresDato > FI-TilFraTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-FraTilTilbud <> ? THEN
            DO:
              IF PrisKo.GyldigTilDato < FI-FraTilTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-TilTilTilbud <> ? THEN
            DO:
              IF PrisKo.GyldigTilDato > FI-TilTilTilbud THEN
                NEXT ARTBAS.
            END.
          END. /*----------*/
          /* Sjekker artpris */
          ELSE DO:
            IF FI-FraFraTilbud <> ? THEN
            DO:
              IF ArtPris.TilbudFraDato = ? THEN
                NEXT ARTBAS.
              IF ArtPris.TilbudFraDato < FI-FraFraTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-TilFraTilbud <> ? THEN
            DO:
              IF ArtPris.TilbudFraDato = ? THEN
                NEXT ARTBAS.
              IF ArtPris.TilbudFraDato > FI-TilFraTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-FraTilTilbud <> ? THEN
            DO:
              IF ArtPris.TilbudTilDato = ? THEN
                NEXT ARTBAS.
              IF ArtPris.TilbudTilDato < FI-FraTilTilbud THEN
                NEXT ARTBAS.
            END.
            IF FI-TilTilTilbud <> ? THEN
            DO:
              IF ArtPris.TilbudTilDato = ? THEN
                NEXT ARTBAS.
              IF ArtPris.TilbudTilDato > FI-TilTilTilbud THEN
                NEXT ARTBAS.
            END.
          END.
        END.
      END. /* SJEKKTILBUD2 */
      IF NOT CAN-FIND(FIRST ListeLinje WHERE
                      ListeLinje.ListeType  = Lister.ListeType AND
                      ListeLinje.ListeNr    = Lister.ListeNr AND
                      ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                              "0") /* Allt legges på butikk 0 */ THEN
        DO:
          CREATE ListeLinje.
          ASSIGN
            ListeLinje.ListeType  = Lister.ListeType
            ListeLinje.ListeNr    = Lister.ListeNr
            ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                    "0" /* Allt legges på butikk 0 */
            ListeLinje.CellNr     = wCellNr
            wCellNr               = wCellNr + 1
            ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"999999") +
                                      (IF ArtBas.LopNr = ?
                                        THEN ""
                                        ELSE STRING(ArtBas.LopNr,"999999"))
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
          ASSIGN
            wAntall = wAntall + 1
            FI-Info = "Antall poster opprettet " + string(wAntall).
          IF VALID-HANDLE(wParent) THEN
            RUN Disp-Info IN wParent (FI-Info).
        END.
    END.


  END. /* ARTBAS */
END. /* MAIN_LOOP */

IF AVAILABLE ListeLinje THEN
  RELEASE ListeLinje.

PROCEDURE ByggButikkListe:
  wvButiker = "".
  FOR EACH Butiker NO-LOCK WHERE
    Butiker.Butik > 0:
    wvButiker = wvButiker +
                (IF wvButiker = ""
                   THEN ""
                   ELSE ",") +
                STRING(Butiker.Butik).
  END.
END PROCEDURE.

PROCEDURE InitVaregrupper:

  wvVareGrupper = "".

  /* Varegrupper */
  FOR EACH VarGr NO-LOCK WHERE VarGr.Vg > 0:
    ASSIGN
      wvVareGrupper = wvVareGrupper +
                     (IF wvVareGrupper = ""
                        THEN ""
                        ELSE ",") +
                     string(VarGr.Vg).
  END.
END PROCEDURE.





