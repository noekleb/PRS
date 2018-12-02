/************************************************************
    Program:  byggkolleksjon.P
    Created:  TN    8 Aug 99
Description:

Last change:  TN    8 Aug 100   11:53 am
************************************************************/

DEF INPUT PARAMETER wListerRecid   as RECID NO-UNDO.
DEF INPUT PARAMETER FI-FraDato     as DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilDato     as DATE NO-UNDO.
DEF INPUT PARAMETER FI-FraLevDato  as date NO-UNDO.
DEF INPUT PARAMETER FI-TilLevDato   as date NO-UNDO.
DEF INPUT PARAMETER FI-FraBestNr   as INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilBestNr   as INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraOrdreNr  as INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilOrdreNr  as INT  NO-UNDO.
DEF INPUT PARAMETER FI-FraKateg    as INT  NO-UNDO.
DEF INPUT PARAMETER FI-TilKateg    as INT  NO-UNDO.
DEF INPUT PARAMETER T-Butikk       as LOG  NO-UNDO.
DEF INPUT PARAMETER wvVareGrupper  as char no-undo.
DEF INPUT PARAMETER wvLeverandorer as char no-undo.
DEF INPUT PARAMETER wvBestStatus   as char no-undo.
DEF INPUT PARAMETER wvSesonger     as char no-undo.
DEF INPUT PARAMETER wvFarger       as char no-undo.
DEF INPUT PARAMETER wvMaterial     as char no-undo.
DEF INPUT PARAMETER wvButiker      as char no-undo.
DEF INPUT PARAMETER wvVmId         as char no-undo.
DEFINE INPUT PARAMETER wvProdusent AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER T-Annonse      as LOG  NO-UNDO.
DEF INPUT PARAMETER wBekreftet     as char no-undo.
DEF INPUT PARAMETER wParent        as handle NO-UNDO.

def var wAntall    as int    no-undo.
def var wLoop1     as int    no-undo.
def var wLoop2     as int    no-undo.
def var wStatListe as char   no-undo.
DEF VAR FI-Info    as CHAR   NO-UNDO.
DEF VAR wStatLst   as CHAR   NO-UNDO.
DEF VAR wAlle      as CHAR   NO-UNDO.
DEF VAR wCellNr    as INT    NO-UNDO.

find Lister no-lock where
  recid(Lister) = wListerRecid no-error.
if not available Lister then
  return "AVBRYT".

{syspara.i 5 2 99 wStatListe}
{syspara.i 1 100 1 wAlle}

assign
  wStatLst = wvBestStatus.
if wStatLst = wAlle then
  RUN InitBestStatus.

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
message "TEST byggkolleksjon.p" skip
  "wListerRecid"        wListerRecid skip
  "FI-FraDato"          FI-FraDato skip
  "FI-TilDato"          FI-TilDato skip
  "FI-FraLevUke"        FI-FraLevUke skip
  "FI-TilLevUke"        FI-TilLevUke skip
  "FI-FraBestNr"        FI-FraBestNr skip
  "FI-TilBestNr"        FI-TilBestNr skip
  "FI-FraOrdreNr"       FI-FraOrdreNr skip
  "FI-TilORdreNR"       FI-TilOrdreNr skip
  "FI-FraKateg"         FI-FraKateg skip
  "FI-TilKateg"         FI-TilKateg skip
  "T-Butikk"            T-Butikk skip
  "wvVareGrupper"       wvVareGrupper skip
  "wvLeverandorer"      wvLeverandorer skip
  "wvBestStatus"        wvBestStatus skip
  "wStatLst"            wStatLst skip
  "wvSesonger"          wvSesonger skip
  "wvFarger"            wvFarger skip
  "wvMaterial"          wvMaterial skip
  "wvButiker"           wvButiker skip
  "wvVmId"              wvVmId skip
  "T-Annonse"           T-Annonse skip
  "wPArent"             wParent skip
view-as alert-box.
*/

MAIN_LOOP:
do wLoop1 = 1 to num-entries(wStatLst):

  BESTHODE:
  for each BestHode no-lock where
    BestHode.BestNr   >= FI-FraBestNr and
    BestHode.BestNr   <= FI-TilBestNr and
    BestHode.BestStat  = int(entry(wLoop1,wStatLst)) and
    (if FI-TilOrdreNr - FI-FraOrdreNr = 9999999
      THEN true
     else
      BestHode.OrdreNr <> ?) and
    (if BestHode.OrdreNr = ?
      THEN true
     else
      BestHode.OrdreNr >= FI-FraOrdreNr) and
    (if BestHode.OrdreNr = ?
      then true
     else BestHode.OrdreNr <= FI-TilOrdreNr):

    /* Avgrensning p† bestillingsdato. */
    if FI-FraDato <> ? then
      DO:
        if BestHode.BestillingsDato = ? then
          NEXT BESTHODE.
        if BestHode.BestillingsDato < FI-FraDato then
          NEXT BESTHODE.
      END.
    if FI-TilDato <> ? then
      DO:
        if BestHode.BestillingsDato = ? then
          NEXT BESTHODE.
        if BestHode.BestillingsDato > FI-TilDato then
          NEXT BESTHODE.
      END.

    /* Avgrensning p† leveringsdato. */
    if FI-FraLevDato <> ? then
      DO:
        if BestHode.LevDato = ? then
          NEXT BESTHODE.
        if BestHode.LevDato < FI-FraLevDato then
          NEXT BESTHODE.
      END.
    if FI-TilLevDato <> ? then
      DO:
        if BestHode.LevDato = ? then
          NEXT BESTHODE.
        if BestHode.LevDato > FI-TilLevDato then
          NEXT BESTHODE.
      END.

    if wvLeverandorer <> wAlle THEN
      DO:
        if can-do(wvLeverandorer,string(BestHode.LevNr)) then.
           else next BESTHODE.
      END.
    IF wBekreftet <> "0" THEN DO:
        IF wBekreftet = "1" AND BestHode.bekreftetdato = ? THEN
            next BESTHODE.
        ELSE IF wBekreftet = "2" AND BestHode.bekreftetdato <> ? THEN
            next BESTHODE.
    END.
    find ArtBas no-lock where
      ArtBas.ArtikkelNr = BestHode.ArtikkelNr no-error.
    if not available ArtBas then
      next BESTHODE.

    if ArtBas.VgKat >= FI-FraKateg and
       ArtBas.VgKat <= FI-TilKateg then.
    else next BESTHODE.

    if T-Annonse then
      DO:
        if ArtBas.AnonseArtikkel = FALSE then
          next BESTHODE.
      END.

    if wvVareGrupper <> wAlle then
      DO:
        if can-do(wvVareGrupper,string(ArtBas.Vg)) then.
          else next BESTHODE.
      END.
    if wvVmId <> wAlle then
      DO:
        if can-do(wvVmId,string(ArtBas.VmId)) then.
          else next BESTHODE.
      END.

    if TRIM(wvProdusent) <> '' then
      DO:
        wvProdusent = REPLACE(wvProdusent,'|',','). 
        if can-do(wvProdusent,string(ArtBas.ProdNr)) then.
          else next BESTHODE.
      END.
      
    if wvSesonger <> wAlle then
      DO:
        if can-do(wvSesonger,string(ArtBas.SaSong)) then.
          else next BESTHODE.
      END.
    if wvFarger <> wAlle then
      DO:
        if can-do(wvFarger,string(ArtBas.Farg)) then.
          else next BESTHODE.
      END.
    if wvMaterial <> wAlle then
      DO:
        if can-do(wvMaterial,string(ArtBas.MatKod)) then.
          else next BESTHODE.
      END.
    /* Er butikklisten blank, tas alle butikker. */
    if wvButiker = wAlle then
      RUN ByggButikkListe.

    if T-Butikk then
      BUTIKKER:
      do wLoop2 = 1 to num-entries(wvButiker):
        /* Tar kun med der hvor det ligger bestillinger for butikken */
        if can-find(first BestStr where
                      BestStr.BestNr   = BestHode.BestNr and
                      BestStr.Butik    = int(entry(wLoop2,wvButiker)) and
                      BestStr.Storl    > "" and
                      BestStr.BestStat = BestHode.BestStat and
                      BestStr.Bestilt > 0) then.
        else next BUTIKKER.

        if NOT CAN-FIND(FIRST ListeLinje where
                        ListeLinje.ListeType  = Lister.ListeType and
                        ListeLinje.ListeNr    = Lister.ListeNr and
                        ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                                string(BestHode.BestNr) + "," +
                                                entry(wLoop2,wvButiker) and
                        ListeLinje.Div1       = entry(wLoop2,wvButiker) and
                        listelinje.Div2       = entry(BestHode.BestStat,wStatListe)) then
          DO:
            create ListeLinje.
            assign
              ListeLinje.ListeType  = Lister.ListeType
              ListeLinje.ListeNr    = Lister.ListeNr
              ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                      string(BestHode.BestNr) + "," +
                                      entry(wLoop2,wvButiker)
              ListeLinje.Div1       = entry(wLoop2,wvButiker)
              listelinje.Div2       = entry(BestHode.BestStat,wStatListe)
              ListeLinje.CellNr     = wCellNr
              wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"9999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[2]    = STRING(ArtBas.LevNr,"9999999") +
                                      STRING(ArtBas.LevKod)
              ListeLinje.DivX[3]    = STRING(wCellNr,"999999999")
              ListeLinje.DivX[4]    = STRING(ArtBas.Vg,"9999") +
                                      STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[5]    = STRING(ArtBas.BildNr,"999999999")
              ListeLinje.DivX[6]    = STRING(ArtBas.Vg,"9999") +
                                      STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
              ListeLinje.DivX[ 7]   = STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.Farg,"999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[ 8]   = STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.Vg,"9999") +
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
                                      STRING(ArtBas.Vg,"9999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[12]   = STRING(ArtBas.Farg,"999") +
                                      STRING(ArtBas.LevNr,"9999999") +
                                      ArtBas.LevKod
              ListeLinje.DivX[13]   = STRING(ArtBas.Vg,"9999") +
                                      STRING(ArtBas.VgKat,"99") +
                                      STRING(ArtBas.Farg,"999")
                .
            assign
              wAntall = wAntall + 1
              FI-Info = "Butikk " + entry(wLoop2,wvButiker) + " - Antall poster opprettet " + string(wAntall).
            IF VALID-HANDLE(wParent) then
              RUN Disp-Info in wParent (FI-Info).
          END.
      end. /* BUTIKKER */
    else do:
      IF NOT CAN-FIND(FIRST ListeLinje where
                      ListeLinje.ListeType  = Lister.ListeType and
                      ListeLinje.ListeNr    = Lister.ListeNr and
                      ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                              string(BestHode.BestNr) + "," +
                                              "0" /* Allt legges på butikk 0 */ and
                      listelinje.Div2       = entry(BestHode.BestStat,wStatListe)) then
        DO:
          create ListeLinje.
          assign
            ListeLinje.ListeType  = Lister.ListeType
            ListeLinje.ListeNr    = Lister.ListeNr
            ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                    string(BestHode.BestNr) + "," +
                                    "0" /* Allt legges på butikk 0 */
            listelinje.Div2       = entry(BestHode.BestStat,wStatListe)
            ListeLinje.CellNr     = wCellNr
            wCellNr               = wCellNr + 1
            /* Listesorteringer. */
            ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"9999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    STRING(ArtBas.LevKod)
            ListeLinje.DivX[2]    = STRING(ArtBas.LevNr,"9999999") +
                                    STRING(ArtBas.LevKod)
            ListeLinje.DivX[3]    = STRING(wCellNr,"999999999")
            ListeLinje.DivX[4]    = STRING(ArtBas.Vg,"9999") +
                                    STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    STRING(ArtBas.LevKod)
            ListeLinje.DivX[5]    = STRING(ArtBas.BildNr,"999999999")
            ListeLinje.DivX[6]    = STRING(ArtBas.Vg,"9999") +
                                    STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[ 7]   = STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.Farg,"999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[ 8]   = STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.Vg,"9999") +
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
                                    STRING(ArtBas.Vg,"9999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[12]   = STRING(ArtBas.Farg,"999") +
                                    STRING(ArtBas.LevNr,"9999999") +
                                    ArtBas.LevKod
            ListeLinje.DivX[13]   = STRING(ArtBas.Vg,"9999") +
                                    STRING(ArtBas.VgKat,"99") +
                                    STRING(ArtBas.Farg,"999")
            .
          assign
            wAntall = wAntall + 1
            FI-Info = "Antall poster opprettet " + string(wAntall).
          IF VALID-HANDLE(wParent) then
            RUN Disp-Info in wParent (FI-Info).
        END.
    end.

  end. /* BESTHODE */

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

PROCEDURE InitBestStatus:
  wStatLst = "".

  /* Bestillingsstatus - Bruker en passende temprærtabell. */
  SYSPARA:
  for each SysPAra no-lock where
    SysPara.SysHId = 5 and
    SysPara.SysGr  = 2 and
    SysPara.ParaNr < 99:
    
    assign
      wStatLst = wStatLst +
                 (if wStatLst = ""
                   then ""
                   else ",") +
                 string(SysPara.ParaNr).
  end. /* SYSPARA */

END PROCEDURE.





