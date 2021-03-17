/************************************************************
    Program:  dagsrappartikkel.p
    Created:  TN    30 Jun 02
Description:  Skriver ut artikkelomsetning for en dag.

Last change:  TN   27 Apr 100   10:40 am
************************************************************/

DEF INPUT PARAMETER  wLayout        as INT   NO-UNDO.
DEF INPUT PARAMETER  wLSort         as INT   NO-UNDO.
DEF INPUT PARAMETER  wDato          AS DATE  NO-UNDO.
DEF INPUT PARAMETER  wButikker      AS LOG   NO-UNDO.

DEF var wListerRecid   as RECID  NO-UNDO.
def var wListeType     as char   no-undo.
def var wKriterier     as char   no-undo.
def var wJobbNr        as int    no-undo.
def var wDivData       as char   no-undo.
def var wStatus        as char   no-undo.
DEF VAR wAlle          as CHAR   NO-UNDO.
DEF VAR wBestHLevRec   as RECID  NO-UNDO.
DEF VAR wChild         AS HANDLE NO-UNDO.
DEF VAR piButikkNr     AS INT    NO-UNDO.
DEF VAR wCellNr        AS INT    NO-UNDO.
DEF VAR wInnData       AS CHAR   NO-UNDO.
DEF VAR wPeriTekst     AS CHAR   NO-UNDO.

{syspara.i 7 1 2 wListeType}
{syspara.i 1 100 1 wAlle}

STATUS DEFAULT "Oppretter liste.".

run listehode.p ("NEG", "Ny", wListeType, output wListerRecid).

find Lister no-lock where
 recid(Lister) = wListerRecid.
if available Lister then
  do TRANSACTION:
    find current Lister exclusive-lock.
    assign
        Lister.Beskrivelse   = "Dagsrapport artikkelomsetning " + STRING(wDato) + "."
        Lister.Merknad       = "" /*FI-Merknad*/
        Lister.Eier          = userid("skotex") /*FI-Eier*/
        Lister.Kriterier[ 1] = wAlle + "|" + wAlle /*wvVareGrupper + "|" + wVareGrupper */
        Lister.Kriterier[ 2] = wAlle + "|" + wAlle /* wvLeverandorer + "|" + wLeverandorer */
        Lister.Kriterier[ 3] = "3" /*string(RS-Aktiv)*/  
        Lister.Kriterier[ 4] = wAlle + "|" + wAlle /* wvSesonger + "|" + wSesonger */   
        Lister.Kriterier[ 5] = wAlle + "|" + wAlle /* wvFarger + "|" + wFarger */      
        Lister.Kriterier[ 6] = wAlle + "|" + wAlle /* wvMaterial + "|" + wMaterial */   
        Lister.Kriterier[ 7] = wAlle + "|" + wAlle /* wvButiker + "|" + wButiker */
        Lister.Kriterier[ 8] = if wButikker 
                                 then "TRUE"
                                 else "FALSE" 
        Lister.Kriterier[ 9] = ";" /*(if FI-FraDato = ?
                                   then ""
                                   else string(FI-FraDato)) + ";" + 
                                (if FI-TilDato = ?
                                   then "" 
                                   else string(FI-TilDato))*/
        Lister.Kriterier[10] = ";" /*(if FI-FraLevDato = ?
                                   then ""
                                   else string(FI-FraLevDato)) + ";" + 
                                (if FI-TilLevDato = ?
                                   then "" 
                                   else string(FI-TilLevDato)) */
        Lister.Kriterier[11] = ";" /*(if FI-FraLopNr = ?
                                   then "?"
                                   else string(FI-FraLopNr)) + ";" + 
                                (if FI-TilLopNr = ?
                                   then "?"
                                   else string(FI-TilLopNr)) */
        Lister.Kriterier[12] = "0;999" /*string(FI-FraKateg) + ";" + string(FI-TilKateg)*/
        Lister.Kriterier[13] = "0;9999999999999" /*string(FI-FraArtikkelNr) + ";" + string(FI-TilArtikkelNr)*/
        Lister.Kriterier[14] = "*" /*string(FI-LevKod)*/
        Lister.Kriterier[15] = "0;999999" /*string(FI-FraBildNr) + ";" + string(FI-TilBildNr)*/
        Lister.Kriterier[16] = "FALSE" /*if T-Annonse 
                                 then "TRUE"
                                 else "FALSE" */
        Lister.Kriterier[17] = wAlle + "|" + wAlle /*wvVmId + "|" + wVmId*/
        Lister.Kriterier[18] = "?,?,?,?" /*(IF FI-FraFraTilbud = ?
                                  THEN "?"
                                  ELSE STRING(FI-FraFraTilbud)) + "," +  
                                (IF FI-TilFraTilbud = ?
                                  THEN "?"
                                  ELSE STRING(FI-TilFraTilbud)) + "," +                                
                                (IF FI-FraTilTilbud = ?
                                  THEN "?"
                                  ELSE STRING(FI-FraTilTilbud)) + "," +  
                                (IF FI-TilTilTilbud = ?
                                  THEN "?"
                                  ELSE STRING(FI-TilTilTilbud)) */
        Lister.Kriterier[19] = "FALSE" /*if T-AktivTilbud 
                                 then "TRUE"
                                 else "FALSE" */
      .

     release Lister.
  end. /* TRANSACTION */

find Lister no-lock where
  recid(Lister) = wListerRecid.

STATUS DEFAULT "Bygger listelinjene.".

/* Bygger listelinjene */
LISTELINJER:
FOR EACH TransLogg NO-LOCK WHERE
    TransLogg.Dato = wDato:

    /* Setter butikknummer */
    IF wButikker THEN
        ASSIGN
        piButikkNr = TransLogg.Butik.
    ELSE
        ASSIGN
        piButikkNr = 0.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT LISTELINJER.

    if NOT CAN-FIND(FIRST ListeLinje where
                    ListeLinje.ListeType  = Lister.ListeType and
                    ListeLinje.ListeNr    = Lister.ListeNr and
                    ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                            string(piButikkNr) and
                    ListeLinje.Div1       = string(piButikkNr)) then
      DO:
        PAUSE 0.
        STATUS DEFAULT "Artikkel " + STRING(Translogg.ArtikkelNr) + "/" + STRING(TransLogg.Butik) + ".".

        create ListeLinje.
        assign
          ListeLinje.ListeType  = Lister.ListeType
          ListeLinje.ListeNr    = Lister.ListeNr
          ListeLinje.DataObjekt = string(ArtBas.ArtikkelNr) + "," +
                                  string(piButikkNr)
          ListeLinje.Div1       = string(piButikkNr)
          ListeLinje.CellNr     = wCellNr
          wCellNr               = wCellNr + 1
          ListeLinje.DivX[1]    = STRING(ArtBas.Vg,"9999") +
                                  (if ArtBas.LopNr = ?
                                    then ""
                                    else STRING(ArtBas.LopNr,"9999"))
          ListeLinje.DivX[2]    = STRING(ArtBas.Vg,"9999") + STRING(ArtBas.LevNr,"9999999") + STRING(ArtBas.LevKod)
          ListeLinje.DivX[3]    = STRING(ArtBas.LevNr,"9999999") + STRING(ArtBas.LevKod)
          ListeLinje.DivX[4]    = STRING(wCellNr,"999999999")
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
                                  STRING(ArtBas.VgKat,"9999") +
                                  STRING(ArtBas.LevNr,"9999999") +
                                  ArtBas.LevKod
          ListeLinje.DivX[11]   = STRING(ArtBas.Farg,"999") +
                                  STRING(ArtBas.Vg,"9999") +
                                  STRING(ArtBas.LevNr,"9999999") +
                                  ArtBas.LevKod
          ListeLinje.DivX[12]   = STRING(ArtBas.Farg,"999") +
                                  STRING(ArtBas.LevNr,"9999999") +
                                  ArtBas.LevKod
            .
    END.
END. /* LISTELINJER */

STATUS DEFAULT "Skriver ut rapporten.".

assign 
  wInnData = string(year(wDato)) + "," + 
             string(year(wDato)) + "," +  
             string(wDato - date(1,1,year(wDato)) + 1) + "," +  
             string(wDato - date(1,1,year(wDAto)) + 1)
  wPeriTekst = "      " + string(wDato) + " - " + string(wDato)
  .                       
if wInnData = ? then 
  wInnData = "".
else 
  wInnData = wInnData + 
             ",<Ledig>" + "," + 
             "DAG" + "," + 
             "false" + "," +
             wPeriTekst.

run skrivkolleksjon.p (wListerRecid, wLayout, wLSort, "1",wInnData,?).
if return-value = "AVBRYT" then
  do:
    message "Feil ved utskrift av datasett!"
      view-as alert-box error title "Utskriftsfeil".
    return no-apply.
  end.

/* DØDEN */
do TRANSACTION:
  FIND Lister EXCLUSIVE-LOCK where
    RECID(Lister) = wListerRecid NO-ERROR.
  if AVAILABLE Lister then
    DO:
      for each ListeLinje of Lister exclusive-lock:
        delete ListeLinje.
      end.
    DELETE Lister.
    END.
END. /* TRANSACTION DØDEN */

STATUS DEFAULT "".



