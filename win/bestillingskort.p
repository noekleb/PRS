/************************************************************
    Program:  bestillingskort.p
    Created:  TN    9 Aug 99
Description:  Skriver ut en bestilling.

Last change:  TN   27 Apr 100   10:40 am
************************************************************/

DEF INPUT PARAMETER  wBestHodeRecid as RECID NO-UNDO.
DEF INPUT PARAMETER  wLayout        as INT   NO-UNDO.
DEF INPUT PARAMETER  wLSort         as INT   NO-UNDO.
DEF INPUT PARAMETER  wFolgeseddler  as LOG   NO-UNDO.

DEF var wListerRecid   as RECID NO-UNDO.
def var wListeType     as char  no-undo.
def var wKriterier     as char  no-undo.
def var wJobbNr        as int   no-undo.
def var wDivData       as char  no-undo.
def var wStatus        as char  no-undo.
DEF VAR wAlle          as CHAR  NO-UNDO.
DEF VAR wBestHLevRec   as RECID NO-UNDO.

{syspara.i 7 1 1 wListeType}
{syspara.i 1 100 1 wAlle}

/*
assign
  wLayout = 101
  wLSort  = 4.
*/

/* Innleveranserapport og pakkseddler benytter leveranse. */
IF CAN-DO("107,109",STRING(wLayout)) then
  DO:
    IF wLayout = 109 then
      DO:
        FIND BestHode no-lock where
          RECID(BestHode) = wBestHodeRecid NO-ERROR.
        assign
          wBestHLevRec = wBestHodeRecid.
      END.
    ELSE DO:
      assign
        wBestHLevRec = wBestHodeRecid.
      FIND BestHLev NO-LOCK where
        RECID(BestHLev) = wBestHodeRecid NO-ERROR.
      IF AVAILABLE BestHLev then
        FIND BestHode OF BestHLev NO-LOCK NO-ERROR.
    END.
  END.
/* Alle andre rapporter benytter bestilling. */
ELSE
  FIND BestHode NO-LOCK WHERE
    RECID(BestHode) = wBestHodeRecid NO-ERROR.
if not available BestHode then
  DO:
    MESSAGE "Ukjent bestillingshode" VIEW-AS ALERT-BOX error title "Feil".
    return.
  END.

run listehode.p ("NEG", "Ny", wListeType, output wListerRecid).

find Lister no-lock where
 recid(Lister) = wListerRecid.
if available Lister then
  do TRANSACTION:
    find current Lister exclusive-lock.
    assign
      Lister.Beskrivelse   = "Bestilling " + string(BestHode.BestNr)
      Lister.Merknad       = if wLAyout = 107
                               then "Utskrift av innleveransrapport "
                             else if wLayout = 109
                               then "Utskrift av innleveranserapport pr. butikk "
                             else "Utskrift av bestilling " +
                             string(BestHode.BestNr) + " Artikkel " + string(BestHode.ArtikkelNr) +
                             " fra lev. " +
                             string(BestHode.LevNr)
      Lister.Eier          = userid("dictdb")
      Lister.Kriterier[ 1] = wAlle + "|" + wAlle
      Lister.Kriterier[ 2] = wAlle + "|" + wAlle
      Lister.Kriterier[ 3] = wAlle + "|" + wAlle
      Lister.Kriterier[ 4] = wAlle + "|" + wAlle
      Lister.Kriterier[ 5] = wAlle + "|" + wAlle
      Lister.Kriterier[ 6] = wAlle + "|" + wAlle
      Lister.Kriterier[ 7] = wAlle + "|" + wAlle
      Lister.Kriterier[ 8] =  "FALSE"
      Lister.Kriterier[ 9] = "01,01,1980;31,12,2999"
      /*
      Lister.Kriterier[ 9] = string(day(BestHode.BestillingsDato),"99") + "," +
                             string(month(BestHode.BestillingsDato),"99") + "," +
                             string(year(BestHode.BestillingsDato),"9999") + ";" +
                             string(day(BestHode.BestillingsDato),"99") + "," +
                             string(month(BestHode.BestillingsDato),"99") + "," +
                             string(year(BestHode.BestillingsDato),"9999")
      */
      Lister.Kriterier[10] = "" + ";" + ""
      Lister.Kriterier[11] = string(BestHode.BestNr) + ";" + string(BestHode.BestNr)
      Lister.Kriterier[12] = "0" + ";" + "99"
      Lister.Kriterier[13] = "0" + ";" + "9999999"
      Lister.Kriterier[14] = "FALSE"
      Lister.Kriterier[15] = wAlle + "|" + wAlle
      Lister.Kriterier[17] = "0"                          .

     release Lister.
  end. /* TRANSACTION */

find Lister no-lock where
  recid(Lister) = wListerRecid.

run byggkolleksjon (wListerRecid,
                    BestHode.BestillingsDato,
                    BestHode.BestillingsDato,
                    "",
                    "",
                    BestHode.BestNr,
                    BestHode.BestNr,
                    0,
                    99999999,
                    0,
                    99,
                    false,
                    wAlle,
                    wAlle,
                    string(BestHode.BestStat),
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    "",
                    FALSE,
                    "0",
                    ?).

IF CAN-DO("107,109",STRING(wLayout)) then
  run skrivkolleksjon.p (wListerRecid, wLayout, wLSort, "1",STRING(wBestHLevRec)+ "|" + if wFolgeseddler then "1" else "0",?).
else
  run skrivkolleksjon.p (wListerRecid, wLayout, wLSort, "1"," ",?).
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



