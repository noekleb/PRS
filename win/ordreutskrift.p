/************************************************************
    Program:  ordreutskrift.p
    Created:  TN    15 Aug 00
Description:  Skriver ut en bestilling.

Last change:  TN   15 Aug 100    4:10 pm
************************************************************/

DEF INPUT PARAMETER  wOrdreRecid    as RECID NO-UNDO.
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

FIND Ordre NO-LOCK WHERE
    RECID(Ordre) = wOrdreRecid NO-ERROR.
if not available Ordre then
  DO:
    MESSAGE "Ukjent ordre" VIEW-AS ALERT-BOX error title "Feil".
    return.
  END.

run listehode.p ("NEG", "Ny", wListeType, output wListerRecid).

find Lister no-lock where
 recid(Lister) = wListerRecid.
if available Lister then
  do TRANSACTION:
    find current Lister exclusive-lock.
    assign
      Lister.Beskrivelse   = "Ordre " + string(Ordre.OrdreNr)
      Lister.Merknad       = if wLayout = 105
                               then "Utskrift av ordre "
                             ELSE ""
      Lister.Eier          = userid("dictdb")
      Lister.Kriterier[ 1] = wAlle + "|" + wAlle
      Lister.Kriterier[ 2] = wAlle + "|" + wAlle
      Lister.Kriterier[ 3] = wAlle + "|" + wAlle
      Lister.Kriterier[ 4] = wAlle + "|" + wAlle
      Lister.Kriterier[ 5] = wAlle + "|" + wAlle
      Lister.Kriterier[ 6] = wAlle + "|" + wAlle
      Lister.Kriterier[ 7] = wAlle + "|" + wAlle
      Lister.Kriterier[ 8] =  "FALSE"
      Lister.Kriterier[ 9] = "?;?"
      Lister.Kriterier[10] = STRING(Ordre.OrdreNr)  + ";" + STRING(Ordre.OrdreNr)
      Lister.Kriterier[11] = ""  + ";" + ""
      Lister.Kriterier[12] = "0" + ";" + "99"
      Lister.Kriterier[13] = "0" + ";" + "9999999"
      Lister.Kriterier[14] = "FALSE"
      Lister.Kriterier[15] = wAlle + "|" + wAlle
      Lister.Kriterier[17] = "0".

     release Lister.
  end. /* TRANSACTION */

find Lister no-lock where
  recid(Lister) = wListerRecid.

run byggkolleksjon (wListerRecid,
                    ?,
                    ?,
                    ?,
                    ?,
                    0,
                    9999999,
                    Ordre.OrdreNr,
                    Ordre.OrdreNr,
                    0,
                    99,
                    false,
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    wAlle,
                    FALSE,
                    "0",
                    ?).

run skrivkolleksjon.p (wListerRecid, wLayout, wLSort, "1"," ",?).

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



