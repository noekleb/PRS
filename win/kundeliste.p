/************************************************************
    Program:  kundeliste.p
    Created:  TN    8 Jan 00
Description:  Utskrifter fra kunderegisteret.

Last change:  TN    8 Jan 100    2:33 pm
************************************************************/

DEF INPUT PARAMETER wLayout        as INT   NO-UNDO.
DEF INPUT PARAMETER wLSort         as INT   NO-UNDO.

DEF var wListerRecid   as RECID NO-UNDO.
def var wListeType     as char  no-undo.
def var wKriterier     as char  no-undo.
def var wJobbNr        as int   no-undo.
def var wDivData       as char  no-undo.
def var wStatus        as char  no-undo.
DEF VAR wAlle          as CHAR  NO-UNDO.

{syspara.i 7 1 3 wListeType}
{syspara.i 1 100 1 wAlle}
{kundeliste.i}

if NOT CAN-FIND(FIRST tmpKunde) then
  DO:
    MESSAGE "Ingen kunder i utskriftslisten." VIEW-AS ALERT-BOX error title "Feil".
    return.
  END.

run listehode.p ("NEG", "Ny", wListeType, output wListerRecid).

find Lister no-lock where
 recid(Lister) = wListerRecid.
if available Lister then
  do TRANSACTION:
    find current Lister exclusive-lock.
    assign
      Lister.Beskrivelse   = "Adresselapper (4 x 5) "
      Lister.Merknad       = "Adresselapper (4 x 5) "
      Lister.Eier          = userid("dictdb").

     release Lister.
  end. /* TRANSACTION */

find Lister no-lock where
  recid(Lister) = wListerRecid.

RUN byggkundeliste.p (wListerRecid,?).

run skrivkolleksjon.p (wListerRecid, wLayout, wLSort, "1","",?).
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



