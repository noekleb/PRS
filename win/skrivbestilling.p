/************************************************************
    Program:  skrivbestilling.p
    Created:  TN    8 Aug 99
Description:

Last change:  TN    8 Aug 99   10:10 pm
************************************************************/

DEF INPUT PARAMETER wBestHodeRecid as RECID NO-UNDO.
DEF INPUT PARAMETER wModus         as CHAR  NO-UNDO.
DEF INPUT PARAMETER wListeType     as CHAR  NO-UNDO.

DEF var FI-Navn      as CHAR  NO-UNDO.
DEF var FI-Merknad   as CHAR  NO-UNDO.
DEF var FI-Eier      as CHAR  NO-UNDO.
DEF var T-Butikk     as LOG   NO-UNDO.
DEF var FI-FraDato   as DATE  NO-UNDO.
DEF var FI-TilDato   as DATE  NO-UNDO.
DEF var FI-FraLevUke as char  NO-UNDO.
DEF var FI-TilLevUke as char  NO-UNDO.
DEF var FI-FraBestNr as INT   NO-UNDO.
DEF var FI-TilBestNr as INT   NO-UNDO.
DEF var FI-FraKateg  as INT   NO-UNDO.
DEF var FI-TilKateg  as INT   NO-UNDO.
DEF VAR wListerRecid as RECID NO-UNDO.

FIND BestHode NO-LOCK where
  RECID(BestHode) = wBestHodeRecid NO-ERROR.
IF NOT AVAILABLE BestHode then
  DO:
    MESSAGE "Ukjent bestilling!" VIEW-AS ALERT-BOX.
    RETURN "AVBRYT".
  END.

/* Tar vare på skjermverdiene. */
assign
  FI-Navn      = "Utskrift bestilling " + STRING(BestHode.BestNr)
  FI-Merknad   = ""
  FI-Eier      = USERID("dictdb")
  T-Butikk     = false
  FI-FraDato   = BestHode.BestillingsDato
  FI-TilDato   = BestHode.BestillingsDato
  FI-FraLevUke = ""
  FI-TilLevUke = ""
  FI-FraBestNr = BestHode.BestNr
  FI-TilBestNr = BestHode.BestNr
  FI-FraKateg  = 1
  FI-TilKateg  = 99.

if wModus = "NY" then
  do:
    run listehode.p ("", wModus, wListeType, output wListerRecid).

   find Lister no-lock where
     recid(Lister) = wListerRecid.
   if available Lister then
     do TRANSACTION:
       find current Lister exclusive-lock.
       assign
         Lister.Beskrivelse   = FI-Navn
         Lister.Merknad       = FI-Merknad
         Lister.Eier          = FI-Eier
         Lister.Kriterier[ 1] = "0" + "|" + "999"
         Lister.Kriterier[ 2] = "0" + "|" + "999999"
         Lister.Kriterier[ 3] = "0" + "|" + "99"
         Lister.Kriterier[ 4] = "0" + "|" + "999"
         Lister.Kriterier[ 5] = "0" + "|" + "999"
         Lister.Kriterier[ 6] = "0" + "|" + "999"
         Lister.Kriterier[ 7] = "0" + "|" + "99999999"
         Lister.Kriterier[ 8] =  "FALSE"
         Lister.Kriterier[ 9] = string(day(BestHode.BestillingsDato),"99") + "," +
                                string(month(BestHode.BestillingsDato),"99") + "," +
                                string(year(BestHode.BestillingsDato),"9999") + ";" +
                                string(day(BestHode.BestillingsDato),"99") + "," +
                                string(month(BestHode.BestillingsDato),"99") + "," +
                                string(year(BestHode.BestillingsDato),"9999")
         Lister.Kriterier[10] = string(FI-FraLevUke) + ";" + string(FI-TilLevUke)
         Lister.Kriterier[11] = string(FI-FraBestNr) + ";" + string(FI-TilBestNr)
         Lister.Kriterier[12] = string(FI-FraKateg)  + ";" + string(FI-TilKateg)
         .

         release Lister.
     end. /* TRANSACTION */
  end.

find Lister no-lock where
  recid(Lister) = wListerRecid.

/* Her bygger og oppdaterer vi */
if wModus = "NY" then
  run ByggKoleksjon.
else do:
  run SlettKoleksjon.
  run ByggKoleksjon.
end.


