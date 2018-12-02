/************************************************************
    Program:  byggkundeliste
    Created:  TN    8 Jan 00
Description:

Last change:  TN   25 Apr 100   11:05 am
************************************************************/

DEF INPUT PARAMETER wListerRecid     as RECID NO-UNDO.
DEF INPUT PARAMETER wParent          as handle NO-UNDO.

def var wAntall    as int    no-undo.
def var wLoop1     as int    no-undo.
def var wLoop2     as int    no-undo.
def var wStatListe as char   no-undo.
DEF VAR FI-Info    as CHAR   NO-UNDO.
DEF VAR wStatLst   as CHAR   NO-UNDO.
DEF VAR wAlle      as CHAR   NO-UNDO.
DEF VAR wCellNr    as INT    NO-UNDO.

{kundeliste.i}

find Lister no-lock where
  recid(Lister) = wListerRecid no-error.
if not available Lister then
  return "AVBRYT".

{syspara.i 1 100 1 wAlle}

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

MAIN_LOOP:
do wLoop1 = 1 to 1:
  KUNDER:
  FOR EACH tmpKunde:
    /* Henter kunden og sjekker at det er en gyldig kunde. */
    FIND Kunde NO-LOCK where
      RECID(Kunde) = tmpKunde.KundeRecid NO-ERROR.
    if NOT AVAILABLE Kunde then
      NEXT KUNDER.

    IF NOT CAN-FIND(FIRST ListeLinje where
                    ListeLinje.ListeType  = Lister.ListeType and
                    ListeLinje.ListeNr    = Lister.ListeNr and
                    ListeLinje.DataObjekt = string(Kunde.KundeNr)) then
      DO:
        create ListeLinje.
        assign
          ListeLinje.ListeType  = Lister.ListeType
          ListeLinje.ListeNr    = Lister.ListeNr
          ListeLinje.DataObjekt = string(Kunde.KundeNr)
          ListeLinje.CellNr     = wCellNr
          wCellNr               = wCellNr + 1
          ListeLinje.DivX[ 1]   = STRING(Kunde.KundeNr,">>>>>>>>>>>>>")
          ListeLinje.DivX[ 2]   = Kunde.Navn
          ListeLinje.DivX[ 3]   = Kunde.PostNr + Kunde.Navn
          ListeLinje.DivX[ 4]   = STRING(wCellNr,"99999999")
          ListeLinje.DivX[ 5]   = Kunde.KontTelefon
          ListeLinje.DivX[ 6]   = Kunde.KontTelefaks
          ListeLinje.Divx[ 7]   = Kunde.KontMobil
          ListeLinje.Divx[ 8]   = Kunde.Adresse1
          ListeLinje.Divx[ 9]   = Kunde.KontNavn
          ListeLinje.Divx[10]   = Kunde.Land
          ListeLinje.Divx[11]   = Kunde.KontE-Post
          ListeLinje.Divx[12]   = Kunde.Navn
          ListeLinje.Divx[14]   = Kunde.Adresse2
          ListeLinje.Divx[15]   = Kunde.PostNr
          ListeLinje.Divx[16]   = STRING(Kunde.MaksKredit)
          ListeLinje.Divx[17]   = Kunde.Telefon
          ListeLinje.Divx[18]   = Kunde.LevAdresse1
          ListeLinje.Divx[19]   = Kunde.LevLand
          ListeLinje.Divx[21]   = Kunde.LevPostNr
          ListeLinje.Divx[22]   = Kunde.Telefaks
          ListeLinje.Divx[23]   = Kunde.Mobil
          ListeLinje.Divx[24]   = STRING(Kunde.TypeId)
          ListeLinje.Divx[25]   = STRING(Kunde.GruppeId).

        FIND Post NO-LOCK WHERE
            Post.PostNr = Kunde.LevPostNr NO-ERROR.
        IF AVAILABLE Post THEN
            ListeLinje.Divx[20]   = Post.Beskrivelse.
        
        assign
          wAntall = wAntall + 1
          FI-Info = "Antall poster opprettet " + string(wAntall).
        IF VALID-HANDLE(wParent) then
          RUN Disp-Info in wParent (FI-Info).
      END.

  end. /* KUNDER */
end. /* MAIN_LOOP */

if available ListeLinje then
  release ListeLinje.

