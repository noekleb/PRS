/************************************************************
    Program:  byggmedlemsliste
    Created:  TN    12 Des 00
Description:

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

{medlem.i}

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
  MEDLEMMER:
  FOR EACH tmpMedlem:
    /* Henter kunden og sjekker at det er en gyldig kunde. */
    FIND Medlem NO-LOCK where
      RECID(Medlem) = tmpMEdlem.MedlemsRecid NO-ERROR.
    if NOT AVAILABLE Medlem then
      NEXT MEDLEMMER.

    IF NOT CAN-FIND(FIRST ListeLinje where
                    ListeLinje.ListeType  = Lister.ListeType and
                    ListeLinje.ListeNr    = Lister.ListeNr and
                    ListeLinje.DataObjekt = string(Medlem.MedlemsNr)) then
      DO:
        create ListeLinje.
        assign
          ListeLinje.ListeType  = Lister.ListeType
          ListeLinje.ListeNr    = Lister.ListeNr
          ListeLinje.DataObjekt = string(Medlem.MedlemsNr)
          ListeLinje.CellNr     = wCellNr
          wCellNr               = wCellNr + 1
          ListeLinje.DivX[ 1]   = STRING(Medlem.MedlemsNr,">>>>>>>>>>>>>")
          ListeLinje.DivX[ 2]   = Medlem.ForNavn + " " + Medlem.EtterNavn
          ListeLinje.DivX[ 3]   = Medlem.PostNr + " " + Medlem.ForNavn + " " + Medlem.EtterNavn
          ListeLinje.DivX[ 4]   = STRING(wCellNr,"99999999")
          ListeLinje.DivX[ 5]   = Medlem.Telefon
          ListeLinje.DivX[ 6]   = Medlem.Telefaks
          ListeLinje.Divx[ 7]   = Medlem.Mobil
          ListeLinje.Divx[ 8]   = Medlem.Adresse1
          ListeLinje.Divx[ 9]   = Medlem.Adresse2
          ListeLinje.Divx[10]   = Medlem.Land
          ListeLinje.Divx[11]   = Medlem.ePostAdresse
          ListeLinje.Divx[12]   = Medlem.ForNavn + " " + Medlem.EtterNavn
          ListeLinje.Divx[14]   = Medlem.Adresse2
          ListeLinje.Divx[15]   = Medlem.PostNr
          ListeLinje.Divx[16]   = "" /* STRING(Kunde.MaksKredit) */
          ListeLinje.Divx[17]   = "" /* Kunde.Telefon */
          ListeLinje.Divx[18]   = "" /* Kunde.LevAdresse1 */
          ListeLinje.Divx[19]   = "" /* Kunde.LevLand */
          ListeLinje.Divx[21]   = "" /* Kunde.LevPostNr */
          ListeLinje.Divx[22]   = "" /* Kunde.Telefaks */
          ListeLinje.Divx[23]   = "" /* Kunde.Mobil */
          ListeLinje.Divx[24]   = STRING(Medlem.MedType)
          ListeLinje.Divx[25]   = STRING(Medlem.MedGruppe).

        FIND Post NO-LOCK WHERE
            Post.PostNr = Medlem.PostNr NO-ERROR.
        IF AVAILABLE Post THEN
            ASSIGN
              ListeLinje.Divx[20]   = Post.Beskrivelse
              ListeLinje.Divx[26]   = Post.Beskrivelse
              .
        
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

