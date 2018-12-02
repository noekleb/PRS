/************************************************************
    Program:  skrivkolleksjon.p
    Created:  TN    8 Aug 99
Description:

Last change:  TN   25 Oct 100    3:49 pm
************************************************************/

DEF INPUT PARAMETER wListerRecid      as RECID  NO-UNDO.
DEF INPUT PARAMETER wLayout           as INT    NO-UNDO.
DEF INPUT PARAMETER wLSort            as INT    NO-UNDO.
DEF INPUT PARAMETER wRB-NUMBER-COPIES as char   NO-UNDO.
DEF INPUT PARAMETER wInnData          as CHAR   NO-UNDO.
DEF INPUT PARAMETER wParentHandle     as HANDLE NO-UNDO.

/* For styring av jobbkø */
DEF var wJobbNr    as INT  NO-UNDO.
DEF var wDivData   as CHAR NO-UNDO.
DEF var wStatus    as CHAR NO-UNDO.
def var wKriterier as char no-undo.
DEF VAR wPeriTekst as CHAR NO-UNDO.
DEF VAR wKndLstTyp as CHAR NO-UNDO.

DEF VAR w109         as INT  NO-UNDO.
DEF VAR wPeriode     as CHAR NO-UNDO.
def var wTekst       as char no-undo.
def var wAlle        as char no-undo.
def var wFilter      as char no-undo.
def var wDato1       as date format "99/99/9999" no-undo.
def var wDato2       as date format "99/99/9999" no-undo.
def var wRapTittel   as char no-undo.
def var wBrukerId    as char no-undo.
def var wTittel      as char no-undo.
def var wKunde       as char no-undo.
def var wButikkLbl   as char no-undo.
def var wSkrevetLbl  as char no-undo.
def var wKollonneLbl as char no-undo.
def var wSideLbl     as char no-undo.
def var wVi          as char no-undo.
def var wKontoSpes   as char no-undo.
def var wSortering   as char no-undo.
def var wkBestDato   as char no-undo.
def var wkLevUke     as char no-undo.
def var wkBestNr     as char no-undo. /* BetNr for kolleksjonsrapp. LopNr for artikkelrapp. */
def var wkKategori   as char no-undo.
def var wkVaregruppe as char no-undo.
def var wkLeverand   as char no-undo.
def var wkBestStat   as char no-undo.
def var wkAktiv      as char no-undo.
def var wkSesong     as char no-undo.
def var wkFarger     as char no-undo.
def var wkMaterial   as char no-undo.
DEF VAR wkArtikkelNr as CHAR NO-UNDO.
DEF VAR wkLevKod     as CHAR NO-UNDO.
def var wSortField   as char no-undo.
def var wT-Butikk    as log  no-undo.
DEF VAR wTyper       as CHAR NO-UNDO.
DEF VAR wKalkLbl     as CHAR NO-UNDO.
DEF VAR wlSideBryt   AS LOG  NO-UNDO.
DEF VAR wSideBryt    AS CHAR NO-UNDO.

def var wOTHER-PARAMETERS     as char no-undo.
def var wRB-REPORT-LIBRARY    as char no-undo.
def var wRB-REPORT-NAME       as char no-undo.
def var wRB-DB-CONNECTION     as char no-undo.
DEF VAR wRB-PRINT-DESTINATION AS CHAR NO-UNDO.
DEF VAR wRB-PRINTER-NAME      AS CHAR NO-UNDO.

/* Rapport 109 er en spesialversjon av 107 */
IF wLayout = 109 then
  DO:
    assign
      wLayout = 107
      w109    = 109.
  END.
else
  w109 = ?.

/*Sjekker om det skal gis sidebryt */
IF CAN-DO("200,201,202",STRING(wLayout)) THEN
DO:
    IF VALID-HANDLE(wParentHandle) THEN
        RUN GetSidebryt IN wParentHandle (OUTPUT wlSideBryt).
END.
ELSE
    wlSideBryt = FALSE.
wSideBryt = "SIDEBRYT = " + (IF wlSideBryt THEN "True" ELSE "False").

{runlib.i}
{syspara.i 7 1 3 wKndLstTyp}

find Lister no-lock where
  recid(Lister) = wListerRecid no-error.
if not available Lister then
  return "AVBRYT".

/* Bygger en liste over listetyper. */
wTyper = "".  
for each SysPara no-lock where
  SysPara.SysHId = 7 and
  SysPara.SysGr  = 1:
  
  wTyper = wTyper + 
           (if wTyper = ""
              then ""
              else ",") + 
           SysPara.Parameter1.   
end.    

/* Dette skal ikke utføres for kundelister */
UTPAKKING:
DO:
  if wKndLstTyp <> Lister.ListeType then
    assign
      wT-Butikk  = if Lister.Kriterier[ 8] = "TRUE" then true else FALSE
      .
  ASSIGN
    wPeriTekst = if NUM-ENTRIES(wInnData) >= 8
                   then ENTRY(8,wInnData)
                   ELSE "".

  /* Skal ikke utf›res for kundelister og vgstatistikk. */
  /* Fra-Til opprettelsesdato for artikkellistene.      */
  if not CAN-DO("KUNDER,VAREGR,LEVERAND,HOVEDGR,SELGERE,SELGER",STRING(Lister.ListeType)) then
  DO:
    if NUM-ENTRIES(Lister.Kriterier[ 9]) = 2 then
      assign
        wDato1 = date(entry(1,Lister.Kriterier[ 9],";"))
        wDato2 = date(entry(2,Lister.Kriterier[ 9],";"))
        .
    else
      assign
        wDato1 = ?
        wDato2 = ?
        .
  END.
END.

run ByggWorkBase.
if return-value = "AVBRYT" then
  do:
    message "Feil ved opprettelse av datasett - bygging av workbase (skrivkolleksjon)!"
      view-as alert-box error title "Utskriftsfeil".
    return no-apply.
  end.

/* Navn på firma */
{syspara.i 1 1 100 wKunde}           
wKunde  = "KUNDE = " + wKunde.

/* Oppkobling av Work database */                      
if valid-handle(wLibHandle) then
  run RpbWrDB in wLibHandle (output wRB-DB-CONNECTION).
if wRB-DB-CONNECTION = "" then
 {syspara.i 1 1 11 wRB-DB-CONNECTION}
 
/* Rapportens tittel */
case wLayout:
  when 100 then 
    do:
      if wT-Butikk then {syspar2.i 6 100 1 wTittel}           
      else {syspara.i 6 100 1 wTittel}           
    end.
  when 110 then
    do:
      if wT-Butikk then {syspar2.i 6 100 1 wTittel}           
      else {syspara.i 6 100 1 wTittel}           
    end.
  WHEN 107 THEN
    DO:
      IF int(Lister.Kriterier[16]) <> 0 THEN
      DO:
        {syspara.i 6 wLayout 28 wTittel}
        {syspara.i 5 4 11 wRB-PRINTER-NAME}
        ASSIGN
          wRB-PRINT-DESTINATION = ""
          .
      END.
    END.
  otherwise {syspara.i 6 wLayout 1 wTittel}
end case.
wTittel = "TITTEL = " + wTittel.


/* Ledetekst kriterier tittel */
{syspara.i 6 wLayout 19 wAlle}           

/* Ledetekst kriterier tittel */
{syspara.i 6 wLayout 2 wKriterier}           
wKriterier = "KRITERIER = " + 
             wKriterier.
             
/* Vårt firmanavn */
{syspara.i 1 1 101 wTekst}           
wVi = "VI = " + wTekst.

/* Label forran butikknavn og nummer. */
{syspara.i 6 wLayout 4 wButikkLbl}           
wButikkLbl = "BUTIKK = " + wButikkLbl.

/* Label forran skrevet dato/tid. */
{syspara.i 6 wLayout 5 wSkrevetLbl}           
wSkrevetLbl = "SKREVET = " + wSkrevetLbl.

/* Kollonnelabler. */
{syspara.i 6 wLayout 6 wKollonneLbl}           
wKollonneLbl = "KOLLONNER = " + wKollonneLbl.

/* Sidenummerlabl. */
{syspara.i 6 wLayout 7 wSideLbl}           
wSideLbl = "SIDE = " + wSideLbl.

/* Sidenummerlabl. */
{syspara.i 6 wLayout 20 wPeriode}
wPeriode = "PERIODE = " + wPeriode + " " + wPeriTekst.

/* Sortering. */
{syspara.i 6 wLayout 8 wSortering}           
{syspara.i 9 wLayout wLSort wTekst}
wSortering = "SORTERING = " + 
             wSortering + " " +
             wTekst.
case wLayout:
  when 100 then 
    do:
      wSortField = "SORTFIELD = " +
                   (if wLSort = 1
                      then "CHAR2"
                    ELSE if wLSort = 3
                      THEN "DivX[31]"
                    else "CHAR3").
    end.
  when 101 then 
      wSortField = "SORTFIELD = " + string(wLSort).
  when 102 then 
      wSortField = "SORTFIELD = " + string(wLSort).
  when 105 then
      wSortField = "SORTFIELD = " + STRING(wLayout) + "-" + string(wLSort).
  when 106 then
      wSortField = "SORTFIELD = " + STRING(wLayout) + "-" + string(wLSort).
  when 107 then
      wSortField = "SORTFIELD = " + string(wLSort).
  when 108 then
      wSortField = "SORTFIELD = " + string(wLSort).
  when 110 then
    do:
      wSortField = "SORTFIELD = " +
                   (if wLSort = 1
                      then "CHAR2"
                    ELSE if wLSort = 3
                      THEN "DivX[31]"
                    else "CHAR3").
    end.
  when 111 then
      wSortField = "SORTFIELD = " + string(wLSort).
  WHEN 300 then
    do:
      wSortField = "SORTFIELD = " +
                   (if wLSort = 2
                      then "DivX[ 2]"
                    ELSE if wLSort = 3
                      THEN "DivX[ 3]"
                    else "DivX[ 1]").
    end.
  WHEN 820 then
    do:
      wSortField = "SORTFIELD = " +
                   (if wLSort = 2
                      then "DivX[ 2]"
                    ELSE if wLSort = 3
                      THEN "DivX[ 3]"
                    else "DivX[ 1]").
    end.
end case.

CASE LOOKUP(Lister.ListeType,wTyper):
  WHEN 1 THEN
    DO:
      if CAN-DO("105,106",STRING(wLayout)) then
        run Hentkriterier1-1.     /* Ordreutskrifter */
      else
        run Hentkriterier1.       /* Kolleksjonsrapporter */
    END.
  WHEN 2  THEN run Hentkriterier2.  /* Artikkellister        */
  WHEN 3  THEN run Hentkriterier3.  /* Kundelister.          */
  WHEN 8  THEN RUN Hentkriterier8.  /* Medlemsstatistikk     */
  OTHERWISE RETURN "AVBRYT".
END CASE.

/* Henter labler for for visning av kalkyle for layout. */
if CAN-DO("101,107,108",STRING(wLayout)) then
  DO:
    /* Ledetekst kriterier tittel */
    {syspara.i 6 wLayout 26 wKalkLbl}
    wKalkLbl = "KOL-LBL = " +
               wKalkLbl.
  END.

/* Setter rapportnavn */
case wLayout:
  when 100 then 
    do:
      wRB-REPORT-LIBRARY   = "rpb\kolleksjon.prl".
      if wT-Butikk then
        do:
          assign 
            wRB-REPORT-NAME = if wLSort = 1
                                then "ButKolleksjon"
                                else "But2Kolleksjon".  
        end.
      else 
        wRB-REPORT-NAME = "Kolleksjon".        
    end.
  when 101 then 
    assign
      wRB-REPORT-LIBRARY   = "rpb\bestilling.prl"
      wRB-REPORT-NAME = "Bestillingskort".
  when 102 then 
    assign
      wRB-REPORT-LIBRARY   = "rpb\bestilling.prl"
      wRB-REPORT-NAME = "Bestillingsoversikt".
  when 105 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\Ordre.prl"
      wRB-REPORT-NAME = "OrdreS-A4-1".
  when 106 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\Ordre.prl"
      wRB-REPORT-NAME = "OrdreS-A4-2".
  when 107 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\bestilling.prl"
      wRB-REPORT-NAME = "Innleveransrapport".
  when 108 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\bestilling.prl"
      wRB-REPORT-NAME = "Bestillingskort2".
  when 110 then
    do:
      wRB-REPORT-LIBRARY   = "rpb\kolleksjon.prl".
      if wT-Butikk then
        do:
          assign 
            wRB-REPORT-NAME = if wLSort = 1
                                then "ButKolleksjon4x3"
                                else "But2Kolleksjon4x3".
        end.
      else 
        wRB-REPORT-NAME = "Kolleksjon4x3".
    end.
  when 111 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\bestilling.prl"
      wRB-REPORT-NAME = "VgBestillingsoversikt".
  when 150 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\lager.prl"
      wRB-REPORT-NAME = "Lagerliste01".
  when 151 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\lager.prl"
      wRB-REPORT-NAME = "VgLagerliste01".
  when 152 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\lager.prl"
      wRB-REPORT-NAME = "LevLagerliste01".
  when 200 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\bilderapporter.prl"
      wRB-REPORT-NAME = "Bilderapport1".
  when 201 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\bilderapporter.prl"
      wRB-REPORT-NAME = "Bilderapport2".
  when 202 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\bilderapporter.prl"
      wRB-REPORT-NAME = "Bilderapport3".
  when 210 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\priser.prl"
      wRB-REPORT-NAME = "Prisliste01".
  when 211 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\priser.prl"
      wRB-REPORT-NAME = "Prisliste02".
  when 212 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\priser.prl"
      wRB-REPORT-NAME = "Prisliste03".
  when 213 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\priser.prl"
      wRB-REPORT-NAME = "Prisliste04".
  when 251 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\statistik.prl"
      wRB-REPORT-NAME = IF can-do("true,yes",entry(7,wInnData))
                          THEN "ArtStat02-Periode"
                          ELSE "ArtStat02".
  when 252 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\statistik.prl"
      wRB-REPORT-NAME = "ArtStat03".
  when 300 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\kundelister.prl"
      wRB-REPORT-NAME = "Avery#5161".
  WHEN 620 THEN. /* Gjør ingenting. xPrint rapport. */
  when 820 then
    assign
      wRB-REPORT-LIBRARY   = "rpb\medlemslister.prl"
      wRB-REPORT-NAME = "Avery#5161".
end case.

CASE LOOKUP(Lister.ListeType,wTyper):
  WHEN 1 THEN
    DO:
    if CAN-DO("105,106",STRING(wLayout)) then
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    =
                             wVi          + "~n" +
                             wKollonneLbl.

    else assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wTittel      + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkBestStat   + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wKollonneLbl + "~n" +
                             wPeriode     + "~n" +
                             wKalkLbl.
    END.
  WHEN 2 THEN
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wTittel      + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkAktiv      + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wPeriode     + "~n" +
                             wSideBryt    + "~n" +
                             wKollonneLbl.
  WHEN 3 THEN
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkAktiv      + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wPeriode     + "~n" +
                             wKollonneLbl + "~n" +
                             wTittel.
  WHEN 4 THEN
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wTittel      + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkAktiv      + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wPeriode     + "~n" +
                             wKollonneLbl.

  WHEN 5 THEN
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wTittel      + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkAktiv      + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wPeriode     + "~n" +
                             wKollonneLbl.
  WHEN 6 THEN
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wTittel      + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkAktiv      + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wPeriode     + "~n" +
                             wKollonneLbl.
  WHEN 7 THEN
    assign
      wBrukerId  = "BRUKER = " + userid("dictdb")
      wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                             wKunde       + "~n" +
                             wTittel      + "~n" +
                             wKriterier   + "~n" +
                             wButikkLbl   + "~n" +
                             wSkrevetLbl  + "~n" +
                             wSideLbl     + "~n" +
                             wVi          + "~n" +
                             wkLevKod     + "~n" +
                             wkArtikkelNr + "~n" +
                             wkBestDato   + "~n" +
                             wkLevUke     + "~n" +
                             wkBestNr     + "~n" +
                             wkKategori   + "~n" +
                             wkVaregruppe + "~n" +
                             wkLeverand   + "~n" +
                             wkAktiv      + "~n" +
                             wkSesong     + "~n" +
                             wkFarger     + "~n" +
                             wkMaterial   + "~n" +
                             wSortField   + "~n" +
                             wSortering   + "~n" +
                             wPeriode     + "~n" +
                             wKollonneLbl.
    WHEN 8 THEN
      assign
        wBrukerId  = "BRUKER = " + userid("dictdb")
        wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                               wKunde       + "~n" +
                               wTittel      + "~n" +
                               wKriterier   + "~n" +
                               wButikkLbl   + "~n" +
                               wSkrevetLbl  + "~n" +
                               wSideLbl     + "~n" +
                               wVi          + "~n" +
                               wkLevKod     + "~n" +
                               wkArtikkelNr + "~n" +
                               wkBestDato   + "~n" +
                               wkLevUke     + "~n" +
                               wkBestNr     + "~n" +
                               wkKategori   + "~n" +
                               wkVaregruppe + "~n" +
                               wkLeverand   + "~n" +
                               wkAktiv      + "~n" +
                               wkSesong     + "~n" +
                               wkFarger     + "~n" +
                               wkMaterial   + "~n" +
                               wSortField   + "~n" +
                               wSortering   + "~n" +
                               wPeriode     + "~n" +
                               wKollonneLbl.
    WHEN 9 THEN
      assign
        wBrukerId  = "BRUKER = " + userid("dictdb")
        wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                               wKunde       + "~n" +
                               wTittel      + "~n" +
                               wKriterier   + "~n" +
                               wButikkLbl   + "~n" +
                               wSkrevetLbl  + "~n" +
                               wSideLbl     + "~n" +
                               wVi          + "~n" +
                               wkLevKod     + "~n" +
                               wkArtikkelNr + "~n" +
                               wkBestDato   + "~n" +
                               wkLevUke     + "~n" +
                               wkBestNr     + "~n" +
                               wkKategori   + "~n" +
                               wkVaregruppe + "~n" +
                               wkLeverand   + "~n" +
                               wkAktiv      + "~n" +
                               wkSesong     + "~n" +
                               wkFarger     + "~n" +
                               wkMaterial   + "~n" +
                               wSortField   + "~n" +
                               wSortering   + "~n" +
                               wPeriode     + "~n" +
                               wKollonneLbl.
    WHEN 10 THEN
      assign
        wBrukerId  = "BRUKER = " + userid("dictdb")
        wOTHER-PARAMETERS    = wBrukerId    + "~n" +
                               wKunde       + "~n" +
                               wTittel      + "~n" +
                               wKriterier   + "~n" +
                               wButikkLbl   + "~n" +
                               wSkrevetLbl  + "~n" +
                               wSideLbl     + "~n" +
                               wVi          + "~n" +
                               wkLevKod     + "~n" +
                               wkArtikkelNr + "~n" +
                               wkBestDato   + "~n" +
                               wkLevUke     + "~n" +
                               wkBestNr     + "~n" +
                               wkKategori   + "~n" +
                               wkVaregruppe + "~n" +
                               wkLeverand   + "~n" +
                               wkAktiv      + "~n" +
                               wkSesong     + "~n" +
                               wkFarger     + "~n" +
                               wkMaterial   + "~n" +
                               wSortField   + "~n" +
                               wSortering   + "~n" +
                               wPeriode     + "~n" +
                               wKollonneLbl.
  OTHERWISE
    assign
      wBrukerId  = "BRUKER = " + USERID("dictdb").
END CASE.

assign
  wFilter   = "JobbLinje.JobbNr = " + string(wJobbNr).
/*
message
  "BrukerID:" wBrukerId skip
  "wOTHER-PARAMETERS:" skip
        wOTHER-PARAMETERS skip
  "Filter:" wFilter skip
  "wRB-REPORT-LIBRARY" wRB-REPORT-LIBRARY skip     
  "wRB-REPORT-NAME"    wRB-REPORT-NAME skip
  "wRB-DB-CONNECTION"  wRB-DB-CONNECTION skip

    "wBrukerId"     wBrukerId    skip
    "wKunde"        wKunde       skip
    "wTittel"       wTittel      skip
    "wKriterier"    wKriterier   skip
    "wButikkLbl"    wButikkLbl   skip
    "wSkrevetLbl"   wSkrevetLbl  skip
    "wSideLbl"      wSideLbl     skip
    "wVi"           wVi          skip
    "wkLevKod"      wkLevKod     skip
    "wkArtikkelNr"  wkArtikkelNr skip
    "wkBestDato"    wkBestDato   skip
    "wkLevUke"      wkLevUke     skip
    "wkBestNr"      wkBestNr     skip
    "wkKategori"    wkKategori   skip
    "wkVaregruppe"  wkVaregruppe skip
    "wkLeverand"    wkLeverand   skip
    "wkBestStat"    wkBestStat   skip
    "wkSesong"      wkSesong     skip
    "wkFarger"      wkFarger     skip
    "wkMaterial"    wkMaterial   skip
    "wSortField"    wSortField   skip
    "wSortering"    wSortering   skip
    "wKollonneLbl"  wKollonneLbl skip
    "wPeriode"      wPeriode     skip
    "wKalkLbl"      wKalkLbl     skip

  view-as alert-box. 
*/

/* xPrint rapport */
IF CAN-DO("620",STRING(wLayout)) THEN
DO:
    CASE wLayout:
        WHEN 620 THEN RUN xpr-kundesaldo.p (INPUT wJobbNr).
    END CASE.

END.
/* Report Builder rapport */
ELSE DO:

/* MESSAGE                                        */
/* program-name(1)                                */
/* SKIP "Skriver rapport i report buiilder:" skip */
/*     wRB-REPORT-LIBRARY skip                    */
/*     wRB-REPORT-NAME SKIP                       */
/*     wRB-DB-CONNECTION SKIP                     */
/*     wFilter                                    */
/* VIEW-AS ALERT-BOX INFO BUTTONS OK.             */

    RUN  aderb\_prntrb2(
    wRB-REPORT-LIBRARY,   /* RB-REPORT-LIBRARY */
       wRB-REPORT-NAME,   /* RB-REPORT-NAME */
     wRB-DB-CONNECTION,   /* RB-DB-CONNECTION */
                   "O",   /* RB-INCLUDE-RECORDS */
               wFilter,   /* RB-FILTER */
                    "",   /* RB-MEMO-FILE */
    (IF wRB-PRINTER-NAME <> ""
       THEN wRB-PRINT-DESTINATION
       ELSE        "?"),   /* RB-PRINT-DESTINATION */
    (IF wRB-PRINTER-NAME <> ""
       THEN wRB-PRINTER-NAME
       ELSE        "?"),   /* RB-PRINTER-NAME */
                    "",   /* RB-PRINTER-PORT */
                    "",   /* RB-OUTPUT-FILE */
    wRB-NUMBER-COPIES,    /* RB-NUMBER-COPIES  - zero */
                    0,    /* RB-BEGIN-PAGE - zero */
                    0,    /* RB-END-PAGE - zero */
                   no,    /* RB-TEST-PATTERN */
                   "",    /* RB-WINDOW-TITLE */
                  yes,    /* RB-DISPLAY-ERRORS */
                  yes,    /* RB-DISPLAY-STATUS */
                   no,    /* RB-NO-WAIT */
    wOTHER-PARAMETERS,    /* RB-OTHER-PARAMETERS */
           ""). /* RB-STATUS-FILE */
END.

/* -------------- Subrutiner ---------------- */
/* Rutine for opprettelse av workbase. */
PROCEDURE ByggWorkBase:
  DEF VAR wMerknad as CHAR NO-UNDO.
  DEF VAR wExeProg as CHAR NO-UNDO.

  /* Pakker Recid til Lister */
  assign
    wKriterier = string(recid(Lister)) + "," +
                 (if w109 = 109
                    then "109"
                    else STRING(wLayout)) + "," +
                 STRING(wLSort).

  /* Finner første ledige jobbnummer. */
  run finnjobb.p (input-output wJobbNr, input-output wDivData, output wStatus).
  if wStatus <> "OK" then
    return "AVBRYT".

/*MESSAGE 'wTyper' LOOKUP(Lister.ListeType,wTyper) skip*/
/*        'wLayout' wLayout                            */
/*VIEW-AS ALERT-BOX.                                   */
  /* Setter ekekveringsprogram. */
  wExeProg = "".
  CASE LOOKUP(Lister.ListeType,wTyper):
    WHEN 1 THEN assign
                  wMerknad = "Kolleksjons"
                  wExeProg = if CAN-DO("105,106",STRING(wLayout))
                               then "x-ordre"
                               else "x-kolleksjon".
    WHEN 2 THEN
      DO:
        assign
          wMerknad = "Artikkel"
          wExeProg = if CAN-DO("210,211,212,213",STRING(wLayout))
                       THEN "x-prisliste"
                     else if wLayout >= 250 AND wLayout <= 299
                       THEN "x-artstat"
                     ELSE if CAN-DO("150,151,152",STRING(wLayout))
                       THEN "x-lager"
                     ELSE "x-artikkler".
      END.
    WHEN 3 THEN 
             DO:
                 assign
                   wMerknad = "Kundeliste"
                 .
                 if wLayout >= 600 and wLayout <= 699 THEN
                     DO:
                       IF can-do("620",string(wLayout)) then 
                           wExeProg = "x-kundekonto".
                       ELSE
                           wExeProg = "x-kundestat".
                     END.
                 else wExeProg = "x-kundeliste".
             END.
    WHEN 8 THEN assign
          wMerknad = "Medlemslister"
          wExeProg = IF can-do("820",string(wLayout))
                       THEN "x-medlemsliste"
                     ELSE "x-medlemsstat".
    OTHERWISE RETURN "AVBRYT".
  END CASE.

  /* Sender med JobbNr og programnavn */
  assign
    wDivData = 
      string(wJobbNr)  + ";" + /* Jobbnummer */
      program-name(1)  + ";" + /* Bestillende program */
      wExeProg         + ";" + /* Program som skal utføres */
      ""               + ";" + /* Start dato */
      ""               + ";" + /* Start tid */
      userid("dictdb") + ";" + /* Brukerid på den som bestiller jobben. */
      "F"              + ";" + /* Startes av - Forgrunn */
      wMerknad         + ";" + /* Merknad */
      wKriterier       + "," + /* Kriterier for utskriftsjobb. */
      wInnData.                /* Periodekriterier (NB: Samme entry som kriterier). */

  /* Starter program som oppretter jobbrecord. */
  run opprjobb.p (input-output wDivData, output wStatus).
  if wStatus <> "OK" then
    do:
      run DeBugJobb ("Etter opprjobb.p " + wStatus).
      return "AVBRYT".
    end.
    
  /* Starter oppdatering av datasett i forgrunnen */
  run runjobb.p (input wJobbNr, input-output wDivData, output wStatus,wParentHandle).

  if wStatus <> "OK" then
    do:
      run DeBugJobb ("Etter runjobb.p " + wStatus).
      return no-apply "AVBRUTT".
    end.
END PROCEDURE.

PROCEDURE Hentkriterier1:

/* Bestillingsdatorange. */
{syspara.i 6 wLayout 9 wkBestDato}           
wkBestDato = "BESTDATO = " + wkBestDato + " " + (if wDato1 <> ? then string(wDato1) ELSE ".") + "-" + (if wDato2 <> ? then string(wDato2) ELSE ".").

/* Leveringsuke. */
{syspara.i 6 wLayout 10 wkLevUke}           
wkLevUke = "LEVUKE = " + wkLevUke + " " +  entry(1,Lister.Kriterier[10],";") + "-" + 
                                           entry(2,Lister.Kriterier[10],";").
/* Inneholder bestilligsnummer for Kolleksjonsrapporter og */
/* løpenummer for artikkelrapporter.                       */
{syspara.i 6 wLayout 11 wkBestNr}           
wkBestNr = "BESTNR = " + wkBestNr + " " +  entry(1,Lister.Kriterier[11],";") + "-" + 
                                           entry(2,Lister.Kriterier[11],";").

/* Kategori. */
{syspara.i 6 wLayout 12 wkKategori}           
wkKategori = "KATEGORI = " + wkKategori + " " +  entry(1,Lister.Kriterier[12],";") + "-" + 
                                                 entry(2,Lister.Kriterier[12],";").

/* Varegrupper. */
{syspara.i 6 wLayout 13 wkVaregruppe}           
wkVaregruppe = "VAREGRUPPE = " + wkVaregruppe + " " +
  (if entry(1,Lister.Kriterier[1],"|") = entry(2,Lister.Kriterier[1],"|")
     then wAlle
     else entry(1,Lister.Kriterier[1],"|")).

/* Leverandør. */
{syspara.i 6 wLayout 14 wkLeverand}           
wkLeverand = "LEVERANDOR = " + wkLeverand + " " + 
  (if entry(1,Lister.Kriterier[2],"|") = entry(2,Lister.Kriterier[2],"|")
     then wAlle
     else entry(1,Lister.Kriterier[2],"|")).

/* BestStat. */
{syspara.i 6 wLayout 15 wkBestStat}           
wkBestStat = "BESTSTAT = " + wkBestStat + " " +
             (if entry(1,Lister.Kriterier[3],"|") = entry(2,Lister.Kriterier[3],"|")
               then wAlle
               else entry(1,Lister.Kriterier[3],"|")).

/* Sesong. */
{syspara.i 6 wLayout 16 wkSesong}           
wkSesong = "SESONG = " + wkSesong + " " + 
  (if entry(1,Lister.Kriterier[4],"|") = entry(2,Lister.Kriterier[4],"|")
     then wAlle
     else entry(1,Lister.Kriterier[4],"|")).

/* Farger. */
{syspara.i 6 wLayout 17 wkFarger}           
wkFarger = "FARGER = " + wkFarger + " " + 
  (if entry(1,Lister.Kriterier[5],"|") = entry(2,Lister.Kriterier[5],"|")
     then wAlle
     else entry(1,Lister.Kriterier[5],"|")).

/* Material. */
{syspara.i 6 wLayout 18 wkMaterial}           
wkMaterial = "MATERIAL = " + wkMaterial + " " + 
  (if entry(1,Lister.Kriterier[6],"|") = entry(2,Lister.Kriterier[6],"|")
     then wAlle
     else entry(1,Lister.Kriterier[6],"|")).

END PROCEDURE.

/* Utskrift av Ordre. */
PROCEDURE Hentkriterier1-1:
  FOR EACH SysPara NO-LOCK where
    SysPara.SysHId = 6 and
    SysPara.SysGr  = wLayout:
    assign
      wKollonneLbl = wKollonneLbl +
                     (if wKollonneLbl = "KOLLONNER = "
                       THEN ""
                       ELSE ",") +
                     SysPara.Parameter1.
  END.
END PROCEDURE.

PROCEDURE Hentkriterier2:
/* Bestillingsdatorange. */
{syspara.i 6 wLayout 9 wkBestDato}           
wkBestDato = "BESTDATO = " + wkBestDato + " " + (if wDato1 <> ? then string(wDato1) ELSE ".") + "-" + (if wDato2 <> ? then string(wDato2) ELSE ".").

/* Leveringsuke. */
{syspara.i 6 wLayout 10 wkLevUke}           
wkLevUke = "LEVUKE = " + wkLevUke + " " +  entry(1,Lister.Kriterier[10],";") + "-" + 
                                           entry(2,Lister.Kriterier[10],";").
/* Leverandørs artikkelnummer. */
{syspara.i 6 wLayout 21 wkLevKod}
wkLevKod = "LEVKOD = " + wkLevKod + " " +  Lister.Kriterier[14].

/* Artikkelnummer. */
{syspara.i 6 wLayout 20 wkArtikkelNr}
wkArtikkelNr = "ARTIKKELNR = " + wkArtikkelNr + " " +  entry(1,Lister.Kriterier[13],";") + "-" +
                                                   entry(2,Lister.Kriterier[13],";").
/* Inneholder bestilligsnummer for Kolleksjonsrapporter og */
/* løpenummer for artikkelrapporter.                       */
{syspara.i 6 wLayout 11 wkBestNr}           
wkBestNr = "BESTNR = " + wkBestNr + " " +  entry(1,Lister.Kriterier[11],";") + "-" + 
                                           entry(2,Lister.Kriterier[11],";").
/* Kategori. */
{syspara.i 6 wLayout 12 wkKategori}           
wkKategori = "KATEGORI = " + wkKategori + " " +  entry(1,Lister.Kriterier[12],";") + "-" + 
                                                 entry(2,Lister.Kriterier[12],";").

/* Varegrupper. */
{syspara.i 6 wLayout 13 wkVaregruppe}           
wkVaregruppe = "VAREGRUPPE = " + wkVaregruppe + " " +
  (if entry(1,Lister.Kriterier[1],"|") = entry(2,Lister.Kriterier[1],"|")
     then wAlle
     else entry(1,Lister.Kriterier[1],"|")).

/* Leverandør. */
{syspara.i 6 wLayout 14 wkLeverand}           
wkLeverand = "LEVERANDOR = " + wkLeverand + " " + 
  (if entry(1,Lister.Kriterier[2],"|") = entry(2,Lister.Kriterier[2],"|")
     then wAlle
     else entry(1,Lister.Kriterier[2],"|")).

/* BestStat. */
{syspara.i 6 wLayout 15 wkBestStat}
wkAktiv = "AKTIV = " + wkAktiv + " " +
          (if Lister.Kriterier[3] = "3"
             then wAlle
             else if Lister.Kriterier[3] = "2"
           then "Passive"
            else "Aktive").

/* Sesong. */
{syspara.i 6 wLayout 16 wkSesong}           
wkSesong = "SESONG = " + wkSesong + " " + 
  (if entry(1,Lister.Kriterier[4],"|") = entry(2,Lister.Kriterier[4],"|")
     then wAlle
     else entry(1,Lister.Kriterier[4],"|")).

/* Farger. */
{syspara.i 6 wLayout 17 wkFarger}           
wkFarger = "FARGER = " + wkFarger + " " + 
  (if entry(1,Lister.Kriterier[5],"|") = entry(2,Lister.Kriterier[5],"|")
     then wAlle
     else entry(1,Lister.Kriterier[5],"|")).

/* Material. */
{syspara.i 6 wLayout 18 wkMaterial}           
wkMaterial = "MATERIAL = " + wkMaterial + " " + 
  (if entry(1,Lister.Kriterier[6],"|") = entry(2,Lister.Kriterier[6],"|")
     then wAlle
     else entry(1,Lister.Kriterier[6],"|")).
END PROCEDURE.

/* Kundelister */
PROCEDURE Hentkriterier3:

END PROCEDURE.

/* Medlemsstatistikk */
PROCEDURE Hentkriterier8:

END PROCEDURE.


/* Dummy */
PROCEDURE DeBugJobb:
  DEF INPUT PARAMETER ipTekst as CHAR NO-UNDO.

  /* MESSAGE ipTekst VIEW-AS ALERT-BOX ERROR TITLE "Feil". */

END PROCEDURE.

