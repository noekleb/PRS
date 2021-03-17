/************************************************************
    Program:  byggtmpliste.i
    Created:  TN    4 May 100
Description:

Last change:  TN    4 May 100   11:30 am
************************************************************/

  def var wCl         as int  no-undo.
  def var wFilNavn    as char no-undo.
  def var wAntall     as int no-undo.
  def var wRGB        as char no-undo.
  def var wSort       as int no-undo.

  assign frame DEFAULT-FRAME
    CB-Sortering
    wSort = lookup(CB-Sortering,CB-Sortering:List-items).

  /* Sentrallager. */
  {syspara.i 5 1 1 wCl int}
  find Butiker no-lock where
    butiker.butik = wCl no-error.   
  if not available Butiker then
    do:
      message "Sentrallager er ikke satt opp i systemet!"
        view-as alert-box error title "Feil".
      return.
    end.
    
  /* Nullstiller temp-liste */
  for each tBild: delete tBild. end.

  assign
    wCellNr = 0
    wAntall = 0.

  /* Teller opp listen først for å kunne sette maks antall for progress bar. */
  TELL_OPP_LISTE:
  for each ListeLinje no-lock where
    ListeLinje.ListeType  = Lister.ListeType and
    ListeLinje.ListeNr    = Lister.ListeNr
    by ListeLinje.ListeType
    by ListeLinje.ListeNr
    by ListeLinje.CellNr:
    assign
      wAntall = wAntall + 1
      wCellNR = if ListeLinje.CellNr <> ?
                  then ListeLinje.CellNr
                  else wCellNr.
  end. /* TELL_OPP_LISTE */

  if wAntall < 5 then wAntall = 0. /* For få poster */
  if wAntall > 0 then
    do:
      assign
        chCtrlFrame:ProgressBar:Min   = 1
        chCtrlFrame:ProgressBar:Max   = wAntall
        chCtrlFrame:ProgressBar:Value = 1.
      view frame FRAME-Progress.
    end.


  assign wAntall = 0.
  {rbyggartikkel.i
  &BySats = "by (if wSort = 1 then ListeLinje.DivX[1]
                 else if wSort =  2 then ListeLinje.DivX[ 2]
                 else if wSort =  3 then ListeLinje.DivX[ 3]
                 else if wSort =  4 then ListeLinje.DivX[ 4]
                 else if wSort =  5 then ListeLinje.DivX[ 5]
                 else if wSort =  7 then ListeLinje.DivX[ 7]
                 else if wSort =  8 then ListeLinje.DivX[ 8]
                 else if wSort =  9 then ListeLinje.DivX[ 9]
                 else if wSort = 10 then ListeLinje.DivX[10]
                 else if wSort = 11 then ListeLinje.DivX[11]
                 else if wSort = 12 then ListeLinje.DivX[12]
                 else ListeLinje.DivX[6]
                )"}

  FI-Info = " ".
  display FI-Info with frame {&FRAME-NAME}.
  chCtrlFrame:ProgressBar:Value = 1.
  hide frame FRAME-Progress no-pause.


/*
/* TEST */
OUTPUT TO gurre.dat.
  EXPORT DELIMITER ";"
      "CellNr"
      "FArge"
      "VgKat"
      "ArtikkelNr"
      "BildTxt"
      "LevNr"
      "LevArtNr"
      "LevTid"
      "BestNr"
      "OrdreNr"
      "VgLopNr"
      "ValutaPris"
      "InnkjPris"
      "VareKost"
      "Bild"
      "ListeLinje"
      skip.

FOR EACH tBild NO-LOCK:
  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = tBild.ArtikkelNr.
  EXPORT DELIMITER ";"
      tBild.CellNr
      ArtBas.Farg
      ArtBas.VgKat
      tBild.ArtikkelNr
      tBild.BildTxt
      tBild.LevNr
      tBild.LevArtNr
      tBild.LevTid
      tBild.BestNr
      tBild.OrdreNr
      tBild.VgLopNr
      tBild.ValutaPris
      tBild.InnkjPris
      tBild.VareKost
      tBild.Bild
      tBild.ListeLinje
      .
END.
OUTPUT CLOSE.
*/
