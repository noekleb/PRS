/************************************************************
    Program:  rbyggartikkel.i
    Created:  TN    3 May 100
Description:

Last change:  TN    8 Nov 100   11:04 pm
************************************************************/

  /* Bygger temp listen */
  BYGG_LISTE:
  for each ListeLinje no-lock where
    ListeLinje.ListeType  = Lister.ListeType and
    ListeLinje.ListeNr    = Lister.ListeNr
    {&bySats}:
    
    find ArtBas no-lock where
      artBas.ArtikkelNr = dec(entry(1,ListeLinje.DataObjekt)) no-error.
    if not available ArtBAs then
      next BYGG_LISTE.
    find ArtPris no-lock where
      ArtPris.ArtikkelNr = dec(entry(1,ListeLinje.DataObjekt)) and
      ArtPris.ProfilNr   = Butiker.ProfilNr no-error.      

    /* Henter bilde. Finnes ikke bilde, benyttes blankt bilde. */
    FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
    if VALID-HANDLE(wLibHandle) then
      RUN HentBildePeker in wLibHandle
                         (input BildeRegister.BildNr,
                          INPUT 1,
                           (if available Bilderegister
                            then BildeRegister.Filnavn
                            else ""),
                          OUTPUT wFilNavn).

    /* Oppretter og initierer record. */
    create tBild.    
    assign
      wAntall          = wAntall + 1
      tBild.CellNr     = wAntall - 1 /*ListeLinje.CellNr  */
      tBild.ArtikkelNr = dec(entry(1,ListeLinje.DataObjekt))      
      tBild.BildTxt    = string(ArtBas.Vg) + "/" +
                           (if ArtBas.LopNr <> ?
                             then string(ArtBas.LopNr)
                             else "?") + " " + 
                         string(ArtBas.LevNr) + "/" + 
                         string(ArtBas.LevKod)                          
      tBild.LevNr      = ArtBas.LevNr
      tBild.LevArtNr   = ArtBas.LevKod
      tBild.LevTid     = ""
      tBild.BestNr     = 0
      tBild.OrdreNr    = 0
      tBild.VgLopNr    = string(ArtBas.Vg) + "/" + 
                           (if ArtBas.LopNr <> ? 
                               then string(ArtBas.LopNr)
                               else "")
      tBild.LopNr      = ArtBas.LopNr
      tBild.ValutaPris = if available ArtPris
                           then ArtPris.ValPris[if ArtPris.Tilbud then 2 else 1]
                           ELSE 0
      tBild.InnkjPris  = if available ArtPris
                           then ArtPris.InnkjopsPris[if ArtPris.Tilbud then 2 else 1]
                           ELSE 0
      tBild.VareKost   = if available ArtPris
                           then ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1]
                           ELSE 0
      tBild.Bild       = wFilNavn
      tBild.ListeLinje = recid(ListeLinje)
      .
    RUN TekstLinje IN THIS-PROCEDURE (1) NO-ERROR.

    FIND BestHode NO-LOCK where
      BestHode.BestNr = INT(ENTRY(2,ListeLinje.DataObjekt)) NO-ERROR.
    if available BestHode then
      do:
        /* Setter tekstlinjen */
        RUN TekstLinje IN THIS-PROCEDURE (2) NO-ERROR.

        /* Henter fargekode for bestillingsstatus. */
        IF BestHode.BestStat = 4 AND BestHode.bekreftetdato <> ? THEN
            {syspar2.i 5 2 8 wRGB}
        ELSE
            {syspar2.i 5 2 BestHode.BestStat wRGB}
        assign
          tBild.BestNr    = BestHode.BestNr
          tBild.OrdreNr   = BestHode.OrdreNr
          tBild.Farg      = Hex2Int(wRGB)
          .
      end.
    ELSE 
        /* Setter tekstlinjen */
        RUN TekstLinje IN THIS-PROCEDURE (2) NO-ERROR.

    if wAntall > 0 then
      do:
        if wAntall modulo 5 = 0 then
          do:  
            FI-Info = "Bygger temporær liste for redigering... (" + 
                      string(ListeLinje.CellNr) + ").".
            display FI-Info with frame {&FRAME-NAME}.
            chCtrlFrame:ProgressBar:Value = wAntall.
          end.
      end.


    /*
    /* TEST */
    OUTPUT to test.dat append.
    EXPORT DELIMITER ";"
    tbild.cellnr
    (     if wSort =  1 then ListeLinje.DivX[ 1]
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
     else                    ListeLinje.DivX[ 6])
     wSort.
    OUTPUT CLOSE.
    */

  end. /* BYGG_LISTE */

