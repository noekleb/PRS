CURRENT-WINDOW:WIDTH = 250.

DEF BUFFER bArtPris FOR ArtPris.

FOR EACH ArtBas WHERE
    ArtBas.ArtSlag = 1 AND
    ArtBas.ArtikkelNr <= 999999:

    ArtBas.Vekt = TRUE.

    DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.LevKod
        ArtBas.Beskr
        ArtBas.Vekt
        ArtBas.ArtSlag
        WITH WIDTH 250.
    
    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK WHERE
      Butiker.ProfilNr > 1 AND
      Butiker.harButikksystem = TRUE AND
      Butiker.ApningsDato <= TODAY AND
      Butiker.NedlagtDato  = ?:
    
      /* ------------------ */

      ELOGG_ARTPRIS:
      DO WHILE TRUE:
        FIND ELogg EXCLUSIVE-LOCK WHERE 
           ELogg.TabellNavn     = "ArtPris" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) + CHR(1) + string(Butiker.ProfilNr) NO-ERROR NO-WAIT.
        IF LOCKED ELogg THEN 
          NEXT ELOGG_ARTPRIS.
        IF NOT AVAIL Elogg THEN DO:
          CREATE Elogg.
          ASSIGN ELogg.TabellNavn     = "ArtPris"
                 ELogg.EksterntSystem = "POS"   
                 ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) + CHR(1) + string(Butiker.ProfilNr) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          DO:
            DELETE ELogg.
            NEXT ELOGG_ARTPRIS.
          END.
        END.

        IF ELogg.EndringsType <> 2 OR ELogg.Behandlet = TRUE THEN 
        DO:
          ASSIGN ELogg.EndringsType = 1
                 ELogg.Behandlet    = FALSE NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          NEXT ELOGG_ARTPRIS.
        END.  

        IF AVAILABLE ELogg THEN 
          RELEASE ELogg.

        LEAVE ELOGG_ARTPRIS.
      END. /* ELOGG_ARTPRIS */
      /* ------------------ */

    END. /* BUTIKKLOOP */
END.
