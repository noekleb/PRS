/* fix-vareslag.p */

PUBLISH 'infoDisp' ("Initierer VPI informasjon..").

  /* Frisker opp VPI informasjonen i artikelkortet fra VPI registeret. */
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
      ArtBas.ArtikkelNr >= 8500000:

      FIND VPIArtBas NO-LOCK WHERE
          VPIArtBas.EkstVPILevNr = 1 AND
          VPIArtBas.VareNr       = STRING(ArtBas.ArtikkelNr) NO-ERROR.
      IF AVAILABLE VPIArtBas THEN
      DO:
          ASSIGN
              ArtBas.AnbefaltPris = VPIArtBas.AnbefaltPris
              ArtBas.KatalogPris  = VPIArtBas.KatalogPris[1]
              ArtBas.forhRab%     = VPIArtBas.forhRab%[1]
              ArtBas.supRab%      = VPIArtBas.suppRab%[1]
              ArtBas.VPIDato      = VPIArtBas.VPIDato
              .
      END.
  END.
PUBLISH 'infoDisp' ("..").


