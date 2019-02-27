/* Setter status på VPI korreksjonsposter. */
CURRENT-WINDOW:WIDTH = 350.
                            
DEF VAR iAnt AS INT NO-UNDO.

LOOPEN:
FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE
    VPIArtBas.EkstVPILevNr = 1000016 AND
    VPIArtBas.VAreNr = '162499':

    ASSIGN VPIArtBas.KorrArtikkelNr = 0
           VPIArtBas.KorrStatus     = 1. /* Ubehandlet */

    iAnt = iAnt + 1.
    /*
    IF iAnt MODULO 100 = 0 THEN
        LEAVE LOOPEN.
    */
    IF iAnt MODULO 1000 = 0 THEN
    DO:
        PAUSE 0.
        DISPLAY iAnt.
    END.


    /* Kobling av artikkel */
    /* Finnes EAN koden på en artikkel på hk, skal den kobles med en gang. */
    /* Vi tar ikke med plu artikklene.                                     */
    IF DEC(VPIArtBas.VareNr) > 9999 THEN
    STREKKODESJEKK:
    FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:

      /* Automatisk kobling ved hjelp av strekkode. */
      FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
      
      IF AVAILABLE Strekkode AND 
          Strekkode.ArtikkelNr <> DECIMAL(VPIArtBas.VareNr) THEN 
      DO:
        ASSIGN VPIArtBas.KorrArtikkelNr = Strekkode.ArtikkelNr
               VPIArtBas.KorrStatus     = 11. /* Automatisk koblet */
        LEAVE STREKKODESJEKK.
      END.

      ELSE IF AVAILABLE Strekkode AND 
          Strekkode.ArtikkelNr = DECIMAL(VPIArtBas.VareNr) and
          Strekkode.ArtikkelNr < 8500000 THEN 
      DO:
        ASSIGN VPIArtBas.KorrArtikkelNr = Strekkode.ArtikkelNr
               VPIArtBas.KorrStatus     = 21. /* Automatisk opprettet */
        LEAVE STREKKODESJEKK.
      END.
      
      ELSE IF AVAILABLE Strekkode AND 
          Strekkode.ArtikkelNr = DECIMAL(VPIArtBas.VareNr) and
          Strekkode.ArtikkelNr >= 8500000 THEN 
      DO:
        ASSIGN VPIArtBas.KorrArtikkelNr = Strekkode.ArtikkelNr
               VPIArtBas.KorrStatus     = 11. /* Automatisk koblet */
        LEAVE STREKKODESJEKK.
      END.

      ELSE 
          ASSIGN VPIArtBas.KorrArtikkelNr = 0
                 VPIArtBas.KorrStatus     = 1. /* Ubehandlet */

      /* Automatisk kobling ved hjelp av artikkelnr */
      IF VPIArtBas.KorrArtikkelNr = 0 THEN
      DO:
          IF AVAILABLE ArtBAs THEN RELEASE ArtBas.
          FIND ArtBas WHERE
              ArtBas.ArtikkelNr = dec(VPIArtBas.VareNr) NO-ERROR.



      END.
    END. /* STREKKODESJEKK */

    
    /* Kobling ferdig */
    IF VPIArtBas.KorrArtikkelNr > 0 THEN
        FIND ArtBas WHERE
            ArtBas.ArtikkelNr = KorrArtikkelNr NO-ERROR.

    DISPLAY 
        VPIArtBas.VareNr
        VPIArtBas.ArtikkelNr
        VPIArtBas.LevKod
        VPIArtBAs.Beskr
        VPIArtBAs.LEvFargKod
        VPIArtBas.KorrStatus
        VPIArtBAs.KorrArtikkelNr
        CAN-FIND(ArtBas WHERE
                 ArtBAs.ArtikkelNr = dec(VPIArtBAs.VareNr))
        Strekkode.ArtikkelNr WHEN AVAILABLE Strekkode
        ArtBas.LevKod WHEN AVAILABLE ArtBas
        ArtBAs.Beskr WHEN AVAILABLE ArtBas
        ArtBAs.LEvFargKod WHEN AVAILABLE ArtBas
        WITH WIDTH 350.
    

END.
