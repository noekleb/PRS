/*CONNECT -pf q:\db\sport1\clskotex.pf -ld Sp1Skotex.*/
/*CONNECT -pf q:\db\sport1\clvpi.pf -ld Sp1VPI. */
                                            
CURRENT-WINDOW:WIDTH = 250.

/* Kobler ut databasetrigger på VPI artbas (HK databasen). */
ON WRITE OF Sp1VPI.VPIArtBas OVERRIDE DO: END.
ON WRITE OF Sp1SkoTex.Vareboklinje OVERRIDE DO: END.
ON CREATE OF Sp1SkoTex.Vareboklinje OVERRIDE DO: END.

DEF BUFFER bufVareBokHode FOR SkoTex.VareBokHode.
DEF BUFFER bufArtBas      FOR Sp1SkoTex.ArtBas.

FIND Skotex.Varebokhode NO-LOCK WHERE
    SkoTex.Varebokhode.VarebokNr = 7000000004.
FOR EACH SkoTex.VarebokLinje OF SkoTex.VareBokHode NO-LOCK /*WHERE
    SkoTex.VareBokLinje.Beskr BEGINS 'ciero'*/:

    FIND SkoTex.LevBas OF SkoTex.VareBokLinje.
    FIND FIRST Sp1SkoTex.EkstVPILev NO-LOCK WHERE
        Sp1SkoTex.EkstVPILev.KortNavn BEGINS 'XON' AND
        Sp1SkoTex.EkstVPILev.LevNr = SkoTex.VareBokLinje.LevNr NO-ERROR.

    FIND Sp1SkoTex.ArtBas WHERE
        Sp1SkoTex.ArtBas.ArtikkelNr = SkoTex.VareBokLinje.ArtikkelNr NO-ERROR.

    /* Hvis artikkelen ikke ligger i ArtBas i HK databasen, lister vi linjene opp på skjermen. */
    IF NOT AVAILABLE Sp1SkoTex.ArtBas THEN 
    DO:
        IF AVAILABLE Sp1SkoTex.EkstVPILev THEN
            FIND Sp1VPI.VPIArtBas EXCLUSIVE-LOCK WHERE
                 Sp1VPI.VPIArtBas.EkstVPILevNr = Sp1SkoTex.EkstVPILEv.EkstVPILevNr AND
                 Sp1VPI.VPIArtBas.VareNr       = string(SkoTex.VareBokLinje.ArtikkelNr) NO-ERROR.

        /*
        /* Markerer de XON Sport artiklene som skal inn i XON sport varebok med dagens dato som VPI dato. */
        IF AVAILABLE Sp1VPI.VPIArtBas THEN
            Sp1VPI.VPIArtBas.VPIDato = TODAY.
        */
        DISPLAY
            SkoTex.VareBokLinje.ArtikkelNr
            SkoTex.VareBokLinje.LevKod
            SkoTex.VareBokLinje.Beskr
            SkoTex.VareBokLinje.LevFargKod
            SkoTex.VareBokLinje.LevNr
            SkoTex.LevBAs.LevNamn
            '+' WHEN SkoTex.VareBokLinje.LevNr <> 1
         Sp1SkoTex.EkstVPILev.EkstVPILevNr WHEN AVAILABLE Sp1SkoTex.EkstVPILev
         Sp1SkoTex.EkstVPILev.KortNavn WHEN AVAILABLE Sp1SkoTex.EkstVPILev
         Sp1VPI.VPIArtBas.VareNr WHEN AVAILABLE Sp1VPI.VPIArtBas
            '*' WHEN AVAILABLE Sp1SkoTex.ArtBas
        WITH WIDTH 250.
    END.

    /* Kopierer vareboklinjer */
    ELSE DO:
      FIND Sp1SkoTex.VareBokLinje EXCLUSIVE-LOCK WHERE
          Sp1SkoTex.VareBokLinje.VareBokNr = 9000011 AND
          Sp1SkoTex.VarebokLinje.ArtikkelNr = SkoTex.VareBokLinje.ArtikkelNr NO-ERROR.
      /*
      DISPLAY Sp1SkoTex.VareBokLinje.ArtikkelNr
          WITH WIDTH 250.
      */
      
      
      IF NOT AVAILABLE Sp1SkoTex.VareBokLinje THEN
      DO:
          CREATE Sp1SkoTex.VareBokLinje.
          ASSIGN
              Sp1SkoTex.VareBokLinje.VareBokNr = 9000011 
              Sp1SkoTex.VareBokLinje.ArtikkelNr = SkoTex.VareBokLinje.ArtikkelNr.
      END.
      BUFFER-COPY SkoTex.VareBokLinje 
          EXCEPT VareBokNr ArtikkelNr
          TO Sp1SkoTex.VareBokLinje.
      
    END.
END.


