DEF VAR bOk        AS LOG  NO-UNDO.
DEF VAR cEAN       AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR lDec       AS DEC  NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.
DEFINE VARIABLE iAntall AS INTEGER NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

LOOPEN:
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
  ArtBas.ArtikkelNr > 0:
  
  /* Dette er artikler som har mistet sine strekkoder via HK VPI importen. */
  /* De kan ike saneres, men må bare merkes som utgått.                    */
  IF NOT CAN-FIND(FIRST Strekkode OF ArtBas) THEN 
    DO:
      ASSIGN ArtBas.Beskr = 'UTGÅTT: ' + ArtBas.Beskr.
      NEXT LOOPEN.
    END.
  
  IF ArtBas.OPris THEN NEXT.
  IF ArtBas.Pakke THEN NEXT.
  IF ArtBas.Pant  THEN NEXT. 
      
    ASSIGN
      bOk = FALSE
      iAntall = iAntall + 1
      .       
    
    /* TEST TEST */
    IF iAntall > 100 THEN LEAVE LOOPEN.
    
    FOR EACH Strekkode OF ArtBas NO-LOCK WHERE
           length(Strekkode.Kode) = 13 AND
           substring(Strekkode.Kode,1,2) <> '02':
        
        /* Skal ikke ha ugyldige tegn. */
        ASSIGN lDec = dec(Strekkode.Kode) no-error.
        IF ERROR-STATUS:ERROR THEN NEXT.
        
        /* Sjekker sjekksiffer. */
        cEAN = Strekkode.Kode.
        RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
        IF Strekkode.Kode <> cEAN THEN NEXT.
        
        bOk = TRUE.
    END.
                       
    /*if bOk then   TN Allt skal ut. */ 
      DO:
        RAPPORT-LOKALE-ARTIKLER:
        DO:
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "ArtBas"  AND
                 ELogg.EksterntSystem = "TILKORRPOS" AND
                 ELogg.Verdier        = string(ArtBas.ArtikkelNr) NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "ArtBas"
                       ELogg.EksterntSystem = "TILKORRPOS"   
                       ELogg.Verdier        = string(ArtBas.ArtikkelNr)
                       ELogg.ETid           = TIME + 20.
            END.
            ASSIGN ELogg.EndringsType = 1
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END. /* RAPPORT-LOKALE-ARTIKLER */
        
        
        IF piLoop MODULO 100 > 0 THEN 
          DO:
            PAUSE 0 BEFORE-HIDE.
            DISPLAY piLoop WITH FRAME g. 
          END.
        /*
        find LevBas of ArtBas.
        display
          ArtBas.ArtikkelNr
          ArtBas.LevKod
          ArtBas.Beskr
          ArtBas.LevFargKod
          LevBas.LevNamn
          cEAN
          can-find(VPIArtBAs where VPIArtBas.EkstVPILEvNR = 1 and VPIArtBAs.VareNR = string(ArtBAs.ArtikkelNr))
        with width 250.
        */

      END.                    
END. /* LOOPEN */           
