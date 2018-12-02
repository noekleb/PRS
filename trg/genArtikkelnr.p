DEFINE OUTPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER              NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE ArtBas.ArtikkelNr NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* artikkelnr mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 23 cHkNumSerier}
   
/* TN 11/6-09 Create trigger logger artikkelnr i ArtikkelNr serie.
   Ved opprettelse av artikler på HK, benyttes en annen artikkelnrserie.
   Det er derfor ikke nødvendig å legge inn opprettelse av
   poster i ArtikkelNrSerie her.
*/   
   
IF lLokaltHK THEN 
HKLOOP: 
DO:
    NUMMERLOOP:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
      HKBLOKK:
      DO:     
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST ArtBas WHERE ArtBas.ArtikkelNr >= dFraNr AND
                               ArtBas.ArtikkelNr <= dTilNr 
                               USE-INDEX Artikkelnr NO-LOCK NO-ERROR.
        FIND LAST VPIArtBas WHERE VPIArtBas.ArtikkelNr >= dFraNr AND
                                  VPIArtBas.ArtikkelNr <= dTilNr 
                                  USE-INDEX Artikkelnr NO-LOCK NO-ERROR.

        /* Det ligger ingen artikler i noen av tabellene. */
        IF NOT AVAILABLE ArtBas AND NOT AVAILABLE VPIArtBas THEN 
        DO:
            ASSIGN dArtikkelNr = dFraNr.
            LEAVE HKBLOKK.
        END.
        
        /* Det ligger artikler i begge tabeller. */
        IF AVAILABLE ArtBas AND AVAILABLE VPIArtBas THEN 
        DO:
            /* ArtBas har siste artikkelnr */
            IF ArtBas.ArtikkelNr >= VPIArtBas.ArtikkelNr THEN 
            DO:
                ASSIGN dArtikkelNr = ArtBas.ArtikkelNr + 1.
                LEAVE HKBLOKK.
            END.
            /* VPIArtBas har siste artikkelnr */
            ELSE DO:
                ASSIGN dArtikkelNr = VPIArtBas.ArtikkelNr + 1.
                LEAVE HKBLOKK.
            END.
        END.
        
        /* Det ligger artikler bare i ArtBas */
        IF AVAILABLE ArtBas THEN 
        DO:
            ASSIGN dArtikkelNr = ArtBas.ArtikkelNr + 1.
            LEAVE HKBLOKK.
        END.
        
        /* Det ligger artikler bare i VPIArtBas */
        IF AVAILABLE VPIArtBas THEN 
        DO:
            ASSIGN dArtikkelNr = VPIArtBas.ArtikkelNr + 1.
            LEAVE HKBLOKK.
        END.
      END. /* HKBLOKK */
      
      /* Tildelt nummer innenfor nummerserie. */
      IF dArtikkelNr <= dTilNr THEN 
        LEAVE NUMMERLOOP.
      ELSE dArtikkelNr = 0.
        
      /* Er vi på siste nummerserie og den er full. Gis melding */
      IF iCount = NUM-ENTRIES(cHkNumSerier) THEN
      DO:
          MESSAGE "Artikkelnummerserie er full. Kontakt systemansvarlig." AVAILABLE ArtBas ArtBas.ArtikkelNr dArtikkelNr SKIP
              "HK nummeriserier:" cHkNumSerier
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          dArtikkelNr = 0.
          LEAVE NUMMERLOOP.
      END.
    END. /* NUMMERLOOP */
END.
ELSE DO:
    BUTIKKLOOP: 
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik > 0: /* Skal ikke hente nummerserie fra kommisjonsbutikker. */
        ASSIGN dFraNr = Butiker.Butik * 10000 + 1
               dTilNr = Butiker.Butik * 10000 + 9999.
        FIND LAST ArtBas WHERE 
            ArtBas.ArtikkelNr >= dFraNr AND
            ArtBas.ArtikkelNr <= dTilNr USE-INDEX Artikkelnr 
            NO-LOCK NO-ERROR.

        IF NOT AVAIL ArtBas OR 
           (AVAIL ArtBas AND ArtBas.ArtikkelNr < dTilNr) THEN 
        LOOPEN:
        DO:
            ASSIGN 
                dArtikkelNr = IF AVAIL ArtBas 
                                THEN ArtBas.ArtikkelNr + 1 
                                ELSE dFraNr.
            /* Nummer er i bruk i VPI registeret. */
            IF CAN-FIND(FIRST VPIArtBas WHERE
                        VPIArtBas.ArtikkelNr = dArtikkelNr) THEN
                LEAVE LOOPEN.
            /* OK - Ledig nummer. */
            ELSE LEAVE BUTIKKLOOP.
        END. /* LOOPEN */
    END. /* BUTIKKLOOP */
END.


