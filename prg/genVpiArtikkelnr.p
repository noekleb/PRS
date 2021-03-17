DEFINE INPUT-OUTPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER              NO-UNDO.
DEFINE VARIABLE cVpiNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE ArtBas.ArtikkelNr NO-UNDO.  
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE oldArtikkelNr AS DEC NO-UNDO.

{syspara.i 1 1 18 strLokaltHk}
IF CAN-DO("Ja,Yes,True,1",strLokaltHk) THEN lLokaltHk = true. 

/* Hovedkontor/kjedekontor */
HOVEDKONTOR:
DO:
    /* Det skal aldri ligge mer enn en serie */
    {syspara.i 1 1 26 cVpiNumSerier}

    /* TN 17/6-08. forenklet logikken her. Det skal bare ligge en entry. Det blir for */
    /* komplisert å forholde seg til flere fulle nummerserier.                        */
    HKLOOP: 
    DO iCount = 1 TO 1:
        ASSIGN 
            dFraNr      = DECI(ENTRY(1,ENTRY(iCount,cVpiNumSerier),"-"))
            dTilNr      = DECI(ENTRY(2,ENTRY(iCount,cVpiNumSerier),"-"))
            .
            
        /* Sjekker Artikkelsekvensen */
        SEKVENS:
        DO:
          /* Henter neste artikkelNr. */
          dArtikkelNr = NEXT-VALUE ( ArtikkelNr , skotex ).
          
          /* Ligger artikkelnr utenfor aktuell range, skal sekvens initierers pånytt. */
          if dArtikkelNr < dFraNr or dArtikkelNr > dTilNr then
            DO:
              CURRENT-VALUE ( ArtikkelNr, skotex) = int(dFraNr).
              dArtikkelNr = NEXT-VALUE ( ArtikkelNr , skotex ).
            END.
  
          /* LOOPER til vi finner et ledig nummer */
          EVIGLOOP:
          DO while true:
            IF CAN-FIND(ArtBas WHERE
                        ArtBas.ArtikkelNr = dArtikkelNr) OR 
               CAN-FIND(first VPIArtBas WHERE
                        VPIArtBas.ArtikkelNr = dArtikkelNr) OR 
               CAN-FIND(ArtikkelNrSerie where
                        ArtikkelNrSerie.ArtikkelNr = dArtikkelNr) THEN
            DO:
              dArtikkelNr = NEXT-VALUE ( ArtikkelNr , skotex ).
              if dArtikkelNr > dTilNr then
                DO:
                  dArtikkelNr = 0.
                  leave SEKVENS.
                END.
            end.
            ELSE DO TRANSACTION:
                create ArtikkelNrSerie.
                assign
                    ArtikkelNrSerie.ArtikkelNr = dArtikkelNr.
                leave EVIGLOOP.
            END. /* TRANSACTION */
          END. /* EVIGLOOP */          
        END. /* SEKVENS */
        
        /* ---------------------------------------------------------------------------------------
        /* I dArtikkelNr ligger forrige benyttede artikkelnr.                                    */
        /* Vi trekker opp nedre grense på rangen, slik at ikke dette nummeret blir brukt pånytt. */
        IF (dArtikkelNr > dFraNr AND dArtikkelNr < dTilNr) THEN
            dFraNr = dArtikkelNr.

        /* Sjekker VPIArtBas */
        FIND LAST VPIArtBas WHERE 
              VpiArtBas.ArtikkelNr >= dFraNr AND
              VpiArtBas.ArtikkelNr <= dTilNr USE-INDEX ArtikkelNr NO-LOCK NO-ERROR.

        IF NOT AVAIL VpiArtBas OR 
           (AVAIL VpiArtBas AND VpiArtBas.ArtikkelNr < dTilNr) THEN 
        DO:
            ASSIGN dArtikkelNr = IF AVAIL VpiArtBas
                                   THEN VpiArtBas.ArtikkelNr + 1 
                                   ELSE dFraNr.
        END.
        ELSE DO: 
            dArtikkelNr = 0.
            /* Nummerserie er brukt opp */
            LEAVE HKLOOP.
        END.

        IF dArtikkelNr > dTilNr THEN
        DO:
            dArtikkelNr = 0.
            LEAVE HKLOOP. /* Nummerserie er full, bruk neste */
        END.

        /* Sikrer at nummeret er ledig i ArtBas. */
        LOOPEN:
        DO WHILE TRUE:
            IF CAN-FIND(ArtBas WHERE
                        ArtBas.ArtikkelNr = dArtikkelNr) OR 
               CAN-FIND(ArtikkelNrSerie where
                        ArtikkelNrSerie.ArtikkelNr = dArtikkelNr) THEN
            DO:
                dArtikkelNr = dArtikkelNr + 1.
                IF dArtikkelNr > dTilNr THEN
                DO:
                    dArtikkelNr = 0.
                    LEAVE HKLOOP. /* Nummerserie er full, bruk neste */
                END.
                ELSE NEXT LOOPEN.
            END.
            /* TN 8/6-09 Logge at nummer er brukt og avslutte */
            ELSE DO for ArtikkelNrSerie TRANSACTION:            
                create ArtikkelNrSerie.
                assign
                    ArtikkelNrSerie.ArtikkelNr = dArtikkelNr no-error.
                if ERROR-STATUS:ERROR THEN 
                DO:
                    dArtikkelNr = dArtikkelNr + 1.
                    IF dArtikkelNr > dTilNr THEN
                    DO:
                        dArtikkelNr = 0.
                        LEAVE HKLOOP.  
                    END.
                    ELSE NEXT LOOPEN.
                END.
                ELSE do:
                  RELEASE ArtikkelNrSerie.
                  LEAVE HKLOOP.
                END.
            END. /* TRANSACTION */
        END. /* LOOPEN */
        -----------------------------------------------------------------------------------------*/
    END. /* HKLOOP */    
END. /* HOVEDKONTOR */

/* Returnerer  dArtikkelNr = 0 hvis nummerserie er full. */
