DEFINE VARIABLE ix         AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cReturnMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFraDato      AS DATE      NO-UNDO.
DEFINE VARIABLE dTilDato      AS DATE      NO-UNDO.
DEFINE VARIABLE iTime      AS INTEGER NO-UNDO.
DEFINE VARIABLE cFil AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rHentUNI2Data AS cls.UNI2.HentUNI2Data NO-UNDO.

/* Endringer her skal ikke utløse ny ELogg post og resending av ordre. */    
ON CREATE OF ArtBas OVERRIDE DO: END.
ON WRITE  OF ArtBas OVERRIDE DO: END.
ON CREATE OF ArtBas OVERRIDE DO: END.

{cls\UNI2\tmpTblvArticle_NO.i}
{cls\UNI2\tmpDsvArticle_NO.i}
{cls\UNI2\tmpTblvArticles.i}
{cls\UNI2\tmpDsvArticles.i}
{cls\UNI2\tmpTblSeasons.i}
{cls\UNI2\tmpDsSeasons.i}
{cls\UNI2\tmpTblvSupplier.i}
{cls\UNI2\tmpDsvSupplier.i}
{cls\UNI2\tmpTblregArticles.i}
{cls\UNI2\tmpDsregArticles.i}
{cls\UNI2\tmpTblregArtSKU.i}
{cls\UNI2\tmpDsregArtSKU.i}
{cls\UNI2\tmpTblregEanSKU.i}
{cls\UNI2\tmpDsregEanSKU.i}
{cls\UNI2\tmpTblArtEan.i}
{cls\UNI2\tmpDsArtEan.i}

DEFINE TEMP-TABLE tmpArtSjekk NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    INDEX idxArtikkelNr ArtikkelNr.                       

DEFINE STREAM Ut.

ASSIGN 
    dFraDato = 01/01/2016
    dTilDato = 12/31/2016
    iTime    = TIME 
    cFil  = 'konv\Article\ArtEan191118084348.JSon'
    .

HOVEDBLOKK:
DO  ON ERROR  UNDO, LEAVE
    ON ENDKEY UNDO, LEAVE
    ON STOP   UNDO, LEAVE
    ON QUIT   UNDO, LEAVE:

    ASSIGN 
        cLogg    = 'HentUNI2DataModell' + REPLACE(STRING(TODAY),'/','')
        .
    rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( cLogg ).
    /* Starter med tom linje i loggen. */
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start.' 
        ).    
    /* Starter og kjører klassen. */
    rHentUNI2Data  = NEW cls.UNI2.HentUNI2Data( INPUT cLogg ).

    /* Leser inn data fra fil hvis den finnes. */
    IF SEARCH(cFil) <> ? THEN
    DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start les inn datasett.' 
            ).    

        DATASET dsArtEan:READ-JSON ('file', cFil,'EMPTY').

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Ferdig les inn datasett.' 
            ).    
    END.
    
    IF NOT CAN-FIND(FIRST tmpArtEan) THEN 
    HENTFRASQL:
    DO:
        /* Kobler opp SQL server databasen */
        rHentUNI2Data:oppkoblingSQL( OUTPUT bOk).

        IF bOk THEN 
        DO:    
            rHentUNI2Data:hentvArticlesData( OUTPUT DATASET DsvArticles ).
    
            rHentUNI2Data:hentregArticlesData( OUTPUT DATASET DsregArticles ).
    
            rHentUNI2Data:hentregArtSKUData( OUTPUT DATASET DsregArtSKU ).
    
            rHentUNI2Data:hentregEanSKUData( OUTPUT DATASET dsregEanSKU ).
        END.    

        /* Kobler ned SQL server databasen */
        rHentUNI2Data:nedkoblingSQL( OUTPUT bOk).    

        /* Bygger sjekk tabell for modell */
        IF CAN-FIND(FIRST tmpregArtSKU) AND 
           CAN-FIND(FIRST tmpregEanSku) AND 
           CAN-FIND(FIRST tmpregArticles) THEN
         DO: 
             rHentUNI2Data:byggtmpArtEan (OUTPUT DATASET dsArtEan ).
         END.
    END. /* HENTFRASQL */
    
     /* Sjekker og logger artikler som skal splittes. */
     RUN sjekkArtEan.         
     
END. /* HOVEDBLOKK */

FINALLY.
    /*    IF VALID-OBJECT(rTemp) THEN DELETE OBJECT rTemp NO-ERROR.*/
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Tidsbruk ' + STRING(TIME - iTime,'HH:MM:SS') 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Ferdig.' 
        ).    
END FINALLY.


/* **********************  Internal Procedures  *********************** */

PROCEDURE sjekkArtEan:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE pnArtKey AS INT64 NO-UNDO.
    DEFINE VARIABLE pcArtNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcArtLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE piTotAnt AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE tmpArtSjekk.
    
    IF NOT CAN-FIND(FIRST tmpArtEan) THEN
    DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  SjekkEan - ingen artikler å sjekke.' 
            ).    
        RETURN.
    END.
    
    ASSIGN 
        pcArtLogg = 'konv\sjekkEan' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
        .
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   Start lese ArtBas.' 
        ). 
        
    /* Totalt antall artikler å sjekke */   
    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.RegistrertDato >= dFraDato AND  
        ArtBas.RegistrertDato <= dTilDato:
        piTotAnt = piTotAnt + 1.
    END.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   Antall artikler å lese: ' + STRING(piTotant) + '.' 
        ). 
        

    /* Leser alle artikler og sjekker om artikkelens strekkoder ligger på ulike artikler i UNI2. */
    ARTLOPP:    
    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.RegistrertDato >= dFraDato AND 
        ArtBas.RegistrertDato <= dTilDato:

        piAnt = piAnt + 1.
        IF piAnt MODULO 1000 = 0 THEN  
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '   Lest ' + STRING(piAnt) + ' av ' + STRING(piTotAnt) + '.' 
                ).    

        ASSIGN
            pnArtKey = 0
            pcArtNo  = '' 
            . 
        KODESJEKK:
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE 
            LENGTH(StrekKode.Kode) > 7:
                                
            FIND FIRST tmpArtEan NO-LOCK WHERE 
                tmpArtEan.nEan = DEC(StrekKode.Kode) NO-ERROR.
                
            /* Setter testpunkt. */    
            IF AVAILABLE tmpArtEan THEN 
            DO: 
                IF pnArtKey = 0 AND 
                   pcArtNo = '' THEN 
                ASSIGN
                    pnArtKey = tmpArtEan.nArtKey
                    pcArtNo  = tmpArtEan.cArtNo 
                    . 
            END.
            
            /* Sjekker om det er avvik. */
            IF AVAILABLE tmpArtEan THEN 
            DO:
                IF pnArtKey <> tmpArtEan.nArtKey OR 
                   pcArtNo  <> tmpArtEan.cArtNo THEN 
                DO: 
                    IF NOT CAN-FIND(FIRST tmpArtsjekk WHERE 
                                    tmpArtSjekk.ArtikkelNr = ArtBas.ArtikkelNr) THEN 
                    DO:
                        CREATE tmpArtSjekk.
                        ASSIGN 
                            tmpArtSjekk.ArtikkelNr = ArtBas.ArtikkelNr
                            .
                        END.
                    NEXT KODESJEKK.
                END.
            END.
        END. /* KODESJEKK */ 
    END. /* ARTLOPP */
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   Ferdig lese ArtBas.' 
        ).    
    
    IF NOT CAN-FIND(FIRST tmpArtSjekk) THEN 
    DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '   Ingen artikler med feil funnet.' 
            ).    
        RETURN.
    END.
    
    OUTPUT STREAM Ut TO VALUE(pcArtLogg).
    
    /* Skriver resultatet til loggen */
    FOR EACH tmpArtSjekk,
        FIRST ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = tmpArtSjekk.ArtikkelNr:
        PUT STREAM Ut UNFORMATTED 
            ArtBas.ArtikkelNr ';'
            ArtBas.Beskr ';'
            ArtBas.LevKod ';'
            ArtBas.LevFargKod 
            SKIP.
        FOR EACH StrekKode NO-LOCK WHERE 
            StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND 
            LENGTH(StrekKode.Kode) > 7:
            FIND FIRST tmpArtEan NO-LOCK WHERE 
                tmpArtEan.nEan = DEC(StrekKode.Kode) NO-ERROR.
            IF AVAILABLE tmpArtEan THEN 
                DO:
                    PUT STREAM Ut UNFORMATTED 
                        ';'
                        Strekkode.ArtikkelNr ';'
                        StrekKode.Kode ';'
                        ArtBas.LevKod ';'
                        tmpArtEan.nArtKey ';' 
                        tmpArtEan.cArtNo ';' 
                        tmpArtEan.dPriceFOB ';' 
                        tmpArtEan.dPriceLC ';' 
                        tmpArtEan.dPriceWholesale ';' 
                        tmpArtEan.dPriceRetail ';' 
                        SKIP.
                END.
        END.
    END.
    
    OUTPUT STREAM Ut CLOSE.
    
END PROCEDURE.
