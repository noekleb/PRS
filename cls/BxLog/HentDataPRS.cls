 
 /*------------------------------------------------------------------------
    File        : HentDataPRS
    Purpose     : 
    Syntax      : 
    Description : Henter registerdata fra PRS databasen.
    Author(s)   : tny
    Created     : Mon Nov 06 16:56:58 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS cls.BxLog.HentDataPRS: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
        
    DEFINE VARIABLE bTest    AS LOG NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBxProductLog AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAntallDager AS INTEGER NO-UNDO.
    DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTimeLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTime    AS CHARACTER NO-UNDO.
        
    {cls\BxLog\tempTabeller.i}    
    {cls\BxLog\tempDatasett.i}       
        
    CONSTRUCTOR PUBLIC HentDataPRS ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            bTest            = TRUE    
            cLogg            = pcLogg
            cDatoTid         = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            cBxProductLog    = 'konv\BxProductLog' + REPLACE(STRING(TODAY),'/','') + '.JSon'
            iAntallDager     = 0 /* Bare dagens endringer */
            iButNr           = 1
            cTimeLst         = '23,01'
            .
                
        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose: Oppdatere nye og endrede produkter mot SQL basen.
     Notes: 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL initBxProduct( OUTPUT DATASET dsBxProduct,
                                         OUTPUT DATASET dsBxUnit ):

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'HentDataPRS:initBxProduct Start. TEST-1' 
            ).    
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE piMax AS INTEGER NO-UNDO.
        DEFINE VARIABLE piantall AS INTEGER NO-UNDO.

        EMPTY TEMP-TABLE tmpBxProduct.
        EMPTY TEMP-TABLE tmpBxProductLog.
        EMPTY TEMP-TABLE tmpBxUnit.
        
        /* TEST */
        IF bTest THEN  
            piMax = 0.
        
        /* Leser inn logg.                                                              */
        /* Benytter produktlogg for å unngå å sende over artikler som er sendt fra før. */
        IF SEARCH(cBxProductLog) <> ? THEN
        DO:
            DATASET dsBxProductLog:READ-JSON ('file', cBxProductLog,'EMPTY').
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Loggfil funnet og innlest: ' + cBxProductLog + '.' 
                ).    
        END.
        ELSE 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Ingen loggfil funnet: ' + cBxProductLog + '.' 
                ).    

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'HentDataPRS:initBxProduct. TEST-2' 
            ).    
        
        /* Avslutter i tidsrommet det ikke får kjøres. */
        cTime = TRIM(ENTRY(1,STRING(TIME,"HH:MM"),':')).
        IF CAN-DO(cTimeLst,cTime) THEN 
            iAntallDager = 12000.
        ELSE 
            iAntallDager = 0.
        
        INIT_LOOP:
        FOR EACH ArtBas NO-LOCK WHERE 
            ArtBas.EDato >= TODAY - iAntallDager,
            FIRST ArtPris OF ArtBas NO-LOCK, 
            EACH Strekkode OF ArtBas NO-LOCK:
                
            /* Er artikkelen oppdatert tidligere, skal den ikke tas med nå. */
            IF CAN-FIND(FIRST tmpBxProductLog WHERE 
                        tmpBxProductLog.Warehouseno = (IF iButNr > 0 THEN STRING(iButNr) 
                                                       ELSE '') AND 
                        tmpBxProductLog.Productno   = Strekkode.Kode) THEN 
                NEXT INIT_LOOP.    
                
            FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
            
            IF Strekkode.Kode = '' OR Strekkode.Kode = ? THEN 
                NEXT INIT_LOOP.
            IF ArtBas.Vg = ? OR ArtBas.LopNr = ? OR ArtBas.Beskr = '' OR ArtBas.Beskr = '' OR ArtBas.LevKod = '' OR ArtBas.LevKod = ? THEN 
                NEXT INIT_LOOP. 
            IF ArtPris.Pris[1] = ? OR ArtPris.Varekost[1] = ? THEN
                NEXT INIT_LOOP. 
                
            CREATE tmpBxProduct.
            ASSIGN 
                tmpBxProduct.Warehouse   = IF iButNr > 0 THEN STRING(iButNr) ELSE ''
                tmpBxProduct.Productno   = Strekkode.Kode
                tmpBxProduct.ProductName = STRING(ArtBas.Vg) + '/' + 
                                           STRING(ArtBas.LopNr) + ' ' + 
                                           (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '  ') + 
                                           ' (' + STRING(RANDOM(1,50)) + '-' + STRING(RANDOM(1,50)) + ')' 
                tmpBxProduct.GTIN        = Strekkode.Bestillingsnummer
                tmpBxProduct.Price       = ArtPris.Pris[1]
                tmpBxProduct.Cost        = ArtPris.VareKost[1]
                tmpBxProduct.Location    = TRIM(ArtBas.LevKod)
                tmpBxProduct.Unit        = (IF Strekkode.StrKode = ? THEN 1 ELSE Strekkode.StrKode)
                tmpBxProduct.UnitName    = (IF (AVAILABLE StrKonv AND StrKonv.Storl <> ?) THEN StrKonv.Storl ELSE '')
                piAntall                 = piAntall + 1
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                IF AVAILABLE tmpBxProduct AND 
                    tmpBxProduct.Id = 0 THEN 
                    DELETE tmpBxProduct.
                
            END.

/*            /* Oppretter Unit for artikkelen. */                                                                           */
/*            IF AVAILABLE Strekkode THEN                                                                                    */
/*            DO:                                                                                                            */
/*                IF  Strekkode.StrKode = ? THEN                                                                             */
/*                DO:                                                                                                        */
/*                    FIND FIRST tmpBxUnit WHERE                                                                             */
/*                        tmpBxUnit.Productno = Strekkode.Kode AND                                                           */
/*                        tmpBxUnit.Unit      = 1 NO-ERROR.                                                                  */
/*                    IF NOT AVAILABLE tmpBxUnit THEN                                                                        */
/*                    DO:                                                                                                    */
/*                        CREATE tmpBxUnit.                                                                                  */
/*                        ASSIGN                                                                                             */
/*                            tmpBxUnit.Productno = Strekkode.Kode                                                           */
/*                            tmpBxUnit.Unit      = 1                                                                        */
/*                            tmpBxUnit.GTIN      = Strekkode.Bestillingsnummer                                              */
/*                            tmpBxUnit.Price     = (IF ArtPris.Pris[1] <> ? THEN ArtPris.Pris[1] ELSE 0)                    */
/*                            tmpBxUnit.UnitName  = (IF AVAILABLE StrKonv  AND StrKonv.Storl <> ? THEN StrKonv.Storl ELSE '')*/
/*                    NO-ERROR.                                                                                              */
/*                    END.                                                                                                   */
/*                END.                                                                                                       */
/*                ELSE DO:                                                                                                   */
/*                    FIND FIRST tmpBxUnit WHERE                                                                             */
/*                        tmpBxUnit.Productno = Strekkode.Kode AND                                                           */
/*                        tmpBxUnit.Unit      = Strekkode.StrKode NO-ERROR.                                                  */
/*                    IF NOT AVAILABLE tmpBxUnit THEN                                                                        */
/*                    DO:                                                                                                    */
/*                        CREATE tmpBxUnit.                                                                                  */
/*                        ASSIGN                                                                                             */
/*                            tmpBxUnit.Productno = Strekkode.Kode                                                           */
/*                            tmpBxUnit.Unit      = Strekkode.StrKode                                                        */
/*                            tmpBxUnit.GTIN      = Strekkode.Bestillingsnummer                                              */
/*                            tmpBxUnit.Price     = (IF ArtPris.Pris[1] <> ? THEN ArtPris.Pris[1] ELSE 0)                    */
/*                            tmpBxUnit.UnitName  = (IF AVAILABLE StrKonv AND StrKonv.Storl <> ? THEN StrKonv.Storl ELSE '') */
/*                    NO-ERROR.                                                                                              */
/*                    END.                                                                                                   */
/*                END.                                                                                                       */
/*            END.                                                                                                           */
            
            /* Oppdaterer log */
            IF NOT CAN-FIND(tmpBxProductLog WHERE 
                            tmpBxProductLog.Warehouseno = tmpBxProduct.Warehouse AND 
                            tmpBxProductLog.Productno = tmpBxProduct.Productno) THEN 
            DO:
                CREATE tmpBxProductLog.
                BUFFER-COPY tmpbxProduct
                    TO tmpBxProductLog.
            END.
            
            IF piMax > 0 AND piAntall > piMax THEN 
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Maks antall varer nådd : ' + STRING(piAntall) + '(' + STRING(piMax) + ').' 
                    ).    
                LEAVE INIT_LOOP.
            END.
        END. /* INIT_LOOP */
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxProductPRS' + cDatoTid + '.JSon'.
            DATASET dsBxProduct:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Antall varer klargjort : ' + STRING(piAntall) + '.' 
            ).    

        /* Skriver oppdatert produkt logg til fil. */
        DATASET dsBxProductLog:WRITE-JSON('file', cBxProductLog, TRUE).
        
        /* TEST 
        DATASET dsBxUnit:WRITE-JSON('file', 'konv\dsBxUnitTEST.json', TRUE).
        TEMP-TABLE tmpBxUnit:WRITE-JSON('file', 'konv\tmpBxUnitTEST.json', TRUE).
        */
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID initBxSupplier( OUTPUT DATASET dsBxSupplier ):
        DEFINE VARIABLE bResult AS LOG NO-UNDO.
        
        EMPTY TEMP-TABLE tmpBxSupplier.
        
        FOR EACH LevBas NO-LOCK:
            CREATE tmpBxSupplier.
            ASSIGN 
                tmpBxSupplier.Supplierno   = LevBas.LevNr
                tmpBxSupplier.Suppliername = STRING(LevBas.LevNamn)
                tmpBxSupplier.Address      = LevBas.levadr 
                tmpBxSupplier.PostCode     = STRING(LevBas.LevPoNr)
                tmpBxSupplier.Country      = LevBas.LevLand
                tmpBxSupplier.City         = ''
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                IF AVAILABLE tmpBxSupplier AND 
                tmpBxSupplier.Supplierno = 0 THEN 
                    DELETE tmpBxSupplier.
                
            END.
        END.
        IF NOT ERROR-STATUS:ERROR THEN 
            bResult = TRUE.

        IF bTest THEN 
        DO:
            cTekst = 'konv\BxSupplierPRS' + cDatoTid + '.JSon'.
            DATASET dsBxSupplier:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    METHOD LOGICAL initCompany( OUTPUT DATASET dsCompany ):
        DEFINE VARIABLE bResult AS LOG NO-UNDO.
        
        EMPTY TEMP-TABLE tmpCompany.

        FOR EACH Butiker NO-LOCK:
            CREATE tmpCompany.
            ASSIGN 
                tmpCompany.CompanyName = Butiker.butNamn
                tmpCompany.Warehouseno = STRING(Butiker.butik)
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                IF AVAILABLE tmpCompany AND 
                    tmpCompany.Warehouseno = '' THEN 
                    DELETE tmpCompany.
                
            END.
        END.
        IF NOT ERROR-STATUS:ERROR THEN 
            bResult = TRUE.

        IF bTest THEN
        DO: 
            cTekst = 'konv\CompanyPRS' + cDatoTid + '.JSon'.
            DATASET dsCompany:WRITE-JSON('file', cTekst, TRUE). 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END.
        
        RETURN bResult.

    END METHOD.

    DESTRUCTOR PUBLIC HentDataPRS ( ):

    END DESTRUCTOR.

END CLASS.