 
 /*------------------------------------------------------------------------
    File        : HentvShipmentPackages
    Purpose     : 
    Syntax      : 
    Description : Henter vShipmentPackages fra Consignor.
    Author(s)   : tny
    Created     : Mon des 10 18:16:10 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Consignor.HentConsignorData: 

    DEFINE VARIABLE cSystemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOk     AS LOG       NO-UNDO.
    DEFINE VARIABLE bTest   AS LOG       NO-UNDO.
    DEFINE VARIABLE ctekst  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
    DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderNumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAntDager AS INTEGER NO-UNDO.
    DEFINE VARIABLE cNetButikkLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
        
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rLesSkrivConsignor AS cls.Consignor.LesSkrivConsignor NO-UNDO.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {cls\Consignor\tmpTblvShipmentPackages.i}
    {cls\Consignor\tmpDsvShipmentPackages.i}
    {cls\Consignor\tmpTblKOrdreHode.i}
    {cls\Consignor\tmpDsKOrdreHode.i}
        
    CONSTRUCTOR PUBLIC HentConsignorData ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN  
            cKatalog     = 'konv'
            cLogg        = pcLogg
            bTest        = FALSE
            cDatoTid     = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            cOrderNumber = '' /* '500071152' For Test - avgrenser til en ordre. Skal være blank i produksjon */
            iAntDager    = 20  /*TODAY - DATE(12/31/2017)*/ 
            .
        {syspara.i 150 1 2 cNetButikkLst}
        
        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        rLesSkrivConsignor = NEW cls.Consignor.LesSkrivConsignor( INPUT cLogg ) NO-ERROR.
      
        cSystemName = rStandardFunksjoner:SystemName.

        rStandardFunksjoner:opprettKatalog(cKatalog).
        

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentKOrdreHode( OUTPUT DATASET DsKOrdreHode ):
        
        IF cOrderNumber <> '' THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  TEST - Avgrenset til KOrdre: ' + cOrderNumber + '.' 
                ).    
        
        IF cNetbutikkLst <> '' THEN 
        NETTBUTII_LOOP:
        DO iLoop = 1 TO NUM-ENTRIES(cNetButikkLst):
            iButNr = INT(ENTRY(iLoop,cNetButikkLst)).

            /* Er det satt avgrensing til en ordre, behandles denne, hvis ikke kjøres som normalt. */
            FOR EACH KOrdreHode NO-LOCK WHERE
                (IF cOrderNumber <> '' THEN KOrdreHode.EkstOrdreNr = cOrderNumber ELSE TRUE) AND  
                (IF cOrderNumber  = '' THEN KOrdreHode.ButikkNr = iButNr ELSE TRUE) AND  
                (IF cOrderNumber  = '' THEN KOrdreHode.RegistrertDato >= TODAY - iAntDager ELSE TRUE) AND
                ( 
                    (IF cOrderNumber  = '' 
                        THEN (KOrdreHode.SendingsNr = '' OR  KOrdreHode.SendingsNr BEGINS 'UTLEVERT') 
                        ELSE TRUE) 
                ) AND 
                (IF cOrderNumber  = '' THEN KOrdreHode.Opphav = 10 ELSE TRUE) AND 
                (IF cOrderNumber  = '' THEN KOrdreHode.AntPPEti > 0 ELSE TRUE)
                :
                
                /* Skal ikke ha med RETUR ordre */
                IF KOrdreHode.SendingsNr = 'RETUR' THEN 
                    NEXT.
                
                IF NOT CAN-FIND(tmpKOrdreHode WHERE 
                                tmpKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id) THEN 
                DO:
                    CREATE tmpKOrdreHode.
                    BUFFER-COPY KOrdreHode TO tmpKOrdreHode.                
                END.        
            END.        
             
        END. /* NETTBUTII_LOOP */

        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\hentKOrdreHode_' + cDatoTid + '.JSon'.
            DATASET dsKOrdreHode:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil hentKOrdreHode(): ' + cTekst 
                ).    
        END.

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentvShipmentPackagesData( INPUT-OUTPUT DATASET DsKOrdreHode ):
        DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE pi2Ant AS INTEGER NO-UNDO.
                
        IF NOT CAN-FIND(FIRST tmpKOrdreHode) THEN 
            RETURN.
        
        /* Prøver å hente sendingsnr for alle loggede kundeordre. */
        FOR EACH tmpKOrdreHode:
            EMPTY TEMP-TABLE tmpvShipmentPackages.
             
            /* Henter hele tabellen fra SQL basen og legger den i en temp-tabell. */
            rLesSkrivConsignor:lesvShipmentPackages( INPUT tmpKOrdreHode.EkstOrdreNr, OUTPUT DATASET dsvShipmentPackages ).
            IF bTest THEN 
            DO:
                cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
                cTekst = cKatalog + '\vShipmentPackages_' + tmpKOrdreHode.EkstOrdreNr + '_' + cDatoTid + '.JSon'.
                DATASET dsvShipmentPackages:WRITE-JSON('file', cTekst, TRUE).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset skrevet til fil hentvShipmentPackagesData(): ' + cTekst 
                    ).    
            END.
            
            piAnt = 0.
            /* Teller opp antall poster. Kommer to og to. Vi skal ha de to siste. Men skriver over til vi kommer til siste. */
            FOR EACH tmpvShipmentPackages
                BREAK BY tmpvShipmentPackages.shi_ID:
                piAnt = piAnt + 1.
                
                /* Fyller ut SendingsNR først, deretter returnr. på neste runde. */
                IF piAnt = 1 THEN 
                    tmpKOrdreHode.SendingsNr = tmpvShipmentPackages.pac_parcelNumber.
                ELSE 
                    tmpKOrdreHode.ReturNr = tmpvShipmentPackages.pac_parcelNumber.
                
                IF piAnt = 2 THEN 
                    piant = 0.       
            END.
        END.
        
        IF bTest THEN 
        DO:
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
            cTekst = cKatalog + '\tmpKOrdreHode_' + cDatoTid + '.JSon'.
            DATASET dsKOrdreHode:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil tmpKOrdreHode fra hentvShipmentPackagesData(): ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.


    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID nedkoblingSQL( OUTPUT pbOk AS LOG ):

        /* Kobler ned SQL server. */
        pbOk = rLesSkrivConsignor:nedkoblingSqlServer(  ).
        IF NOT pbOk THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved nedkobling av Sql Server.' 
                ).
        ELSE DO:
            IF bTest THEN     
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  SQL server nedkoblet.' 
                    ).    
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: NB: Brukes ikke foreløpig.....
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID oppdaterKOrdreHode( INPUT DATASET dsKOrdreHode ):

        DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE piAntSasong AS INTEGER NO-UNDO.
        
        IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Start oppdater KOrdreHode.'
                ).    

        FOR EACH tmpKOrdreHode WHERE 
            tmpKOrdreHode.SendingsNr > '':

            FIND KOrdreHode EXCLUSIVE-LOCK WHERE 
                KOrdreHode.KOrdre_Id = tmpKOrdreHode.KOrdre_Id NO-ERROR.
            IF AVAILABLE KOrdreHode THEN 
            DO:
                ASSIGN 
                    KOrdreHode.SendingsNr = (IF tmpKOrdreHode.SendingsNr <> '' THEN tmpKOrdreHode.SendingsNr ELSE KOrdreHode.SendingsNr)
                    KOrdreHode.ReturNr    = (IF tmpKordreHode.ReturNr <> '' THEN tmpKordreHode.ReturNr ELSE KOrdreHode.ReturNr)  
                    piAnt                 = piAnt + 1
                    .
                    
                IF KOrdreHode.SendingsNr = '' THEN 
                DO:
                    IF KOrdreHode.RegistrertDato < TODAY THEN 
                        ASSIGN 
                            KOrdreHode.SendingsNr = 'UTLEVERT' + ' ' + REPLACE(STRING(TODAY),'/','') + ' ' + REPLACE(STRING(TIME),':','').
                    ELSE IF (TIME - KOrdrEHode.RegistrertTid) > 1800 THEN   
                            ASSIGN 
                                KOrdreHode.SendingsNr = 'UTLEVERT' + ' ' + REPLACE(STRING(TODAY),'/','') + ' ' + REPLACE(STRING(TIME),':','').
                END.    
                    
                RELEASE KOrdreHode.
            END.
        END.

        IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Ferdig oppdater KOrdreHode. Antall endret: ' + STRING(piAnt) + '.'
                ).    
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID oppkoblingSQL( OUTPUT pbOk AS LOG ):

        /* Kobler opp Sql server. */
        pbOk = rLesSkrivConsignor:oppkoblingSqlServer( ).
        IF NOT pbOk THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved oppkobling av Sql Server.' 
                ).    
            RETURN.
        END.
        ELSE DO:
            IF bTest THEN  
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  SQL server oppkoblet.' 
                    ).
        END.    
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC HentConsignorData ( ):

    END DESTRUCTOR.
END CLASS.