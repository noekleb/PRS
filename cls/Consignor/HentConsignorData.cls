 
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
            bTest        = TRUE
            cDatoTid     = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            cOrderNumber = '' /* '50012853' For Test - avgrenser til en ordre. Skal være blank i produksjon */
            iAntDager    = 20  /*TODAY - DATE(12/31/2017)*/ 
            .
        {syspara.i 150 1 2 cNetButikkLst}
        
        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).
        rLesSkrivConsignor   = NEW cls.Consignor.LesSkrivConsignor( INPUT cLogg ).
      
        cSystemName = rStandardFunksjoner:SystemName.

        rStandardFunksjoner:opprettKatalog(cKatalog).
        

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentKOrdreHode( INPUT-OUTPUT DATASET DsKOrdreHode ):
        
        IF cNetbutikkLst <> '' THEN 
        NETTBUTII_LOOP:
        DO iLoop = 1 TO NUM-ENTRIES(cNetButikkLst):
            iButNr = INT(ENTRY(iLoop,cNetButikkLst)).

            /* Er det satt avgrensing til en ordre, behandles denne, hvis ikke kjøres som normalt. */
            /* Tar også med Klik&Hent og RETUR ordre her.                                          */
            FOR EACH KOrdreHode NO-LOCK WHERE
                (IF cOrderNumber <> '' THEN KOrdreHode.EkstOrdreNr = cOrderNumber ELSE TRUE) AND  
                (IF cOrderNumber  = '' THEN KOrdreHode.ButikkNr = iButNr ELSE TRUE) AND  
                (IF cOrderNumber  = '' THEN KOrdreHode.RegistrertDato >= TODAY - iAntDager ELSE TRUE) AND
                (IF cOrderNumber  = '' THEN KOrdreHode.Opphav = 10 ELSE TRUE) AND 
                (IF cOrderNumber  = '' THEN KOrdreHode.AntPPEti > 0 ELSE TRUE)
                :
                
                IF NOT CAN-FIND(tmpKOrdreHode WHERE 
                                tmpKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id) THEN 
                DO:
                    CREATE tmpKOrdreHode.
                    BUFFER-COPY KOrdreHode
                      EXCEPT DatoTidEndret
                      TO tmpKOrdreHode
                      ASSIGN
                        tmpKOrdreHode.DatoTidEndret = ? 
                        .                

/*                    IF bTest THEN                                                         */
/*                        rStandardFunksjoner:SkrivTilLogg(cLogg,                           */
/*                            '  Henter KORdreHode: ' + STRING(KORdrEHode.EkstOrdreNr) + '.'*/
/*                            ).                                                            */
        

                END.        
            END.        
             
        END. /* NETTBUTII_LOOP */

/*        IF bTest AND CAN-FIND(FIRST tmpKOrdreHode)THEN                                         */
/*        DO:                                                                                    */
/*            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').*/
/*            cTekst = cKatalog + '\hentKOrdreHode_' + cDatoTid + '.JSon'.                       */
/*            DATASET dsKOrdreHode:WRITE-JSON('file', cTekst, TRUE).                             */
/*            rStandardFunksjoner:SkrivTilLogg(cLogg,                                            */
/*                '  Dataset skrevet til fil hentKOrdreHode(): ' + cTekst                        */
/*                ).                                                                             */
/*        END.                                                                                   */

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
        DO: 
          IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '  Finner ingen tmpKOrdreHode. Avbryter. ' 
                  ).    
          RETURN.
        END.
        IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Begynner behandling av tmpKOrdreHode.' 
                ).    

        /* Prøver å hente sendingsnr for alle loggede kundeordre. */
        RECORDLOOP:
        FOR EACH tmpKOrdreHode:

          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  OrdreNr: ' + STRING(tmpKOrdreHode.EkstOrdreNr)). 


          EMPTY TEMP-TABLE tmpvShipmentPackages NO-ERROR.
        
          /* Henter hele tabellen som gjelder ordren som behandles fra SQL basen og legger den i en temp-tabell. */
          rLesSkrivConsignor:lesvShipmentPackages( INPUT tmpKOrdreHode.EkstOrdreNr, OUTPUT DATASET dsvShipmentPackages ).
/*            IF bTest AND CAN-FIND(FIRST tmpvShipmentPackages) THEN                                               */
/*            DO:                                                                                                  */
/*                cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').              */
/*                cTekst = cKatalog + '\vShipmentPackages_' + tmpKOrdreHode.EkstOrdreNr + '_' + cDatoTid + '.JSon'.*/
/*                DATASET dsvShipmentPackages:WRITE-JSON('file', cTekst, TRUE).                                    */
/*                rStandardFunksjoner:SkrivTilLogg(cLogg,                                                          */
/*                    '  Dataset skrevet til fil hentvShipmentPackagesData(): ' + cTekst                           */
/*                    ).                                                                                           */
/*            END.                                                                                                 */

          ASSIGN 
              piAnt = 0
              NO-ERROR.

          /* Teller opp antall poster. Kommer to og to. Vi skal ha de to siste. Men skriver over til vi kommer til siste. */
          FOR EACH tmpvShipmentPackages
              BREAK BY tmpvShipmentPackages.shi_ID DESC:

              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    OrdreNr: ' + STRING(tmpKOrdreHode.EkstOrdreNr) + 
                  '    shi_ID: ' + STRING(tmpvShipmentPackages.shi_ID) + 
                  '    pac_parcelNumber:' + tmpvShipmentPackages.pac_parcelNumber
                  ).

              piAnt = piAnt + 1.

              IF piAnt <= 2 AND tmpvShipmentPackages.pac_parcelNumber <> ? THEN
              ETIKETT:
              DO: 
                  /* Fyller ut SendingsNr først, deretter returnr. på neste runde.       */
                  /* På Utlevert i butikk (Klikk&Hent), skal ikke sendingsnr legges inn. */
                  /* Skriver bare over når verdien er forskjellig.                       */
                  IF piAnt = 1 THEN 
                  DO:
                      rStandardFunksjoner:SkrivTilLogg(cLogg,
                          '    1 OrdreNr: ' + STRING(tmpKOrdreHode.EkstOrdreNr) + 
                          '    shi_ID: ' + STRING(tmpvShipmentPackages.shi_ID) + 
                          '    tmpKOrdreHode.ReturNr:' + (IF tmpKOrdreHode.ReturNr = ? THEN '?' ELSE tmpKOrdreHode.ReturNr) +  
                          '    pac_parcelNumber:' + tmpvShipmentPackages.pac_parcelNumber + 
                          '    Test:' + STRING(tmpKOrdreHode.ReturNr <> tmpvShipmentPackages.pac_parcelNumber)  
                          ).
                      IF tmpKOrdreHode.ReturNr <> tmpvShipmentPackages.pac_parcelNumber THEN
                      DO: 
                        ASSIGN 
                            tmpKOrdreHode.ReturNr       = tmpvShipmentPackages.pac_parcelNumber
                            tmpKOrdrEHode.shi_ID        = STRING(tmpvShipmentPackages.shi_ID)
                            tmpKORdreHode.DatoTidEndret = NOW
                            .
                        IF bTest THEN 
                          rStandardFunksjoner:SkrivTilLogg(cLogg,
                              '      Endring logget KOrdre_Id/ReturNr: ' + STRING(tmpKOrdreHode.KOrdre_ID) + '/' + STRING(tmpKOrdreHode.ReturNr)
                              ).
                      END.
                  END.
                  IF piAnt = 2 THEN  
                  DO: 
                      rStandardFunksjoner:SkrivTilLogg(cLogg,
                          '    2 OrdreNr: ' + STRING(tmpKOrdreHode.EkstOrdreNr) + 
                          '    shi_ID: ' + STRING(tmpvShipmentPackages.shi_ID) + 
                          '    tmpKOrdreHode.SendingsNr:' + (IF tmpKOrdreHode.SendingsNr = ? THEN '?' ELSE tmpKOrdreHode.SendingsNr) +  
                          '    pac_parcelNumber:' + tmpvShipmentPackages.pac_parcelNumber +  
                          '    Test:' + STRING(tmpKOrdreHode.SendingsNr <> tmpvShipmentPackages.pac_parcelNumber)  
                          ).
                      IF tmpKOrdreHode.SendingsNr <> tmpvShipmentPackages.pac_parcelNumber THEN
                      DO: 
                          ASSIGN 
                              tmpKOrdreHode.SendingsNr    = tmpvShipmentPackages.pac_parcelNumber
                              tmpKOrdrEHode.shi_ID        = STRING(tmpvShipmentPackages.shi_ID)
                              tmpKORdreHode.DatoTidEndret = NOW
                              .
                        IF bTest THEN 
                          rStandardFunksjoner:SkrivTilLogg(cLogg,
                              '      Endring logget KOrdre_Id/SendingsNr: ' + STRING(tmpKOrdreHode.KOrdre_ID) + '/' + STRING(tmpKOrdreHode.SendingsNr)
                              ).
                      END.
                  END.

              END. /* ETIKETT */      
          END.
        END. /* RECORDLOOP */
        
        /* Tar bort ikke endrede poster slik at vi bare viderebehandler de som er blitt endret. */
        FOR EACH tmpKORdreHode WHERE 
            tmpKORdreHode.DatoTidEndret = ?:
            DELETE tmpKORdrEHode.
        END.
        
        
/*        IF bTest AND CAN-FIND(FIRST tmpKORdreHode) THEN                                             */
/*        DO:                                                                                         */
/*            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').     */
/*            cTekst = cKatalog + '\tmpKOrdreHode_' + cDatoTid + '.JSon'.                             */
/*            DATASET dsKOrdreHode:WRITE-JSON('file', cTekst, TRUE).                                  */
/*            rStandardFunksjoner:SkrivTilLogg(cLogg,                                                 */
/*                '  Dataset skrevet til fil tmpKOrdreHode fra hentvShipmentPackagesData(): ' + cTekst*/
/*                ).                                                                                  */
/*        END.                                                                                        */
        
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
            tmpKOrdreHode.DatoTidEndret <> ?:

            FIND KOrdreHode EXCLUSIVE-LOCK WHERE 
                KOrdreHode.KOrdre_Id = tmpKOrdreHode.KOrdre_Id NO-ERROR.
            IF AVAILABLE KOrdreHode THEN 
            DO:
                piAnt = piAnt + 1.
                IF KOrdrEHode.SendingsNr = ? THEN KOrdrEHode.SendingsNr = ''.
                IF KOrdrEHode.ReturNr    = ? THEN KOrdreHode.ReturNr    = ''.
                
                IF (tmpKOrdreHode.SendingsNr <> '' AND 
                    tmpKOrdreHode.SendingsNr <> ? AND
                    tmpKOrdreHode.SendingsNr <> KOrdreHode.SendingsNr AND                     
                    NOT (KOrdreHode.SendingsNr BEGINS 'RETUR')
                   ) THEN 
                DO:
                  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  OrdreNr: ' + STRING(KOrdreHode.EkstOrdreNr) + 
                      '  shi_ID: ' + STRING(tmpKOrdreHode.shi_ID) + 
                      ' SendingsNr gml/ny: ' + (IF KOrdreHode.SendingsNr = ? THEN '?' ELSE KOrdreHode.SendingsNr) + '/' + 
                      STRING(tmpKOrdreHode.SendingsNr) + '.'
                      ).    
                  ASSIGN 
                      KOrdreHode.SendingsNr = tmpKOrdreHode.SendingsNr. 
                END.
                
                IF (tmpKordreHode.ReturNr <> KOrdreHode.ReturNr AND 
                    tmpKordreHode.ReturNr <> ? AND 
                    tmpKordreHode.ReturNr <> ''
                    ) THEN 
                DO:
                  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  OrdreNr: ' + STRING(KOrdreHode.EkstOrdreNr) + 
                      '  shi_ID: ' + STRING(tmpKOrdreHode.shi_ID) + 
                      ' ReturNr gml/ny: ' + (IF KOrdreHode.ReturNr = ? THEN '?' ELSE  KOrdreHode.ReturNr) + '/' + 
                      STRING(tmpKOrdreHode.ReturNr) + '.'
                      ).    
                  ASSIGN 
                    KOrdreHode.ReturNr = tmpKordreHode.ReturNr.
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