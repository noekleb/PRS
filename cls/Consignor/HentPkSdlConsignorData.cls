 
/*------------------------------------------------------------------------
   File        : HentPkSdlConsignorData.cls
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : tny
   Created     : 
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Consignor.HentPkSdlConsignorData: 

  DEFINE VARIABLE cSystemName             AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE cLogg                   AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE bOk                     AS LOG                                  NO-UNDO.
  DEFINE VARIABLE bTest                   AS LOG                                  NO-UNDO.
  DEFINE VARIABLE ctekst                  AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE cDatoTid                AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE hDataset                AS HANDLE                               NO-UNDO.
  DEFINE VARIABLE cKatalog                AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE cPkSdlNr                AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE iButNr                  AS INTEGER                              NO-UNDO.
  DEFINE VARIABLE iLoop                   AS INTEGER                              NO-UNDO.
  DEFINE VARIABLE cPkSdlLogg              AS CHARACTER                            NO-UNDO.
        
  DEFINE VARIABLE rStandardFunksjoner     AS cls.StdFunk.StandardFunksjoner       NO-UNDO.
  DEFINE VARIABLE rLesSkrivPkSdlConsignor AS cls.Consignor.LesSkrivPkSdlConsignor NO-UNDO.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  {cls\Consignor\tmpTblvShipmentPackages.i}
  {cls\Consignor\tmpDsvShipmentPackages.i}
  {cls\Consignor\tmpTblPkSdlHode.i}
  {cls\Consignor\tmpDsPkSdlHode.i}
        
  CONSTRUCTOR PUBLIC HentPkSdlConsignorData ( INPUT pcLogg AS CHARACTER ):
    SUPER ().

    ASSIGN  
      cKatalog   = 'konv'
      cLogg      = pcLogg
      bTest      = TRUE
      cDatoTid   = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
      cPkSdlNr   = '' /* For Test - avgrenser til en ordre. Skal være blank i produksjon */
      cPkSdllogg = 'PkSDlSendtFraConsignor' 
      .
          
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( cLogg ).
    rLesSkrivPkSdlConsignor = NEW cls.Consignor.LesSkrivPkSdlConsignor( INPUT cLogg ).
    
    cSystemName = rStandardFunksjoner:SystemName.

    rStandardFunksjoner:opprettKatalog(cKatalog).
        
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID hentPkSdlHode( INPUT-OUTPUT DATASET DsPkSdlHode ):
        
    EMPTY TEMP-TABLE tmpPkSdlHode.
        
    FOR EACH PkSdlHode NO-LOCK WHERE 
      PkSdlHode.PkSdlStatus = 10 AND 
      PkSdlHode.SendtOutlet = 0:
      
      CREATE tmpPkSdlHode.
        
      ASSIGN
        tmpPkSdlHode.PkSdlId = PkSdlHode.PkSdlId
        tmpPkSdlHode.PkSdlNr = PkSdlHode.PkSdlNr
        .
    END.    

    IF bTest THEN 
      DATASET dsPkSdlHode:WRITE-JSON('file', 'konv\PkSdlHodeConsignor.json', TRUE).
      
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID hentvShipmentPackagesData( INPUT-OUTPUT DATASET DsPkSdlHode, INPUT-OUTPUT DATASET dsvShipmentPackages ):
    DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
              
    IF NOT CAN-FIND(FIRST tmpPkSdlHode) THEN
    DO: 
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Finner ingen tmpPkSdlHode. Avbryter. ' 
          ).    
      RETURN.
    END.
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Begynner behandling av tmpPkSdlHode.' 
        ).    

/*    /* Prøver å hente sendingsnr for alle loggede pakkseddler. */                                                    */
/*    RECORDLOOP:                                                                                                      */
/*    FOR EACH tmpPkSdlHode:                                                                                           */
/*      /* Henter recordene som inneholder pksdlnr. */                                                                 */
/*      /* bygger opp tabellen. */                                                                                     */
/*      rLesSkrivPkSdlConsignor:lesvShipmentPackages( INPUT tmpPkSdlHode.PkSdlNr, OUTPUT DATASET dsvShipmentPackages ).*/
/*    END. /* RECORDLOOP */                                                                                            */
      
    /* Henter hele tabellen. */
    rLesSkrivPkSdlConsignor:lesvShipmentPackages( OUTPUT DATASET dsvShipmentPackages ).

    piAnt = 0.
      
    /* Renser opp i skilletegn */
    RENS:
    FOR EACH tmpvShipmentPackages:        
      ASSIGN 
        piAnt                                    = piAnt + 1
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,' ',',')
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,';',',')
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,'-',',')
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,'.',',')
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,'/',',')
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,'\',',')
        tmpvShipmentPackages.shi_ref_OrderNumber = REPLACE(tmpvShipmentPackages.shi_ref_OrderNumber,'GF','') /* Nummeret prefixes noen ganger */
        .
    END. /* RENS */

    IF bTest THEN 
      TEMP-TABLE tmpvShipmentPackages:WRITE-JSON('file', 'konv\tmpvShipmentPackages.json', TRUE).
          
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Antall shipment poster funnet ' + STRING(piAnt) + '.' 
        ).    
        
    RETURN.

  END METHOD.


  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID nedkoblingSQL( OUTPUT pbOk AS LOG ):

    /* Kobler ned SQL server. */
    pbOk = rLesSkrivPkSdlConsignor:nedkoblingSqlServer(  ).
    IF NOT pbOk THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ** Feil ved nedkobling av Sql Server.' 
        ).
    ELSE 
    DO:
      IF bTest THEN     
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  SQL server nedkoblet.' 
          ).    
    END.
        
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: 
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID oppdaterPksdlHode( INPUT-OUTPUT DATASET dsPkSdlHode, INPUT-OUTPUT DATASET dsvShipmentPackages ):

    DEFINE VARIABLE piAnt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE piLoop    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pcPkSdlNr AS CHARACTER NO-UNDO.
        
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Start markering av pakkseddler som er sendt Outlet.'
        ).    
                
    PAKKELOOP:        
    FOR EACH tmpvShipmentPackages:
      DO piLoop = 1 TO NUM-ENTRIES(tmpvShipmentPackages.shi_ref_OrderNumber):
        cPkSdlNr = TRIM(ENTRY(piLoop,tmpvShipmentPackages.shi_ref_OrderNumber)).

        /* Flagger alle pakkseddler med det pakkseddelnummeret. */
        /* Det kan være fler enn en.                            */
        IF cPkSdlNr <> '' THEN 
        DO:
          FOR EACH tmpPkSdlHode WHERE 
            tmpPkSdlhode.PkSdlNr = pcPkSdlNr:
            FIND PkSdlHode EXCLUSIVE-LOCK WHERE 
              PkSdlHode.PkSdlId = tmpPkSdlHode.PkSdlId NO-ERROR NO-WAIT.
            IF AVAILABLE PkSdlHode AND NOT LOCKED PkSdlHode AND PkSdlHode.SendtOutlet = 0 THEN 
            DO:
              ASSIGN
                piAnt = piAnt + 1
                                    PkSdlHode.SendtOutlet            = 1
                                    PkSdlHode.SendtFraLagerTilOutlet = NOW
                                    PkSdlHode.Varetype               = 'Consignor ' + PkSdlHode.Varetype
                .
              rStandardFunksjoner:SkrivTilLogg(cPkSdlLogg,
                '    Pakkseddel ' + PkSdlHode.PkSdlNr + ' (Id: ' + STRING(PkSdlHode.PkSdlId) + ').'
                ).    
              RELEASE PkSdlHode.
            END.  
          END.
        END.
      END.
    END. /* PAKKELOOP */

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Ferdig markering av PkSdlHode. Antall endret: ' + STRING(piAnt) + '.'
        ).    
        
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID oppkoblingSQL( OUTPUT pbOk AS LOG ):

    /* Kobler opp Sql server. */
    pbOk = rLesSkrivPkSdlConsignor:oppkoblingSqlServer( ).
        
    IF NOT pbOk THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ** Feil ved oppkobling av Sql Server.' 
        ).    
      RETURN.
    END.
    ELSE 
    DO:
      IF bTest THEN  
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  SQL server oppkoblet.' 
          ).
    END.    
        
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC HentPkSdlConsignorData ( ):

  END DESTRUCTOR.
END CLASS.