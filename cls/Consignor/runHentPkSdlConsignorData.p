DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg                   AS CHARACTER                            NO-UNDO.
DEFINE VARIABLE bOk                     AS LOG                                  NO-UNDO.
DEFINE VARIABLE cReturnMsg              AS CHARACTER                            NO-UNDO.
DEFINE VARIABLE dDato                   AS DATE                                 NO-UNDO.
DEFINE VARIABLE iTime                   AS INTEGER                              NO-UNDO.
DEFINE VARIABLE cTime                   AS CHARACTER                            NO-UNDO.
DEFINE VARIABLE cTimeLst                AS CHARACTER                            NO-UNDO.
DEFINE VARIABLE bTest                   AS LOG                                  NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner     AS cls.StdFunk.StandardFunksjoner       NO-UNDO.
DEFINE VARIABLE rHentPkSdlConsignorData AS cls.Consignor.HentPkSdlConsignorData NO-UNDO.

{cls\Consignor\tmpTblvShipmentPackages.i}
{cls\Consignor\tmpDsvShipmentPackages.i}
{cls\Consignor\tmpTblPkSdlHode.i}
{cls\Consignor\tmpDsPkSdlHode.i}

HOVEDBLOKK:
DO  ON ERROR  UNDO, LEAVE
  ON ENDKEY UNDO, LEAVE
  ON STOP   UNDO, LEAVE
  ON QUIT   UNDO, LEAVE:

  ASSIGN 
    dDato    = TODAY 
    cLogg    = 'HentPkSdlConsignorData' + REPLACE(STRING(TODAY),'/','')
    cTimeLst = '23,01'
    bTest    = TRUE 
    .
  rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).
  /* Starter med tom linje i loggen. */
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start HentPksdlConsignorData.' 
    ).    

  /* Starter klassene. */
  rHentPkSdlConsignorData  = NEW cls.Consignor.HentPksdlConsignorData( INPUT cLogg ).

  EVIGHETEN:
  DO /*WHILE TRUE*/:
    /* Tid på døgnet for backup m.m. */
    ASSIGN 
      cTime = ENTRY(1,STRING(TIME,"HH:MM:SS"),':').
    IF cTimeLst <> '' AND CAN-DO(cTimeLst,cTime) OR SEARCH('consignor2_stop.txt') <> ? THEN 
      LEAVE EVIGHETEN.

    /* Kobler opp SQL server databasen */
    rHentPksdlConsignorData:oppkoblingSQL( OUTPUT bOk).

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   Henter pakkseddel hodene.' 
        ).    

    /* Henter alle ikke innleverte pakkseddler som har kode = 0. */
    rHentPkSdlConsignorData:hentPkSdlHode( INPUT-OUTPUT DATASET dsPkSdlHode ).
        
    /* Henter KOrdre m.m. */
    IF CAN-FIND(FIRST tmpPkSdlHode) THEN 
      rHentPkSdlConsignorData:hentvShipmentPackagesData( INPUT-OUTPUT DATASET dsPkSdlHode, INPUT-OUTPUT DATASET dsvShipmentPackages).

    /* Kobler ned SQL server databasen */
    rHentPkSdlConsignorData:nedkoblingSQL( OUTPUT bOk).    

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   Oppdaterer pakkseddel hodene.' 
        ).    
    
    /* Oppdaterer vShipmentPackages. */
    IF CAN-FIND(FIRST tmpPkSdlHode) THEN 
      rHentPkSdlConsignorData:oppdaterPkSdlHode( INPUT-OUTPUT DATASET dsPkSdlHode, INPUT-OUTPUT DATASET dsvShipmentPackages ).

    EMPTY TEMP-TABLE tmpPkSdlHode NO-ERROR.
    EMPTY TEMP-TABLE tmpvShipmentPackages NO-ERROR.   

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '   Pauser kjøring 1 sec.' 
        ).    
    PAUSE 1 NO-MESSAGE.
        
  END. /* EVIGHETEN */

  /*    IF VALID-OBJECT(rTemp) THEN DELETE OBJECT rTemp NO-ERROR.*/
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Tidsbruk ' + STRING(TIME - iTime,'HH:MM:SS') 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Ferdig.' 
    ).    

  CATCH e1 AS Progress.Lang.AppError:
    DO ix = 1 TO e1:NumMessages:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + e1:GetMessage(ix) 
        ).    
    END.
    
    IF e1:ReturnValue > "" THEN
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Returverdi: ' + e1:ReturnValue 
        ).    
  END CATCH.
  CATCH e2 AS Progress.Lang.Error:
    DO ix = 1 TO e2:NumMessages:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + e2:GetMessage(ix) 
        ).    
    END.
  END CATCH.
END. /* HOVEDBLOKK */

FINALLY.

END FINALLY.
