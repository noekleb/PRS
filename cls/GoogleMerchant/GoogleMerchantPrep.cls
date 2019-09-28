 
 /*------------------------------------------------------------------------
    File        : GoogleMerchantPrep
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Sat Sep 28 10:46:29 CEST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.GoogleMerchant.GoogleMerchantPrep: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE iErr-Status AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.  
  DEFINE VARIABLE cBku AS CHARACTER NO-UNDO.
  
  /* Definerer Properties */
  DEFINE PROPERTY iAktivEkstraLogg AS INTEGER NO-UNDO GET. SET.
  DEFINE PROPERTY cLoggfilEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY cKatalogEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY cEksportFilEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY cButLstEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY iButNr AS INTEGER NO-UNDO GET. SET.

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

  DEFINE TEMP-TABLE ttEan NO-UNDO XML-NODE-NAME 'InventoryFeed' 
    FIELD Ean AS CHARACTER FORMAT "x(20)" XML-NODE-NAME 'itemid' 
    FIELD ButNr AS INTEGER FORMAT ">>>>>9" XML-NODE-NAME 'store_code' 
    FIELD Antall AS INTEGER FORMAT "->>>>>>>9" XML-NODE-NAME 'quantity'
    FIELD Pris AS DECIMAL FORMAT "->>>>>>>9.99" XML-NODE-NAME 'price'
    INDEX idxEan Ean.
    
  DEFINE STREAM Inn.
  DEFINE STREAM Ut.
      
  CONSTRUCTOR PUBLIC GoogleMerchantPrep ( INPUT pcLogg AS CHARACTER ):
    SUPER ().

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
    ASSIGN 
      cLogg = pcLogg
      cBku  = '\bku'
      .
    IF SEARCH('test.txt') <> ? THEN 
      bTest = TRUE.

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  PrepGoogleMerchant.cls - NEW'
          ). 
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL lesLoggFil(  ):    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcEan AS CHARACTER NO-UNDO.

      IF bTest THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '  lesLoggFil(  )'
            ). 
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '    Leser fil ' + SEARCH(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log'))
            ). 
      END.

    IF SEARCH(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')) <> ? THEN 
    DO:
      INPUT STREAM Inn FROM VALUE(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')).
      REPEAT:
        IMPORT STREAM Inn DELIMITER ';' 
          pcEan.
        IF NOT CAN-FIND(ttEan WHERE 
                        ttEan.Ean = pcEan) THEN 
        DO:
          CREATE ttEan. 
          ASSIGN 
            ttEan.Ean = pcEan
            .
        END.  
      END. 
      INPUT STREAM Inn CLOSE.
      
      /* Preparerer tabellen med data. */
      IF CAN-FIND(FIRST ttEan) THEN       
        preppTabell().
      
/*      IF bTest THEN                                                                                           */
/*        TEMP-TABLE ttEan:WRITE-JSON('file', cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.json'), TRUE).*/
      
      /* Legger ut xml filen */
      IF CAN-FIND(FIRST ttEan) THEN
      DO: 
        TEMP-TABLE ttEan:WRITE-XML('file', cKatalogEkstra + cEksportFilEkstra, TRUE).
        OS-DELETE VALUE(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')).
          iErr-Status = OS-ERROR.
          IF iErr-Status <> 0 THEN
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                '    OS-ERROR: ' + STRING(iErr-Status) + ' ' + rStandardFunksjoner:Error-Status(iErr-Status) 
                ).
          ELSE  
            bResult = TRUE.  
      END.  
      EMPTY TEMP-TABLE ttEan.
    END.
      
    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOG prepLoggFil(  ):
    DEFINE VARIABLE pbOk AS LOG NO-UNDO.
    DEFINE VARIABLE pcFil AS CHARACTER NO-UNDO.
    
    ASSIGN 
      pcFil = cKatalogEkstra + cLoggfilEkstra
      .
    
    IF SEARCH(pcFil) <> ? THEN 
      DO:
        IF bTest THEN 
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '  prepLoggFil(  )'
              ). 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '    Rename av loggfil fra ' + SEARCH(pcFil) + ' til ' + (cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')) + '.'
              ). 
        END.
        OS-RENAME VALUE(SEARCH(pcFil)) VALUE(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')).
        iErr-Status = OS-ERROR.
        IF iErr-Status <> 0 THEN
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '    OS-ERROR: ' + STRING(iErr-Status) + ' ' + rStandardFunksjoner:Error-Status(iErr-Status) 
              ).
        ELSE  
          pbOk = TRUE.  
      END.
    RETURN pbOk.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL preppTabell(  ):    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iAntall AS INTEGER NO-UNDO.    

    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = iButNr NO-ERROR.

    IF AVAILABLE Butiker AND CAN-FIND(FIRST ttEan) THEN 
    DO:
      FOR EACH ttEan:
        iAntall = 0.
        FIND Strekkode NO-LOCK WHERE 
          Strekkode.Kode = ttEan.Ean NO-ERROR.
        IF AVAILABLE Strekkode THEN
          /* Sum lagerantall alle størrelser for butikk og farge. */ 
          FOR EACH ArtLag NO-LOCK WHERE 
            ArtLag.ArtikkelNr = Strekkode.ArtikkelNr AND 
            ArtLag.Butik = iButNr:
            ASSIGN 
              iAntall = iAntall + ArtLag.LagAnt
              .
          END.
        FIND ArtPris NO-LOCK WHERE 
          ArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND 
          ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
              
        ASSIGN 
          ttEan.ButNr  = iButNr
          ttEan.Antall = IF iAntall > 0 THEN iAntall ELSE 0
          ttEan.Pris   = IF AVAILABLE ArtPris THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE 0
          .
      END.
      bResult = TRUE.
    END.
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID renameEkstraLoggFil(  ):
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID setVariablerEkstraLogg(  ):
    {syspara.i 50 65 1 iAktivEkstraLogg INT}
    {syspara.i 50 65 2 cLoggfilEkstra}
    {syspar2.i 50 65 2 cKatalogEkstra}    
    {syspara.i 50 65 3 cEksportFilEkstra}
    {syspara.i 50 65 4 cButLstEkstra}
    {syspar2.i 50 65 4 iButNr INT}

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  setVariablerEkstraLogg(  )'
          ). 

    IF cEksportFilEkstra <> '' THEN 
      ASSIGN 
        cEksportFilEkstra = REPLACE(cEksportFilEkstra,'DDMMYY',REPLACE(STRING(TODAY,"99/99/9999"),'/',''))
        cEksportFilEkstra = REPLACE(cEksportFilEkstra,'HHMMSS',REPLACE(STRING(TIME,"HH:MM:SS"),':',''))
        cKatalogEkstra    = TRIM(cKatalogEkstra,'\')
        cKatalogEkstra    = TRIM(cKatalogEkstra,'/')
        .
    /* Oppretter kataloger hvis de mangler. */
    cTekst = ''.
    DO iLoop = 1 TO NUM-ENTRIES(cKatalogEkstra):
      cTekst = cTekst +
               (IF cTekst <> '' THEN '\' ELSE '') +  
               ENTRY(iLoop,cKatalogEkstra).
      OS-CREATE-DIR VALUE(cTekst).
      iErr-Status = OS-ERROR.
      IF iErr-Status <> 0 THEN
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '    OS-ERROR: ' + STRING(iErr-Status) + ' ' + rStandardFunksjoner:Error-Status(iErr-Status) 
            ).
    END.
    cTekst = cTekst + cBku.
    OS-CREATE-DIR VALUE(cTekst).

    ASSIGN 
      cKatalogEkstra    = cKatalogEkstra + '\'
      .
      
    IF bTest THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  setVariablerEkstraLogg(  )'
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '    Parametre EkstraFeed: '
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '      iAktivEkstraLogg.: ' + STRING(iAktivEkstraLogg)
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '      cLoggfilEkstra...: ' + cLoggfilEkstra
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '      cKatalogEkstra...: ' + cKatalogEkstra
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '      cEksportFilEkstra: ' + cEksportFilEkstra
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '      cButLstEkstra....: ' + cButLstEkstra
          ). 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '      iButNr...........: ' + STRING(iButNr)
          ). 
    END.
    
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC GoogleMerchantPrep ( ):

    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '  PrepGoogleMerchant - DESTRUCTOR'
        ). 
  END DESTRUCTOR.

END CLASS.