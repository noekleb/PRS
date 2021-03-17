 
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
  DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest       AS LOG       NO-UNDO.
  DEFINE VARIABLE iErr-Status AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLoop       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.  
  DEFINE VARIABLE cBku        AS CHARACTER NO-UNDO.
  
  /* Definerer Properties */
  DEFINE PROPERTY iAktivEkstraLogg AS INTEGER NO-UNDO GET. SET.
  DEFINE PROPERTY cLoggfilEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY cKatalogEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY cEksportFilEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY cButLstEkstra AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROPERTY iButNr AS INTEGER NO-UNDO GET. SET.

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

  DEFINE TEMP-TABLE ttEan NO-UNDO 
    FIELD Ean        AS CHARACTER FORMAT "x(20)"
    FIELD ButNr      AS INTEGER   FORMAT ">>>>>9"
    FIELD ArtikkelNr AS DECIMAL   FORMAT ">>>>>>>>>>>>>9"
    FIELD StrKode    AS INTEGER     
    INDEX idxEan Ean ButNr .

  DEFINE TEMP-TABLE ttEanPrep NO-UNDO XML-NODE-NAME 'lokalinventory' 
    FIELD Ean    AS CHARACTER FORMAT "x(20)" XML-NODE-NAME 'itemid'
    FIELD ButNr  AS INTEGER   FORMAT ">>>>>9" XML-NODE-NAME 'store_code' 
    FIELD Antall AS INTEGER   FORMAT "->>>>>>>9" XML-NODE-NAME 'quantity'
    FIELD Pris   AS DECIMAL   FORMAT "->>>>>>>9.99" XML-NODE-NAME 'price'
    INDEX idxEan Ean ButNr .

  DEFINE BUFFER bufttEanPrep FOR ttEanPrep.
  DEFINE BUFFER bufStrekkode FOR Strekkode.
        
  DEFINE STREAM Inn.
  DEFINE STREAM Ut.
      
  CONSTRUCTOR PUBLIC GoogleMerchantPrep ( INPUT pcLogg AS CHARACTER ):
    SUPER ().

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
    ASSIGN 
      cLogg = pcLogg
      cBku  = 'bku\'
      .
    IF SEARCH('test.txt') <> ? THEN 
      bTest = TRUE.

    /* Setter parametre for behandling av loggfil for ekstrafeed. */
    setVariablerEkstraLogg().

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '  PrepGoogleMerchant.cls - NEW'
        ). 
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL eksporterTabell(  ):    
    DEFINE VARIABLE bResult      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE pcFil        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHeader      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE crssStart    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE crssSlutt    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChanelStart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChanelSlutt AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemStart   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemSlutt   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItem        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStore_Code  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrice       AS CHARACTER NO-UNDO.

    ASSIGN 
      pcFil        = cKatalogEkstra + cEksportFilEkstra
      cHeader      = '<?xml version="1.0"?>'
      crssStart    = '<rss xmlns:g="http://base.google.com/ns/1.0" version="2.0">'
      cChanelStart = '<channel>'
      cItemStart   = '<item>'
      cItemSlutt   = '</item>'
      cChanelSlutt = '</channel>'
      crssSlutt    = '</rss>'
      cItem        = '<g:itemid>&Item</g:itemid>'
      cStore_Code  = '<g:store_code>&Store_Code</g:store_code>'
      cQuantity    = '<g:quantity>&Quantity</g:quantity>'
      cPrice       = '<g:price>&Price</g:price>'
      .

    IF CAN-FIND(FIRST ttEanPrep) THEN
    UTLEGG: 
    DO:
      OUTPUT STREAM Ut TO VALUE(pcFil).
      PUT STREAM Ut UNFORMATTED cHeader SKIP.
      PUT STREAM Ut UNFORMATTED crssStart SKIP.
      PUT STREAM Ut UNFORMATTED '    ' + cChanelStart SKIP.
      
      FOR EACH ttEanPrep:
        PUT STREAM Ut UNFORMATTED '        ' + cItemStart SKIP.
        cTekst = '            ' + REPLACE(cItem,'&Item',ttEanPrep.Ean).
        PUT STREAM Ut UNFORMATTED cTekst SKIP.
        
        cTekst = '            ' + REPLACE(cStore_Code,'&Store_Code','GANT' + STRING(ttEanPrep.ButNr)).
        PUT STREAM Ut UNFORMATTED cTekst SKIP.
        
        cTekst = '            ' + REPLACE(cQuantity,'&Quantity',STRING(ttEanPrep.Antall)).
        PUT STREAM Ut UNFORMATTED cTekst SKIP.
        
        cTekst = '            ' + REPLACE(cPrice,'&Price',STRING(ttEanPrep.Pris)).
        PUT STREAM Ut UNFORMATTED cTekst SKIP.
        PUT STREAM Ut UNFORMATTED '        ' + cItemSlutt SKIP.        
      END.  
      
      PUT STREAM Ut UNFORMATTED '    ' + cChanelSlutt SKIP.
      PUT STREAM Ut UNFORMATTED crssSlutt SKIP.
      OUTPUT STREAM Ut CLOSE.
      bResult = TRUE.  
    END. /* UTLEGG */
      
    /*    /* Legger ut xml filen */                                                                             */
    /*    IF CAN-FIND(FIRST ttEanPrep) THEN                                                                     */
    /*    DO:                                                                                                   */
    /*      TEMP-TABLE ttEanPrep:WRITE-XML('file', cKatalogEkstra + cEksportFilEkstra, TRUE).                   */
    /*      OS-DELETE VALUE(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')).                         */
    /*        iErr-Status = OS-ERROR.                                                                           */
    /*        IF iErr-Status <> 0 THEN                                                                          */
    /*        DO:                                                                                               */
    /*          bResult = FALSE.                                                                                */
    /*          rStandardFunksjoner:SkrivTilLogg(cLogg,                                                         */
    /*              '    OS-ERROR: ' + STRING(iErr-Status) + ' ' + rStandardFunksjoner:Error-Status(iErr-Status)*/
    /*              ).                                                                                          */
    /*        END.                                                                                              */
    /*        ELSE                                                                                              */
    /*          bResult = TRUE.                                                                                 */
    /*    END.                                                                                                  */

    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Leser inn og vasker loggfilen. tar bare vare på unike ean og butikknr.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL lesLoggFil(  ):    
    DEFINE VARIABLE bResult AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE pcEan   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pibutNr AS INTEGER   NO-UNDO.

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
          pcEan
          ^
          ^
          piButNr.
        IF NOT CAN-FIND(ttEan WHERE 
          ttEan.Ean   = pcEan AND 
          ttEan.ButNr = piButNr 
          ) AND 
          CAN-FIND(Strekkode WHERE 
          Strekkode.Kode = pcEan)THEN 
        DO:
          FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = pcEan NO-ERROR.
          CREATE ttEan. 
          ASSIGN 
            ttEan.Ean        = pcEan
            ttEan.ButNr      = piButNr
            ttEan.ArtikkelNr = Strekkode.ArtikkelNr
            ttEan.StrKode    = Strekkode.StrKode
            .
        END.  
      END. 
      INPUT STREAM Inn CLOSE.
      
      /* Preparerer tabellen med data. */
      IF CAN-FIND(FIRST ttEan) THEN
        bResult = TRUE.
      ELSE 
        bResult = FALSE.      
        
      /* Flytter filen til Bku. */
      OS-COPY VALUE(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log'))
        VALUE(cKatalogEkstra + cBku + REPLACE(cEksportFilEkstra,'.xml','.log')).
      IF SEARCH(SEARCH(cKatalogEkstra + cBku + REPLACE(cEksportFilEkstra,'.xml','.log'))) <> ? THEN 
        OS-DELETE VALUE(cKatalogEkstra + REPLACE(cEksportFilEkstra,'.xml','.log')).    
    END.
      
    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL prepAlle(  ):
    
    DEFINE VARIABLE bResult  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iAntall  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pcButLst AS CHARACTER NO-UNDO.    
    
    {syspara.i 50 65 4 pcButLst}

    FOR EACH Butiker NO-LOCK WHERE 
      CAN-DO(pcButLst,STRING(Butiker.butik)):

      FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.WebButikkArtikkel = TRUE:
          
        iAntall = 0.          
        FOR EACH ArtLag NO-LOCK WHERE 
          ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtLag.Butik = Butiker.Butik AND            
          ArtLag.LagAnt > 0 USE-INDEX StrKonv
          BREAK BY ArtLag.ArtikkelNr
                BY ArtLag.butik
                BY ArtLag.StrKode:
          
          FIND LAST Strekkode NO-LOCK WHERE 
            Strekkode.ArtikkelNr = ArtLag.ArtikkelNr AND 
            Strekkode.StrKode    = ArtLag.StrKode NO-ERROR.  
          
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris NO-LOCK WHERE 
              ArtPris.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
          
          /* Oppretter eksportrecord for den endrede posten */
          IF AVAILABLE Strekkode AND NOT CAN-FIND(FIRST ttEanPrep WHERE 
            ttEanPrep.Ean = ttEan.Ean AND 
            ttEanPrep.ButNr = ttEan.ButNr) THEN 
          DO:
            CREATE ttEanPrep.      
            ASSIGN 
              ttEanPrep.Ean    = Strekkode.Kode
              ttEanPrep.ButNr  = ArtLag.Butik
              ttEanPrep.Antall = IF ArtLag.Lagant >= 0 THEN ArtLag.Lagant ELSE 0
              ttEanPrep.Pris   = IF AVAILABLE ArtPris THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE 0
              .
/*            /* Oppretter eksportrecord for de øvrige */  */
/*            OPPRETT_STREKKODE:                           */
/*            FOR EACH bufStrekkode NO-LOCK WHERE          */
/*              bufStrekkode.ArtikkelNr = ttEan.ArtikkelNr:*/
/*              IF bufStrekkode.Kode <> ttEanPrep.Ean THEN */
/*              DO:                                        */
/*                CREATE bufttEanPrep.                     */
/*                BUFFER-COPY ttEanPrep                    */
/*                  EXCEPT Ean                             */
/*                  TO bufttEanPrep                        */
/*                  ASSIGN                                 */
/*                  bufttEanPrep.Ean = bufStrekkode.Kode   */
/*                  .                                      */
/*              END.                                       */
/*            END. /* OPPRETT_STREKKODE */                 */
          END.
        END. /* ArtLag */
      END. /* ArtBas. */
    END. /* Butiker loop. */
    bResult = TRUE.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Endrer navn på loggfilen slik at det ikke skrives mer i den kopien 
            som nå skal behandles.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOG prepLoggFil(  ):
    DEFINE VARIABLE pbOk  AS LOG       NO-UNDO.
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

    IF CAN-FIND(FIRST ttEan) THEN 
    DO:
      FOR EACH ttEan
        BREAK BY ttEan.Ean
        BY ttEan.ButNr:
        iAntall = 0.
        
        IF FIRST-OF(ttEan.Ean) THEN 
          FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = ttEan.Ean NO-ERROR.

        /* Sum lagerantall alle størrelser for butikk og farge. */ 
        IF AVAILABLE Strekkode THEN
          FOR EACH ArtLag NO-LOCK WHERE 
            ArtLag.ArtikkelNr = Strekkode.ArtikkelNr AND 
            ArtLag.Butik = ttEan.ButNr:
            ASSIGN 
              iAntall = iAntall + ArtLag.LagAnt
              .
          END.
        
        /* Henter prisen for butikkens profil. */
        IF AVAILABLE ArtPris THEN 
          RELEASE ArtPris.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = ttEan.ButNr NO-ERROR.
        IF AVAILABLE Butiker THEN 
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        
        /* Oppretter eksportrecord for den endrede posten */
        IF NOT CAN-FIND(FIRST ttEanPrep WHERE 
          ttEanPrep.Ean = ttEan.Ean AND 
          ttEanPrep.ButNr = ttEan.ButNr) THEN 
        DO:
          CREATE ttEanPrep.      
          ASSIGN 
            ttEanPrep.Ean    = ttEan.Ean
            ttEanPrep.ButNr  = ttEan.ButNr
            ttEanPrep.Antall = IF iAntall > 0 THEN iAntall ELSE 0
            ttEanPrep.Pris   = IF AVAILABLE ArtPris THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE 0
            .
          /* Oppretter eksportrecord for de øvrige */
          OPPRETT_STREKKODE:
          FOR EACH bufStrekkode NO-LOCK WHERE 
            bufStrekkode.ArtikkelNr = ttEan.ArtikkelNr:
            IF bufStrekkode.Kode <> ttEanPrep.Ean THEN 
            DO:
              CREATE bufttEanPrep.
              BUFFER-COPY ttEanPrep
                EXCEPT Ean
                TO bufttEanPrep
                ASSIGN 
                bufttEanPrep.Ean = bufStrekkode.Kode
                . 
            END. 
          END. /* OPPRETT_STREKKODE */
        END.
      END.
      bResult = TRUE.
    END.
    ELSE 
      bResult = TRUE.
    
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
    rStandardFunksjoner:prepKatalog(cKatalogEkstra,cBku).

    ASSIGN 
      cKatalogEkstra = cKatalogEkstra + '\'
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
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL slettTabell(  ):    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    EMPTY TEMP-TABLE ttEan.
    EMPTY TEMP-TABLE ttEanPrep.
    ASSIGN 
      bResult = TRUE.    

    RETURN bResult.
  END METHOD.

  DESTRUCTOR PUBLIC GoogleMerchantPrep ( ):

    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  PrepGoogleMerchant - DESTRUCTOR'
      ). 
  END DESTRUCTOR.

END CLASS.