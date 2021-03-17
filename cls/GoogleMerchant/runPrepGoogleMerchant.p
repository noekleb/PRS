
/*------------------------------------------------------------------------
    File        : runPrepGoogleMerchant.p
    Purpose     : Preparerer loggfilene og oppretter xml filer.

    Syntax      :

    Description : Preparerer loggfiler

    Author(s)   : tomn
    Created     : Sat Sep 28 10:42:30 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rGoogleMerchantPrep AS CLASS cls.GoogleMerchant.GoogleMerchantPrep NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

ASSIGN 
    cLogg       = 'PrepGoogleMerchant' + REPLACE(STRING(TODAY),'/','')
    .
IF SEARCH('tnc.txt') <> ? THEN 
  bTest = TRUE.
  
rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'PrepGoogleMerchant - Init'
    ). 
rGoogleMerchantPrep = NEW cls.GoogleMerchant.GoogleMerchantPrep( INPUT cLogg ).

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      'PrepGoogleMerchant - setVariablerEkstraLogg()'
      ). 
/* Er logging av ekstrafeed aktivert, kjøres preparering av loggfilen. */
IF rGoogleMerchantPrep:iAktivEkstraLogg = 1 THEN 
  DO:
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          'PrepGoogleMerchant - prepLoggFil()'
          ). 
    /* Endrer navn på loggfilen. */      
    IF rGoogleMerchantPrep:prepLoggFil() THEN 
      DO:
        /* Leser inn loggfilen i ttEan tabellen. */
        IF rGoogleMerchantPrep:lesLoggFil() THEN
        DO:
          /* Fyller på med data og ekspanderer slik at alle EAN for fargen kommer med. */
          IF rGoogleMerchantPrep:preppTabell() THEN 
          DO:
            /* Legger ut innsamlede data. */
            rGoogleMerchantPrep:eksporterTabell().
            /* Tømmer tabellen. */
            rGoogleMerchantPrep:slettTabell().
            
            /* Sender filen. */
            RUN cls\GoogleMerchant\runGoogleFtpSendfile.p.
          END.
        END. 
      END.
  END.
ELSE DO:
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          'PrepGoogleMerchant - EkstraFeed ikke aktiv.'
          ). 
  END.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'PrepGoogleMerchant - Avslutt'
    ). 

QUIT.
    