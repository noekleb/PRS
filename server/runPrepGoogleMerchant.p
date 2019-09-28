
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
/* Setter parametre for behandling av loggfil for ekstrafeed. */
rGoogleMerchantPrep:setVariablerEkstraLogg().

/* Er logging av ekstrafeed aktivert, kjøres preparering av loggfilen. */
IF rGoogleMerchantPrep:iAktivEkstraLogg = 1 THEN 
  DO:
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          'PrepGoogleMerchant - prepLoggFil()'
          ). 
    IF rGoogleMerchantPrep:prepLoggFil() THEN 
      DO:
        rGoogleMerchantPrep:lesLoggFil().
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
    