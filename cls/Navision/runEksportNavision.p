
/*------------------------------------------------------------------------
    File        : runEksportNavision.p
    Purpose     : 

    Syntax      :

    Description : Starter og kjører eksportklassen for Navisioon  

    Author(s)   : tomn
    Created     : Mon Sep 30 15:44:08 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rEksportNavision AS CLASS cls.Navision.EksportNavision NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

ASSIGN 
    cLogg       = 'EksportNavision' + REPLACE(STRING(TODAY),'/','')
    .
IF SEARCH('tnc.txt') <> ? THEN 
  bTest = TRUE.
  
rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'runEksportNavision - Init'
    ). 
rEksportNavision = NEW cls.Navision.EksportNavision( INPUT cLogg ).

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      'runEksportNavision - settParametre()'
      ). 
/* Setter parametre for behandling av loggfil for ekstrafeed. */
rEksportNavision:settParametre().

/* Er logging av ekstrafeed aktivert, kjøres preparering av loggfilen. */
IF rEksportNavision:iAktiv = 1 THEN 
  DO:
    
    FOR EACH Butiker NO-LOCK WHERE 
      Butiker.ApningsDato <> ? AND 
      Butiker.harButikksystem = TRUE AND 
      (Butiker.NedlagtDato = ? OR 
       Butiker.NedlagtDato <= TODAY) AND 
      NOT CAN-DO('848,849', STRING(Butiker.Butik)): 

      /* Looper rundt de dagene det skal eksporteres dagsoppgjør for. */
      DATOLOOP:
      DO dDato = DATE(1,1,YEAR(TODAY)) /*TODAY - 30*/ TO TODAY:
        obOk = FALSE.
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              'runEksportNavision - prepLoggFil()'
              ). 
        IF rEksportNavision:prepKatalog( ) THEN 
          DO:
            rEksportNavision:emptyTempFile( ).
            IF rEksportNavision:prepDagsrapp( 1, Butiker.butik, dDato ) /* Vanlige butikker */  THEN
                rEksportNavision:eksporterDagsrapp( 1, Butiker.butik ). 
  
            rEksportNavision:emptyTempFile( ).
            IF rEksportNavision:prepDagsrapp( 2, Butiker.butik, dDato ) /* Outlet butikker */ THEN   
                rEksportNavision:eksporterDagsrapp( 2, Butiker.butik ). 
            ASSIGN 
              obOk = TRUE.
          END.
        ELSE DO:
/*          rStandardFunksjoner:SkrivTilLogg(cLogg,                                                                                   */
/*            'Feil ved eksport av dagsoppgjør ' + STRING(dDato) + 'for butikk ' + STRING(Butiker.butik) + ' ' + Butiker.butNamn + '.'*/
/*            ).                                                                                                                      */
        END.
      END. /* DATOLOOP */    
    END. /* Butiker */
  END.
ELSE DO:
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          'runEksportNavision - ikke aktiv.'
          ). 
  END.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'runEksportNavision - Avslutt'
    ). 

QUIT.
    