
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
    obOk = FALSE.
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          'runEksportNavision - prepLoggFil()'
          ). 
    IF rEksportNavision:prepKatalog() THEN 
      DO:
        IF rEksportNavision:lesDagsrapp() THEN 
        DO:
          IF rEksportNavision:eksportDagsrapp() THEN 
            ASSIGN 
              obOk = TRUE.
          
        END.
      END.
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
    