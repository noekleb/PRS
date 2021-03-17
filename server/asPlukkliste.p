
/*------------------------------------------------------------------------
    File        : asPlukkliste.p 
    Purpose     : 

    Syntax      :

    Description : Kjører plukkliste fra kassene.

    Author(s)   : tomn
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER bNullstill AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER bOk AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding AS CHARACTER NO-UNDO.

DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTid      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cButNr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lPlListeId AS DECIMAL FORMAT ">>>>>>>9" NO-UNDO.
DEFINE VARIABLE cfilnavn AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rPlukkliste AS cls.Plukking.Plukkliste NO-UNDO.

ASSIGN
  iTid  = TIME
  cLogg = 'asPlukkliste' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rPlukkliste = NEW cls.Plukking.Plukkliste ( cLogg ).

IF SEARCH('test.txt') <> ? THEN 
  bTest = TRUE.
btest = TRUE.

FIND Butiker NO-LOCK WHERE 
  Butiker.butik = iButNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
DO:
  ASSIGN 
    bOk = FALSE 
    cMelding = '**Ukjent butikk ' + STRING(iButNr) + '.'
    .
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Ukjent Butikk: ' + STRING(iButNr) + '. Avbryter. ' 
      ).
  RETURN.
END.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start.' 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Butikk: ' + STRING(iButNr) + ' ' + Butiker.butnamn 
      ).
END.

/* Kjører generering av ordreforslag. */
IF rPlukkliste:genPlukkListeFraTranslogg( iButNr,
                              0,
                              20,
                              bNullstill,                                           
                              OUTPUT lplListeId,
                              OUTPUT cMelding  
                             ) THEN 
  bOk = TRUE.
ELSE 
  bOk = FALSE.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Response generering: ' + STRING(TIME - iTid,"HH:MM:SS") + ' Status: ' + STRING(bOk) + '.'
      ).

/* Sender plukklisten på mail. */  
IF lplListeId > 0 THEN 
  RUN skrivplukkliste.p (lPlListeId, iButNr, 3, OUTPUT cfilnavn).

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Filnavn: ' + cfilnavn + '.'
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Tidsbruk: ' + STRING(TIME - iTid,"HH:MM:SS") + ' Status: ' + STRING(bOk) + '.'
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt.' 
      ).
END.



