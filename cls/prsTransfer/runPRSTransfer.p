
/*------------------------------------------------------------------------
    File        : testRFIDEtikett.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 16 09:06:30 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEkstReturNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEkstOrdreNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iAntDg AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rprsTransfer        AS CLASS cls.prsTransfer.prsTransfer    NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
  iAntDg       = 30
  bTest        = TRUE 
  cLogg        = 'PRSTransfer' + REPLACE(STRING(TODAY),'/','')
  /* Initieres disse variablene, leses bare den ordre som har matchene EkstORdreNr. */
  cEkstOrdreNr = ''
  cEkstReturNr = '' 
  .

rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).
rprsTransfer  = NEW cls.prsTransfer.prsTransfer( INPUT cLogg ).

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start.' 
      ).    

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Parametre:' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cEkstOrdreNr: ' + cEkstOrdreNr 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    cEkstReturNr: ' + cEkstReturNr 
      ).    
END. 

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rprsTransfer = NEW cls.prsTransfer.prsTransfer( INPUT cLogg ).

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start setResendPHX' 
      ).    
rprsTransfer:setResendPHX().

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start lesSendteOrdre' 
      ).    
rprsTransfer:lesSendteOrdre(cEkstOrdreNr, iAntDg).

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start lesSendteReturer' 
      ).    
rprsTransfer:lesSendteReturer(cEkstReturNr, iAntDg).

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt.' 
      ).    

RETURN.

/* **********************  Internal Procedures  *********************** */

