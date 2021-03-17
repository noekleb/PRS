
/*------------------------------------------------------------------------
    File        : prstransfer_getPRICE.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Tue Oct 15 09:39:55 CEST 2019
    Notes       : -db c:\db\gant\prstransfer -ld prstransfer -H localhost -S  10095 -U pub -P pub -N tcp
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE bConnected AS LOG NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rprsTransfer AS CLASS cls.prsTransfer.prsTransfer NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().

IF cLogg = '' THEN 
  cLogg = 'prstransfer_getPRICE' + REPLACE(STRING(TODAY),'/','') /*+ '_' + REPLACE(STRING(TIME),':','')*/
  .

rprsTransfer = NEW cls.prsTransfer.prsTransfer(cLogg).

IF SEARCH('test.txt') <> ? THEN 
  bTest = TRUE.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      'Start.' 
      ). 

bConnected = rprsTransfer:connectDB().
IF NOT bConnected THEN
DO:
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '  ** Kunne ikke koble opp DB - avslutter.' 
        ). 

  RETURN.
END.
ELSE  
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  DB oppkoblet.' 
      ). 


PAUSE 1 NO-MESSAGE.




bConnected = NOT rprsTransfer:disconnectDB().
IF bTest AND NOT bConnected THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  DB nedkoblet.' 
      ). 
ELSE        
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  ** Klarte ikke koble ned DB.' 
      ). 

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      'Slutt.' 
      ). 

/* **********************  Internal Procedures  *********************** */

