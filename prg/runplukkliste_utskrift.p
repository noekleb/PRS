
/*------------------------------------------------------------------------
    File        : runplukkliste_utskrift.p
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : tomn
    Created     : Wed Oct 23 16:48:36 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE cMelding AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
  cButLst = '2,3,6,9,11,12,13,14,15,17,18,19'
  .

DO iLoop = 1 TO NUM-ENTRIES(cButLst):
  iButNr = INT(ENTRY(iLoop,cButLst)).
  
  FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iButNr AND 
    Butiker.harButikksystem = TRUE NO-ERROR.
  IF AVAILABLE Butiker AND Butiker.ePostAdresse > '' THEN     
    RUN asPlukkliste.p (Butiker.Butik,TRUE, OUTPUT bOk, OUTPUT cMelding).    
  
END.