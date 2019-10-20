
/*------------------------------------------------------------------------
    File        : dsttImpFil.i.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu Oct 17 10:35:46 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttImpFil NO-UNDO  
  FIELD FilId AS INTEGER 
  FIELD FilNavn AS CHARACTER
  INDEX idxFilNavn FilNavn.
  
DEFINE TEMP-TABLE ttImpFilLinje NO-UNDO
  FIELD FilId AS INTEGER 
  FIELD LinjeNr AS INTEGER 
  FIELD Record AS CHARACTER
  INDEX FilId LinjeNr. 

DEFINE DATASET dsttImpFil SERIALIZE-NAME "dsttImpFil"
    FOR ttImpFil, ttImpFilLinje
    DATA-RELATION drttFil FOR ttImpFil, ttImpFilLinje RELATION-FIELDS (FilId,FilId) NESTED.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
