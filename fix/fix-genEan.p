
/*------------------------------------------------------------------------
    File        : fix-genEan.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Wed Jan 23 15:47:48 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cEan AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE STREAM UtFil.
OUTPUT STREAM UtFil TO value('konv\GantEanLst23012019.csv')
DO iLoop = 1 TO 10:
    RUN hentEAN.p (13,2,OUTPUT cEan).
    PUT STREAM UtFil UNFORMATTED
        cEan
    SKIP.   
END.      
OUTPUT STREAM UtFilClose.
