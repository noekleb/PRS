
/*------------------------------------------------------------------------
    File        : asHentDataPRS.p
    Purpose     : 

    Syntax      :

    Description : Server rutine som trigges fra appserver.

    Author(s)   : 
    Created     : Sat Feb 24 12:06:01 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\bxLog\tmpTblStockCount.i}
{cls\BxLog\tmpDsStockCount.i}

DEFINE INPUT  PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsStockCount.

DEFINE VARIABLE cSystem             AS CHARACTER                      NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rTelleliste AS cls.Telling.Telleliste NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).

rTelleliste  = NEW cls.Telling.Telleliste( INPUT cLogg ) NO-ERROR.
rTelleliste:posterTelleliste( INPUT-OUTPUT DATASET dsStockCount ).
