
/*------------------------------------------------------------------------
    File        : asinitPlukklistePRS.p
    Purpose     : 

    Syntax      :

    Description : Server rutine som trigges fra appserver.

    Author(s)   : 
    Created     : Sat Feb 24 12:06:01 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\bxLog\tmpTblBxPickinglist.i}
{cls\BxLog\tmpDsBxPickinglist.i}        

DEFINE INPUT  PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBxPickinglist.

DEFINE VARIABLE cSystem             AS CHARACTER                      NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rPlukkliste AS cls.Plukking.Plukkliste NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).

rPlukkliste  = NEW cls.Plukking.Plukkliste( INPUT cLogg ).
rPlukkliste:posterPlukkliste( INPUT-OUTPUT DATASET dsBxPickinglist ).
