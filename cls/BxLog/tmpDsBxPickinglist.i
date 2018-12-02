
/*------------------------------------------------------------------------
    File        : tmpDsStockCount.i
    Purpose     : 

    Syntax      :

    Description : Definisjoner av datasett for varetelling.

    Author(s)   : Tom Nøkleby
    Created     : Fri Dec 29 10:23:52 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE DATASET dsBxPickinglist SERIALIZE-NAME "dsBxPickinglist"
    FOR tmpBxPickinglist, tmpBxPickinglistLine
    DATA-RELATION drBxPickinglistLine FOR tmpBxPickinglist, tmpBxPickinglistLine RELATION-FIELDS (Orderno,Orderno) NESTED.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
