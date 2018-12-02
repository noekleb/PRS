
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
DEFINE DATASET dsStockCount SERIALIZE-NAME "dsStockCount"
    FOR tmpStockCount, tmpStockCountLine
    DATA-RELATION drStockCountLine FOR tmpStockCount, tmpStockCountLine RELATION-FIELDS (cId,cStockCount) NESTED.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
