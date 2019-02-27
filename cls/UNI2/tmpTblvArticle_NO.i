
/*------------------------------------------------------------------------
    File        : tmpArticle.i
    Purpose     : 

    Syntax      :

    Description : Definisjon av temp tabell for Article.

    Author(s)   : Tom Nøkleby
    Created     : Fri Oct 5 13:30:14:44 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tmpvArticle_NO NO-UNDO SERIALIZE-NAME "tmpvArticle_NO"
    FIELD nArtKey         AS INT64 
    FIELD cArtno          AS CHARACTER FORMAT "x(30)"
    FIELD cEan            AS CHARACTER FORMAT "x(20)"
    FIELD cArtName        AS CHARACTER FORMAT "x(30)"
    FIELD nSeason         AS INT64
    FIELD cSeasonName     AS CHARACTER FORMAT "x(30)"
    FIELD nSupplierCode   AS INT64 
    FIELD cSupplierName   AS CHARACTER FORMAT "x(40)"
    FIELD MadeInCountry   AS CHARACTER FORMAT "x(30)"
    FIELD nArtGroup       AS INT64
    FIELD cArtGroup       AS CHARACTER FORMAT "x(30)"
    FIELD nSubGroup       AS INT64
    FIELD cSubGroup       AS CHARACTER FORMAT "x(30)"
    FIELD nMainGroup      AS INT64
    FIELD cMainGroup      AS CHARACTER FORMAT "x(30)"
    FIELD nConcept        AS INT64
    FIELD cConcept        AS CHARACTER FORMAT "x(30)"
    FIELD dPriceFOB       AS DECIMAL FORMAT "->>>>>>>>9.99"
    FIELD dPriceLC        AS DECIMAL FORMAT "->>>>>>>>9.99"
    FIELD dPriceWholesale AS DECIMAL FORMAT "->>>>>>>>9.99"
    FIELD dPriceRetail    AS DECIMAL FORMAT "->>>>>>>>9.99"
    FIELD nDesignerId     AS INT64
    FIELD ArtSeasonstatus AS CHARACTER FORMAT "x(30)"
    FIELD isDeleted       AS LOG
    FIELD dtLastChanged   AS DATETIME 
    FIELD cArtStructName  AS CHARACTER FORMAT "x(30)"
    FIELD isLocked        AS LOG
    FIELD isGlobalOut     AS LOG
    FIELD isLocalOut      AS LOG
    FIELD isRemoved       AS LOG 
    FIELD nColCode        AS INT64
    FIELD cColName        AS CHARACTER FORMAT "x(30)"
    FIELD cCode1          AS CHARACTER FORMAT "x(30)"
    FIELD cCode2          AS CHARACTER FORMAT "x(30)"
    FIELD cCode3          AS CHARACTER FORMAT "x(30)"
    INDEX idxEAN          AS PRIMARY cEAN
    INDEX idxArticle cArtNo cEan cCode1 
    INDEX idxEndret dtLastChanged
    INDEX idxBreakBy cArtno nColCode nSeason dtLastChanged 
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
