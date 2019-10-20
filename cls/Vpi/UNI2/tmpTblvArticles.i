
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
DEFINE TEMP-TABLE tmpvArticles NO-UNDO SERIALIZE-NAME "tmpvArticles"
    FIELD nArtKey AS INT64
    FIELD cArtno AS CHARACTER FORMAT "x(30)"
    FIELD cArtName AS CHARACTER FORMAT "x(30)"
    FIELD nSeason AS INT64
    FIELD cConcept AS CHARACTER FORMAT "x(30)"
    FIELD MadeInCountry AS CHARACTER FORMAT "x(30)"
    FIELD dPriceFOB  AS DECIMAL  
    FIELD dPriceLC  AS DECIMAL 
    FIELD dPriceWholesale  AS DECIMAL 
    FIELD dPriceRetail  AS DECIMAL 
    FIELD dtLastChanged AS DATETIME 
    FIELD cDesc AS CHARACTER FORMAT "x(30)"
    FIELD cShortDesc AS CHARACTER FORMAT "x(30)"
    FIELD cSupplierArtno AS CHARACTER FORMAT "x(30)"
    FIELD nArtStructId AS INT64
    FIELD cArtStructName AS CHARACTER FORMAT "x(30)"
    FIELD nSupplierCode AS INT64
    FIELD ShippedFromCountry AS CHARACTER FORMAT "x(30)"
    FIELD cShippedFrom AS CHARACTER FORMAT "x(30)"
    FIELD nOrder AS INT64
    FIELD nWeight AS INT64
    FIELD isLocked AS LOG
    FIELD isGlobalOut AS LOG
    FIELD isLocalOut AS LOG
    FIELD isRemoved AS LOG
    FIELD nArtGroup AS INT64
    FIELD cArtGroup AS CHARACTER FORMAT "x(30)"
    FIELD nSubGroup AS INT64
    FIELD cSubGroup AS CHARACTER FORMAT "x(30)"
    FIELD nMainGroup AS INT64
    FIELD cMainGroup AS CHARACTER FORMAT "x(30)"
    FIELD nConcept AS INT64
    FIELD dPriceFOBconfirmed AS DATETIME 
    FIELD nDesignerId AS INT64
    FIELD cFirstName AS CHARACTER FORMAT "x(30)"
    FIELD cLastName AS CHARACTER FORMAT "x(30)"
    FIELD ArtSeasonstatus AS CHARACTER FORMAT "x(30)"
    FIELD isDeleted AS LOG
    FIELD cAttributeName1 AS CHARACTER FORMAT "x(30)"
    FIELD cAttributeName2 AS CHARACTER FORMAT "x(30)"
    FIELD nArtStatus AS INT64
    FIELD cArtStatus_name AS CHARACTER FORMAT "x(30)"
    FIELD cArtStatus_shortname AS CHARACTER FORMAT "x(30)"
    FIELD nPackingMode AS INT64
    FIELD cCurrency AS CHARACTER FORMAT "x(30)"
    FIELD BoxType1 AS CHARACTER FORMAT "x(30)"
    FIELD BoxSize1 AS CHARACTER FORMAT "x(30)"
    FIELD BoxQty1 AS CHARACTER FORMAT "x(30)"
    FIELD BoxType2 AS CHARACTER FORMAT "x(30)"
    FIELD BoxSize2 AS CHARACTER FORMAT "x(30)"
    FIELD BoxQty2 AS CHARACTER FORMAT "x(30)"
    FIELD BoxType3 AS CHARACTER FORMAT "x(30)"
    FIELD BoxSize3 AS CHARACTER FORMAT "x(30)"
    FIELD BoxQty3 AS CHARACTER FORMAT "x(30)"
    FIELD cPackConfig AS CHARACTER FORMAT "x(30)"
    FIELD isBRP AS LOG
    FIELD nMinBuy AS INT64
    INDEX idxvArticles AS PRIMARY UNIQUE nArtKey nSeason
    INDEX idxPris nArtKey cArtNo nSeason
    INDEX idxArtNo cArtNo nSeason 
    INDEX idxEndret dtLastChanged
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
