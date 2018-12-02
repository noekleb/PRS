
/*------------------------------------------------------------------------
    File        : tmpTblArtEan.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Sat Oct 27 18:12:17 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/****** Script for SelectTopNRows command from SSMS  ******/

DEFINE TEMP-TABLE tmpArtEan NO-UNDO SERIALIZE-NAME "tmpArtEan"
    FIELD nArtKey AS INT64 
    FIELD cArtNo AS CHARACTER 
    FIELD nEan AS DECIMAL 
    FIELD nSKU AS DECIMAL     
    FIELD dPriceFOB  AS DECIMAL  
    FIELD dPriceLC  AS DECIMAL 
    FIELD dPriceWholesale  AS DECIMAL 
    FIELD dPriceRetail  AS DECIMAL 
    INDEX idxArtEan AS PRIMARY UNIQUE nArtKey cArtNo nEan 
    INDEX idxSKUEan nSKU nEan 
    INDEX idxEan nEan
    .
