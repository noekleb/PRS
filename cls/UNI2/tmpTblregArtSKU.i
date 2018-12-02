
/*------------------------------------------------------------------------
    File        : tmpTblSeasong.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Sat Oct 27 18:12:17 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/****** Script for SelectTopNRows command from SSMS  ******/

DEFINE TEMP-TABLE tmpregArtSKU NO-UNDO SERIALIZE-NAME "tmpregArtSKU"
    FIELD nSKU AS INT64
    FIELD nArtKey AS INT64
    FIELD nColCode AS INT64
    FIELD nSizeKey AS INT64
    FIELD dtLastChanged AS DATETIME 
    FIELD dtLastChanged_Stg AS DATETIME 
    INDEX idxSKU AS PRIMARY UNIQUE nSKU
    INDEX idxArt nArtKey nColCode nSizeKey  
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
