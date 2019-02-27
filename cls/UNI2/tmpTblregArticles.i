
/*------------------------------------------------------------------------
    File        : tmpTblregArticles.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Sat Oct 27 18:12:17 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/****** Script for SelectTopNRows command from SSMS  ******/

DEFINE TEMP-TABLE tmpregArticles NO-UNDO SERIALIZE-NAME "tmpregArticles"
    FIELD nArtKey AS INT64 
    FIELD cArtno AS CHARACTER 
    FIELD dtLastChanged AS DATETIME 
    INDEX idxregArticles AS PRIMARY UNIQUE nArtKey
    INDEX idxArtNo cArtNo
    INDEX idxKeyArt nArtKey cArtNo
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
