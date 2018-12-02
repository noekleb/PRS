
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

DEFINE TEMP-TABLE tmpSeasons NO-UNDO SERIALIZE-NAME "tmpSeasons"
    FIELD nSeason AS INT64 
    FIELD cSeasonName AS CHARACTER 
    FIELD nCompSeason AS INT64 
    FIELD cCompSeasonName AS CHARACTER 
    FIELD dtLastChanged AS DATETIME  
    INDEX idxSeason AS PRIMARY UNIQUE nSeason
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
