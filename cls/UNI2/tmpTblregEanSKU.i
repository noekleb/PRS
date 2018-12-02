
/*------------------------------------------------------------------------
    File        : tmpTblregEanSKU.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Sat Oct 27 18:12:17 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/****** Script for SelectTopNRows command from SSMS  ******/

DEFINE TEMP-TABLE tmpregEanSKU NO-UNDO SERIALIZE-NAME "tmpregEanSKU"
    FIELD nEan AS INT64
    FIELD nSKU AS INT64
    FIELD dtLastChanged AS DATETIME 
    FIELD dtLastChanged_Stg AS DATETIME 
    INDEX idxSKU AS PRIMARY UNIQUE nSKU
    INDEX idxEan nEan 
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
