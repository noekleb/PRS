
/*------------------------------------------------------------------------
    File        : tmpTblregSupplier.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Sat Oct 27 18:12:17 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/****** Script for SelectTopNRows command from SSMS  ******/

DEFINE TEMP-TABLE tmpvSupplier NO-UNDO SERIALIZE-NAME "tmpvSupplier"
    FIELD nSeason AS INT64 
    FIELD nSupplierCode AS INT64 
    FIELD cSupplierName AS CHARACTER 
    FIELD cSupplierShort AS CHARACTER
    FIELD cVatNo AS CHARACTER
    FIELD cNotes AS CHARACTER
    FIELD cCurrency AS CHARACTER
    FIELD cRemarkLocal AS CHARACTER
    FIELD cRemarkPublic AS CHARACTER
    FIELD nDivider AS INT64 
    FIELD nMinTotalBuy AS INT64 
    FIELD nMinTotalBuyPerColour AS INT64 
    FIELD nDocCharge AS INT64 
    FIELD nDuty AS INT64 
    FIELD nFinance AS INT64 
    FIELD nFreight AS INT64 
    FIELD nGoetz AS INT64 
    FIELD nInsurance AS INT64 
    FIELD nRoyalty AS INT64 
    FIELD nSampleUpcharge AS INT64 
    FIELD cAddress1 AS CHARACTER
    FIELD cAddress2 AS CHARACTER
    FIELD cAddress3 AS CHARACTER
    FIELD cZip AS CHARACTER
    FIELD cCity AS CHARACTER
    FIELD cState AS CHARACTER
    FIELD cCountry AS CHARACTER
    FIELD cPhone AS CHARACTER
    FIELD cFax AS CHARACTER
    FIELD cFirstname AS CHARACTER
    FIELD cMiddlename AS CHARACTER
    FIELD cLastName AS CHARACTER
    FIELD cEmail AS CHARACTER
    FIELD dtLastChanged AS DATETIME
    INDEX idxvSupplier AS PRIMARY UNIQUE nSupplierCode nSeason
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
