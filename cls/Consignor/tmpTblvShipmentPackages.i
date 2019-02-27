
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

DEFINE TEMP-TABLE tmpvShipmentPackages NO-UNDO SERIALIZE-NAME "tmpvShipmentPackages"
    FIELD shi_ID AS INT64 
    FIELD shi_ref_OrderNumber AS CHARACTER 
    FIELD shi_number AS CHARACTER 
    FIELD pac_parcelNumber AS CHARACTER 
    INDEX idxshi_ID AS PRIMARY UNIQUE shi_ID 
    .
