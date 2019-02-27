
/*------------------------------------------------------------------------
    File        : tmpStockCount.i
    Purpose     : 

    Syntax      :

    Description : Definisjon av temp tabell for tellefilens hode og linjer.

    Author(s)   : Tom Nøkleby
    Created     : Fri Dec 29 10:14:44 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tmpStockCount SERIALIZE-NAME "tmpStockCount"
    FIELD cId AS CHARACTER 
    FIELD Name AS CHARACTER  
    FIELD Source AS CHARACTER
    FIELD bLocked AS LOG  
    FIELD cUSER AS CHARACTER  
    FIELD WareHouse AS CHARACTER 
    FIELD Received AS DATETIME   
    FIELD cCompany AS CHARACTER  
    FIELD cChild AS CHARACTER 
    FIELD cSTATUS AS CHARACTER 
    FIELD ImportId AS CHARACTER 
    FIELD Exported AS DATETIME  
    FIELD NumLines AS INTEGER 
    INDEX idxStockcount AS PRIMARY UNIQUE cCompany Name.

DEFINE TEMP-TABLE tmpStockCountLine SERIALIZE-NAME "tmpStockCountLine"
    FIELD cId AS CHARACTER 
    FIELD cProductno AS CHARACTER
    FIELD cQuantity AS CHARACTER
    FIELD cWarehouse AS CHARACTER
    FIELD cUNIT AS CHARACTER
    FIELD cLocation AS CHARACTER
    FIELD cStockCount AS CHARACTER
    FIELD cExpectedQuantity AS CHARACTER
    FIELD CountedTime AS DATETIME
    FIELD cProductname AS CHARACTER
    FIELD cUnitcost AS CHARACTER
    FIELD cUnitfactor AS CHARACTER
    FIELD cUnitname AS CHARACTER
    FIELD cImportId AS CHARACTER
    FIELD cGTIN AS CHARACTER
    FIELD cSTATUS AS CHARACTER
    INDEX idxStockCountLine AS PRIMARY UNIQUE cStockCount cId. 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
