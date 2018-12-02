
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
DEFINE TEMP-TABLE tmpBxPickinglist SERIALIZE-NAME "tmpBxPickinglist"
    FIELD Id AS INT64 
    FIELD Orderno AS INT64 
    FIELD dDate AS DATETIME 
    FIELD DelDate AS DATETIME
    FIELD Ordertype AS CHARACTER FORMAT "x(10)"
    FIELD Orderstatus AS CHARACTER FORMAT "x(10)"
    FIELD Customerno AS INT64 
    FIELD Customername AS CHARACTER FORMAT "x(30)"
    FIELD Warehouseno AS CHARACTER FORMAT "x(10)"
    FIELD DelName AS CHARACTER FORMAT "x(30)"
    FIELD DelAddress AS CHARACTER FORMAT "x(30)"
    FIELD DelPostcode AS CHARACTER FORMAT "x(15)"
    FIELD DelCity AS CHARACTER FORMAT "x(20)"
    FIELD Orderlines AS INT64 
    FIELD RemainingPickQuantity AS CHARACTER  
    FIELD TotalPrice AS CHARACTER 
    FIELD TotalTax AS CHARACTER 
    FIELD OurRef AS CHARACTER FORMAT "x(30)"
    FIELD YourRef AS CHARACTER FORMAT "x(30)"
    FIELD CustPO AS CHARACTER FORMAT "x(10)"
    FIELD Contact AS CHARACTER FORMAT "x(30)"
    FIELD Projectno AS INT64 
    FIELD Projectname AS CHARACTER FORMAT "x(30)"
    FIELD Employeeno AS INT64 
    FIELD Employeename AS CHARACTER FORMAT "x(30)"
    FIELD cLocked AS CHARACTER 
    FIELD LockedBy AS CHARACTER 
    FIELD ColorCode AS CHARACTER FORMAT "x(20)"
    FIELD PlListeId AS DECIMAL 
    FIELD PlListeStatus AS INTEGER  
    INDEX idxBxPickinglist AS PRIMARY UNIQUE Orderno
    INDEX idxOrderno Orderno
    INDEX idxPlListeId PlListeId.

DEFINE TEMP-TABLE tmpBxPickinglistLine SERIALIZE-NAME "tmpBxPickinglistLine"
    FIELD Id AS INT64 
    FIELD Orderno AS INT64 
    FIELD Orderline AS INT64
    FIELD Productno AS CHARACTER FORMAT "x(30)"
    FIELD Productname AS CHARACTER FORMAT "x(30)"
    FIELD Location AS CHARACTER FORMAT "x(30)"
    FIELD Warehouseno AS CHARACTER FORMAT "x(30)"
    FIELD DelDate AS DATETIME 
    FIELD Comment AS CHARACTER FORMAT "x(30)"
    FIELD Price AS CHARACTER
    FIELD FreeQuantity AS CHARACTER
    FIELD ReservedQuantity AS CHARACTER
    FIELD PickingQuantity AS CHARACTER
    FIELD PickedQuantity AS CHARACTER
    FIELD Discount1 AS CHARACTER
    FIELD Discount2 AS CHARACTER
    FIELD Discount3 AS CHARACTER
    FIELD Unit AS INT64
    FIELD Unitname AS CHARACTER FORMAT "x(30)"
    FIELD Unitfactor AS CHARACTER
    FIELD GTIN AS CHARACTER FORMAT "x(30)"
    FIELD UseSerialno AS CHARACTER FORMAT "x(30)"
    FIELD PLU AS CHARACTER FORMAT "x(30)"
    FIELD Serialnomask AS CHARACTER FORMAT "x(30)"
    FIELD zusr_UnitQuantity AS CHARACTER FORMAT "x(30)"
    INDEX idxBxPickinglistLine AS PRIMARY UNIQUE Orderno Orderline.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
