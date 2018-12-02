CURRENT-WINDOW:WIDTH = 300.

DEFINE VARIABLE cSourceType             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRetOK                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hDSet                   AS HANDLE    NO-UNDO.
 
DEF VAR hReITradeItemPriceItem AS HANDLE NO-UNDO.
DEF VAR hReIPriceCostPrice AS HANDLE NO-UNDO.
DEF VAR hReIPriceRetailPrice AS HANDLE NO-UNDO.
DEF VAR hReITradeItemPriceHeader AS HANDLE NO-UNDO.
DEF VAR hReItemSalesRestriction AS HANDLE NO-UNDO.
DEF VAR hReItemCharacteristic AS HANDLE NO-UNDO.
DEF VAR hRelItemDepositItem AS HANDLE NO-UNDO.
DEF VAR hRelItemSupplierItem AS HANDLE NO-UNDO.
DEF VAR hRelItemProperty AS HANDLE NO-UNDO.
DEF VAR hRelItemPrice AS HANDLE NO-UNDO.
DEF VAR hRelItemAdditionalGTIN AS HANDLE NO-UNDO.

DEFINE VARIABLE chMode                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chFile                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chFormatted           AS LOG NO-UNDO.
DEFINE VARIABLE chOmit-Initial-Values AS CHAR  NO-UNDO.

DEFINE TEMP-TABLE VPITradeItemPrice NO-UNDO SERIALIZE-NAME "TradeItemPrice"
    FIELD Dummy   AS CHAR
    INDEX Idx1    AS PRIMARY UNIQUE Dummy.

DEFINE TEMP-TABLE VPIPrice NO-UNDO SERIALIZE-NAME "Price"
    FIELD itemID  AS CHAR INITIAL ? 
    FIELD Dummy AS CHAR
    INDEX Idx1 AS PRIMARY UNIQUE Dummy.

DEFINE TEMP-TABLE VPIHeader NO-UNDO SERIALIZE-NAME "header"
    FIELD Dummy AS CHAR INITIAL ?
    FIELD TimeStamp AS CHAR
    FIELD SellerGLN AS CHAR
    FIELD BrokerGLN AS CHAR
    FIELD CustomerGLN AS CHAR
    FIELD CustomerID AS CHAR
    INDEX Idx1 AS PRIMARY UNIQUE Dummy TimeStamp.

/* Salgsrestriksjoner som henger på iTem */		
DEFINE TEMP-TABLE VPIItem NO-UNDO SERIALIZE-NAME "Item"
    FIELD Dummy AS CHAR INITIAL ?
    FIELD itemID AS CHAR 
    FIELD ArticleID AS CHAR 
    FIELD Description AS CHAR   
    FIELD Brand AS CHAR   
    FIELD DescriptionShort AS CHAR   
    FIELD WeightVolume AS CHAR   
    FIELD PackageTypeCode AS CHAR   
    FIELD PackageTypeDescription AS CHAR   
    FIELD Label1Name AS CHAR   
    FIELD Label1Text1 AS CHAR   
    FIELD Label1Text2 AS CHAR   
    FIELD Label2Name AS CHAR   
    FIELD LabelPriceCompareUnit AS CHAR   
    FIELD ReplacedItemID AS CHAR   
    FIELD UOM AS CHAR   
    FIELD SeasonCategory AS CHAR   
    FIELD HierarchyLevel AS INT   
    FIELD ArticleHierarchy AS CHAR   
    FIELD StartAvailable AS DATE   
    FIELD EndAvailability AS DATE   
    FIELD Warranty AS CHAR   
    FIELD WeightItem AS CHAR   
    FIELD NetContent AS DEC   
    FIELD NetContentUnit AS CHAR   
    FIELD ComparePriceFactor AS DEC   
    FIELD ExposureCode AS CHAR   
    FIELD CountryOfOrigin AS CHAR   
    FIELD GrossWeight AS DEC   
    FIELD AgeLimit AS INT   
    FIELD NetWeight AS DEC   
    FIELD WeightUnit AS CHAR   
    FIELD Volume AS CHAR   
    FIELD VolumeUnit AS CHAR       
    FIELD Length AS DEC   
    FIELD Width AS DEC   
    FIELD Height AS DEC   
    FIELD MeasurementUnit AS CHAR   
    FIELD TaxCode AS CHAR   
    FIELD TaxRate AS CHAR   
    INDEX Idx1 AS PRIMARY UNIQUE itemID.

/* Salgsrestriksjoner som henger på iTem */		
DEFINE TEMP-TABLE VPISalesRestriction NO-UNDO SERIALIZE-NAME "SalesRestriction"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD ID          AS CHAR 
    FIELD Description AS CHAR   
    INDEX Idx1 AS PRIMARY UNIQUE itemID ID.

/* Karrakteristikk som henger på iTem */		
DEFINE TEMP-TABLE VPICharacteristic NO-UNDO SERIALIZE-NAME "Characteristic"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD ID          AS CHAR 
    FIELD Description AS CHAR   
    INDEX Idx1 AS PRIMARY UNIQUE itemID ID.

/* DepositItem som henger på iTem */		
DEFINE TEMP-TABLE VPIDepositItem NO-UNDO SERIALIZE-NAME "DepositItem"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD ID          AS CHAR 
    FIELD Price       AS CHAR   
    FIELD Quantity    AS INT 
    INDEX Idx1 AS PRIMARY UNIQUE itemID ID.

/* Supplier som henger på iTem */		
DEFINE TEMP-TABLE VPISupplierItem NO-UNDO SERIALIZE-NAME "SupplierItem"
    FIELD itemID         AS CHAR INITIAL ? 
    FIELD SupplierID     AS CHAR 
    FIELD SupplierItemID AS CHAR 
    FIELD SupplierName   AS CHAR 
    FIELD GTIN           AS CHAR 
    FIELD UOM            AS CHAR 
    FIELD Preffered      AS LOG 
    FIELD ExpirationDate AS CHAR 
    FIELD RemovalFlag    AS CHAR 
    FIELD StockItemCount AS INT 
    FIELD PalletType     AS CHAR 
    FIELD PalletSize     AS DEC 
    INDEX Idx1 AS PRIMARY UNIQUE itemID SupplierID.

/* Property som henger på iTem og Supplier */		
DEFINE TEMP-TABLE VPIProperty NO-UNDO SERIALIZE-NAME "Property"
    FIELD itemID     AS CHAR INITIAL ? 
    FIELD SupplierID AS CHAR INITIAL ?
    FIELD PropValue  AS CHAR SERIALIZE-NAME "Value"
    INDEX Idx1 AS PRIMARY itemID SupplierID PropValue.

/* Kostpriser som henger på iTem */		
DEFINE TEMP-TABLE VPICostPrice NO-UNDO SERIALIZE-NAME "CostPrice"
    FIELD itemID      AS CHAR INITIAL ? 
    FIELD Dummy AS CHAR INITIAL ?
    FIELD StartDate AS DATE 
    FIELD Amount    AS DEC  
    INDEX Idx1 AS PRIMARY UNIQUE ItemId Dummy StartDate.

/* Kostpriser som henger på iTem */		
DEFINE TEMP-TABLE VPIRetailPrice NO-UNDO SERIALIZE-NAME "RetailPrice"
    FIELD itemID    AS CHAR INITIAL ? 
    FIELD Dummy     AS CHAR INITIAL ?
    FIELD StartDate AS DATE 
    FIELD Amount    AS DEC  
    INDEX Idx1 AS PRIMARY UNIQUE ItemID Dummy StartDate.

/* Strekkoder som henger på iTem */		
DEFINE TEMP-TABLE VPIAdditionalGTIN NO-UNDO SERIALIZE-NAME "AdditionalGTIN"
    FIELD itemID AS CHAR INITIAL ? 
    FIELD GTIN AS CHAR 
    INDEX Idx1 AS PRIMARY UNIQUE itemID GTIN.

CREATE DATASET hDset.
hDset:NAME = "DsetTradeItemPrice". 

/*hDset:ADD-BUFFER(BUFFER VPITradeItemPrice:HANDLE).*/

hDset:ADD-BUFFER(BUFFER VPIHeader:HANDLE).
/*
hDset:ADD-BUFFER(BUFFER VPIItem:HANDLE).
hDset:ADD-BUFFER(BUFFER VPISalesRestriction:HANDLE).
hDset:ADD-BUFFER(BUFFER VPICharacteristic:HANDLE).
hDset:ADD-BUFFER(BUFFER VPIDepositItem:HANDLE).
hDset:ADD-BUFFER(BUFFER VPISupplierItem:HANDLE).
hDset:ADD-BUFFER(BUFFER VPIProperty:HANDLE).
hDset:ADD-BUFFER(BUFFER VPICostPrice:HANDLE).
hDset:ADD-BUFFER(BUFFER VPIRetailPrice:HANDLE).
hDset:ADD-BUFFER(BUFFER VPIAdditionalGTIN:HANDLE).
hDset:ADD-BUFFER(BUFFER VPIPrice:HANDLE).
*/

/*
hReITradeItemPriceHeader = hDset:ADD-RELATION(BUFFER VPITradeItemPrice:HANDLE, 
                                             BUFFER VPIHeader:HANDLE, 
                                             "Dummy,Dummy",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE
                                             ). 
*/
/*
hReITradeItemPriceItem = hDset:ADD-RELATION(BUFFER VPITradeItemPrice:HANDLE, 
                                             BUFFER VPIItem:HANDLE, 
                                             "Dummy,Dummy",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE
                                             ). 

hReItemSalesRestriction = hDset:ADD-RELATION(BUFFER VPIItem:HANDLE, 
                                             BUFFER VPISalesRestriction:HANDLE, 
                                             "itemID,itemID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE
                                             ). 
hReItemCharacteristic   = hDset:ADD-RELATION(BUFFER VPIItem:HANDLE, 
                                             BUFFER VPICharacteristic:HANDLE, 
                                             "itemID,itemID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 
hRelItemDepositItem     = hDset:ADD-RELATION(BUFFER VPIItem:HANDLE, 
                                             BUFFER VPIDepositItem:HANDLE, 
                                             "itemID,itemID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 
hRelItemSupplierItem    = hDset:ADD-RELATION(BUFFER VPIItem:HANDLE, 
                                             BUFFER VPISupplierItem:HANDLE, 
                                             "itemID,itemID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 
hRelItemProperty        = hDset:ADD-RELATION(BUFFER VPISupplierItem:HANDLE,
                                             BUFFER VPIProperty:HANDLE, 
                                             "itemID,itemID,SupplierID,SupplierID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 

hRelItemPrice       = hDset:ADD-RELATION(BUFFER VPIItem:HANDLE, 
                                             BUFFER VPIPrice:HANDLE, 
                                             "itemID,itemID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 

hReIPriceCostPrice     = hDset:ADD-RELATION(BUFFER VPIPrice:HANDLE, 
                                             BUFFER VPICostPrice:HANDLE, 
                                             "itemID,itemID,Dummy,Dummy",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 
hReIPriceRetailPrice   = hDset:ADD-RELATION(BUFFER VPIPrice:HANDLE, 
                                             BUFFER VPIRetailPrice:HANDLE, 
                                             "itemID,itemID,Dummy,Dummy",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE). 

hRelItemAdditionalGTIN  = hDset:ADD-RELATION(BUFFER VPIItem:HANDLE, 
                                             BUFFER VPIAdditionalGTIN:HANDLE, 
                                             "itemID,itemID",
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE,
                                             TRUE).                                                                                 

 MESSAGE 'Test-0-2 Write Schema'
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
 /* Skriver xsd fil */
ASSIGN  
  chMode                   = "file"
  chFile                   = "wrk\TradeItemPrice_liten.xsd"
  chFormatted              = TRUE
  chOmit-Initial-Values    = ?
  .

lRetOK = hDSet:WRITE-XMLSCHEMA(chMode,
                         chFile, 
                         chFormatted,  
                         ?,FALSE).



 MESSAGE 'Test-0-1 Create data'
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Laster inn data i datasett. */
DEF VAR piLoop AS INT NO-UNDO.
/*
CREATE VPITradeItemPrice.
ASSIGN
    VPITradeItemPrice.Dummy = 'Gurre'.

CREATE VPIHeader.
ASSIGN
    /*VPIHeader.Dummy       = VPITradeItemPrice.Dummy*/
    VPIHeader.TimeStamp   = "20130703003908"
    VPIHeader.SellerGLN   = "7320240010014"
    VPIHeader.BrokerGLN   = "1111111111111"
    VPIHeader.CustomerGLN = "0000000000000"
    VPIHeader.CustomerID  = "200553"
    .


DO piLoop = 1 TO 2:
    CREATE VPIItem.
    ASSIGN
        VPIItem.Dummy       = VPITradeItemPrice.Dummy
        VPIItem.ItemId      = STRING(piLoop)
        VPIItem.ArticleID   = STRING(piLoop)
        VPIItem.DESCRIPTION = STRING(piLoop)
        .
    CREATE VPISalesRestriction.
    ASSIGN
        VPISalesRestriction.ItemId      = VPIItem.ItemId
        VPISalesRestriction.Id          = STRING(piLoop)
        VPISalesRestriction.Description = 'SalesRestriction' +  STRING(piLoop).

    CREATE VPICharacteristic.
    ASSIGN
        VPICharacteristic.ItemId      = VPIItem.ItemId
        VPICharacteristic.Id          = STRING(piLoop)
        VPICharacteristic.Description = 'Characteristic' +  STRING(piLoop).

    CREATE VPIDepositItem.
    ASSIGN
        VPIDepositItem.ItemId      = VPIItem.ItemId
        VPIDepositItem.Id          = STRING(piLoop)
        VPIDepositItem.Price       = STRING(piLoop * 3.5).

    CREATE VPISupplierItem.
    ASSIGN
        VPISupplierItem.ItemId          = VPIItem.ItemId
        VPISupplierItem.SupplierID      = STRING(piLoop)
        VPISupplierItem.SupplierItemID  = STRING(piLoop * 3)
        VPISupplierItem.SupplierName = 'SupplierName' + STRING(piLoop).

    CREATE VPIProperty.
    ASSIGN
        VPIProperty.ItemId     = VPISupplierItem.ItemId 
        VPIProperty.SupplierId = VPISupplierItem.SupplierID
        VPIProperty.PropValue  = 'Prop1'
        .

    CREATE VPIProperty.
    ASSIGN
        VPIProperty.ItemId     = VPISupplierItem.ItemId 
        VPIProperty.SupplierId = VPISupplierItem.SupplierID
        VPIProperty.PropValue  = 'Prop2'
        .
    
    CREATE VPISupplierItem.
    ASSIGN
        VPISupplierItem.ItemId          = VPIItem.ItemId
        VPISupplierItem.SupplierID      = '10000' + STRING(piLoop)
        VPISupplierItem.SupplierItemID  = STRING(piLoop * 3)
        VPISupplierItem.SupplierName = 'SupplierName' + STRING(piLoop).
    
    CREATE VPIProperty.
    ASSIGN
        VPIProperty.ItemId     = VPISupplierItem.ItemId 
        VPIProperty.SupplierId = VPISupplierItem.SupplierID
        VPIProperty.PropValue  = 'Prop1'
        .

    CREATE VPIPrice.
    ASSIGN
        VPIPrice.ItemId = VPIItem.ItemId
        VPIPrice.Dummy  = 'Gurre' + STRING(piLoop * 3)
        .

    CREATE VPICostPrice.
    ASSIGN
        VPICostPrice.ItemId    = VPIPrice.ItemId
        VPICostPrice.Dummy     = VPIPrice.Dummy
        VPICostPrice.StartDate = TODAY
        VPICostPrice.Amount    = 123.45
        .

    CREATE VPIRetailPrice.
    ASSIGN
        VPIRetailPrice.ItemId    = VPIPrice.ItemId
        VPIRetailPrice.Dummy     = VPIPrice.Dummy
        VPIRetailPrice.StartDate = TODAY
        VPIRetailPrice.Amount    = 700.30
        .

END.
*/
 MESSAGE 'Test-1 Write xml'
     VIEW-AS ALERT-BOX INFO BUTTONS OK.


ASSIGN  
  chMode                   = "file"
  chFile                   = "wrk\TradeItemPrice_liten2.xml"
  chFormatted              = TRUE
  chOmit-Initial-Values    = ?
  .

lRetOK = hDSet:WRITE-XML(chMode,                         
                         chFile, 
                         chFormatted,  
                         ?).

 MESSAGE 'Test-2 Read xml'
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

ASSIGN  
  cSourceType             = "file"
  cFile                   = "wrk\TradeItemPrice_liten_small.xml" /*  "wrk\TradeItemPrice_liten2.xml" */
  cReadMode               = "empty"
  cSchemaLocation         = "wrk\TradeItemPrice_liten.xsd" /* "wrk\TradeItemPrice_liten.xsd" */
  lOverrideDefaultMapping = ?
  cFieldTypeMapping       = ?
  cVerifySchemaMode       = ?.
 
 ASSIGN lRetOK = hDSet:READ-XML(cSourceType, 
                         cFile, 
                         cReadMode, 
                         cSchemaLocation,  
                         lOverrideDefaultMapping, 
                         cFieldTypeMapping, 
                         cVerifySchemaMode)   .


 MESSAGE 'Test-3'
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

 FOR EACH VPIHeader:
     DISPLAY
         VPIHeader
     WITH WIDTH 300.
     FOR EACH VPIItem WHERE VPIItem.Dummy = VPIHeader.Dummy:
         DISPLAY
             VPIItem
         WITH WIDTH 300.
     END.
 END.

MESSAGE 'ferdig'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
