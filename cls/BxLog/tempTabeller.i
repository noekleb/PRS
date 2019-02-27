
/*------------------------------------------------------------------------
    File        : tempTabeller.i
    Purpose     : Ønsker å kunne bruke temp tabellene uavhengig av datasett definisjonene.

    Syntax      :

    Description : Definisjon av nødvendige temp-tabeller.

    Author(s)   : Tom Nøkleby
    Created     : Mon Nov 06 16:02:44 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE tmpBxSupplier 
    FIELD Id AS INT64
    FIELD Supplierno AS INT      
    FIELD Suppliername AS CHARACTER 
    FIELD Address AS CHARACTER
    FIELD Postcode AS CHARACTER 
    FIELD Country AS CHARACTER 
    FIELD City AS CHARACTER 
    FIELD DelAddress AS CHARACTER 
    FIELD DelPostcode AS CHARACTER 
    FIELD DelCity AS CHARACTER 
    FIELD DelCountry AS CHARACTER 
    INDEX idxSupplierno AS UNIQUE PRIMARY Supplierno .  
    
DEFINE TEMP-TABLE tmpCompany
    FIELD Id AS INT64 
    FIELD CompanyName AS CHARACTER
    FIELD Warehouseno AS CHARACTER  
    INDEX idxCompanyId AS UNIQUE PRIMARY Warehouseno.
    
DEFINE TEMP-TABLE tmpBxProduct 
    FIELD Id          AS INT64 
    FIELD Productno   AS CHARACTER FORMAT "x(20)"
    FIELD ProductName AS CHARACTER FORMAT "x(30)"
    FIELD GTIN        AS CHARACTER FORMAT "x(20)"
    FIELD Price       AS DECIMAL     
    FIELD Cost        AS DECIMAL  
    FIELD Location    AS CHARACTER FORMAT "x(10)"
    FIELD Unit        AS INT64  
    FIELD UnitName    AS CHARACTER 
    FIELD Warehouse AS CHARACTER 
    INDEX idxProduct AS PRIMARY UNIQUE Warehouse Productno. 

DEFINE TEMP-TABLE tmpBxProductLog 
    FIELD Id          AS INT64 
    FIELD Productno   AS CHARACTER FORMAT "x(20)"
    FIELD ProductName AS CHARACTER FORMAT "x(30)"
    FIELD GTIN        AS CHARACTER FORMAT "x(20)"
    FIELD Price       AS DECIMAL     
    FIELD Cost        AS DECIMAL  
    FIELD Location    AS CHARACTER FORMAT "x(10)"
    FIELD Unit        AS INT64  
    FIELD UnitName    AS CHARACTER 
    FIELD Warehouseno AS CHARACTER 
    INDEX idxProduct AS PRIMARY UNIQUE Warehouseno Productno. 
    
DEFINE TEMP-TABLE tmpClientProfile
    FIELD Id AS INT64 
    FIELD CompanyId AS INT64
    FIELD Profilename AS CHARACTER FORMAT "x(30)"
    FIELD Comment AS CHARACTER FORMAT "x(30)"
    FIELD Language AS CHARACTER FORMAT "x(15)"
    INDEX idxCompanyId AS PRIMARY UNIQUE CompanyId.
    
DEFINE TEMP-TABLE tmpUser
    FIELD Id AS INT64
    FIELD ProfileId AS INT64
    FIELD Username AS CHARACTER 
    FIELD Password AS CHARACTER
    FIELD Fullname AS CHARACTER 
    FIELD ENABLED AS LOG 
    FIELD UserType AS CHARACTER 
    FIELD Warehouse AS CHARACTER 
    FIELD Employee AS INT64 
    FIELD Employeename AS CHARACTER 
    /*     
    FIELD ERPUserId AS CHARACTER 
    FIELD ERPUserName AS CHARACTER 
    FIELD Employee AS INTEGER  
    FIELD BatteryStatus AS CHARACTER 
    FIELD VERSION AS CHARACTER 
    FIELD OnWireless AS LOG 
    FIELD InDocking AS LOG 
    FIELD IPAddresses AS CHARACTER 
    FIELD ActiveScreen AS CHARACTER 
    FIELD PreviousScreen AS CHARACTER 
    FIELD AliveAt DATETIME 
    FIELD ScreenshotRequestedAt AS DATETIME 
    FIELD ScreenshotReceivedAt AS DATETIME 
    FIELD ERPCustomerno CHARACTER
    */ 
    INDEX idxUser AS PRIMARY UNIQUE Warehouse Username.           

DEFINE TEMP-TABLE tmpBxcustomer
    FIELD Id AS INT64
    FIELD Customerno AS INT64 
    FIELD Customername AS CHARACTER 
    /*
    FIELD Address]
    FIELD Postcode]
    FIELD City]
    FIELD Country]
    FIELD DelAddress]
    FIELD DelPostcode]
    FIELD DelCity]
    FIELD DelCountry]
    FIELD Credit]
    FIELD CreditDays]
    FIELD Mail]
    */
    INDEX idxBxCustomer AS PRIMARY UNIQUE CustomerNo.
    
DEFINE TEMP-TABLE tmpBxUnit
    FIELD Id AS INT64
    FIELD Productno AS CHARACTER FORMAT "x(30)"
    FIELD GTIN        AS CHARACTER FORMAT "x(20)"
    FIELD Price       AS DECIMAL     
    FIELD Unit        AS INT64  
    FIELD UnitName    AS CHARACTER 
    FIELD FLOAT       AS DECIMAL  
    INDEX idxUnit AS PRIMARY UNIQUE Productno Unit. 
        



    
    