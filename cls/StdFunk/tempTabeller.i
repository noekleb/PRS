
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
    FIELD Warehouseno AS CHARACTER 
    INDEX idxProduct AS PRIMARY UNIQUE Warehouseno Productno. 

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
    