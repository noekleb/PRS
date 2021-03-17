
/*------------------------------------------------------------------------
    File        : ttOrder.i
    Purpose     : 

    Syntax      :

    Description : Temp tabell definisjoner for ordre datasettet.

    Author(s)   : Tom Nøkleby
    Created     : Fri Oct 16 15:46:26 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrder NO-UNDO SERIALIZE-NAME 'order'
  FIELD orderid            AS DECIMAL FORMAT ">>>>>>>>>>>>9" 
  FIELD amount             AS DECIMAL 
  FIELD vat_amount         AS INTEGER  
  FIELD currency           AS CHARACTER 
  FIELD merchant_reference AS CHARACTER
  FIELD merchant_reference_2 AS CHARACTER 
  FIELD partial_payment    AS LOG FORMAT "true/false" 
  FIELD shipping_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD customer_id  AS CHARACTER 
  INDEX idxOrder AS UNIQUE PRIMARY orderId
  INDEX idxShipping_id shipping_id
  INDEX idxCustomer customer_id
  .
    
DEFINE TEMP-TABLE ttItems  NO-UNDO SERIALIZE-NAME 'item'
  FIELD orderid AS DECIMAL FORMAT ">>>>>>>>>>>>9" SERIALIZE-HIDDEN
  FIELD Id          AS CHARACTER 
  FIELD line_id     AS CHARACTER  
  FIELD Group_Id    AS CHARACTER SERIALIZE-HIDDEN
  FIELD description AS CHARACTER 
  FIELD quantity    AS INTEGER 
  FIELD amount      AS DECIMAL  
  FIELD vat_amount  AS DECIMAL 
  FIELD vat         AS DECIMAL
  INDEX idxItems AS UNIQUE PRIMARY orderid line_id 
  INDEX idxGroups Group_Id
  .

DEFINE TEMP-TABLE ttShipping_Option SERIALIZE-NAME 'shipping_option'
  FIELD orderid AS DECIMAL FORMAT ">>>>>>>>>>>>9" SERIALIZE-HIDDEN 
  FIELD shipping_id AS CHARACTER SERIALIZE-NAME 'id'
  FIELD line_id AS CHARACTER FORMAT "x(6)"
  FIELD amount AS DECIMAL
  FIELD vat_amount AS INTEGER 
  FIELD vat AS DECIMAL
  FIELD shipping_title AS CHARACTER SERIALIZE-NAME 'title'
  FIELD shipping_option_id AS CHARACTER SERIALIZE-NAME 'id'
  FIELD description AS CHARACTER FORMAT "x(30)"
  FIELD delivery_method AS CHARACTER
  INDEX idxShipping_Option AS UNIQUE PRIMARY orderid shipping_id line_id
  .

DEFINE TEMP-TABLE ttGroups  NO-UNDO SERIALIZE-NAME 'groups'
  FIELD Group_Id   AS CHARACTER SERIALIZE-NAME 'id'
  FIELD GroupName AS CHARACTER SERIALIZE-NAME 'name' 
  INDEX idxGroups AS UNIQUE PRIMARY Group_Id 
  .
        
DEFINE TEMP-TABLE ttCustomer  NO-UNDO SERIALIZE-NAME 'customer'
  FIELD customer_id  AS CHARACTER 
  FIELD email        AS CHARACTER 
  FIELD phone_number AS CHARACTER 
  INDEX idxCustomer AS UNIQUE PRIMARY customer_id   
  .   

DEFINE TEMP-TABLE ttPick_Up_Address NO-UNDO  SERIALIZE-NAME 'pick_up_adress' 
  FIELD orderid AS DECIMAL FORMAT ">>>>>>>>>>>>9" 
  FIELD shipping_id AS CHARACTER SERIALIZE-NAME 'id'
  FIELD line_id AS CHARACTER FORMAT "x(6)"
  FIELD address_line AS CHARACTER FORMAT "x(30)"
  FIELD address_line_2 AS CHARACTER FORMAT "x(30)"
  FIELD business_name AS CHARACTER FORMAT "x(30)"
  FIELD postal_code AS CHARACTER FORMAT "x(10)"
  FIELD postal_place AS CHARACTER FORMAT "x(20)"
  FIELD country AS CHARACTER FORMAT "x(15)"
  FIELD phone_number AS CHARACTER FORMAT "x(15)"
  FIELD email AS CHARACTER FORMAT "x(30)"
  FIELD comment AS CHARACTER FORMAT "x(40)"
  INDEX idxPick_Up_Address AS UNIQUE PRIMARY orderid shipping_id line_id 
  .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
