/*---------------chprimary ar ---------------------------------------------------------
    File        : ttReceipts.i
    Purpose     : Inklude for felles definisjon av kvittering.

    Syntax      :

    Description : Definerer kvittering.

    Author(s)   : Tom Nøkleby
    Created     : Mon Nov 16 10:42:25 CET 2020
    Notes       :
      
    Minimumm:
  ----------------------------------------------------------------------
   [
    {
      "store": {
        "id": "sc029",
        "name": "SC Oslo"
      },
      "receipt_id": "string",
      "gross_amount": 59800,
      "net_amount": 47840,
      "round_off_to_coin": 100,
      "currency": "NOK",
      "purchase_at": "2020-11-12T19:47:10Z"
    } 
   ]
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttReceipts NO-UNDO SERIALIZE-NAME 'receipts'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)"
  FIELD gross_amount AS INTEGER FORMAT ">>>>>>>>9"
  FIELD net_amount AS INTEGER FORMAT ">>>>>>>>9"
  FIELD round_off_to_coin AS INTEGER FORMAT ">>9"
  FIELD currency AS CHARACTER FORMAT "x(5)"
  FIELD purchase_at AS CHARACTER FORMAT "x(25)"
  FIELD receipt_text AS CHARACTER 
  FIELD no_of_items AS INTEGER FORMAT ">>>9"
  FIELD total_discount AS INTEGER FORMAT ">>>>>>>>9"
  FIELD operator_id AS CHARACTER
  FIELD operator_name AS CHARACTER
  FIELD salesperson_id AS CHARACTER
  FIELD salesperson_name AS CHARACTER
  FIELD comment AS CHARACTER
  FIELD customer_id AS CHARACTER   
  FIELD is_changed AS LOG
  FIELD lcDiscounts AS CLOB SERIALIZE-HIDDEN
  INDEX idxReceipts AS PRIMARY UNIQUE receipt_id
  . 

DEFINE TEMP-TABLE ttStore NO-UNDO SERIALIZE-NAME 'store'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD id AS CHARACTER FORMAT "x(10)"
  FIELD cName AS CHARACTER FORMAT "x(40)" SERIALIZE-NAME 'name'
  FIELD organization_number AS CHARACTER FORMAT "x(30)"
  INDEX idxStore AS PRIMARY UNIQUE receipt_id Id
  .
  
DEFINE TEMP-TABLE ttStoreAdress NO-UNDO SERIALIZE-NAME 'adress'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD store_id AS CHARACTER FORMAT "x(10)" SERIALIZE-HIDDEN
  FIELD address_line AS CHARACTER FORMAT "x(40)"
  FIELD address_line_2 AS CHARACTER FORMAT "x(40)"
  FIELD postal_code AS CHARACTER FORMAT "x(20)"
  FIELD postal_place AS CHARACTER FORMAT "x(30)"
  FIELD country AS CHARACTER FORMAT "x(10)"
  INDEX idxStoreAdress AS PRIMARY receipt_id store_id address_line   
  .
  
DEFINE TEMP-TABLE ttPayment NO-UNDO SERIALIZE-NAME 'payments'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>9"
  FIELD amount AS INTEGER FORMAT ">>>>>>>9"
/*  FIELD type_id AS CHARACTER FORMAT "x(30)"   */
/*  FIELD token_id AS CHARACTER FORMAT "x(30)"  */
/*  FIELD token_type AS CHARACTER FORMAT "x(20)"*/
  FIELD cDescription AS CHARACTER FORMAT "x(40)" SERIALIZE-NAME 'description' 
  FIELD voided AS LOG FORMAT "true/false"
/*  FIELD reference_id AS CHARACTER FORMAT "x(20)"*/
  INDEX idxPayments AS PRIMARY UNIQUE receipt_id line_id
  .

DEFINE TEMP-TABLE ttTax_Lines NO-UNDO SERIALIZE-NAME 'tax_lines'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD tax_code AS CHARACTER 
  FIELD amount AS INTEGER 
  FIELD exempt AS LOG 
  FIELD included_in_price AS LOG 
  FIELD percentage AS INTEGER 
  FIELD tax_basis AS INTEGER 
  FIELD tax_group AS CHARACTER 
  INDEX idxTax_Lines AS PRIMARY UNIQUE receipt_id tax_code
  . 

DEFINE TEMP-TABLE ttItemTax_Lines NO-UNDO SERIALIZE-NAME 'tax_lines'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>>9" SERIALIZE-HIDDEN
  FIELD tax_code AS CHARACTER 
  FIELD amount AS INTEGER 
  FIELD exempt AS LOG 
  FIELD included_in_price AS LOG 
  FIELD percentage AS INTEGER 
  FIELD tax_basis AS INTEGER 
  FIELD tax_group AS CHARACTER 
  INDEX idxTax_Lines AS PRIMARY UNIQUE receipt_id line_id tax_code
  . 
           
DEFINE TEMP-TABLE ttCard_info NO-UNDO SERIALIZE-NAME 'card_info'        
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>9" SERIALIZE-HIDDEN
  FIELD balance_amount AS INTEGER FORMAT ">>>>>>>9"
  FIELD card_amount AS INTEGER FORMAT ">>>>>>>9"
  FIELD issuer_id AS INTEGER FORMAT ">>>>>>>9"
  FIELD issuer_name AS CHARACTER FORMAT "x(30)"
  FIELD session_id AS CHARACTER FORMAT "x(30)"
  FIELD terminal_id AS CHARACTER FORMAT "x(30)"
  FIELD card_number AS CHARACTER FORMAT "x(30)"
  FIELD card_type AS CHARACTER FORMAT "x(30)"
  FIELD receipt AS CHARACTER FORMAT "x(30)"
  INDEX idxCard_Info AS PRIMARY receipt_id line_id
  .      
  
DEFINE TEMP-TABLE ttItem NO-UNDO SERIALIZE-NAME 'items'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>>9"
  FIELD item_id AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'id'
  FIELD quantity AS INTEGER FORMAT ">>>>>>>9"
  FIELD unit AS CHARACTER FORMAT "x(5)"
  FIELD description AS CHARACTER FORMAT "x(30)"
  FIELD description_alias AS CHARACTER FORMAT "x(30)"
  FIELD net_amount AS INTEGER FORMAT ">>>>>>>9"
  FIELD gross_amount AS INTEGER FORMAT ">>>>>>>9"
  FIELD tax_percent AS INTEGER FORMAT ">>9"
  FIELD barcode AS CHARACTER FORMAT "x(30)"
  FIELD cost_price AS INTEGER FORMAT ">>>>>>>9"
  FIELD voided AS LOG FORMAT "true/false"
  FIELD eligible_for_discount AS LOG FORMAT "true/false"
  FIELD included_in_total_discount AS LOG FORMAT "true/false"
  FIELD is_return_item AS LOG FORMAT "true/false"
  FIELD is_virtual_product AS LOG FORMAT "true/false"
  FIELD comment AS CHARACTER FORMAT "x(30)"
  FIELD lcDiscount_lines AS CLOB SERIALIZE-HIDDEN
  INDEX idxItem AS PRIMARY UNIQUE receipt_id line_id 
  .
  
DEFINE TEMP-TABLE ttItemGroups NO-UNDO SERIALIZE-NAME 'groups'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>>9"  SERIALIZE-HIDDEN
  FIELD group_id AS CHARACTER FORMAT "x(8)"  SERIALIZE-NAME 'id'
  FIELD group_name AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'name'
  INDEX idxItemGroup AS PRIMARY receipt_id line_id group_id /* Unnlater unik pga. feil i testdata. */
  .    
    
DEFINE TEMP-TABLE ttItemDimension NO-UNDO SERIALIZE-NAME 'dimension'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>>9"  SERIALIZE-HIDDEN
  FIELD cColor AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'color'
  FIELD cSize AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'size'
  FIELD cStyle AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'style'
  FIELD cConfig AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'config'
  FIELD cVariant AS CHARACTER FORMAT "x(30)"  SERIALIZE-NAME 'variant'
  INDEX idxItemDimension AS PRIMARY UNIQUE receipt_id line_id 
  .    
  
DEFINE TEMP-TABLE ttDiscount_lines NO-UNDO SERIALIZE-NAME 'discount_lines'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER FORMAT ">>>>>>>9" SERIALIZE-HIDDEN
  FIELD discount_line_id AS INTEGER  SERIALIZE-NAME 'line_id'
  FIELD amount AS INTEGER 
  FIELD DESCRIPTION AS CHARACTER 
  FIELD discount_id AS CHARACTER 
  FIELD discount_type AS CHARACTER 
  INDEX idxDiscount_lines AS PRIMARY receipt_id line_id
  . 
  
DEFINE TEMP-TABLE ttExtra_info NO-UNDO SERIALIZE-NAME 'extra_info'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD cKey AS CHARACTER
  FIELD cValue AS CHARACTER SERIALIZE-NAME 'value'
  FIELD value_type AS CHARACTER
  INDEX idxEkstra_Info AS PRIMARY receipt_id
  .
   
DEFINE TEMP-TABLE ttDiscounts NO-UNDO SERIALIZE-NAME 'discounts'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-NAME 'id'
  FIELD created_at AS CHARACTER 
  FIELD created_by AS CHARACTER 
  FIELD updated_at AS CHARACTER 
  FIELD deleted_by AS CHARACTER 
  FIELD deleted_at AS CHARACTER 
  FIELD campaign_id AS CHARACTER 
  FIELD bActive AS LOG FORMAT "true/false" SERIALIZE-NAME 'active'
  FIELD bPrivate AS LOG FORMAT "true/false" SERIALIZE-NAME 'private'
  FIELD updated_by AS CHARACTER
  FIELD cType AS CHARACTER SERIALIZE-NAME 'type'
  FIELD cName AS CHARACTER SERIALIZE-NAME 'name'
  FIELD receipt_text AS CHARACTER
  FIELD visible_from AS CHARACTER
  FIELD cDescription AS CHARACTER SERIALIZE-NAME 'description'  
  INDEX idxDiscounts AS PRIMARY UNIQUE receipt_id discounts_id
  .    

DEFINE TEMP-TABLE ttRequirement NO-UNDO SERIALIZE-NAME 'requirement'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD purchase_to AS CHARACTER  
  FIELD purchase_from AS CHARACTER  
  INDEX idxRequirement AS PRIMARY receipt_id discounts_id
  . 
               
DEFINE TEMP-TABLE ttLimitation NO-UNDO SERIALIZE-NAME 'limitation'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD discount_repeat_usage AS INTEGER FORMAT "->>9" 
  INDEX idxLimitation AS PRIMARY receipt_id discounts_id
  .  
   
DEFINE TEMP-TABLE ttReward NO-UNDO SERIALIZE-NAME 'reward'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD cType AS CHARACTER   SERIALIZE-NAME 'type' 
  FIELD iValue AS INTEGER   SERIALIZE-NAME 'value'
  INDEX idxReward AS PRIMARY UNIQUE receipt_id discounts_id cType
  . 
   
DEFINE TEMP-TABLE ttRefs NO-UNDO SERIALIZE-NAME 'refs'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD refs_id AS CHARACTER  SERIALIZE-NAME 'id'
  FIELD usage AS INTEGER 
  FIELD amount AS INTEGER 
  INDEX idxRefs AS PRIMARY UNIQUE receipt_id discounts_id refs_id
  . 
  
DEFINE TEMP-TABLE ttDiscountItems NO-UNDO SERIALIZE-NAME 'items'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD refs_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD line_id AS INTEGER 
  FIELD amount AS INTEGER 
  INDEX idxDiscountItems AS PRIMARY UNIQUE receipt_id discounts_id refs_id line_id
  . 
   
DEFINE TEMP-TABLE ttStatistics NO-UNDO SERIALIZE-NAME 'statistics'
  FIELD receipt_id AS CHARACTER FORMAT "x(30)" SERIALIZE-HIDDEN
  FIELD discounts_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD refs_id AS CHARACTER SERIALIZE-HIDDEN
  FIELD usage AS INTEGER 
  FIELD amount AS INTEGER 
  INDEX idxStatistics AS PRIMARY receipt_id discounts_id refs_id
  .
  
   
   