
/*------------------------------------------------------------------------
    File        : dsReceipts.i
    Purpose     : Felles definisjon av datasettet.  

    Syntax      :

    Description : Datasett definisjon for kvittering til Dintero.

    Author(s)   : Tom Nøkleby
    Created     : Mon Nov 16 10:55:31 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*{cls\dintero\ttReceipts.i}*/
DEFINE DATASET dsReceipts SERIALIZE-HIDDEN
  FOR ttReceipts, 
      ttItem, 
      ttItemGroups,
      ttItemDimension,
      ttDiscount_lines, 
      ttStore,
      ttStoreAdress,
      ttDiscounts, 
      ttRequirement, 
      ttLimitation, 
      ttReward, 
      ttRefs, 
      ttDiscountItems, 
      ttStatistics, 
      ttPayment, 
      ttCard_info, 
      ttTax_Lines, 
      ttItemTax_Lines,
      ttExtra_info
         
  DATA-RELATION drStore FOR ttReceipts, ttStore RELATION-FIELDS (receipt_id, receipt_id) NESTED
  DATA-RELATION drStoreAdress FOR ttStore, ttStoreAdress RELATION-FIELDS (receipt_id, receipt_id, id, store_id) NESTED
  
  DATA-RELATION drDiscounts FOR ttReceipts, ttDiscounts RELATION-FIELDS (receipt_id, receipt_id) NESTED
  DATA-RELATION drRequirement FOR ttDiscounts, ttRequirement RELATION-FIELDS (receipt_id, receipt_id, discounts_id, discounts_id) NESTED
  DATA-RELATION drLimitation FOR ttDiscounts, ttLimitation RELATION-FIELDS (receipt_id, receipt_id, discounts_id, discounts_id) NESTED
  DATA-RELATION drReward FOR ttDiscounts, ttReward RELATION-FIELDS (receipt_id, receipt_id, discounts_id, discounts_id) NESTED
  DATA-RELATION drRefs FOR ttDiscounts, ttRefs RELATION-FIELDS (receipt_id, receipt_id, discounts_id, discounts_id) NESTED
  DATA-RELATION drDiscountItems FOR ttRefs, ttDiscountItems RELATION-FIELDS (receipt_id, receipt_id, discounts_id, discounts_id, refs_id, refs_id) NESTED
  DATA-RELATION drStatistics FOR ttRefs, ttStatistics RELATION-FIELDS (receipt_id, receipt_id, discounts_id, discounts_id, refs_id, refs_id) NESTED

  DATA-RELATION drItem FOR ttReceipts, ttItem RELATION-FIELDS (receipt_id, receipt_id) NESTED
  DATA-RELATION drItemGroups FOR ttItem, ttItemGroups RELATION-FIELDS (receipt_id, receipt_id, line_id, line_id) NESTED
  DATA-RELATION drItemDimension FOR ttItem, ttItemDimension RELATION-FIELDS (receipt_id, receipt_id, line_id, line_id) NESTED
  DATA-RELATION drDiscount_lines FOR ttItem, ttDiscount_lines RELATION-FIELDS (receipt_id, receipt_id, line_id, line_id) NESTED

  DATA-RELATION drPayment FOR ttReceipts, ttPayment RELATION-FIELDS (receipt_id, receipt_id) NESTED
  DATA-RELATION drCard_info FOR ttPayment, ttCard_info RELATION-FIELDS (receipt_id, receipt_id, line_id, line_id) NESTED

  DATA-RELATION drTax_Lines FOR ttReceipts, ttTax_Lines RELATION-FIELDS (receipt_id, receipt_id) NESTED
  DATA-RELATION drItemTax_Lines FOR ttTax_Lines, ttItemTax_Lines RELATION-FIELDS (receipt_id, receipt_id, tax_code, tax_code) NESTED

  DATA-RELATION drExtra_info FOR ttReceipts, ttExtra_info RELATION-FIELDS (receipt_id, receipt_id) NESTED
  .
