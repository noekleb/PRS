/* Use in FlatViewRecord to enable lookups in dynamic filter 
   Customize this sample code: */
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupfields_Name","Customer;Name;CustNum").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupquery_Name","where false").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupreturnfield_Name","Name").

DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupfields_Lsted","Laerested;distinct Lsted;Navn").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupquery_Lsted","where true").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupreturnfield_Lsted","Lsted").

DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupfields_Salesrep","Salesrep;Salesrep;RepName").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupquery_Salesrep","where true").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupreturnfield_Salesrep","Salesrep").

DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupfields_RepName","SalesRep;RepName;SalesRep").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupquery_RepName","where true").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupreturnfield_RepName","RepName").

DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupfields_Itemnum","Item;Itemnum;ItemName").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupquery_Itemnum","where false").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupreturnfield_Itemnum","Itemnum").

DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupfields_ItemName","Item;ItemName;Itemnum").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupquery_ItemName","where false").
DYNAMIC-FUNCTION("setAttribute",{1},"filterlookupreturnfield_ItemName","ItemName").

/* Dropdown values can either be retrieved from the database or hardcoded (no table for order status in Sports2000): */
/* DYNAMIC-FUNCTION("setAttribute",{1},"filterdropdownfields_OrderStatus","OrderStatus;OrderStatus;OrderStatus"). */
/* DYNAMIC-FUNCTION("setAttribute",{1},"filterdropdownquery_OrderStatus","where true").                   */
DYNAMIC-FUNCTION("setAttribute",{1},"filterdropdownvaluelist_OrderStatus","Ordered|Ordered|Back Ordered|Back Ordered|Partially shipped|Partially shipped|Shipped|Shipped").

DYNAMIC-FUNCTION("setAttribute",{1},"filterdropdownvaluelist_OrderLineStatus","Ordered|Ordered|Back Ordered|Back Ordered|Partially shipped|Partially shipped|Shipped|Shipped").
