
/*------------------------------------------------------------------------
    File        : dsSession.i
    Purpose     : 

    Syntax      :

    Description : Datasett som inneholder elementene i en session response.

    Author(s)   : Tom Nøkleby
    Created     : Thu Oct 22 09:12:56 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE DATASET dsOrder SERIALIZE-HIDDEN
  FOR ttOrder, ttCustomer, ttItems, ttGroups, ttShipping_option, ttPick_Up_Address
  DATA-RELATION drCustomerr FOR ttOrder, ttCustomer RELATION-FIELDS (customer_id, customer_id) NESTED
  DATA-RELATION drOrder FOR ttOrder, ttItems RELATION-FIELDS (orderid, orderid) NESTED
  DATA-RELATION drShipping_option FOR ttOrder, ttShipping_option RELATION-FIELDS (orderid, orderid) NESTED
  DATA-RELATION drPick_Up_Address FOR ttShipping_option, ttPick_Up_Address RELATION-FIELDS (orderid, orderid, shipping_id, shipping_id, line_id, line_id) NESTED
  DATA-RELATION drItemGroupe FOR ttItems, ttGroups RELATION-FIELDS (Group_Id, Group_Id) NESTED.
  .
