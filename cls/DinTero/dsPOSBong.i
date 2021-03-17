
/*------------------------------------------------------------------------
    File        : dsPOSBong.i
    Purpose     : 

    Syntax      :

    Description : Datasett definisjon for bongdatabasens bonghode/linje.

    Author(s)   : Tom Nøkleby
    Created     : Fri Nov 20 11:10:48 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*{cls\dintero\ttPOSBong.i}*/

DEFINE DATASET dsPOSBong /* SERIALIZE-HIDDEN */
  FOR ttPOSBongHode, ttPOSBongLinje   
  DATA-RELATION drPOSBong FOR ttPOSBongHode, ttPOSBongLinje RELATION-FIELDS (B_Id, B_Id) NESTED.

