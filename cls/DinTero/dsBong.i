
/*------------------------------------------------------------------------
    File        : dsBong.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : Sun Nov 15 16:53:10 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*{cls\dintero\ttBong.i}*/
DEFINE DATASET dsBong SERIALIZE-HIDDEN
  FOR ttBongHode, ttBongLinje   
  DATA-RELATION drBong FOR ttBongHode, ttBongLinje RELATION-FIELDS (B_Id, B_Id) NESTED.

