
/*------------------------------------------------------------------------
    File        : ttCategory.i
    Purpose     : 

    Syntax      :

    Description : Produkt kategorier. I denne leges anv-kod og hovedkategori fra PRS.

    Author(s)   : Tom Nøkleby
    Created     : Mon Dec 14 15:17:50 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCategory NO-UNDO SERIALIZE-NAME 'category'
  FIELD id AS CHARACTER
  FIELD created_at AS CHARACTER
  FIELD created_by AS CHARACTER
  FIELD updated_at AS CHARACTER
  FIELD deleted_by AS CHARACTER
  FIELD deleted_at AS CHARACTER
  FIELD category_id AS CHARACTER
  FIELD category_name AS CHARACTER
  INDEX idxCategory AS PRIMARY UNIQUE category_id
  . 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
