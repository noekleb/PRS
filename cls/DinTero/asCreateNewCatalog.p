/*------------------------------------------------------------------------
    File        : asCreateNewCatalog.p
    Purpose     : Oppretter produktkatalog.

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : Thu Feb 13 10:03:15 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  {cls\dintero\ttCustomerObj.i}
  {cls\dintero\dsCustomerObj.i}

  DEFINE INPUT  PARAMETER cCatalog_Id AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER cCatalog_Name AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.  

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
  rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
  rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

  ASSIGN 
    bTest = FALSE
    cLogg = 'asCreateNewCatalog' + REPLACE(STRING(TODAY,'99/99/9999'),'/','')
    .

  IF rCustomerDintero:CreateNewCatalog(INPUT cCatalog_Id, 
       INPUT cCatalog_Name ) THEN 
    bTest = TRUE.

RETURN.

/* **********************  Internal Procedures  *********************** */



