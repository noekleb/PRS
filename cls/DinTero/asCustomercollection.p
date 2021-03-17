/*------------------------------------------------------------------------
    File        : asCustomercollection
    Purpose     : Henter medlemmer fra Dintero.

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : Thu Feb 13 10:03:15 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  {cls\dintero\ttCustomerObj.i}
  {cls\dintero\dsCustomerObj.i}

  DEFINE INPUT  PARAMETER cSearch AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntall AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cReturn AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.  

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
  rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
  rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

  ASSIGN 
    bTest = TRUE
    cLogg = 'asCustomercollection' + REPLACE(STRING(TODAY,'99/99/9999'),'/','')
    .

  IF rCustomerDintero:CustomerCollection(INPUT cSearch, 
       OUTPUT iAntall,
       OUTPUT cReturn ) THEN 
    RUN cls\dintero\customerToMedlem.p (INPUT DATASET dsCustCustomer, OUTPUT cReturn).

RETURN.

/* **********************  Internal Procedures  *********************** */



