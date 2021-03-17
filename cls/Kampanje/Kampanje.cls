 
 /*------------------------------------------------------------------------
    File        : Kampanje
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Thu Oct 17 14:20:25 CEST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Kampanje.Kampanje: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

    
  CONSTRUCTOR PUBLIC Kampanje (  ):
    SUPER ().

    ASSIGN 
      cLogg = 'KampanjeRegister' + REPLACE(STRING(TODAY),'/','') 
      .
      
    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
    
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC Kampanje ( INPUT pcLogg AS CHARACTER ):
    SUPER ().

    ASSIGN 
      cLogg = pcLogg
      cLogg = IF cLogg = '' THEN 'KampanjeRegister' + REPLACE(STRING(TODAY),'/','') ELSE cLogg 
      .
      
    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
    
  END CONSTRUCTOR.





  DESTRUCTOR PUBLIC Kampanje ( ):

  END DESTRUCTOR.

END CLASS.