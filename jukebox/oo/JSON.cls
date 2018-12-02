 
/*------------------------------------------------------------------------
   File        : JSON
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : goo_000
   Created     : Wed Nov 08 10:17:32 CET 2017
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.*.
USING Progress.Json.ObjectModel.*.


ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JSON: 

  DEFINE VARIABLE oHeader      AS CLASS JsonObject        NO-UNDO.
  DEFINE property oObject      AS CLASS JsonObject        NO-UNDO get. private set.
  DEFINE property oArray       AS CLASS JsonArray         NO-UNDO get. set.
  DEFINE VARIABLE oParser      AS CLASS ObjectModelParser NO-UNDO.
  DEFINE VARIABLE oConstructor AS CLASS JsonConstruct     NO-UNDO.   

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC JSON (  ):
    SUPER ().
    
  END CONSTRUCTOR.


  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC JSON (ipJSON AS LONGCHAR):
    def var lc as longchar no-undo.
    SUPER ().
    fix-codepage(lc) = 'utf-8'.
    oParser = NEW ObjectModelParser ().
    oConstructor = oParser:Parse(lc).
        
    oObject = CAST(oConstructor,JsonObject).
  END CONSTRUCTOR.



  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID buildJSON(ipJSON as longchar  ):
    def var lc as longchar no-undo.
    fix-codepage(lc) = 'utf-8'.
    lc = ipJSON.
    oParser = NEW ObjectModelParser ().
    oConstructor = oParser:Parse(lc).
    oObject = CAST(oConstructor,JsonObject).
    oArray = oObject:getJsonArray('value').
    RETURN.

  END METHOD.

  
END CLASS.