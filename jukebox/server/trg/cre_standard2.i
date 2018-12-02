TRIGGER PROCEDURE FOR CREATE OF {2}.
/*Makes a unique decimal object value from seqPart1 and seqPart2. To have a large decimal
  we revert the iObj2 and place it as the decimal object for fObject, iObj1=1234, iObj2=3200 gives 1234,0023 */
def var fObject  as dec  no-undo.
def var iObject1 as int  no-undo.
def var iObject2 as int  no-undo.
def var cObject1 as char no-undo.
def var cObject2 as char no-undo.

FUNCTION cnvRevertInt returns char (input ipiNumber as int) FORWARD.

ASSIGN 
  iObject1     = NEXT-VALUE(seqPart1,jukebox) 
  iObject2     = IF iObject1 = 0 THEN
                   NEXT-VALUE(seqPart2,jukebox) 
                 ELSE 
                   CURRENT-VALUE(seqPart2,jukebox) 
  iObject1 = IF iObject1 = 0 THEN NEXT-VALUE(seqPart1,jukebox) ELSE iObject1
  cObject1 = STRING(iObject1)
  cObject2 = cnvRevertInt(iObject2)
  fObject  = DEC(cObject1 + SESSION:NUMERIC-DECIMAL-POINT + cObject2) 
.
ASSIGN {2}.{1}Obj = fObject.

FUNCTION cnvRevertInt RETURNS char (input ipiNumber as int) :
  DEF VAR iCount    AS INT  NO-UNDO.
  DEF VAR cRetValue AS CHAR NO-UNDO.
  DO iCount = LENGTH(STRING(ipiNumber)) TO 1 by -1:
    cRetValue = cRetValue + SUBSTRING(STRING(ipiNumber),iCount,1).
  END.
  RETURN cRetValue.
END FUNCTION.

