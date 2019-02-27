 

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.



CLASS manager.MayFlowerData SERIALIZABLE : 

  DEFINE PUBLIC STATIC PROPERTY  iMember  AS LOGICAL    NO-UNDO GET. SET. 

/*
  DEFINE PUBLIC STATIC PROPERTY instance AS CLASS manager.MayFlowerData NO-UNDO
  GET():
     
      IF instance = ? THEN instance = NEW manager.MayFlowerData().
      RETURN instance.
  END GET.
  PRIVATE SET.
  */
  

 	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC MayFlowerData ():
	
    END CONSTRUCTOR.


    METHOD PUBLIC static LOGICAL destroy (): 
        /*DELETE OBJECT instance. */
    END. 




	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC MayFlowerData ( ):
             DELETE OBJECT THIS-OBJECT NO-ERROR. 
	END DESTRUCTOR.
	
	
  
END CLASS.

