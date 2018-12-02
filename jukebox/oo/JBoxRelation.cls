 
 /*------------------------------------------------------------------------
    File        : JBoxRelation
    Purpose     : To handle relations between query/browse/fieldmap/toolbar etc. done with linking
    Syntax      : 
    Description : 
    Author(s)   : goo
    Created     : Fri Sep 05 10:03:36 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxRelation: 
  def temp-table ttQuery 
    field querySeq        as int 
    field queryTableName  as char
    field queryHdl        as handle
    field queryIsBase     as logical
    field queryExtra      as char
    index querySeq is primary unique querySeq
    .
  def temp-table ttParameter 
    field hParameterId    as handle
    field cParameterName  as char
    field cParameterValue as char
    index iParameterId is primary unique hParameterId cParameterName
    .
    
  define public static property instance as class JBoxRelation no-undo
  get():
  if instance = ? then instance = new JBoxRelation().
  return instance.
  end get.
  private set.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR private JBoxRelation ():
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addParameter( input ihId as handle, input icName as char, icValue as char):

    find first ttParameter where ttParameter.hParameterId = ihId and cParameterName = icName no-error.
    if avail ttParameter then delete ttParameter.
    
		create ttParameter no-error.
		assign 
		  ttParameter.hParameterId    = ihId
		  ttParameter.cParameterName  = icName
		  ttParameter.cParameterValue = icValue
		  .
		  
		RETURN.

	END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC character getParameter( input ihId as handle, icName as char ):
    
    DEFINE VARIABLE result AS char NO-UNDO.
    find first ttParameter where ttParameter.hParameterId = ihId and ttParameter.cParameterName = icName no-lock no-error.
    if avail ttParameter then result = ttParameter.cParameterValue.
    else 
    do:
/*      MESSAGE 'BaseQuery med navn:' + icTableName + ' ble ikke funnet.'*/
/*      VIEW-AS ALERT-BOX.                                               */
      result = ?.
    end.
    RETURN result.

  END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC HANDLE BaseQueryHdl( input icTableName as char ):
		
		DEFINE VARIABLE result AS HANDLE NO-UNDO.
    find first ttQuery where ttQuery.queryTableName = icTableName and ttQuery.queryIsBase no-lock no-error.
    if avail ttQuery then result = ttQuery.QueryHdl.
    else 
    do:
/*      MESSAGE 'BaseQuery med navn:' + icTableName + ' ble ikke funnet.'*/
/*      VIEW-AS ALERT-BOX.                                               */
      result = ?.
    end.
		RETURN result.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addBaseQueryHdl(input icTableName as char, input ihBaseQuery as handle  ):
	  def var ix as int no-undo.
	  
	  /*Delete it if it already is avail*/
	  find first ttQuery where ttQuery.queryTableName = icTableName and ttQuery.queryIsBase no-error.
	  if avail ttQuery then delete ttQuery.
	  
	  find last ttQuery no-lock no-error.
	  if not avail ttQuery then ix = 0.
	  else ix = ttQuery.querySeq.
	  
		create ttQuery.
		assign
		  ttQuery.querySeq = ix + 1
		  ttQuery.queryHdl = ihBaseQuery
		  ttQuery.queryIsBase = true
		  ttQuery.queryTableName = icTableName
		. 
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC JBoxRelation ( ):

	END DESTRUCTOR.
	
	
  
END CLASS.

