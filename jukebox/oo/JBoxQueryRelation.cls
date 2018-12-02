 
 /*------------------------------------------------------------------------
    File        : JBoxQueryRelation
    Purpose     : To handle relations between query/browse/fieldmap/toolbar etc. done with linking
    Syntax      : 
    Description : 
    Author(s)   : goo
    Created     : Fri Sep 05 10:03:36 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxQueryRelation: 
  def temp-table ttQuery 
    field querySeq        as int 
    field queryTableName  as char
    field queryHdl        as handle
    field queryIsBase     as logical
    index querySeq is primary unique querySeq
    .

  define public static property instance as class JBoxQueryRelation no-undo
  get():
  if instance = ? then instance = new JBoxQueryRelation().
  return instance.
  end get.
  private set.

  define public property ThisProcedureHdl as handle no-undo get. set.
    

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR private JBoxQueryRelation ():
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose: Used to bind i.e SearchField to Query or Browse
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addRelation( input ihFrom as handle, input ihTo as handle ):
		DYNAMIC-FUNCTION("CreateObjectLink",ihFrom,ihTo).
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Add all realtion for Toolbar Browse and FieldMap, input Hdl's
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addRelationTQF(input ihToolbar as handle, input ihQuery as handle, input ihFieldMap as handle  ):
	  if ThisProcedureHdl = ? then undo, throw new Progress.Lang.AppError("ThisProcedureHdl må settes",1).
		else addRelation(ihQuery,ThisProcedureHdl).
		
    addRelation(ihFieldMap,ihQuery).
    addRelation(ihFieldMap,ihToolbar).
    addRelation(ihQuery,ihToolbar).
    
		
		RETURN.
    CATCH myError AS Progress.Lang.Error :
      MESSAGE myError:getMessage(1)
      VIEW-AS ALERT-BOX.
    END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC HANDLE getBaseQueryHdl( input icTableName as char ):
		
		DEFINE VARIABLE result AS HANDLE NO-UNDO.
    find first ttQuery where ttQuery.queryTableName = icTableName and ttQuery.queryIsBase no-lock no-error.
    result = if avail ttQuery then ttQuery.QueryHdl else ?.
		RETURN result.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addBaseQueryHdl(input icTableName as char, input ihBaseQuery as handle  ):
	  def var ix as int no-undo.
	  
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
	DESTRUCTOR PUBLIC JBoxQueryRelation ( ):

	END DESTRUCTOR.
	
	
  
END CLASS.

