/* In order to speed up processing of calculated fields it can be crucial
   to place the procedures in a single persistent procedure that is loaded
   once before processing the records.
   To invoke:
   
   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
   
   Usage in browse (the name must not end on .p) :
   
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
   
   (vs, when calling a .p each time - : 
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total.p|Total"
   )
------------------------------------------------------------------------*/     
PROCEDURE DbAccess:
  DEF INPUT PARAM  irJBoxUser   AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam      AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

  DEF VAR hBuffer AS HANDLE NO-UNDO.
  DEF VAR bOk     AS LOG    NO-UNDO.

  FIND JBoxUser NO-LOCK
       WHERE ROWID(JBoxUser) = irJBoxUser
       NO-ERROR.
  IF AVAIL JBoxUser THEN DO:
    CREATE BUFFER hBuffer FOR TABLE icParam + "._user".
  
    bOk = hBuffer:FIND-FIRST("WHERE _userid = '" + JBoxUser.cJBoxUserId + "'") NO-ERROR.

    ocReturn = STRING(bOk).

    DELETE OBJECT hBuffer.
  END.

END PROCEDURE.

