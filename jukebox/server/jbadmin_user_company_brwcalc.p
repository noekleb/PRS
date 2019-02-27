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
DEF INPUT PARAM  icUserId     AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

FIND FIRST JBoxUser NO-LOCK
     WHERE JBoxUser.cJBoxUserId = icUserId
     NO-ERROR.
IF AVAIL JBoxUser THEN DO:
  IF JBoxUser.bSuperUser THEN ocReturn = "*".
  ELSE
    FOR EACH JBoxCompanyUser NO-LOCK
        WHERE JBoxCompanyUser.cJBoxUserId = icUserId
       ,FIRST JBoxCompany NO-LOCK
              WHERE JBoxCompany.iJBoxCompanyId = JBoxCompanyUser.iJBoxCompanyId
        :
      ocReturn = ocReturn + (IF ocReturn NE "" THEN ", " ELSE "") + JBoxCompany.cCompanyName.
    END.
END.
