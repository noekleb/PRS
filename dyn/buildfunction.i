FUNCTION buildFilter
RETURNS CHARACTER
  ( INPUT icWhere      AS CHAR,
    INPUT ihField      AS HANDLE, 
    INPUT icQueryField AS CHAR,
    INPUT icOperator   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    cWhere = buildFilter(cWhere,varetekst:HANDLE,'beskr','BEGINS').      
    cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraVPIdato:HANDLE,'VPIdato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilVPIdato:HANDLE,'VPIdato','LE').
    cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').
    
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.
    cReturn = ''.

    IF ihField:INPUT-VALUE = '' OR ihField:INPUT-VALUE = ? THEN
      RETURN cReturn.

   CASE icOperator:
     WHEN 'BEGINS' OR WHEN 'MATCHES' THEN
     DO:
       ASSIGN 
         cReturn = (IF INDEX(ihField:INPUT-VALUE,'*') GT 0 THEN
                      (icQueryField + ' MATCHES ' + QUOTER(ihField:INPUT-VALUE + '*'))
                    ELSE 
                      (icQueryField + ' BEGINS ' + QUOTER(ihField:INPUT-VALUE))).
     END.
     WHEN 'CONTAINS' THEN
     DO:
       ASSIGN 
         cReturn = (icQueryField + ' CONTAINS ' + QUOTER(ihField:INPUT-VALUE)).
     END.

     WHEN '=' OR WHEN 'EQ' OR WHEN '<' OR WHEN 'LT' OR WHEN '<=' 
              OR WHEN 'LE' OR WHEN '>' OR WHEN 'GT' OR WHEN '>=' 
              OR WHEN 'GE' OR WHEN '<>' OR WHEN 'NE' THEN
     DO:
       /*INTEGER = 0 blir ikke tatt med*/
       IF CAN-DO('INTEGER,DECIMAL',ihField:DATA-TYPE) AND ihField:INPUT-VALUE = 0 THEN
         RETURN cReturn.
       ASSIGN 
         cReturn = IF CAN-DO('CHAR,CHARACTER',ihField:DATA-TYPE) AND INDEX(ihField:FORMAT,'9') LE 0 THEN
                     (icQueryField + ' ' + icOperator + ' ' + QUOTER(ihField:INPUT-VALUE))
                   ELSE IF CAN-DO('DATE',ihField:DATA-TYPE) THEN
                     (icQueryField + ' ' + icOperator + ' DATE(' + QUOTER(ihField:INPUT-VALUE) + ')')
                   ELSE
                     (icQueryField + ' ' + icOperator + ' ' + ihField:INPUT-VALUE).


     END.          
   END CASE.
   IF icWhere BEGINS ' WHERE' THEN
     cReturn = ' AND ' + cReturn.
   ELSE
     cReturn = ' WHERE ' + cReturn.
   RETURN cReturn.
END FUNCTION.
