/* Get liked companies for menu
   Parameters:  <menuid>
      
   Created:  13.03.09 by Brynjar Hasle                  
   Modified: 26.06.13 by Brynjar
             Support of bWriteAccess field in JBoxCompanyMenu table        
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO INIT YES.

DEF VAR iMenuId              AS INT    NO-UNDO.
DEF VAR hBuffJBoxCompanyMenu AS HANDLE NO-UNDO.
DEF VAR hBuffJBoxCompany     AS HANDLE NO-UNDO.
DEF VAR hQuery               AS HANDLE NO-UNDO.
DEF VAR hWriteAccess         AS HANDLE NO-UNDO.

iMenuId = INT(ENTRY(1,icParam,";")).

CREATE BUFFER hBuffJBoxCompanyMenu FOR TABLE "JBoxCompanyMenu" NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  obOk = NO.  
  RETURN.
END.

CREATE BUFFER hBuffJBoxCompany FOR TABLE "JBoxCompany".

IF NOT ERROR-STATUS:ERROR THEN DO:

  hWriteAccess = hBuffJBoxCompanyMenu:BUFFER-FIELD("bWriteAccess") NO-ERROR.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffJBoxCompanyMenu,hBuffJBoxCompany).
  hQuery:QUERY-PREPARE("FOR EACH JBoxCompanyMenu NO-LOCK WHERE JBoxCompanyMenu.iJBoxMenuId = " 
                     + icParam + ",FIRST JBoxCompany OF JBoxCompanyMenu NO-LOCK").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ocReturn = ocReturn + hBuffJBoxCompany:BUFFER-FIELD("cCompanyName"):BUFFER-VALUE 
             + (IF VALID-HANDLE(hWriteAccess) THEN
                 (IF hWriteAccess:BUFFER-VALUE THEN " (Write)" ELSE " (Read)")
                ELSE "")
             + CHR(10).
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffJBoxCompany.
END.

DELETE OBJECT hBuffJBoxCompanyMenu.



