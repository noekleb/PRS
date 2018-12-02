/* Get liked companies for menu
   Parameters:  <menuid>
      
   Created: 13.03.09 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO INIT YES.

DEF VAR iMenuId              AS INT    NO-UNDO.
DEF VAR hBuffJBoxAppModuleItem AS HANDLE NO-UNDO.
DEF VAR hBuffJBoxAppModule     AS HANDLE NO-UNDO.
DEF VAR hQuery             AS HANDLE NO-UNDO.

iMenuId = INT(ENTRY(1,icParam,";")).

CREATE BUFFER hBuffJBoxAppModuleItem FOR TABLE "JBoxAppModuleItem" NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  obOk = NO.  
  RETURN.
END.

CREATE BUFFER hBuffJBoxAppModule FOR TABLE "JBoxAppModule".

IF NOT ERROR-STATUS:ERROR THEN DO:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffJBoxAppModuleItem,hBuffJBoxAppModule).
  hQuery:QUERY-PREPARE("FOR EACH JBoxAppModuleItem NO-LOCK WHERE JBoxAppModuleItem.iJBoxMenuId = " 
                     + icParam + ",FIRST JBoxAppModule OF JBoxAppModuleItem NO-LOCK").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ocReturn = ocReturn + hBuffJBoxAppModule:BUFFER-FIELD("cModuleName"):BUFFER-VALUE + CHR(10).
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffJBoxAppModule.
END.

DELETE OBJECT hBuffJBoxAppModuleItem.



