/* Determine startup menu for user:
   Usage:
   
  RUN JBoxJlwDynMenu.w PERSISTENT SET hDynMenu.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",hDynMenu).

  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  ELSE DO:
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen meny er definert for bruker","","").  */
/*     QUIT. */
  END.

  RUN InitializeObject IN hDynMenu (iMenuId).
  
  Created 01 may 08 by brynjar@chemistry.no
-----------------------------------------------------------------------*/
                                                                         
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix         AS INT  NO-UNDO.
DEF VAR iMenuId    AS INT  NO-UNDO.
DEF VAR cUserId    AS CHAR NO-UNDO.

cUserId = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE).

FIND FIRST JBoxMenu NO-LOCK
    WHERE JBoxMenu.cMenuType = "menu" AND
          JBoxMenu.cMenuNumber = (icParam) NO-ERROR.
IF AVAILABLE JBoxMenu THEN
    iMenuId = JBoxMenu.iJBoxMenuId.
         .
IF iMenuId > 0 THEN
DO:
    ASSIGN ocReturn = STRING(iMenuId)
           obOk     = YES.
END.
ELSE DO:
    ASSIGN ocReturn = STRING(iMenuId)
           obOk     = NO.
END.
RETURN.

