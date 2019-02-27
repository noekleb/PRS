/* custom initialization after menu initialization (meny is started) */

DEF VAR iLastMenuId AS INT  NO-UNDO.   
DEF VAR cPanelURL   AS CHAR NO-UNDO.
DO:
  IF cMainMenuType = "Ribbon" THEN DO:
/*     DYNAMIC-FUNCTION("setAppStyleSheet" IN hMenu,"vs2008_test.isl").                                                       */
/*     DYNAMIC-FUNCTION("setWindowTitle" IN hMenu,"JukeBox Sports2000 demo").                                                 */
/*     DYNAMIC-FUNCTION("setStatusText" IN hMenu,DYNAMIC-FUNCTION("getASuserName")).                                          */
/*     DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN  "Behandle" ELSE "Actions"). */


    iLastMenuId = INTEGER(DYNAMIC-FUNCTION("getFieldValues","JBoxMenuFavorites","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "' AND cType = 'Last'","iJboxMenuId")).

    IF iLastMenuId NE 0 AND iLastMenuId NE ? THEN
      RUN StartMenuId IN hMenu (iLastMenuId).
    ELSE DO:
      cPanelUrl = DYNAMIC-FUNCTION ("getStartPanelURL" IN hMenu).
      IF cPanelURL NE "" THEN 
        RUN ViewStartPanel IN hMenu.
    END.
    
/*    JBoxMainMenu:Instance:ProgramMultiInstanceList = "Obje_ny.w".*/
    
    DYNAMIC-FUNCTION("SetStartupProgress" IN hProgBar, 100,"Starter menysystem").
    
  END.
  
END.

/*DEF VAR hDDEserver AS HANDLE NO-UNDO.                         */
/*DEF VAR cDDEmsg    AS CHAR   NO-UNDO.                         */
/*RUN JBoxJlwDDEserver.w PERSIST SET hDDEserver ("","DDEevent").*/
/*                                                              */
/*IF SEARCH(SESSION:TEMP-DIR + "ddemsg.txt") NE ? THEN DO:      */
/*  INPUT FROM VALUE(SESSION:TEMP-DIR + "ddemsg.txt").          */
/*  IMPORT UNFORMATTED cDDEmsg.                                 */
/*  INPUT CLOSE.                                                */
/*  OS-DELETE VALUE(SESSION:TEMP-DIR + "ddemsg.txt").           */
/*  RUN DDEevent (cDDEmsg).                                     */
/*END.                                                          */

DYNAMIC-FUNCTION("setCacheFieldFormat","","ob-merk","merk","x(80)").
DYNAMIC-FUNCTION("setCacheFieldFormat","","ei-merk","merk","x(80)").