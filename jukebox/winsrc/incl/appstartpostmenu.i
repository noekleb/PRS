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
      JBoxMainMenu:Instance:StartMenu(iLastMenuId).
/*      RUN StartMenuId IN hMenu (iLastMenuId).*/
    ELSE DO:
      cPanelUrl = DYNAMIC-FUNCTION ("getStartPanelURL" IN hMenu).
      IF cPanelURL NE "" THEN 
        RUN ViewStartPanel IN hMenu.
    END.
  END.
END.


