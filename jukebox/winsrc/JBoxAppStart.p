/* Startup file used with the appfarm distribution system. 
   A sample startup file for standard startup: winsrc/template/JBoxStartup.p 
-------------------------------------------------------------------------------*/ 
DEF INPUT PARAM ibTest         AS LOG  NO-UNDO.
DEF INPUT PARAM icAppTitle     AS CHAR NO-UNDO.
DEF INPUT PARAM icLanguages    AS CHAR NO-UNDO.
DEF INPUT PARAM icBaseLanguage AS CHAR NO-UNDO.
DEF INPUT PARAM icPrefLanguage AS CHAR NO-UNDO.
DEF INPUT PARAM icBehaviour    AS CHAR NO-UNDO.
DEF INPUT PARAM icUserId       AS CHAR NO-UNDO.
DEF INPUT PARAM icRoot         AS CHAR NO-UNDO.

DEF VAR bOk              AS LOG    NO-UNDO INIT TRUE.
DEF VAR hServer          AS HANDLE NO-UNDO.
DEF VAR hMenu            AS HANDLE NO-UNDO.
DEF VAR cPanelFile       AS CHAR   NO-UNDO.
DEF VAR cMainMenuProg    AS CHAR   NO-UNDO.
DEF VAR bRestart         AS LOG    NO-UNDO.
DEF VAR bStartRibbon     AS LOG    NO-UNDO.
DEF VAR hRunWhenIdle     AS HANDLE NO-UNDO.
DEF VAR cSpecialPref     AS CHAR   NO-UNDO.
DEF VAR cMainMenuType    AS CHAR   NO-UNDO.
DEF VAR hWaitForProc     AS HANDLE NO-UNDO.
DEF VAR hFocusTimer      AS HANDLE NO-UNDO.
DEF VAR cOsVer           AS CHAR   NO-UNDO.
DEF VAR cStartupProc     AS CHAR   NO-UNDO.
DEF VAR iClientXsize     AS INT NO-UNDO INIT 1200.
DEF VAR iClientYsize     AS INT NO-UNDO INIT 800.
/*DEF VAR oTaskbarManager  AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager NO-UNDO.*/

cStartupProc = PROGRAM-NAME(2).

SESSION:SUPPRESS-WARNINGS = YES.
DEFAULT-WINDOW:HIDDEN =  YES.

/*IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO: */
/*    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance .*/
/*    oTaskbarManager:ApplicationId = "Chemistry.jboxapp":U .                         */
/*END.                                                                                */


IF SEARCH(".\specialpref.txt") NE ? THEN DO:
  INPUT FROM .\specialpref.txt.  
  REPEAT:
    IMPORT UNFORMATTED cSpecialPref.
    IF ENTRY(1,cSpecialPref) = "menu" THEN
      cMainMenuType = ENTRY(2,cSpecialPref) NO-ERROR.
  END.
END.
    
RUN JBoxLoadLib.p ("ResizeLib.p,JBoxASlib.p,JBoxUIlib.p,JBoxFUlib.p" + (IF SEARCH("controls.dll") NE ? AND cMainMenuType NE "plain" THEN ",controls.p" ELSE "")).

ON 'close':U OF THIS-PROCEDURE RETURN.  

DYNAMIC-FUNCTION("setLanguages",icLanguages).
DYNAMIC-FUNCTION("setAppTitle",icAppTitle).
DYNAMIC-FUNCTION("setBaseLanguageCode",icBaseLanguage).

DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session"). 

{incl/appstartconnect.i}
RUN JBoxWebLogin.w (icUserId,icPrefLanguage,OUTPUT bOk).
IF bOk THEN DO:


/*  IF SEARCH("controls.dll") NE ? THEN DO:                              */
/*    RUN JBoxJlwTimer.w PERSIST SET hFocusTimer ("CheckTimerEvent",500).*/
/*    DYNAMIC-FUNCTION("SetTimerName" IN hFocusTimer,"FocusTimer").      */
/*    RUN SuspendNamedTimer IN hFocusTimer ("FocusTimer",YES).           */
/*  END.                                                                 */
  {incl/appstartpremenu.i}
  DYNAMIC-FUNCTION("setBehaviour",icBehaviour).      

  DYNAMIC-FUNCTION("getSessionData").
  
  DYNAMIC-FUNCTION("NewObject",SESSION:FIRST-PROCEDURE,SESSION:FIRST-PROCEDURE,"Global variables").
  DYNAMIC-FUNCTION("setObjectName",SESSION:FIRST-PROCEDURE,"session:first-procedure").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuProg",cMainMenuProg).

/*   DYNAMIC-FUNCTION("BuildTableCache","JBoxUserSetting|WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'" */
/*                  + ";jbserv_getfieldcache.p"                                                                         */
/*                  ).                                                                                                  */

/*   IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyUser",                            */
/*                               "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")     */
/*                             + " AND cJBoxUserID = '" + DYNAMIC-FUNCTION("getASuserId") + "'" */
/*                               ,"bSuperUserCompany")) THEN                                    */
/*     RUN JboxRunWhenIdle.p PERSIST SET hRunWhenIdle NO-ERROR.                                 */

/*  {incl/appstartpremenu.i}*/

  IF DYNAMIC-FUNCTION("getFieldValues","JBoxMenu",
                      "WHERE iJBoxMenuId LT " + STRING(iMenuId) + " AND cMenuType = 'menu'","iJBoxMenuId") = ? THEN
    cMainMenuType = DYNAMIC-FUNCTION("getUserSetting","SESSION","","","MainMenuType").

  IF cMainMenuType = "" THEN
    cMainMenuType = DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE iJBoxMenuId = " + STRING(iMenuId),"cLaunchType").

  IF (cMainMenuType = "" OR cMainMenuType = ?) AND DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE iJBoxMenuId = " + STRING(iMenuId),"cLaunch") = "JBoxJlwButtomMenuPanel.w" THEN
    ASSIGN cMainMenuProg = "JBoxJlwDynMenu.w"
           cMainMenuType = "buttonpanel".

  IF cMainMenuProg = "" THEN DO:
    IF cMainMenuType = "" THEN DO:
      cMainMenuType = DYNAMIC-FUNCTION("getUserSetting","SESSION","","","MainMenuType").
      IF cMainMenuType = "" THEN DO:
        IF (SEARCH("assemblies\assemblies.xml") NE ? OR SEARCH("assemblies.xml") NE ?)
            AND PROVERSION GE "10.2B" 
            AND (SEARCH("JBoxAppStart_oo.r") NE ? OR SEARCH("JBoxAppStart_oo.p") NE ?) THEN
          cMainMenuType = "Ribbon".
        ELSE IF SEARCH("controls.dll") NE ? THEN
          cMainMenuType = "Nav-bar".
        ELSE
          cMainMenuType = "plain".
      END.
    END.
    IF cMainMenuType = "Ribbon" THEN DO:
      RUN getWinOsVer.p (OUTPUT cOsVer).
      IF cOsVer GE "6" THEN
        cMainMenuProg = "JBoxRibbonMenu.p".
      ELSE
        ASSIGN cMainMenuType = "Nav-bar"
               cMainMenuProg = "JBoxJlwDynMenu.w".
    END.
    ELSE IF cMainMenuType = "Plain" THEN
      cMainMenuProg = "JBoxDynMenu.w".
    ELSE
      cMainMenuProg = "JBoxJlwDynMenu.w".
  END.
  IF SEARCH("controls.dll") NE ? AND cMainMenuType NE "plain" THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"tabfolderprog","JBoxJLWtabFolder.w").

  RUN VALUE(cMainMenuProg) PERSIST SET hMenu.

  CASE cMainMenuType:
    WHEN "buttonpanel" THEN DYNAMIC-FUNCTION("setMenuStyle" IN hMenu,0).
    WHEN "nav-bar"     THEN DYNAMIC-FUNCTION("setMenuStyle" IN hMenu,1).
    WHEN "treeview"    THEN DYNAMIC-FUNCTION("setMenuStyle" IN hMenu,2).
    WHEN "ribbon"      THEN DO:
       IF SESSION:HEIGHT-PIXELS LT 800 THEN
         DYNAMIC-FUNCTION("setRibbonMinimized" IN hMenu,YES).
       DYNAMIC-FUNCTION("setClientSize" IN hMenu,iClientXsize,iClientYsize).
    END.
  END CASE.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuType",cMainMenuType).

  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",STRING(hMenu)).

  IF cMainMenuType = "plain" AND cPanelFile NE "" THEN
    DYNAMIC-FUNCTION("setPanelFile" IN hMenu,cPanelFile) NO-ERROR. /* Optional, you may create your own panel for buttons and stuff */
  ELSE IF cMainMenuType = "buttonPanel" THEN
    DYNAMIC-FUNCTION("setPanelFile" IN hMenu,"JBoxJlwButtonMenuPanel.w") NO-ERROR. 
  
  SESSION:SET-WAIT-STATE("General").

  RUN InitializeObject IN hMenu(iMenuId).

  SUBSCRIBE TO "InvalidateHandle" IN hMenu.

  IF cMainMenuType = "Ribbon" THEN DO:
    DYNAMIC-FUNCTION("setWindowTitle" IN hMenu,icAppTitle + " [" + DYNAMIC-FUNCTION("getCompanyName") + "]").
    DYNAMIC-FUNCTION("setStatusText" IN hMenu,DYNAMIC-FUNCTION("getASuserName")).
    DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
/*     DYNAMIC-FUNCTION("setAppStyleSheet" IN hMenu,"JukeBoxGrey.isl").  */
    IF NOT OS-GETENV("SESSIONNAME") BEGINS "RDP" THEN
      DYNAMIC-FUNCTION("setAppStyleSheet" IN hMenu,"vs2008_test.isl").
  END.

  {incl/appstartpostmenu.i}

  SESSION:SET-WAIT-STATE("").

  IF cMainMenuType = "Ribbon" THEN 
    RUN JBoxAppStart_oo.p PERSIST SET hWaitForProc. 
  ELSE DO:
    IF cMainMenuType = "Plain" THEN DO:
      hMenu:CURRENT-WINDOW:HIDDEN = FALSE.
      hMenu:CURRENT-WINDOW:WINDOW-STATE = 3.
      hMenu:CURRENT-WINDOW:MOVE-TO-TOP().
    END.

    IF VALID-HANDLE(hRunWhenIdle) THEN
      REPEAT:
        WAIT-FOR "close" OF THIS-PROCEDURE PAUSE 1000.
        RUN StartProcedures IN hRunWhenIdle NO-ERROR.
      END.
    ELSE
      RUN JBoxAppStart_wait.p PERSIST SET hWaitForProc.
  END.

END.
ELSE IF NOT ibTest THEN QUIT.

PROCEDURE RestartMenu: 
  DEF INPUT PARAM iiRestartMenu AS INT NO-UNDO.

  DYNAMIC-FUNCTION("msetAttribute",SESSION:FIRST-PROCEDURE,"*","").

  IF iiRestartMenu = 0 THEN iiRestartMenu = iMenuId.

  IF DYNAMIC-FUNCTION("getFieldValues","JBoxMenu",
                      "WHERE iJBoxMenuId LT " + STRING(iiRestartMenu) + " AND cMenuType = 'menu'","iJBoxMenuId") = ? THEN
    cMainMenuType = DYNAMIC-FUNCTION("getUserSetting","SESSION","","","MainMenuType").
  
  IF cMainMenuType = "" THEN
    cMainMenuType = DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE iJBoxMenuId = " + STRING(iiRestartMenu),"cLaunchType").

  cMainMenuProg = "".

  IF (cMainMenuType = "" OR cMainMenuType = ?) AND DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE iJBoxMenuId = " + STRING(iiRestartMenu),"cLaunch") = "JBoxJlwButtomMenuPanel.w" THEN
    ASSIGN cMainMenuProg = "JBoxJlwDynMenu.w"
           cMainMenuType = "buttonpanel".

  IF cMainMenuProg = "" THEN DO:
    IF cMainMenuType = "" THEN DO:
      cMainMenuType = DYNAMIC-FUNCTION("getUserSetting","SESSION","","","MainMenuType").
      IF cMainMenuType = "" THEN DO:
        IF (SEARCH("assemblies\assemblies.xml") NE ? OR SEARCH("assemblies.xml") NE ?)
            AND PROVERSION GE "10.2B" 
            AND (SEARCH("JBoxAppStart_oo.r") NE ? OR SEARCH("JBoxAppStart_oo.p") NE ?) THEN
          cMainMenuType = "Ribbon".
        ELSE IF SEARCH("controls.dll") NE ? THEN
          cMainMenuType = "Nav-bar".
        ELSE
          cMainMenuType = "plain".
      END.
    END.
    IF cMainMenuType = "Ribbon" THEN
      cMainMenuProg = "JBoxRibbonMenu.p".
    ELSE IF cMainMenuType = "Plain" THEN
      cMainMenuProg = "JBoxDynMenu.w".
    ELSE
      cMainMenuProg = "JBoxJlwDynMenu.w".
  END.

  IF VALID-HANDLE(hRunWhenIdle) THEN DO:
    APPLY "close" TO hRunWhenIdle.
    DELETE PROCEDURE hRunWhenIdle.
  END.


  RUN VALUE(cMainMenuProg) PERSIST SET hMenu.

  CASE cMainMenuType:
    WHEN "buttonpanel" THEN DYNAMIC-FUNCTION("setMenuStyle" IN hMenu,0).
    WHEN "nav-bar"     THEN DYNAMIC-FUNCTION("setMenuStyle" IN hMenu,1).
    WHEN "treeview"    THEN DYNAMIC-FUNCTION("setMenuStyle" IN hMenu,2).
    WHEN "ribbon"      THEN DO:
       IF SESSION:HEIGHT-PIXELS LT 800 THEN DO:
         DYNAMIC-FUNCTION("setRibbonMinimized" IN hMenu,YES).
         DYNAMIC-FUNCTION("setClientSize" IN hMenu,1024,768).
       END.
    END.
  END CASE.
    
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuType",cMainMenuType).

  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",STRING(hMenu)).

  IF cMainMenuType = "plain" AND cPanelFile NE "" THEN
    DYNAMIC-FUNCTION("setPanelFile" IN hMenu,cPanelFile) NO-ERROR. /* Optional, you may create your own panel for buttons and stuff */
  ELSE IF cMainMenuType = "buttonPanel" THEN
    DYNAMIC-FUNCTION("setPanelFile" IN hMenu,"JBoxJlwButtonMenuPanel.w") NO-ERROR. 

  RUN InitializeObject IN hMenu(iiRestartMenu).

  SUBSCRIBE TO "InvalidateHandle" IN hMenu.
  bRestart = YES.

  IF cMainMenuType = "Ribbon" THEN DO:
    DYNAMIC-FUNCTION("setWindowTitle" IN hMenu,icAppTitle + " [" + DYNAMIC-FUNCTION("getCompanyName") + "]").
    DYNAMIC-FUNCTION("setStatusText" IN hMenu,DYNAMIC-FUNCTION("getASuserName")).
    DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
    IF NOT OS-GETENV("SESSIONNAME") BEGINS "RDP" THEN
      DYNAMIC-FUNCTION("setAppStyleSheet" IN hMenu,"vs2008_test.isl").
  END.
  ELSE IF cMainMenuType = "Plain" THEN DO:
    hMenu:CURRENT-WINDOW:HIDDEN = FALSE.
    hMenu:CURRENT-WINDOW:WINDOW-STATE = 3.
    hMenu:CURRENT-WINDOW:MOVE-TO-TOP().
  END.


  {incl/appstartpostmenu.i}

END PROCEDURE.    

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihMenu AS HANDLE NO-UNDO.
  
/*   IF ihMenu:FILE-NAME = "JBoxRibbonMenu.p" THEN */
  DELETE PROCEDURE ihMenu NO-ERROR.    
  
  IF NOT bRestart THEN DO:
    DYNAMIC-FUNCTION("runproc","jbserv_logout.p","",?).


    IF cMainMenuType = "Ribbon" THEN 
      System.Windows.Forms.Application:Exit(). 
    ELSE APPLY "close" TO THIS-PROCEDURE.
  END.
  bRestart = NO.
END PROCEDURE.

PROCEDURE SetRestart:
  DEF INPUT PARAMETER ibRestart AS LOG NO-UNDO.
  bRestart = ibRestart.
END PROCEDURE.

PROCEDURE CheckTimerEvent:
  DEF VAR hStatusFillIn AS HANDLE NO-UNDO.
  DEF VAR cFocusName    AS CHAR   NO-UNDO.
  IF FOCUS NE ? AND STRING(FOCUS) = DYNAMIC-FUNCTION("getAttribute",SESSION,"nextFocusWidget") THEN DO:
    cFocusName = FOCUS:NAME.
    PROCESS EVENTS.
    APPLY "entry" TO FOCUS.
    PROCESS EVENTS.
/*     MESSAGE "Nå da??"       cFocusName     */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  END.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"nextFocusWidget","").
  RUN SuspendNamedTimer IN hFocusTimer ("FocusTimer",YES).

/*     hStatusFillIn = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FOCUS:WINDOW,"StatusBarTextFillIn1")) NO-ERROR. */
/*     IF VALID-HANDLE(hStatusFillIn) THEN                                                                           */
/*       hStatusFillIn:SCREEN-VALUE = FOCUS:HELP.                                                                    */
/*  RUN SetWinMenuActionState IN hMenu. */
END PROCEDURE.


/* IF VALID-HANDLE(FOCUS) THEN                */
/*   ON "alt-l" OF FOCUS                      */
/*   DO:                                      */
/*     MESSAGE "focus"                        */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*     RETURN.                                */
/*   END.                                     */
/* ON 'ctrl-y':U ANYWHERE                   */
/* DO:                                      */
/*   MESSAGE "ctrl-y"                       */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*   RETURN.                                */
/* END.                                     */
/*                                          */
