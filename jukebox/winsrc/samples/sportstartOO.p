DEF VAR bOk              AS LOG NO-UNDO INIT TRUE.
DEF VAR hServer          AS HANDLE NO-UNDO.
DEF VAR hMainMenu        AS HANDLE NO-UNDO.
DEF VAR bRestart         AS LOG    NO-UNDO.

RUN JBoxLoadLib.p ("JBoxUIlib.p,JBoxASlib.p,ResizeLib.p,JBoxFUlib.p,controls.p").

DYNAMIC-FUNCTION("setLanguages","EN").
DYNAMIC-FUNCTION("setAppTitle","Sports2000").
DYNAMIC-FUNCTION("setBaseLanguageCode","EN").
DYNAMIC-FUNCTION("setLanguageCode","NL").
DYNAMIC-FUNCTION("setAttribute",SESSION,"copytoolbartobrowse","yes").
DYNAMIC-FUNCTION("setAttribute",SESSION,"windowsbrowse","yes").
DYNAMIC-FUNCTION("setSessionId","validsession").
DYNAMIC-FUNCTION("setCompanyId",1).

/*
/* Toolbar buttons: */
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_first","links16.bmp").

/* Window icons: */
DYNAMIC-FUNCTION("setAttribute",SESSION,"WinIcon_JboxDataBrw.w","acctres.ico").  /* ico\admin%.ico */
*/

/* Reserved color name for shading browse rows: */
DYNAMIC-FUNCTION("AddColor","RowShade",230,230,230).
/* Other colors added this way are available as session attributes: "Color_"<your name> */


RUN JBoxLogin.w (OUTPUT bOk).


DYNAMIC-FUNCTION("setAttribute",SESSION,"tabFolderProg","JBoxJLWtabFolder.w").


/* DYNAMIC-FUNCTION("getSessionData").  */


DYNAMIC-FUNCTION("BuildTableCache","JBoxUserSetting|WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'"
/*                  + ";jbserv_getfieldcache.p"  */
                 ).
/* To cache multiple tables: <table>|<criteria>;<table>|<criteria>.. */

/*DYNAMIC-FUNCTION("BuildTableCache","JBoxTranslation|WHERE true"). */
/* DYNAMIC-FUNCTION("BuildTableCache","get-xpower-jbox-translation.p").  */
                 
RUN JBoxRibbonMenu.p PERSIST SET hMainMenu.

RUN InitializeObject IN hMainMenu(0).

DYNAMIC-FUNCTION("setWindowTitle" IN hMainMenu,"Sports2000 demo").
DYNAMIC-FUNCTION("setStatusText" IN hMainMenu,DYNAMIC-FUNCTION("getASuserName")).
DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hMainMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
DYNAMIC-FUNCTION("setAppStyleSheet" IN hMainMenu,"vs2008_test.isl").
/* DYNAMIC-FUNCTION("setAppStyleSheet" IN hMainMenu,"JukeBoxGrey.isl").  */

SUBSCRIBE TO "InvalidateHandle" IN hMainMenu.

/* WAIT-FOR CLOSE OF THIS-PROCEDURE.  */

/* Use the .Net event blocking loop if the CLR is loaded. */
/* Need some dummy window for this */
/* DEF VAR oForm AS Progress.Windows.Form.  */
/* oForm = NEW Progress.Windows.Form().     */

WAIT-FOR System.Windows.Forms.Application:Run().


PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihMenu AS HANDLE NO-UNDO.
  PUBLISH "ReleaseProgressBar".
  QUIT.
END.

PROCEDURE SetRestart:
  DEF INPUT PARAMETER ibRestart AS LOG NO-UNDO.
  bRestart = ibRestart.
END PROCEDURE.


PROCEDURE RestartMenu: 
  DEF INPUT PARAM iiRestartMenu AS INT NO-UNDO.


  DYNAMIC-FUNCTION("msetAttribute",SESSION:FIRST-PROCEDURE,"*","").

  RUN JBoxRibbonMenu.p PERSIST SET hMainMenu.

    
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",STRING(hMainMenu)).

  
  RUN InitializeObject IN hMainMenu(iiRestartMenu).

  DYNAMIC-FUNCTION("setWindowTitle" IN hMainMenu,"Sports2000 demo").
  DYNAMIC-FUNCTION("setStatusText" IN hMainMenu,DYNAMIC-FUNCTION("getASuserName")).
  DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN hMainMenu,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
  DYNAMIC-FUNCTION("setAppStyleSheet" IN hMainMenu,"vs2008_test.isl").

  SUBSCRIBE TO "InvalidateHandle" IN hMainMenu.
  bRestart = YES.

END PROCEDURE.    

