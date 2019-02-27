DEF VAR bOk              AS LOG NO-UNDO INIT TRUE.
DEF VAR hServer          AS HANDLE NO-UNDO.
DEF VAR hMainMenu        AS HANDLE NO-UNDO.

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
                 

RUN JBoxJlwDynMenu.w PERSIST SET hMainMenu.

RUN InitializeObject IN hMainMenu(0).

SUBSCRIBE TO "InvalidateHandle" IN hMainMenu.

IF PROVERSION GE "10.2" THEN
  RUN JBoxAppStart_oo.p.
ELSE
  WAIT-FOR CLOSE OF THIS-PROCEDURE.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihMenu AS HANDLE NO-UNDO.
  PUBLISH "ReleaseProgressBar".
  QUIT.
END.

