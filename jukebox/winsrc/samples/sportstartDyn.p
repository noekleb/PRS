DEF VAR bOk              AS LOG NO-UNDO INIT TRUE.
DEF VAR hServer          AS HANDLE NO-UNDO.
DEF VAR hMainMenu        AS HANDLE NO-UNDO.

RUN JBoxLoadLib.p ("JBoxUIlib.p,JBoxASlib.p,ResizeLib.p,JBoxFUlib.p").

DYNAMIC-FUNCTION("setLanguages","EN").
DYNAMIC-FUNCTION("setAppTitle","Sports2000").
DYNAMIC-FUNCTION("setBaseLanguageCode","EN").
DYNAMIC-FUNCTION("setLanguageCode","EN").
DYNAMIC-FUNCTION("setAttribute",SESSION,"copytoolbartobrowse","yes").
DYNAMIC-FUNCTION("setAttribute",SESSION,"windowsbrowse","yes").

/*
/* Set spesific Toolbar buttons: */
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_first","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_prev","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_next","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_last","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_browseconfig","links16.bmp"). /* bmp/table.bmp */
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_filter","links16.bmp").       /* gif/filter.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_accum","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_save","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_delete","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_close","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_save","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_excelviewer","links16.bmp").
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_moveup","links16.bmp").  /* gif/moveup.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnImg_movedown","links16.bmp"). /* gif/movedown.gif */

DYNAMIC-FUNCTION("setAttribute",SESSION,"ActiveAccumButton","links16.bmp"). /* gif/statusc.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"PassiveAccumButton","links16.bmp"). /* gif/statusu.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"ActiveFilterButton","links16.bmp"). /* gif/filterc.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"PassiveFilterButton","links16.bmp"). /* gif/filter.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"ActiveConfigButton","links16.bmp"). /* bmp/tbls.bmp */
DYNAMIC-FUNCTION("setAttribute",SESSION,"PassiveConfigButton","links16.bmp"). /* bmp/table.bmp */
DYNAMIC-FUNCTION("setAttribute",SESSION,"ActivePinButton","links16.bmp").     /* gif\pushin.gif */
DYNAMIC-FUNCTION("setAttribute",SESSION,"PassivePinButton","links16.bmp").    /* gif\pushout.gif */

/* Set specific Window icons: */
DYNAMIC-FUNCTION("setAttribute",SESSION,"WinIcon_JboxDataBrw.w","acctres.ico").  /* ico\admin%.ico */
*/


/* Reserved color name for shading browse rows: */
DYNAMIC-FUNCTION("AddColor","RowShade",230,230,230).
/* Other colors added this way are available as session attributes: "Color_"<your name> */


RUN JBoxLogin.w (OUTPUT bOk).

/*
DYNAMIC-FUNCTION("BuildTableCache","JBoxUserSetting|WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'"
/*                  + ";jbserv_getfieldcache.p"  */
                 ).
*/
/*DYNAMIC-FUNCTION("BuildTableCache","JBoxTranslation|WHERE true"). */
/* DYNAMIC-FUNCTION("BuildTableCache","get-xpower-jbox-translation.p").  */
                 

RUN JBoxDynMenu.w PERSIST SET hMainMenu.

RUN InitializeObject IN hMainMenu(0).

SUBSCRIBE TO "InvalidateHandle" IN hMainMenu.

WAIT-FOR CLOSE OF THIS-PROCEDURE.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihMenu AS HANDLE NO-UNDO.
  QUIT.
END.

