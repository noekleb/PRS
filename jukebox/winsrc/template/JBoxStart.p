/* Startup template file for a JukeBox application. 
   To use JukeBox embedded in an existing application just load the libraries and set the language code
   (standards objects are developed with norwegian labels but will be swapped to english if 
    the language code isn't no,da or sv (scandinavian languages)
-------------------------------------------------------------------------------------*/

DEF VAR hResizeLib       AS HANDLE NO-UNDO.
DEF VAR hJBoxASlib       AS HANDLE NO-UNDO.
DEF VAR hJBoxFUlib       AS HANDLE NO-UNDO.
DEF VAR hJBoxUIlib       AS HANDLE NO-UNDO.

{incl/ttDataDict.i NEW}
RUN resizelib.p PERSIST SET hResizeLib.
SESSION:ADD-SUPER-PROC(hResizeLib).
RUN JBoxASlib.p PERSIST SET hJBoxASlib.
SESSION:ADD-SUPER-PROC(hJBoxASlib).
RUN JBoxUIlib.p PERSIST SET hJBoxUIlib.
SESSION:ADD-SUPER-PROC(hJBoxUIlib).
RUN JBoxFUlib.p PERSIST SET hJBoxFUlib.    /* This library requires v10 */
SESSION:ADD-SUPER-PROC(hJBoxFUlib).

/* DYNAMIC-FUNCTION("setLanguageCode","EN"). <- if the user isn't allowed to select language */

/* JukeBox things from here on: */

DEF VAR bOk              AS LOG NO-UNDO INIT TRUE.
DEF VAR hServer          AS HANDLE NO-UNDO.
DEF VAR hMenu            AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpfile","http://217.8.138.173/appfarm/webclient/help/help.html").
DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpdir","http://217.8.138.173/appfarm/webclient/help/").
DYNAMIC-FUNCTION("setAttribute",SESSION,"about1file","http://217.8.138.173/appfarm/webclient/help/about.html").
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"UseHelpLanguageSubCat","yes"). */ /* For multi language help, place help files in language coded sub-catalogs */

ON 'close':U OF THIS-PROCEDURE
  QUIT.

DYNAMIC-FUNCTION("setLanguages","EN,NO").
DYNAMIC-FUNCTION("setAppTitle","<My title>").
DYNAMIC-FUNCTION("setBaseLanguageCode","EN").

IF ENTRY(1,SESSION:PARAM,";") MATCHES "*<my appservice>*" THEN DO:
  CREATE SERVER hServer.
  hServer:CONNECT(ENTRY(1,SESSION:PARAM,";")) NO-ERROR.
  IF hServer:CLIENT-CONNECTION-ID = "" THEN DO:
    MESSAGE PROGRAM-NAME(1) SKIP 
            "Could not connect to Appserver." SKIP
            "Check connection parameters: " SESSION:PARAM
            VIEW-AS ALERT-BOX.
    QUIT.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAppserviceHandle",hServer).
    DYNAMIC-FUNCTION("setAppserviceId",ENTRY(3,hServer:CLIENT-CONNECTION-ID,":")).
  END.
END.

RUN JBoxLogin.w (OUTPUT bOk).

IF bOk THEN DO:

  DYNAMIC-FUNCTION("setBehaviour",
                    "DefaultSortFont|6," +   
                    "DefaultSortColor|15," + 
                    "BrowseSearchDefault|match," + /* alt: goto */
                    "TabOnReturn|yes," +       
                    "SetSortLabel|yes"
                    ).      

  DYNAMIC-FUNCTION("getSessionData"). /* Translations */

  /* To use the dynamic menu component: 
    - Load from dbs JboxMenu.df (replace areas to match your database) 
    - Load initial data to the menu tables from dbs */
  RUN JBoxDynMenu.w PERSIST SET hMenu.
  DYNAMIC-FUNCTION("setPanelFile" IN hMenu,"JBoxDynMenuPanel.w"). /* Optional, you may create your own panel for buttons and stuff */
  RUN InitializeObject IN hMenu (0). /* 0: Use the first menubar available in the menu table */

  /* Static menu: */
/*   RUN template/JBoxStaticMenu.w PERSIST SET hMenu. */

  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",STRING(hMenu)).

  SUBSCRIBE TO "InvalidateHandle" IN hMenu.
  
  WAIT-FOR "close" OF THIS-PROCEDURE.
END.
ELSE QUIT.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihMenu AS HANDLE NO-UNDO.

  APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.


