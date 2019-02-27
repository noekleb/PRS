/*if program-name(2) begins "adecomm" then do:*/
&IF DEFINED(UIB_is_Running) NE 0 &THEN
/*   DYNAMIC-FUNCTION("setAppserviceId","<appserviceid>").           */
  DYNAMIC-FUNCTION("setASUserId",IF USERID(LDBNAME(1)) NE "" THEN USERID(LDBNAME(1)) ELSE "testuser","<username>").  
  DYNAMIC-FUNCTION("setSessionId","validsession").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","super").
/*  DYNAMIC-FUNCTION("setCompanyId","1"). */                  
  DYNAMIC-FUNCTION("setLanguageCode","EN").
  DYNAMIC-FUNCTION("setBehaviour",
                    "DefaultSortFont|6," +   
                    "DefaultSortColor|15," + 
                    "BrowseSearchDefault|goto," +   /* alt: match */
                    "TabOnReturn|yes," +       
                    "SetSortLabel|yes"
                    ).      
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|ServerQuery.log," +   
                    "TransLogFile|ServerTrans.log"
                    ). 


  DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"useAdvGui","yes").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"btnGroupEditDetails","new,edit,copy,undo,save,delete").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"tbEditBrowse","new,copy,delete").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"btnGroupNavBrowse","filter,browseconfig,excel").

  DYNAMIC-FUNCTION("getSessionData").

  /* Reserved color name for shading browse rows: */
  DYNAMIC-FUNCTION("AddColor","RowShade",240,240,240).
  /* Other colors added this way are available as session attributes: "Color_"<your name> */

  DYNAMIC-FUNCTION("setAttribute",SESSION,"shadedRows","yes").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"btnPanelHeight","small").

  IF SEARCH("controls.dll") NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"tabFolderProg","JBoxJLWtabFolder.w").

/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpfile","http://217.8.138.173/appfarm/webclient/help/help.html"). */
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpdir","http://217.8.138.173/appfarm/webclient/help/").           */
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"about1file","http://217.8.138.173/appfarm/webclient/help/about.html").     */
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"UseHelpLanguageSubCat","yes"). */
/*   cPanelFile = "JBoxdynMenuPanel.w" */

&ENDIF
/*end.*/

/* The following is not related to "devmode" only. It is placed here since custdevmode is included in every template 
   Note that both the window container and any suppressed windows containing .net controls must have the preprosessor
   defined in definitions: 
   &SCOPED-DEFINE AdvGuiWin   
*/

&IF DEFINED(AdvGUIWin) &THEN
  &IF DEFINED(WINDOW-NAME) NE 0 &THEN
    DEF VAR hWrapInForm AS HANDLE NO-UNDO.  
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"UseAdvGui") = "yes" THEN DO:
      DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"advGuiWin","yes").
      PUBLISH "IsWindowWrapped" ({&WINDOW-NAME},OUTPUT hWrapInForm).
      IF NOT VALID-HANDLE(hWrapInForm) THEN
        RUN JBoxWrapWindowInForm.p PERSISTENT SET hWrapInForm (THIS-PROCEDURE,{&WINDOW-NAME}).
      THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hWrapInForm).
    END.
  &ENDIF
&ENDIF
