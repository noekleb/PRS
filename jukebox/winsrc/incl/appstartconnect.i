/* Connection parameters for the application appserver. Used in startup procedure */
DO:
  IF CAN-QUERY(SESSION,"WC-ADMIN-APP") OR ENTRY(1,SESSION:PARAM,";") MATCHES "*sports2000*" THEN DO:
    CREATE SERVER hServer.
    IF ENTRY(1,SESSION:PARAM,";") NE "" THEN
      hServer:CONNECT(ENTRY(1,SESSION:PARAM,";")) NO-ERROR.
    ELSE
  /*     hServer:CONNECT("-H aia.appfarm.no -S 31234 -AppService sports2000 -DirectConnect") NO-ERROR. */
      hServer:CONNECT("-URL http://aia.appfarm.no/aia/Aia?AppService=Sports2000") NO-ERROR.

    IF hServer:CLIENT-CONNECTION-ID = "" THEN DO:
      MESSAGE PROGRAM-NAME(1) SKIP 
              "Could not connect to Appserver:" SKIP    
              ERROR-STATUS:GET-MESSAGE(1)
              VIEW-AS ALERT-BOX ERROR.
      QUIT.
    END.
    DYNAMIC-FUNCTION("setAppserviceHandle",hServer).  
    DYNAMIC-FUNCTION("setAppserviceId",ENTRY(3,hServer:CLIENT-CONNECTION-ID,":")).
    IF ENTRY(1,SESSION:PARAM,";") NE "" THEN
      DYNAMIC-FUNCTION("setAppserviceConnectString","-H aia.appfarm.no -S 31234 -AppService sports2000 -DirectConnect") NO-ERROR.
  END.

  /* Switch off automatic menu for setting bgcolor on frames (uses the whole color table) */
  DYNAMIC-FUNCTION("setEnableColor",NO).

  DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpfile","http://aia.appfarm.no/appfarm/webclient/help/help.html").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpdir","http://aia.appfarm.no/appfarm/webclient/help/").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"about1file","http://aia.appfarm.no/appfarm/webclient/help/about.html").
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"UseHelpLanguageSubCat","yes"). */
/*   cPanelFile = "JBoxdynMenuPanel.w" */

  /* Use .net components where available in Gui */
  DYNAMIC-FUNCTION("setAttribute",SESSION,"useAdvGui","yes").

  /* Use Excel .net viewer: */
  DYNAMIC-FUNCTION("setAttribute",SESSION,"excelViewer","JBoxSpreadSheetViewer.w").

  /* Reserved color name for shading browse rows: */
  DYNAMIC-FUNCTION("AddColor","RowShade",240,240,240) NO-ERROR.
  /* Other colors added this way are available as session attributes: "Color_"<your name> */

  DYNAMIC-FUNCTION("setAttribute",SESSION,"shadedRows","yes").

  /* Add a predefined button sequence to the session: */
  DYNAMIC-FUNCTION("setAttribute",SESSION,"btnGroupNavBrowse","filter,browseconfig,excel").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"btnPanelHeight","small").

END.


