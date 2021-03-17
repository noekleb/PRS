
/* Connection parameters for the application appserver. Used in startup procedure */
DO:
/*  IF CAN-QUERY(SESSION,"WC-ADMIN-APP") OR ENTRY(1,SESSION:PARAM,";") NE "" THEN DO:                                              */
/*    CREATE SERVER hServer.                                                                                                       */
/*    IF ENTRY(1,SESSION:PARAM,";") NE "" THEN                                                                                     */
/*      hServer:CONNECT(ENTRY(1,SESSION:PARAM,";")) NO-ERROR.                                                                      */
/*    ELSE                                                                                                                         */
/*  /*     hServer:CONNECT("-H aia.appfarm.no -S 31234 -AppService sports2000 -DirectConnect") NO-ERROR. */                        */
/*      hServer:CONNECT("-URL http://aia.appfarm.no/aia/Aia?AppService=Sports2000") NO-ERROR.                                      */
/*                                                                                                                                 */
/*    IF hServer:CLIENT-CONNECTION-ID = "" THEN DO:                                                                                */
/*      MESSAGE PROGRAM-NAME(1) SKIP                                                                                               */
/*              "Could not connect to Appserver:" SKIP                                                                             */
/*              ERROR-STATUS:GET-MESSAGE(1)                                                                                        */
/*              VIEW-AS ALERT-BOX ERROR.                                                                                           */
/*      QUIT.                                                                                                                      */
/*    END.                                                                                                                         */
/*    DYNAMIC-FUNCTION("setAppserviceHandle",hServer).                                                                             */
/*    DYNAMIC-FUNCTION("setAppserviceId",ENTRY(3,hServer:CLIENT-CONNECTION-ID,":")).                                               */
/*    IF ENTRY(1,SESSION:PARAM,";") NE "" THEN                                                                                     */
/*      DYNAMIC-FUNCTION("setAppserviceConnectString","-H aia.appfarm.no -S 31234 -AppService sports2000 -DirectConnect") NO-ERROR.*/
/*  END.                                                                                                                           */

  /* Switch off automatic menu for setting bgcolor on frames (uses the whole color table) */
  DYNAMIC-FUNCTION("setEnableColor",NO).

  DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpfile","http://aia.appfarm.no/appfarm/webclient/help/help.html").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"defaulthelpdir","http://aia.appfarm.no/appfarm/webclient/help/").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"about1file","http://aia.appfarm.no/appfarm/webclient/help/about.html").
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"UseHelpLanguageSubCat","yes"). */
/*   cPanelFile = "JBoxdynMenuPanel.w" */

  /* Reserved color name for shading browse rows: */
  DYNAMIC-FUNCTION("AddColor","RowShade",248,248,248) NO-ERROR.
/*  DYNAMIC-FUNCTION("AddColor","RowShade",240,240,240) NO-ERROR. */
  /* Reserved for background color of edit fill-ins */
  DYNAMIC-FUNCTION("AddColor","EditBgColor",186,221,218).
  /* Other colors added this way are available as session attributes: "Color_"<your name> */
  DYNAMIC-FUNCTION("AddColor","ActiveSearchField",249,247,211).
  DYNAMIC-FUNCTION("AddColor","NotFoundSearchField",222,61,89).
  
  DYNAMIC-FUNCTION("setAttribute",SESSION,"ProgramToOpenFilTypeTif","i_view64.exe").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"useAdvGui","yes").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"shadedRows","yes").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"btnPanelHeight","small").

/*  RUN JBoxLoadLib.p ("festlib.p").*/
  
END.

PROCEDURE DDEevent:
  DEF INPUT PARAM icEvent AS CHAR NO-UNDO.
  
  DEF VAR ix       AS INT   NO-UNDO.
  DEF VAR cTable   AS CHAR  NO-UNDO.
  DEF VAR cKey     AS CHAR  NO-UNDO.
  
  DO ix = 1 TO NUM-ENTRIES(icEvent,";"):
    IF ENTRY(1,ENTRY(ix,icEvent,";"),"=") = "dde" THEN DO:
      ASSIGN cTable = ENTRY(1,ENTRY(2,ENTRY(ix,icEvent,";"),"="),"#")
             cKey   = SUBSTR(ENTRY(ix,icEvent,";"),INDEX(ENTRY(ix,icEvent,";"),"#") + 1)
  /*            cKey   = ENTRY(3,ENTRY(ix,icEvent,";"),"=") */
             .
      CASE cTable:
        WHEN "eiendom" THEN DO:
/*          JBoxMainMenu:Instance:StartTabWindow("Eien_ny.w").                                  */
/*          IF JBoxServerAPI:Instance:FIND("Eiendom","WHERE prg-nr = " + cKey) THEN             */
/*            PUBLISH "FlatViewDblClick" ("Eiendom",JBoxServerAPI:Instance:FieldValue("ROWID")).*/
          
/*          FIND FIRST Eiendom NO-LOCK                             */
/*               WHERE Eiendom.Prg-nr = INT(cKey)                  */
/*               NO-ERROR.                                         */
/*          IF AVAIL Eiendom THEN                                  */
/*            RUN FlatViewDblClick (cTable,STRING(ROWID(Eiendom))).*/
        END.
        WHEN "objekt" THEN DO:
/*          FIND FIRST Objekt NO-LOCK                              */
/*               WHERE Objekt.Prg-nr   = INTEGER(ENTRY(1,cKey,"#"))*/
/*                 AND Objekt.Lopenr   = INTEGER(ENTRY(2,cKey,"#"))*/
/*                 AND Objekt.Andelsnr = INTEGER(ENTRY(3,cKey,"#"))*/
/*               NO-ERROR.                                         */
/*          IF AVAIL Objekt THEN                                   */
/*            RUN FlatViewDblClick (cTable,STRING(ROWID(Objekt))). */
        END.
      END CASE.
      LEAVE.
    END.
  END.
END PROCEDURE.

