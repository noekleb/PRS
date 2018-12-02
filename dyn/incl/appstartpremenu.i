/* custom initialization before menu initialization (meny is started) Typically decide which menu the user has access to */

DO:
  DEF VAR iMenuId AS INT NO-UNDO.
  DEF VAR hProgBar AS HANDLE NO-UNDO.

  SESSION:SET-WAIT-STATE("general").
  RUN JBoxProgressBar.w PERSISTENT SET hProgBar.
  DYNAMIC-FUNCTION("SetStartupProgress" IN hProgBar, 5,"Starter menysystem").

  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).

  /* If any application events should be notified from the menu: " 
  IF bEventMsg THEN DO:      
    RUN setEventMsgProg  IN hDynMenu ("<program-name>").
    RUN setViewBtnEvents IN hDynMenu (YES).
  END.
  */

  iMenuId = DYNAMIC-FUNCTION("getMainMenuId" IN SOURCE-PROCEDURE) NO-ERROR.
END.

PROCEDURE FlatViewDblClick :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icBuffer   AS CHAR NO-UNDO.
DEF INPUT PARAM icRowident AS CHAR NO-UNDO.

DEF VAR bActive AS LOG NO-UNDO.
DEF VAR hProc   AS HANDLE NO-UNDO.

/*IF icBuffer = "Eiendom" THEN DO:                              */
/*  PUBLISH "getIsProcedureActive" ("Eien_ny.w",OUTPUT bActive).*/
/*  IF NOT bActive THEN DO:                                     */
/*    hProc = JBoxMainMenu:Instance:StartTabWindow("Eien_ny.w").*/
/*    RUN FlatViewDblClick IN hProc (icBuffer,icRowident).      */
/*  END.                                                        */
/*END.                                                          */
/*ELSE IF CAN-DO("Objekt,Konto",icBuffer) THEN DO:              */
/*  PUBLISH "getIsProcedureActive" ("Obje_ny.w",OUTPUT bActive).*/
/*  IF NOT bActive THEN DO:                                     */
/*    hProc = JBoxMainMenu:Instance:StartTabWindow("Obje_ny.w").*/
/*    RUN FlatViewDblClick IN hProc (icBuffer,icRowident).      */
/*  END.                                                        */
/*END.                                                          */

END PROCEDURE.

SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "FlatViewDblClick" ANYWHERE.

