/* custom initialization before menu initialization (meny is started) Typically decide which menu the user has access to */

DO:
  DEF VAR iMenuId AS INT NO-UNDO.

  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).

  /* If any application events should be notified from the menu: " 
  IF bEventMsg THEN DO:      
    RUN setEventMsgProg  IN hDynMenu ("<program-name>").
    RUN setViewBtnEvents IN hDynMenu (YES).
  END.
  */

  /* increase if possible client display size (for ribbon/ mdi container): */
  IF SESSION:HEIGHT-PIXELS > 800 THEN
    ASSIGN iClientYsize = SESSION:HEIGHT-PIXELS - 50
           iClientXsize = iClientYsize * 1.5.

END.


